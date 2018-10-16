package lm;

import lm.Charset;

typedef PatternSet = Array<Pattern>;

@:forward(length)
abstract Table(haxe.ds.Vector<Int>) {
	public function new(len) this = new haxe.ds.Vector<Int>(len);
	@:arrayAccess public inline function get(i) return this.get(i);
	@:arrayAccess public inline function set(i, v) return this.set(i, v);
	public inline function trans(state, per, term) return this.get(state * per + term);
	// Whether a state can exit. if a non-final state can exit, then it must be epsilon.
	public inline function exits(state) return this.get( exitpos(state) );
	public inline function exitpos(state) return this.length - 1 - state;
}

class LexEngine {

	public static inline var U8MAX = 0xFF;
	public static inline var U16MAX = 0xFFFF;

	var uid(default, null): Int;
	var finals: Int;
	var h: Map<String, Int>;
	var final_counter: Int;
	var lstates: List<State>;

	// for LR0Builder
	public var states(default, null): Array<State>;

	/**
	 the segment size. default is 256(Char.MAX + 1)
	*/
	public var per(default, null): Int;

	/**
	 the size of the "exit table" & "rollback table".
	*/
	public var perRB(default, null): Int;

	/**
	 format: [seg0,seg1,......,segN, rollback,rollback_len,exits]
	*/
	public var table(default, null): Table;

	/**
	 state_counter
	*/
	public var segs(default, null): Int;

	/**
	* If no valid opAssoc, then its value will be the same as segs;
	*/
	public var segsEx(default, null): Int;

	public var nrules(default, null): Int;

	public var nstates(default, null): Int;

	public var entrys(default, null): Array<{begin:Int, segs:Int, nrules:Int}>;

	public var invalid(default, null): Int;

	public function new(a: Array<PatternSet>, cmax = Char.MAX, ?lrb: lm.LR0.LR0Builder) {
		this.entrys = [];
		this.per = cmax + 1;
		this.lstates = new List();
		this.segs = 0;  // state_counter
		this.invalid = U16MAX;
		this.final_counter = U16MAX - 1; // compress it later.
		this.nrules = 0;
		var prev = 0;
		var nodes = [];
		for (pats in a) {
			this.h = new Map();
			var len = pats.length;
			nodes.resize(len);
			this.finals = len;
			this.uid = len;
			// Pattern -> NFA(nodes)
			for (i in 0...len) {
				var f = new Node(i);
				var n = initNode(pats[i], f);
				nodes[i] = n;
			}
			// NFA -> DFA
			compile(addNodes([], nodes), true);
			if (final_counter < segs)
				throw "Too many states";
			entrys.push({begin: prev, segs: segs - prev, nrules: len});
			prev = segs;
			nrules += len;
		}
		this.h = null;

		// init some properties
		this.segsEx = this.segs;
		this.nstates = lstates.length;
		this.invalid = nstates < U8MAX ? U8MAX : U16MAX;
		this.perRB = 1 + ((nstates - 1) | 15);

		// compress finalState
		var diff = final_counter + 1 - segs;
		for (s in lstates) {
			for (i in 0...s.targets.length)
				if (s.targets[i] > segs)
					s.targets[i] -= diff;
			if (s.id > segs) s.id -= diff;
		}
		if (lrb != null) {
			this.states = Lambda.array(this.lstates);
			this.states.sort(State.onSort);
			lrb.doPrecedence(this);
		}
		// DFA -> Tables
		this.makeTables();
		// rollback detection, only for lexer, LR0 has its own way to rollback
		if (lrb == null) this.rollback();

		this.lstates = null;
	}

	public inline function posRB() return this.segsEx * this.per;
	public inline function posRBL() return posRB() + this.perRB;

	inline function node() return new Node(uid++);

	function initNode(p: Pattern, f: Node) {
		return switch (p) {
		case Empty:
			f;
		case Match(c):
			var n = node();
			n.trans.push(new NChars(c, f));
			n;
		case Star(p):
			var n = node();
			var an = initNode(p, n);
			n.epsilon.push(an);
			n.epsilon.push(f);
			n;
		case Plus(p):
			var n = node();
			var an = initNode(p, n);
			n.epsilon.push(an);
			n.epsilon.push(f);
			an;
		case Choice(a, b):
			var n = node();
			n.epsilon.push(initNode(a, f));
			n.epsilon.push(initNode(b, f));
			n;
		case Next(a, b):
			initNode( a, initNode(b, f) );
		}
	}

	function compile(nodes: Array<Node>, first: Bool): Int {
		var sid = nodes.map( n -> n.id ).join("+");
		var id = h.get(sid);
		if (id != null)
			return id;
		var ta: Array<NAChars> = getTransitions(nodes);
		var len = ta.length;
		id = if (!first && len == 0) {
			final_counter--; // final state.
		} else {
			segs++;
		}
		h.set(sid, id);

		var trans: Array<Char> = [];
		for (i in 0...len)
			for (c in ta[i].chars)
				trans.push(Char.c3(c.min, c.max, i));

		var targets = []; targets.resize(len);
		for (i in 0...len)
			targets[i] = compile(ta[i].ns, false);

		var f = -1;
		var i = nodes.length;
		while (--i >= 0) {
			if (nodes[i].id < this.finals) {
				f = this.nrules + nodes[i].id;
				break;
			}
		}
		lstates.push(new State(id, trans, targets, f));
		return id;
	}

	function makeTables() {
		var INVALID = this.invalid;
		var bytes = (segsEx * per) + (3 * perRB); // segsN + (rollbak + rollback_len + exits)
		var tbls = new Table(bytes);
		for (i in 0...bytes) tbls.set(i, INVALID);

		for (s in this.lstates) {
			tbls.set(tbls.exitpos(s.id), s.finalID == -1 ? INVALID: s.finalID); // Reverse write checking table
			if (s.id < segsEx)
				makeTrans(tbls, s.id * per, s.trans, s.targets);
		}
		this.table = tbls;
	}

	function rollback() {
		inline function epsilon(seg) return this.table.exits(seg);
		var INVALID = this.invalid;
		var rollpos = posRB();
		var rlenpos = posRBL();
		function loop(exit, seg, smax, length) {
			var base = seg * this.per;
			for (p in base...base + this.per) {
				var nxt = this.table.get(p);
				if (nxt >= smax || epsilon(nxt) != INVALID || nxt == seg) continue;
				this.table.set(rollpos + nxt, exit);
				this.table.set(rlenpos + nxt, length); // junk(length) when rollback.
				loop(exit, nxt, smax, length + 1);
			}
		}
		for (e in this.entrys) {
			var smax = e.begin + e.segs;
			for (seg in e.begin...smax) {
				var exit = epsilon(seg);
				if (exit == INVALID) continue;
				loop(exit, seg, smax, 1);
			}
		}
	}

	public function bytesTable(): haxe.io.Bytes {
		if (invalid == U8MAX) {
			var b = haxe.io.Bytes.alloc(table.length);
			for (i in 0...table.length)
				b.set(i, table[i]);
			return b;
		} else {
			var b = haxe.io.Bytes.alloc(table.length << 1);
			for (i in 0...table.length)
				b.setUInt16(i << 1, table[i]);
			return b;
		}
	}
	#if sys
	public function write(out: haxe.io.Output, split = false) {
		var left = posRB();
		var rest = this.table.length - left;
		if (!split) out.writeByte('"'.code);
		var prefix = this.invalid == U8MAX ? "\\x" : "\\u";
		var padd = this.invalid == U8MAX ? 2 : 4;
		for (i in 0...left) {
			if (split && i > 0 && i % 16 == 0) out.writeString("\n");
			if (split && i > 0 && i % this.per == 0) out.writeString("\n");
			out.writeString( prefix + StringTools.hex(table.get(i), padd).toLowerCase() );
		}
		for (i in 0...rest) {
			if (split && i % 16 == 0) out.writeString("\n");
			if (split && i % this.perRB == 0) out.writeString("\n");
			out.writeString( prefix + StringTools.hex(table.get(left + i), padd).toLowerCase() );
		}
		if (!split) out.writeByte('"'.code);
		out.flush();
	}
	#end
	static function makeTrans(tbls: Table, start: Int, trans: Array<Char>, targets: Array<Int>) {
		for (c in trans) {
			var i = c.min + start;
			var max = c.max + start;
			var s = targets[c.ext];
			while (i <= max) {
				tbls.set(i, s);
				++ i;
			}
		}
	}

	static function addNode(nodes: Array<Node>, n: Node) {
		for (n2 in nodes)
			if (n2 == n)
				return;
		nodes.push(n);
		addNodes(nodes, n.epsilon);
	}

	static function addNodes(nodes: Array<Node>, ns: Array<Node>) {
		for (n in ns)
			addNode(nodes, n);
		return nodes;
	}

	static function getTransitions(nodes: Array<Node>) {
		// Merge transition with the same target
		var tl: Array<NChars> = [];
		var states: Array<NAChars> = [];
		for (n in nodes)
			for (t in n.trans)
				tl.push(t);
		if (tl.length == 0)
			return states;
		tl.sort( NChars.onSort );
		var a = tl[0];
		for (i in 1...tl.length) {
			var b = tl[i];
			if (a.n == b.n) {
				tl[i - 1] = null;
				b = new NChars(CSet.union(a.chars, b.chars), b.n);
				tl[i] = b;
			}
			a = b;
		}
		while ( tl.remove(null) ) {
		}
		// Split char sets so as to make them disjoint
		inline function addState(l: List<NAChars>, chars: Charset, ns: Array<Node>) {
			if (chars.length > 0) l.push(new NAChars(chars, ns));
		}
		var all_chars = CSet.C_EMPTY;
		var all_state = new List<NAChars>();
		for (t in tl) {
			var ls = new List();
			addState(ls, CSet.diff(t.chars, all_chars), [t.n]);
			for (s in all_state) {
				var nodes = s.ns.copy();
				nodes.push(t.n);
				addState(ls, CSet.inter(s.chars, t.chars), nodes);
				addState(ls,  CSet.diff(s.chars, t.chars),  s.ns);
			}
			all_chars = CSet.union(all_chars, t.chars);
			all_state = ls;
		}
		// Epsilon closure of targets
		states.resize(all_state.length);
		var i = 0;
		for (s in all_state) {
			states[i++] = new NAChars(s.chars, addNodes([], s.ns));
		}
		// Canonical ordering
		states.sort(NAChars.onSort);
		return states;
	}

	// ---- Regexp Parsing ---- //
	static function plus(r: Pattern): Pattern {
		return switch (r) {
		case Next(r1, r2): Next(r1, plus(r2));
		default: Plus(r);
		}
	}

	static function star(r: Pattern): Pattern {
		return switch (r) {
		case Next(r1, r2): Next(r1, star(r2));
		default: Star(r);
		}
	}

	static function opt(r: Pattern): Pattern {
		return switch (r) {
		case Next(r1, r2): Next(r1, opt(r2));
		default: Choice(r, Empty);
		}
	}

	static function next(r: Pattern, r2: Pattern): Pattern {
		return switch (r) {
		case Empty: r2;
		default: Next(r, r2);
		}
	}

	/**
	The following meta characters are supported:

		- `*`: zero or more
		- `+`: one or more
		- `?`: zero or one
		- `|`: or
		- `[`: begin char range
		- `]`: end char range
		- `\`: escape characters for '\', '+', '*', '?', '[', ']', '-'
	*/
	static public function parse(s: String, ?c_all: Charset): Pattern {
		if (c_all == null)
			c_all = CSet.C_255;
		var b = haxe.io.Bytes.ofString(s);
		return parseInner(b, 0, b.length, c_all);
	}
	static function parseInner(b: haxe.io.Bytes, i: Int, len: Int, c_all: Charset): Pattern {
		function readChar() {
			var c = b.get(i++);
			switch (c) {
			case "\\".code, "+".code, "*".code, "?".code, "[".code, "]".code, "-".code, "|".code:
			case "x".code:
				c = Std.parseInt("0x" + b.getString(i, 2));
				i += 2;
			default:
				throw "\\";
			}
			return c;
		}
		var r = Empty;
		while (i < len) {
			var c = b.get(i++);
			switch (c) {
			case "+".code if (r != Empty):
				r = plus(r);
			case "*".code if (r != Empty):
				r = star(r);
			case "?".code if (r != Empty):
				r = opt(r);
			case '|'.code if (r != Empty && i < len):
				var inner = parseInner(b, i, len, c_all);
				if (inner == Empty)
					throw 'The right side of "|" is empty';
				return Choice(r, inner);
			case "[".code if (i < len):
				var not = b.get(i) == "^".code;
				if (not) ++i;
				var range = 0;
				var acc = [];
				while (i < len) {
					var c = b.get(i++);
					if (c == "]".code) {
						if (range != 0) acc.push(new Char(range, range));
						break;
					} else if (c == "-".code) {
						if (range != 0) throw "--";
						if (acc.length == 0) {
							acc.push(new Char(c, c)); // ('-', '-');
						} else {
							var last: Char = acc.pop();
							if (last.min != last.max) throw "todo";
							range = last.min;
						}
					} else {
						if (c == "\\".code)
							c = readChar();
						if (range == 0) {
							acc.push(new Char(c, c));
						} else {
							if (range > c) throw "Range out of order: -" + String.fromCharCode(c);
							acc.push(new Char(range, c));
							range = 0;
						}
					}
				}
				if (acc.length > 0) {
					acc = CSet.sorting(acc);
					if (not)
						acc = CSet.complement(acc, c_all);
					r = next(r, Match(acc));
				} else {
					throw "Empty range: []";
				}
			case "\\".code:
				c = readChar();
				r = next(r, Match(CSet.single( c )));
			default:
				r = next(r, Match(CSet.single( c )));
			}
		}
		return r;
	}
}

enum Pattern {
	Empty;
	Match(c: Charset);
	Star(p: Pattern);
	Plus(p: Pattern);
	Choice(p1: Pattern, p2: Pattern);
	Next(p1: Pattern, p2: Pattern);
}

private class NChars {
	public var chars: Charset;
	public var n: Node;
	public function new(cs, n) {
		this.chars = cs;
		this.n = n;
	}
	public static function onSort(a: NChars, b: NChars) return a.n.id - b.n.id;
}
private class NAChars {
	public var chars: Charset;
	public var ns: Array<Node>;
	public function new(cs, ns) {
		this.chars = cs;
		this.ns = ns;
	}
	public static function onSort(s1: NAChars, s2: NAChars) {
		var a = s1.chars.length;
		var b = s2.chars.length;
		var len = Utils.imin(a, b);
		for( i in 0...len) {
			var a = s1.chars[i];
			var b = s2.chars[i];
			if (a.min != b.min)
				return a.min - b.min;
			if (a.max != b.max)
				return a.max - b.max;
		}
		return a - b;
	}
}
private class Node {
	public var id: Int;
	public var trans: Array<NChars>;
	public var epsilon: Array<Node>;
	public function new(id) {
		this.id = id;
		trans = [];
		epsilon = [];
	}
}
private class State {
	public var id: Int;
	public var trans: Array<Char>;
	public var targets: Array<Int>;
	public var finalID: Int;
	public function new(i, ts, tar, f) {
		id = i;
		trans = ts;
		targets = tar;
		finalID = f;
	}
	public static function onSort(a: State, b: State) return a.id - b.id;
}
