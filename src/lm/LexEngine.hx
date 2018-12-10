package lm;

import lm.Charset;

typedef PatternSet = Array<Pattern>;

class LexEngine {

	public static inline var U8MAX = 0xFF;
	public static inline var U16MAX = 0xFFFF;

	var uid(default, null): Int;
	var h: Map<String, Int>;
	var final_counter: Int;
	var lstates: List<State>;

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

	public var nrules(default, null): Int;

	public var entrys(default, null): Array<{begin:Int, width:Int}>;

	public var invalid(default, null): Int;

	public function new(a: Array<PatternSet>, cmax = Char.MAX) {
		this.entrys = [];
		this.per = cmax + 1;
		this.h = new Map();
		this.lstates = new List();
		this.segs = 0;  // state_counter
		this.invalid = U16MAX;
		this.final_counter = U16MAX - 1; // compress it later.
		this.nrules = Lambda.fold(a, (p, n) -> p.length + n, 0);
		this.uid = this.nrules;
		var nodes = [];
		var begin = 0;
		var i = 0;
		for (pats in a) {
			nodes.resize(pats.length);
			// Pattern -> NFA(nodes)
			for (p in 0...pats.length) {
				var f = new Node(i++);
				var n = initNode(pats[p], f);
				nodes[p] = n;
			}
			// NFA -> DFA
			compile(addNodes([], nodes));
			if (final_counter < segs)
				throw "Too many states";
			entrys.push({begin: begin, width: this.segs - begin});
			begin = this.segs;
		}
		// properties
		this.invalid = lstates.length < U8MAX ? U8MAX : U16MAX;
		this.perRB = 1 + ((lstates.length - 1) | 15);

		// compress finalState
		var diff = final_counter + 1 - segs;
		for (s in lstates) {
			for (i in 0...s.targets.length)
				if (s.targets[i] > segs)
					s.targets[i] -= diff;
			if (s.id > segs) s.id -= diff;
		}
		// DFA -> Tables
		this.makeTables();
		// rollback detection
		this.rollback();
	}

	public inline function write(out, split = false) this.table.write(posRB(), per, perRB, isBit16(), out, split);
	public inline function posRB() return this.segs * this.per;
	public inline function posRBL() return posRB() + this.perRB;
	public inline function isBit16() return this.invalid == U16MAX;
	inline function isFinal(n: Node) return n.id < this.nrules;
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

	function compile(nodes: Array<Node>): Int {
		var sid = nodes.map( n -> n.id ).join("+");
		var id = h.get(sid);
		if (id != null)
			return id;
		var ta: Array<NAChars> = getTransitions(nodes);
		var len = ta.length;
		id = if (len == 0) {
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
			targets[i] = compile(ta[i].ns);

		var f = -1;
		for (n in nodes) {
			if ( isFinal(n) ) {
				f = n.id;
				break;
			}
		}
		lstates.push(new State(id, trans, targets, f));
		return id;
	}

	function makeTables() {
		var INVALID = this.invalid;
		var bytes = (segs * per) + (3 * perRB); // segsN + (rollbak + rollback_len + exits)
		var tbls = new Table(bytes);
		for (i in 0...bytes) tbls.set(i, INVALID);

		for (s in this.lstates) {
			tbls.set(tbls.exitpos(s.id), s.finalID == -1 ? INVALID: s.finalID); // Reverse write checking table
			if (s.id < segs)
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
			var smax = e.begin + e.width;
			for (seg in e.begin...smax) {
				var exit = epsilon(seg);
				if (exit == INVALID) continue;
				loop(exit, seg, smax, 1);
			}
		}
	}

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
			case "x".code, "X".code:
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

class NChars {
	public var chars: Charset;
	public var n: Node;  // target, if the chars is matched then goto "n".
	public function new(cs, n) {
		this.chars = cs;
		this.n = n;
	}
	public static function onSort(a: NChars, b: NChars) return a.n.id - b.n.id;
}

class NAChars {
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
class Node {
	public var id: Int;
	public var trans: Array<NChars>;  //  empty or .length == 1
	public var epsilon: Array<Node>;
	public function new(id) {
		this.id = id;
		trans = [];
		epsilon = [];
	}
}
class State {
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

@:forward(length)
abstract Table(haxe.ds.Vector<Int>) {
	public function new(len) this = new haxe.ds.Vector<Int>(len);
	@:arrayAccess public inline function get(i) return this.get(i);
	@:arrayAccess public inline function set(i, v) return this.set(i, v);
	public inline function trans(state, per, term) return this.get(state * per + term);
	// Whether a state can exit. if a non-final state can exit, then it must be epsilon.
	public inline function exits(state) return this.get( exitpos(state) );
	public inline function exitpos(state) return this.length - 1 - state;

	public function toByte(bit16: Bool): haxe.io.Bytes {
		if (!bit16) {
			var b = haxe.io.Bytes.alloc(this.length);
			for (i in 0...this.length)
				b.set(i, this[i]);
			return b;
		} else {
			var b = haxe.io.Bytes.alloc(this.length << 1);
			for (i in 0...this.length)
				b.setUInt16(i << 1, this[i]);
			return b;
		}
	}
	public function write(left:Int, per:Int, perRB:Int, bit16:Bool, out:haxe.io.Output, split:Bool) {
		if (!split) out.writeByte('"'.code);
		var prefix = bit16 ? "\\u" : "\\x";
		var padd = bit16 ? 4: 2;
		for (i in 0...left) {
			if (split && i > 0 && i % 16 == 0) out.writeString("\n");
			if (split && i > 0 && i % per == 0) out.writeString("\n");
			out.writeString( prefix + StringTools.hex(this.get(i), padd).toLowerCase() );
		}
		var rest = this.length - left;
		for (i in 0...rest) {
			if (split && i % 16 == 0) out.writeString("\n");
			if (split && i % perRB == 0) out.writeString("\n");
			out.writeString( prefix + StringTools.hex(this.get(left + i), padd).toLowerCase() );
		}
		if (!split) out.writeByte('"'.code);
		out.flush();
	}
}