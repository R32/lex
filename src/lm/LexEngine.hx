package lm;

import lm.Charset;

typedef PatternSet = Array<Pattern>;

class LexEngine {

	public static inline var U8MAX = 255;
	public static inline var INVALID = U8MAX;

	var uid(default, null): Int;
	var finals: Array<Node>;
	var finals_tmp: Array<Bool>;
	var h: Map<String, Int>;
	var parts: Map<String, Int>;
	var final_counter: Int;
	var part_counter: Int;
	var lparts: List<Array<Charset>>;
	var lstates: List<State>;

	// Associated with "trans" for LR0Builder
	public var states(default, null): Array<State>;
	public var trans(default, null): Array<Array<Char>>;

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
	public var table(default, null): haxe.io.Bytes;

	/**
	 state_counter
	*/
	public var segs(default, null): Int;

	public var nrules(default, null): Int;

	public var nstates(default, null): Int;

	public var entrys(default, null): Array<{begin:Int, segs:Int, nrules:Int}>;

	public function new(a: Array<PatternSet>, cmax = Char.MAX, lr0 = false) {
		this.entrys = [];
		this.parts = new Map();
		this.per = cmax + 1;
		this.lparts = new List();
		this.lstates = new List();
		this.segs = 0;  // state_counter
		this.final_counter = U8MAX - 1; // 255 is used for invalid
		this.part_counter = 0;
		this.nrules = 0;
		this.finals = [];
		this.finals_tmp = [];

		var prev = 0;
		var nodes = [];
		for (pats in a) {
			this.h = new Map();
			var len = pats.length;
			nodes.resize(len);
			finals.resize(len);
			finals_tmp.resize(len);
			this.uid = len;
			// Pattern -> NFA(nodes + finals)
			for (i in 0...len) {
				var f = new Node(i);
				var n = initNode(pats[i], f);
				nodes[i] = n;
				finals[i] = f;
				finals_tmp[i] = false;
			}
			// NFA -> DFA
			compile(addNodes([], nodes));
			if (final_counter < segs)
				throw "Too many states";
			entrys.push({begin: prev, segs: segs - prev, nrules: len});
			prev = segs;
			nrules += len;
		}
		this.h = null;
		this.finals = null;
		this.finals_tmp = null;
		var i = 0;
		this.trans = [];
		this.trans.resize(lparts.length);
		for (part in lparts) {
			var seg: Array<Char> = [];
			for (j in 0...part.length) {
				var chars: Charset = part[j];
				for (c in chars) {
					seg.push(Char.c3(c.min, c.max, j));
				}
			}
			this.trans[i++] = seg;
		}
		this.parts = null;
		this.lparts = null;

		// compress finalState
		this.nstates = lstates.length;
		this.perRB = 1 + ((nstates - 1) | 15);
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
		this.makeRollback();

		if (lr0) {
			this.states = Lambda.array(this.lstates);
			this.states.sort(State.onSort);
		}
		this.lstates = null;
	}

	public inline function posRB() return this.segs * this.per;
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

	function getPart(p: Array<Charset>): Int {
		var sid = p.map(chars -> chars.join("+")).join(",");
		var id = parts.get(sid);
		if (id == null) {
			id = part_counter++;
			lparts.add(p); // add to tail
			parts.set(sid, id);
		}
		return id;
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

		var part: Array<Charset> = []; part.resize(len);
		for (i in 0...len)
			part[i] = ta[i].chars;
		var pid = this.getPart(part);

		var targets = []; targets.resize(len);
		for (i in 0...len)
			targets[i] = compile(ta[i].ns);

		var f = finals_tmp.copy();
		for (n in nodes)
			if (n.id < f.length) f[n.id] = true;

		lstates.push(new State(id, pid, targets, f, nrules));
		return id;
	}

	function makeTables() {
		var bytes = (segs * per) + (3 * perRB); // segsN + (rollbak + rollback_len + exits)
		var tbls = haxe.io.Bytes.alloc(bytes);
		tbls.fill(0, bytes, INVALID);
		for (s in this.lstates) {
			tbls.set(bytes - 1 - s.id, first(0, INVALID, s.prev_nrules, s.finals)); // Reverse write checking table
			if (s.id < segs)
				makeTrans(tbls, s.id * per, trans[s.part], s.targets);
		}
		this.table = tbls;
	}

	function makeRollback() {
		inline function epsilon(seg) return this.table.get(this.table.length - 1 - seg);
		var rollpos = posRB();
		var rlenpos = posRBL();
		function loop(exit, seg, length) {
			var base = seg * this.per;
			for (p in base...base + this.per) {
				var follow = this.table.get(p);
				if (follow == INVALID || follow == seg) continue;
				this.table.set(rollpos + follow, exit);
				this.table.set(rlenpos + follow, length); // junk(length) when rollback.
				if (epsilon(follow) == INVALID && follow < this.segs)
					loop(exit, follow, length + 1);
			}
		}
		for (e in this.entrys) {
			for (seg in e.begin...e.begin + e.segs) {
				var exit = epsilon(seg);
				if (exit == INVALID) continue;
				loop(exit, seg, 1);
			}
		}
	}
	#if sys
	public function write(out: haxe.io.Output, split = false) {
		var left = posRB();
		var rest = this.table.length - left;
		if (!split) out.writeByte('"'.code);
		for (i in 0...left) {
			if (split && i > 0 && i % 16 == 0) out.writeString("\n");
			if (split && i > 0 && i % this.per == 0) out.writeString("\n");
			out.writeString( "\\x" + StringTools.hex(table.get(i), 2).toLowerCase() );
		}
		for (i in 0...rest) {
			if (split && i % 16 == 0) out.writeString("\n");
			if (split && i % this.perRB == 0) out.writeString("\n");
			out.writeString( "\\x" + StringTools.hex(table.get(left + i), 2).toLowerCase() );
		}
		if (!split) out.writeByte('"'.code);
		out.flush();
	}
	#end
	static function makeTrans(tbls: haxe.io.Bytes, start: Int, trans: Array<Char>, targets: Array<Int>) {
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

	static function first(i: Int, fill: Int, start, a: Array<Bool>) {
		var len = a.length;
		while (i < len) {
			if (a[i]) return start + i;
			++ i;
		}
		return fill;
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
			case '|'.code if (r != Empty):
				return Choice(r, parseInner(b, i, len, c_all));
			case "[".code:
				var err = i - 1;
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
	public var part: Int;
	public var targets: Array<Int>;
	public var finals: Array<Bool>;
	public var prev_nrules: Int;  // How many rules are there in the previous PatternSet
	public function new(i, p, t, f, n) {
		id = i;
		part = p;
		targets = t;
		finals = f;
		prev_nrules = n;
	}
	public static function onSort(a: State, b: State) return a.id - b.id;
}
