package lm;

import lm.Charset;

class LexEngine {

	public static inline var BEGIN = 0;  // Don't change this value.

	var uid(default, null): Int;
	var finals: Array<Node>;
	var finals_tmp: Array<Bool>;
	var h: Map<String, Int>;
	var parts: Map<String, Int>;
	var final_counter: Int;
	var part_counter: Int;
	var lparts: List<Array<Charset>>;
	var states: List<State>;

	/**
	 segment size. default is 256(Char.MAX + 1)
	*/
	public var per(default, null): Int;

	/**
	 format: [trans-seg0|trans-seg1|......|trans-segN|check-segs]
	*/
	public var table(default, null): haxe.io.Bytes;

	/**
	 how many segments. used for valid the transition state.
	*/
	public var segs(default, null): Int;

	/**
	 states.length
	*/
	public var size(get, never): Int;
	inline function get_size() return this.per - final_counter - 2 + segs;

	/**
	* see LexBuilder.hx# to how to use it.
	* @param pa
	* @param cmax = Char.MA, The cmax value cannot exceed 255.
	*/
	public function new(pa: Array<Pattern>, cmax = Char.MAX) {
		var len = pa.length;
		// Pattern -> NFA(nodes + finals)
		var nodes = [];       nodes.resize(len);
		this.finals = [];     finals.resize(len);
		this.finals_tmp = []; finals_tmp.resize(len);
		this.uid = len;
		for (i in 0...len) {
			var f = new Node(i);
			var n = initNode(pa[i], f);
			nodes[i] = n;
			finals[i] = f;
			finals_tmp[i] = false;
		}
		// NFA -> DFA
		this.h = new Map();
		this.parts = new Map();
		this.per = cmax + 1;
		this.segs = 0;  // state_counter
		this.part_counter = 0;
		this.final_counter = cmax - 1;
		this.lparts = new List();
		this.states = new List();
		compile(addNodes([], nodes));
		if (final_counter < segs)
			throw "Too many states";
		h = null;
		finals = null;
		finals_tmp = null;
		var i = 0;
		var trans = []; trans.resize(this.lparts.length);
		for (part in this.lparts) {
			var seg: Array<Char> = [];
			for (j in 0...part.length) {
				var chars: Charset = part[j];
				for (c in chars) {
					seg.push(Char.c3(c.min, c.max, j));
				}
			}
			trans[i++] = seg;
		}
		parts = null;
		lparts = null;
		// DFA -> Tables
		this.table = makeTables(states, trans, segs, per);
		states = null;
	}

	inline function node() return new Node(uid++);

	function initNode(p: Pattern, f: Node) {
		return switch (p) {
		case Empty:
			f;
		case Match(c):
			var n = node();
			n.trans.push(new Transition(c, f));
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
		case Choice(p):
			var n = node();
			var an = initNode(p, f);
			n.epsilon.push(an);
			n.epsilon.push(f);
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
		if (nodes.length == 1 && nodes[0].id < this.finals.length) {
			id = final_counter--; // targets will be empty.
		} else {
			id = segs++;
		}
		var ta: Array<TransitionA> = transitions(nodes);
		var len = ta.length;
		if (len == 0 && id < segs) {
			id = final_counter--;
			segs--;  // rollback
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

		states.push(new State(id, pid, targets, f));
		return id;
	}

	#if sys
	public function write(out: haxe.io.Output, tab = "\t") {
		var size = this.table.length;
		var seg = size >> 4; // size / 16, Make sure the size is a multiple of 16
		tab = "\\\n" + tab;
		out.writeByte('"'.code);
		if (size >= 16)      // first line without tab
			for (j in 0...16)
				out.writeString( "\\x" + StringTools.hex(table.get(j), 2) );
		for (i in 1...seg) {
			var p = i * 16;
			out.writeString(tab);
			for (j in p...(p + 16))
				out.writeString( "\\x" + StringTools.hex(table.get(j), 2) );
		}
		out.writeByte('"'.code);
		out.writeByte("\n".code);
		out.flush();
	}
	#end
	// **Unsafe**
	function resize(bper: Int) {
		var size = this.size;
		if (bper <= size) return;
		var tab = haxe.io.Bytes.alloc(bper * (this.segs + 1));
		tab.fill(0, tab.length, this.per - 1);

		var prev = this.per;
		var min = Utils.imin(bper, prev);
		for (i in 0...segs + 1) {
			tab.blit(bper * i, this.table, prev * i, min);
		}
		for (i in 0...segs) {
			tab.set(tab.length - i - 1, this.table.get(this.table.length - i - 1));
		}
		this.final_counter = bper - size - 2 + segs;
		this.per = bper;
		this.table = tab;
	}

	static function makeTrans(tbls: haxe.io.Bytes, start: Int, trans: Array<Char>, tbl: Array<Int>) {
		for (c in trans) {
			var i = c.min + start;
			var max = c.max + start;
			var s = tbl[c.ext];
			while (i <= max) {
				tbls.set(i, s);
				++ i;
			}
		}
	}
	static function first(p: Int, f: Int, a: Array<Bool>) {
		var len = a.length;
		while (p < len) {
			if (a[p]) return p;
			++ p;
		}
		return f;
	}
	static function makeTables(dfa: List<State>, trans: Array<Array<Char>>, segs: Int, per: Int) {
		var len = dfa.length;
		var bytes = (segs + 1) * per;
		var tbls = haxe.io.Bytes.alloc(bytes);
		tbls.fill(0, bytes, per - 1);
		for (s in dfa) {
			tbls.set(bytes - s.id - 1, first(0, per - 1, s.finals)); // Reverse write checking table
			if (s.id < segs)
				makeTrans(tbls, s.id * per, trans[s.part], s.targets);
		}
		return tbls;
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

	static function transitions(nodes: Array<Node>) {
		// Merge transition with the same target
		var tl: Array<Transition> = [];
		var states: Array<TransitionA> = [];
		for (n in nodes)
			for (t in n.trans)
				tl.push(t);
		if (tl.length == 0)
			return states;
		tl.sort( Transition.onSort );
		var a = tl[0];
		for (i in 1...tl.length) {
			var b = tl[i];
			if (a.n == b.n) {
				tl[i - 1] = null;
				b = new Transition(CSet.union(a.chars, b.chars), b.n);
				tl[i] = b;
			}
			a = b;
		}
		while ( tl.remove(null) ) {
		}
		// Split char sets so as to make them disjoint
		var all_chars = CSet.C_EMPTY;
		var all_state = new List<TransitionA>();
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
			states[i++] = new TransitionA(s.chars, addNodes([], s.ns));
		}
		// Canonical ordering
		states.sort(TransitionA.onSort);
		return states;
	}

	static function addState(l: List<TransitionA>, chars: Charset, ns: Array<Node>) {
		if (chars.length > 0) {
			l.push(new TransitionA(chars, ns));
		}
	}

	// ---- Regexp Parsing ---- //
	static inline function s_invalid(b: lm.ByteData, i: Int): String {
		return 'Invalid Regexp: "${b.readString(0, b.length)}" at ${i}(\'${String.fromCharCode(b.readByte(i))}\')';
	}

	static inline function invalid(b, i):Void throw s_invalid(b, i);


	static function escaped(b: lm.ByteData, i: Int): Int {
		var c = b.readByte(i);
		return switch (c) {
		case "\\".code, "+".code, "*".code, "?".code, "[".code, "]".code, "-".code:
			c;
		default:
			throw s_invalid(b, i);
		}
	}

	static function plus(r: Pattern, b, i): Pattern {
		return switch (r) {
		case Next(r1, r2): Next(r1, plus(r2, b, i));
		case Match(_): Plus(r);
		default: throw s_invalid(b, i);
		}
	}

	static function star(r: Pattern, b, i): Pattern {
		return switch (r) {
		case Next(r1, r2): Next(r1, star(r2, b, i));
		case Match(_): Star(r);
		default: throw s_invalid(b, i);
		}
	}

	static function opt(r: Pattern, b, i): Pattern {
		return switch (r) {
		case Next(r1, r2): Next(r1, opt(r2, b, i));
		case Match(_): Choice(r);
		default: throw s_invalid(b, i);
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
		- `[`: begin char range
		- `]`: end char range
		- `\`: escape characters for '\', '+', '*', '?', '[', ']', '-'
	*/
	static public function parse(b: lm.ByteData, ?all: Charset): Pattern {
		var i = 0, len = b.length;
		var r = Empty;
		if (all == null)
			all = CSet.C_255;
		while (i < len) {
			var c = b.readByte(i++);
			r = switch (c) {
			case "+".code:
				plus(r, b, i - 1);
			case "*".code:
				star(r, b, i - 1);
			case "?".code:
				opt(r, b, i - 1);
			case "[".code:
				var err = i - 1;
				var not = b.readByte(i) == "^".code;
				if (not) ++i;
				var range = 0;
				var done = false;
				var acc = [];
				while (i < len) {
					var c = b.readByte(i++);
					if (c == "]".code) {
						done = range == 0;
						break;
					} else if (c == "-".code) {
						if (range != 0) invalid(b, i - 1);
						if (acc.length == 0) {
							acc.push(new Char(c, c)); // ('-', '-');
						} else {
							var last: Char = acc.pop();
							if (last.min != last.max) invalid(b, i - 1);
							range = last.min;
						}
					} else {
						if (c == "\\".code) {
							c = escaped(b, i++);
						}
						if (range == 0) {
							acc.push(new Char(c, c));
						} else {
							acc.push(new Char(range, c));
							range = 0;
						}
					}
				}
				if (!done) invalid(b, err);
				if (acc.length > 0) {
					acc = CSet.sorting(acc);
					if (not)
						acc = CSet.complement(acc, all);
					next(r, Match(acc));
				} else {
					r;
				}
			case "\\".code:
				next(r, Match(CSet.single( escaped(b, i++) )));
			default:
				next(r, Match(CSet.single( c )));
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
	Choice(p: Pattern);
	Next(p1: Pattern, p2: Pattern);
}

private class Transition {
	public var chars: Charset;
	public var n: Node;
	public function new(cs, n) {
		this.chars = cs;
		this.n = n;
	}
	public static function onSort(a: Transition, b: Transition) return a.n.id - b.n.id;
}
private class TransitionA {
	public var chars: Charset;
	public var ns: Array<Node>;
	public function new(cs, ns) {
		this.chars = cs;
		this.ns = ns;
	}
	public static function onSort(s1: TransitionA, s2: TransitionA) {
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
	public var trans: Array<Transition>;
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
	public function new(i, p, t, f) {
		id = i;
		part = p;
		targets = t;
		finals = f;
	}
	public static function onSort(a: State, b: State) return a.id - b.id;
}
