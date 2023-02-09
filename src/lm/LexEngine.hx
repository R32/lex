package lm;

import lm.Charset;

typedef PatternSet = Array<Pattern>;

class LexEngine {

	public static inline var U8MAX = 0xFF;
	public static inline var U16MAX = 0xFFFF;

	var generator(default, null): NodeGenerator;
	var h: Map<String, Int>;
	var final_counter: Int;
	var lstates: List<State>;

	/**
	 the segment size. default is 256(Char.MAX + 1)
	*/
	public var per(default, null): Int;

	/**
	 the size of the "exit table"
	*/
	public var perExit(default, null): Int;

	/**
	 format: [seg0,seg1,......,segN,exits]
	*/
	public var table(default, null): Table;

	/**
	 state_counter
	*/
	public var segs(default, null): Int;

	public var nrules(default, null): Int;

	public var entrys(default, null): Array<{begin : Int, index : Int, ?width : Int}>;

	public var invalid(default, null): Int;

	public function new( patterns : Array<PatternSet>, cmax = 255) {
		this.entrys = [];
		this.per = cmax + 1;
		this.h = new Map();
		this.lstates = new List();
		this.segs = 0;  // state_counter
		this.invalid = U16MAX;
		this.final_counter = U16MAX - 1; // compress it later.
		this.make(patterns);
	}

	function make( patterns : Array<PatternSet>) {
		this.nrules = Lambda.fold(patterns, (p, n) -> p.length + n, 0);
		this.generator = new NodeGenerator(this.nrules);
		var nodes = [];
		var begin = 0;
		for (pats in patterns) {
			nodes.resize(pats.length);
			// Pattern -> NFA(nodes)
			for (p in 0...pats.length) {
				var f = generator.newFinal();
				var n = generator.normalize(pats[p], f);
				nodes[p] = n;
			}
			// NFA -> DFA
			compile(addNodes([], nodes));
			if (final_counter < segs)
				throw "Too many states";
			entrys.push({begin: begin, width: this.segs - begin, index : entrys.length});
			begin = this.segs;
		}
		// properties
		this.invalid = lstates.length < U8MAX ? U8MAX : U16MAX;
		this.perExit = 1 + ((lstates.length - 1) | 15);

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
	}

	public inline function debugWrite(out) this.table.debugWrite(per, perExit, isBit16(), out);
	public inline function isBit16() return this.invalid == U16MAX;

	function closure( nodes : Array<Node> ) : Array<Node> {
		return nodes;
	}

	function compile(nodes: Array<Node>): Int {
		var sid = nodes.map( n -> n.id ).join("+");
		var id = h.get(sid);
		if (id != null)
			return id;
		var ta: Array<FMArrow> = getTransitions(closure(nodes));
		id = if (ta.length == 0) {
			final_counter--; // final state.
		} else {
			segs++;
		}
		h.set(sid, id);

		var sets = [];
		var targets = [];
		for (r in ta) {
			sets.push(r.chars);
			targets.push( compile(r.ns) );
		}

		var f = -1;
		for (n in nodes) {
			if ( generator.isFinal(n) ) {
				f = n.id;
				break;
			}
		}
		lstates.push(new State(id, sets, targets, f));
		return id;
	}

	function makeTables() {
		var INVALID = this.invalid;
		var bytes = (segs * per) + perExit; // segsN + exits
		var tbls = new Table(bytes);
		for (i in 0...bytes) tbls.set(i, INVALID);

		for (s in this.lstates) {
			tbls.set(tbls.exitpos(s.id), s.finalID == -1 ? INVALID: s.finalID); // Reverse write checking table
			if (s.id < segs)
				makeTrans(tbls, s.id * per, s.sets, s.targets);
		}
		this.table = tbls;
	}

	static function makeTrans(tbls: Table, start: Int, sets: Array<Charset>, targets: Array<Int>) {
		for (q in 0...sets.length) {
			var s = targets[q];
			for (c in sets[q]) {
				var i = c.min + start;
				var max = c.max + start;
				while (i <= max) {
					tbls.set(i, s);
					++ i;
				}
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
		var tl: Array<Arrow> = [];
		var states: Array<FMArrow> = [];
		for (n in nodes)
			for (t in n.arrows)
				tl.push(t);
		if (tl.length == 0)
			return states;
		tl.sort( Arrow.onSort );
		var a = tl[0];
		for (i in 1...tl.length) {
			var b = tl[i];
			if (a.n == b.n) {
				tl[i - 1] = null;
				b = new Arrow(CSet.union(a.chars, b.chars), b.n);
				tl[i] = b;
			}
			a = b;
		}
		while ( tl.remove(null) ) {
		}
		// Split char sets so as to make them disjoint
		inline function addState(l: List<FMArrow>, chars: Charset, ns: Array<Node>) {
			if (chars.length > 0) l.push(new FMArrow(chars, ns));
		}
		var all_chars = CSet.C_EMPTY;
		var all_state = new List<FMArrow>();
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
			states[i++] = new FMArrow(s.chars, addNodes([], s.ns));
		}
		// Canonical ordering
		states.sort(FMArrow.onSort);
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
				throw new PError("Unexpected: " + "\\", i-1);
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
					throw new PError("?????", i);
				return Choice(r, inner);
			case "[".code if (i < len):
				var not = b.get(i) == "^".code;
				if (not) ++i;
				var range = -1;
				var acc = [];
				while (i < len) {
					var c = b.get(i++);
					if (c == "]".code) {
						if (range != -1) {
							acc.push(new Char(range, range));
							acc.push(new Char("-".code, "-".code));
						}
						break;
					} else if (c == "-".code) {
						if (acc.length == 0) {
							acc.push(new Char(c, c)); // add('-')
						} else if (range == -1) {
							var last: Char = acc.pop();
							if (last.min != last.max) throw new PError("Unexpected: " + "-", i-1);
							range = last.min;
						}
					} else {
						if (c == "\\".code && i < len)
							c = readChar();
						if (range == -1) {
							acc.push(new Char(c, c));
						} else {
							acc.push(range < c ? new Char(range, c) : new Char(c, range));
							range = -1;
						}
					}
				}
				if (acc.length > 0) {
					acc = CSet.sorting(acc);
					if (not)
						acc = CSet.complement(acc, c_all);
					r = next(r, Match(acc));
				} else {
					throw new PError("UnClosed: " + "[", i-1);
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

class PError {
	public var message(default, null) : String;
	public var offset(default, null) : Int;
	public function new(s, i) {
		message = s;
		offset = i;
	}
	public function toString() {
		return '[message: $message, offset: $offset]';
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

private class Arrow {
	public var chars: Charset;
	public var n: Node;  // target, if the chars is matched then goto "n".
	public function new(cs, n) {
		this.chars = cs;
		this.n = n;
	}
	public static function onSort(a: Arrow, b: Arrow) return a.n.id - b.n.id;
}

// Transformed Arrow
private class FMArrow {
	public var chars: Charset;
	public var ns: Array<Node>;
	public function new(cs, ns) {
		this.chars = cs;
		this.ns = ns;
	}
	public static function onSort(s1: FMArrow, s2: FMArrow) {
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

// DFA Node
class Node {
	public var id: Int;
	public var arrows: Array<Arrow>;  //  empty or .length == 1
	public var epsilon: Array<Node>;
	@:allow(lm.NodeGenerator) function new(id) {
		this.id = id;
		arrows = [];
		epsilon = [];
	}
}

class NodeGenerator {

	var uid: Int;   // normal uid, start at "pivot"
	var fuid: Int;  // final uid, start at 0 and less than "pivot"
	var pivot: Int;

	inline function newNode() return new Node(uid++);

	public inline function isFinal(node: Node) return node.id < this.pivot;

	public function new(n) {
		this.uid = n;
		this.fuid = 0;
		this.pivot = n; // const
	}

	public function newFinal(): Node {
		if (fuid >= pivot) throw "WRONG";
		return new Node(fuid++);
	}

	public function normalize(p: Pattern, f: Node): Node {
		return switch (p) {
		case Empty:
			f;
		case Match(c):
			var n = newNode();
			n.arrows.push(new Arrow(c, f));
			n;
		case Star(p):
			var n = newNode();
			var an = normalize(p, n);
			n.epsilon.push(an);
			n.epsilon.push(f);
			n;
		case Plus(p):
			var n = newNode();
			var an = normalize(p, n);
			n.epsilon.push(an);
			n.epsilon.push(f);
			an;
		case Choice(a, b):
			var n = newNode();
			n.epsilon.push(normalize(a, f));
			n.epsilon.push(normalize(b, f));
			n;
		case Next(a, b):
			normalize( a, normalize(b, f) );
		}
	}
}

class State {
	public var id: Int;
	public var sets: Array<Charset>;
	public var targets: Array<Int>;
	public var finalID: Int;
	public function new(i, a, t, f) {
		id = i;
		sets = a;
		targets = t;
		finalID = f;
	}
	public static function onSort(a: State, b: State) return a.id - b.id;
}

@:forward(length, map)
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
	public function debugWrite(per:Int, perExit:Int, bit16:Bool, out:haxe.io.Output) {
		var left = this.length - perExit;
		var prefix = bit16 ? "\\u" : "\\x";
		var padd = bit16 ? 4: 2;
		for (i in 0...left) {
			if (i > 0 && i % 16 == 0) out.writeString("\n");
			if (i > 0 && i % per == 0) out.writeString("\n");
			out.writeString( prefix + StringTools.hex(this.get(i), padd).toLowerCase() );
		}
		for (i in 0...perExit) {
			if (i % 16 == 0) out.writeString("\n");
			if (i % perExit == 0) out.writeString("\n");
			out.writeString( prefix + StringTools.hex(this.get(left + i), padd).toLowerCase() );
		}
		out.flush();
	}
}