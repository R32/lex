package lm;

import lm.Utils;

class LexEngine {

	var uid: Int;
	var finals: Array<Node>;

	var h: Map<String, Int>;
	var parts: Map<String, Int>;
	var state_counter: Int;
	var part_counter: Int;
	var lparts: List<Array<Charset>>;
	var states: List<State>;

	var dfa: Array<State>;
	var trans: Array<Array<IChar>>;

	public var table: Table;
	public function new(pa: Array<Pattern>) {
		var len = pa.length;
		if (len > 254)
			throw "More than 254 are not supported"; // -1 == 255
		// Pattern -> NFA(nodes + finals)
		var nodes = []; nodes.resize(len);
		this.finals = []; finals.resize(len);
		this.uid = 0;
		for (i in 0...len) {
			var f = node();
			var n = initNode(pa[i], f);
			nodes[i] = n;
			finals[i] = f;
		}
		// NFA -> DFA
		h = new Map();
		parts = new Map();
		state_counter = 0;
		part_counter = 0;
		lparts = new List();
		states = new List();
		loop(addNodes([], nodes));

		dfa = Lambda.array(states);
		dfa.sort(State.onSort);

		trans = []; trans.resize(this.lparts.length);
		var i = 0;
		for (part in this.lparts) {
			var seg: Array<IChar> = [];
			for (i in 0...part.length) {
				var chars: Charset = part[i];
				for (c in chars) {
					seg.push(new IChar(c.min, c.max, i));
				}
			}
			seg.sort( (a, b)-> a.min - b.min );
			trans[i++] = seg;
		}
		// DFA -> Tables
		this.table = makeTables(dfa, trans);
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
		var sid = p.map(chars -> chars.join("-")).join(",");
		var id = parts.get(sid);
		if (id == null) {
			id = part_counter++;
			lparts.add(p); // add to tail
			parts.set(sid, id);
		}
		return id;
	}

	function loop(nodes: Array<Node>): Int {
		var sid = nodes.map( n -> n.id ).join("+");
		var id = h.get(sid);
		if (id != null)
			return id;
		id = state_counter++;
		h.set(sid, id);
		var ta: Array<TransitionA> = transitions(nodes);
		var len = ta.length;

		var part: Array<Charset> = []; part.resize(len);
		for (i in 0...len)
			part[i] = ta[i].chars;
		var pid = this.getPart(part);

		var targets = []; targets.resize(len);
		for (i in 0...len) {
			targets[i] = loop(ta[i].ns);
		}

		var finals = []; finals.resize(this.finals.length);
		for (i in 0...this.finals.length) {
			finals[i] = nodes.indexOf(this.finals[i]) > -1;
		}

		states.push(new State(id, pid, targets, finals));
		return id;
	}

	static function makeTrans(tbls: haxe.io.Bytes, seg: Int, trans: Array<IChar>, tbl: Array<Int>) {
		var start = seg * 256;
		for (c in trans) {
			var i = c.min;
			var max = c.max;
			while (i <= max) {
				tbls.set(start + i, tbl[c.i]);
				++ i;
			}
		}
	}

	static function first(p: Int, a: Array<Bool>) {
		return if (p == a.length) {
			-1;
		} else if (a[p]) {
			p;
		} else {
			first(p + 1, a);
		}
	}

	static function makeTables(dfa: Array<State>, trans: Array<Array<IChar>>) {
		var len = dfa.length;
		var bytes = (len + 1) * 256;
		var tbls = haxe.io.Bytes.alloc(bytes);
		tbls.fill(0, bytes, -1); // 255 == -1
		for (i in 0...len) {
			var s = dfa[i];
			tbls.set(bytes - i - 1, first(0, s.finals)); // bytes - i => Reverse order
			makeTrans(tbls, i, trans[s.part], s.targets);
		}
		return new Table(tbls);
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

	 function transitions(nodes: Array<Node>) {
		// Merge transition with the same target
		var tl: Array<Transition> = [];
		for (n in nodes)
			for (t in n.trans)
				tl.push(t);
		tl.sort( Transition.onSort );
		var a = tl[0];
		for (i in 1...tl.length) {
			var b = tl[i];
			if (a.n == b.n) {
				tl[i - 1] = null;
				b = new Transition(cunion(a.chars, b.chars), b.n);
				tl[i] = b;
			}
			a = b;
		}
		while ( tl.remove(null) ) {
		}

		// Split char sets so as to make them disjoint
		var all_chars = C_EMPTY;
		var all_state = new List<TransitionA>();
		for (t in tl) {
			var ls = new List();
			addState(ls, cdiff(t.chars, all_chars), [t.n]);
			for (s in all_state) {
				var nodes = s.ns.copy();
				nodes.push(t.n);
				addState(ls, cinter(s.chars, t.chars), nodes);
				addState(ls,  cdiff(s.chars, t.chars),  s.ns);
			}
			all_chars = cunion(all_chars, t.chars);
			all_state = ls;
		}
		// Epsilon closure of targets
		var states: Array<TransitionA> = []; states.resize(all_state.length);
		var i = 0;
		for (s in all_state) {
			states[i++] = new TransitionA(s.chars, addNodes([], s.ns));
		}
		// Canonical ordering
		states.sort(TransitionA.onSort);
		//trace("---"); for (s in states) trace(LexEngineTest.rs_charset(s.chars) + "=> [" +s.ns.map( n-> n.id ).join(","));
		return states;
	}

	static function addState(l: List<TransitionA>, chars: Charset, ns: Array<Node>) {
		if (chars.length > 0) {
			l.push(new TransitionA(chars, ns));
		}
	}


	// ------- charset ------ //

	static public final C_EMPTY: Charset = [];
	static public final C_ALL: Charset = [new Char(0, Char.MAX)];

	static function cunion(c1: Charset, c2: Charset): Charset {
		if (c1.length == 0) return c2;
		if (c2.length == 0) return c1;
		var a_min = c1[0].min;
		var a_max = c1[0].max;
		var b_min = c2[0].min;
		var b_max = c2[0].max;
		var i = 1, j = 1;
		var ret = [];
		while (true) {
			if (a_min <= b_min) {
				if (a_max < b_max) {
					if (a_max + 1 < b_min) {
						ret.push(new Char(a_min, a_max));
					} else {
						b_min = a_min;
					}
					if (i < c1.length) {
						a_min = c1[i].min;
						a_max = c1[i++].max;
					} else {
						ret.push(new Char(b_min, b_max));
						while (j < c2.length) {
							ret.push(c2[j++].duplicate());
						}
						break;
					}
				} else {
					if (j < c2.length) {
						b_min = c2[j].min;
						b_max = c2[j++].max;
					} else {
						ret.push(new Char(a_min, a_max));
						while (i < c1.length) {
							ret.push(c1[i++].duplicate());
						}
						break;
					}
				}
			} else {
				var t = c1;
				c1 = c2;
				c2 = t;
				Utils.swapi(i, j);
				Utils.swapi(a_min, b_min);
				Utils.swapi(a_max, b_max);
			}
		}
		return ret;
	}

	static function ccomplement(c: Charset): Charset {
		var i = 0, len = c.length;
		if (len == 0) return C_ALL;
		var start = 0;
		if (c[0].min == 0) {
			start = c[0].max + 1;
			++ i;
		}
		var ret = [];
		while (i < len) {
			ret.push(new Char(start, c[i].min - 1));
			start = c[i].max + 1;
			++ i;
		}
		if (start <= Char.MAX)
			ret.push(new Char(start, Char.MAX));
		return ret;
	}

	static function cinter(c1: Charset, c2: Charset): Charset {
		//return ccomplement(cunion(ccomplement(c1), ccomplement(c2)));
		if (c1.length == 0 || c2.length == 0) return C_EMPTY;
		var a_min = c1[0].min;
		var a_max = c1[0].max;
		var b_min = c2[0].min;
		var b_max = c2[0].max;
		var i = 1, j = 1;
		var ret = [];
		while (true) {
			if (a_max <= b_max) {
				var left = Utils.imax(a_min, b_min);
				if (a_max >= left) {
					var right = Utils.imin(a_max, b_max);
					ret.push(new Char(left, right));
					if (right < b_max) {
						b_min = right + 1;
					} else {
						if (j < c2.length) {
							b_min = c2[j].min;
							b_max = c2[j++].max;
						} else {
							break;
						}
					}
				}
				if (i < c1.length) {
					a_min = c1[i].min;
					a_max = c1[i++].max;
				} else {
					break;
				}
			} else {
				var t = c1;
				c1 = c2;
				c2 = t;
				Utils.swapi(i, j);
				Utils.swapi(a_min, b_min);
				Utils.swapi(a_max, b_max);
			}
		}
		return ret;
	}

	static function cdiff(par: Charset, sub: Charset): Charset {
		//return ccomplement(cunion(ccomplement(par), sub));
		if (par.length == 0) return C_EMPTY;
		if (sub.length == 0) return par;
		var p_min = par[0].min;
		var p_max = par[0].max;
		var s_min = sub[0].min;
		var s_max = sub[0].max;
		var i = 1, j = 1;
		var ret = [];
		while (true) {
			if (p_max >= s_max) {
				if (p_min < s_min)
					ret.push(new Char(p_min, s_min - 1));
				if (p_min <= s_max)
					p_min = s_max + 1;
				if (j < sub.length) {
					s_min = sub[j].min;
					s_max = sub[j++].max;
				} else {
					if (p_min <= p_max)
						ret.push(new Char(p_min, p_max));
					while (i < par.length) {
						ret.push(par[i++].duplicate());
					}
					break;
				}
			} else {
				if (p_max >= s_min)
					p_max = s_min - 1;
				if (p_min <= p_max)
					ret.push(new Char(p_min, p_max));
				if (i < par.length) {
					p_min = par[i].min;
					p_max = par[i++].max;
				} else {
					break;
				}
			}
		}
		return ret;
	}

	// ---- Regexp Parsing ---- //
	static inline function s_invalid(b: lm.ByteData, i: Int): String {
		return 'Invalid Regexp: "${b.readString(0, b.length)}" at ${i}(\'${String.fromCharCode(b.readByte(i))}\')';
	}

	static inline function invalid(b, i): Void {
		throw s_invalid(b, i);
	}

	static inline function single(c: Int): Charset {
		return [new Char(c, c)];
	}

	static function group(cs: Charset): Charset {
		var r = C_EMPTY;
		for (c in cs)
			r = cunion(r, [c]);
		return r;
	}

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
	static public function parse(b: lm.ByteData): Pattern {
		var i = 0, len = b.length;
		var r = Empty;
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
					acc = group(acc);
					if (not)
						acc = ccomplement(acc);
					next(r, Match(acc));
				} else {
					r;
				}
			case "\\".code:
				next(r, Match(single( escaped(b, i++) )));
			default:
				next(r, Match(single( c )));
			}
		}
		return r;
	}
}

private extern abstract Char(Int) {
	var min(get, never): Int;
	private inline function get_min():Int return this & MAX;

	var max(get, never): Int;
	private inline function get_max():Int return (this >> 8);

	inline function new(min: Int, max: Int) this = min | (max << 8);

	inline function duplicate():Char return cast this;
	inline function toInt():Int return this;
	inline function toString(): String return '[$min, $max]';
	static inline function ofInt(i: Int): Char return cast i;

	static inline var MAX = 255;
}

private extern abstract IChar(Int) {
	var min(get, never): Int;
	private inline function get_min():Int return this & MAX;

	var max(get, never): Int;
	private inline function get_max():Int return (this >> 8) & MAX;

	var i(get, never): Int;
	private inline function get_i():Int return (this >> 16);

	inline function new(min: Int, max: Int, i: Int) this = min | (max << 8) | (i << 16);
	inline function toString(): String return '[$min, $max, $i]';
	static inline var MAX = 255;
}

private typedef Charset = Array<Char>;

private enum Pattern {
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

	public static function onSort(a: Transition, b: Transition) {
		return a.n.id - b.n.id;
	}
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
		for( i in 0...(a < b?a:b) ) {
			var a = s1.chars[i];
			var b = s2.chars[i];
			if (a.min != b.min)
				return b.min - a.min;
			if (a.max != b.max)
				return b.max - a.max;
		}
		if (a < b)
			return b - a;
		return 0;
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

// ----- test -----
@:access(lm)
@:dce class LexEngineTest {
	static public function run(): Void {
		inline function c(a, b) return new Char(a, b);
		eq (LexEngine.cunion(
			[c(32, 32), c(51, 59), c(65, 65), c(97, 102)],
			[c(10, 10), c(51, 59), c(65, 70)]
			),
			[c(10, 10), c(32, 32), c(51, 59), c(65, 70), c(97, 102)]
		);
		eq (LexEngine.cinter(
			[c(32, 32), c(51, 59), c(65, 65), c(97, 102)],
			[c(10, 10), c(51, 59), c(65, 70)]
			),
			[c(51, 59), c(65, 65)]
		);
		eq( LexEngine.ccomplement(
			[c(48, 59), c(65, 65)]
			),
			[c(0,47), c(60,64), c(66,255)]
		);
		eq( LexEngine.ccomplement(
			[c(48, 59), c(65, 70), c(97, 102)]
			),
			[c(0,47), c(60,64), c(71,96), c(103, 255)]
		);
		eq( LexEngine.cdiff(
			[c(32, 32), c(34, 51), c(65, 70)],
			[c(33, 33), c(37, 37), c(41, 59), c(61, 63), c(65, 67)]
			),
			[c(32, 32), c(34, 36), c(38, 40), c(68,70)]
		);
		var x = [c(48, 59), c(65, 70), c(97, 102)];
		eq( LexEngine.cdiff(LexEngine.C_ALL, x), LexEngine.ccomplement(x));

		var r = ["ab*c", "ae+f", "ah?g",
			"class", "import", "enum", "function", "var", "cast", "macro", "new",
			"return", "switch", "if", "else", "case", "static", "public", "private",
			"while", "for", "in", "abstract", "return",
		];

		var lex = new LexEngine(r.map( s -> LexEngine.parse(lm.ByteData.ofString(s))));
		var sa = ["ac", "ag", "abc", "function", "cas"];
		for (s in sa) {
			var state = 0; // BEGIN;
			for (i in 0...s.length) {
				var c = StringTools.fastCodeAt(s, i);
				state = lex.table.tbl(state, c);
				if (state == 255) {
					trace("Unmatched: \"" + s + "\", i: " + i);
					break;
				}
			}
			if (state != 255) {
				var p = lex.table.exits(state);
				if (p == 255) {
					trace("Unmatched: \"" + s + "\"");
				} else {
					trace("Matched: \"" + s + "\" with /" + r[p] + "/");
				}
			}
		}
		trace("LexEngine Done!");
	}
	static function eq(c1: Charset, c2: Charset, ?pos: haxe.PosInfos) {
		var b = c1.length == c2.length;
		if (b) {
			for (i in 0...c1.length) {
				if (c1[i].toInt() != c2[i].toInt()) {
					b = false;
					break;
				}
			}
		}
		if (!b) {
			trace("\n c1: " + s_charset(c1) + "\n c2: " + s_charset(c2));
			throw "LineNumber: " + pos.lineNumber;
		}
	}

	static public function s_charset(c: Charset): String {
		var ret = new haxe.ds.Vector<String>(c.length);
		for (i in 0...c.length) {
			ret[i] = c[i].toString();
		}
		return "[" + ret.join(",") + "]";
	}

	static public function rs_charset(cs: Charset): String {
		function unescape(c) {
			return switch(c) {
			case "\\".code, "+".code, "*".code, "?".code, "[".code, "]".code, "-".code:
				"\\" + String.fromCharCode(c);
			default:
				String.fromCharCode(c);
			}
		}
		var s = "";
		for (c in cs) {
			if (c.min == c.max) {
				s += unescape(c.min);
			} else {
				s += '${String.fromCharCode(c.min)}-${String.fromCharCode(c.max)}';
			}
		}
		return if (cs.length > 1) {
			"[" + s + "]";
		} else {
			s;
		}
	}

	static public function s_partern(p: Pattern): String {
		return switch (p) {
		case Empty:
			"";
		case Match(c):
			rs_charset(c);
		case Star(a):
			s_partern(a) + "*";
		case Plus(a):
			s_partern(a) + "+";
		case Choice(a):
			s_partern(a) + "?";
		case Next(a, b):
			s_partern(a) + s_partern(b);
		}
	}
}
