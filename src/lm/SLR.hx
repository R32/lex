package lm;

#if macro
import lm.Parser;
import lm.Charset;
import lm.LexEngine;
import haxe.macro.Type;
import haxe.macro.Expr;
import haxe.macro.Context;

private typedef OpAssocExt = {
	var lvl: Int;    // which state is op value found
	var nxt: Int;    // in lvl state, when hit (a op value) then goto nxt.
	var hit: Int;    // in nxt state, a hit value
	var fid: Int;    // in nxt state, when hit (a hit value) then goto final state.
	var prio: Int;   // op precedence. The higher the value, the higher the priority
	var value: Int;  // op value
	var left: Bool;  // is left assoc?
}

@:access(lm.LexEngine)
class SLRBuilder extends lm.Parser {

	public static inline var U8MAX = 0xFF;
	public static inline var U16MAX = 0xFFFF;

	var uid(default, null): Int;
	var finals: Int;
	var h: Map<String, Int>;
	var final_counter: Int;
	var lstates: List<State>;
	var states: Array<State>;
	var per: Int;
	var perRB: Int;
	var table: Table;
	var segs: Int;
	var segsEx: Int;
	var nrules: Int;
	var nstates: Int;
	var invalid: Int;
	var entrys: Array<{begin:Int, segs:Int, nrules:Int}>;
	var na: Array<Array<Node>>; // assoc with lhsA

	public inline function posRB() return this.segsEx * this.per;
	public inline function posRBL() return posRB() + this.perRB;

	function new(s_it, rest) {
		super(s_it, rest);
		this.h = new Map();
		this.lstates = new List();
		this.segs = 0;  // state_counter
		this.invalid = U16MAX;
		this.entrys = [];
		this.final_counter = U16MAX - 1; // compress it later.
		this.nrules = 0;
	}

	function make() {
		var a = this.toPartern();
		var lcases = Lambda.fold(a, (p, n) -> p.length + n, 0);
		this.per = (this.maxValue + this.lhsA.length | 15) + 1;
		this.uid = lcases;
		this.finals = lcases;
		var i = 0;
		na = [];
		for (pats in a) {
			var len = pats.length;
			var nodes = [];
			for (j in 0...len) {
				var f = new Node(i);
				var n = initNode(pats[j], f);
				nodes[j] = n;
				++ i;
			}
			na.push(nodes);
		}
		// TODO: independence @:indep

		// main entry
		var init = LexEngine.addNodes([], this.na[0]);
		compile(init);

		// properties
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

		this.states = Lambda.array(lstates);
		this.states.sort(State.onSort);
		this.doPrecedence();

		// make talbe
		this.makeTable();
		this.rollback();

		this.checking();
	}

	function makeTable() {
		var INVALID = this.invalid;
		var bytes = (segsEx * per) + (3 * perRB);
		var tbls = new Table(bytes);
		for (i in 0...bytes) tbls.set(i, INVALID);

		for (s in this.lstates) {
			tbls.set(tbls.exitpos(s.id), s.finalID == -1 ? INVALID: s.finalID);
			if (s.id < segsEx)
				LexEngine.makeTrans(tbls, s.id * per, s.trans, s.targets);
		}
		this.table = tbls;
	}

	function closure(nodes: Array<Node>) {
		function noSelf(dst: Array<Node>, src: Array<Node>, self: Int) {
			function isSelf (n: Node) {
				for (nc in n.trans)
					for (c in nc.chars)
						return c.min == self;
				return false;
			}
			for (n in src)
				if (!isSelf(n))
					LexEngine.addNode(dst, n);
		}
		var alt = new haxe.ds.Vector<Bool>(lhsA.length);
		for (n in nodes) {
			for (nc in n.trans) {
				for (c in nc.chars) {
					var isLast = nc.n.id < this.finals;
					var self = c.min;
					if (self >= maxValue) {  // if non-terms
						var i = self - maxValue;
						if (alt[i]) continue;
						var ex = null;
						if (!isLast) {
							LexEngine.addNodes(nodes, this.na[i]);
						} else {
							noSelf(nodes, this.na[i], self);
						}
						alt[i] = true;
						// left subs.
						for (s in lhsA[i].lsubs) {
							var j = s - maxValue;
							if (alt[j]) continue;
							if (!isLast) {
								LexEngine.addNodes(nodes, this.na[j]);
							} else {
								noSelf(nodes, this.na[j], self);
							}
							alt[j] = true;
						}
					}
				}
			}
		}
		return nodes;
	}

	function compile(nodes: Array<Node>): Int {
		var sid = nodes.map( n -> n.id ).join("+");
		var id = h.get(sid);
		if (id != null)
			return id;
		var ta: Array<NAChars> = LexEngine.getTransitions( closure(nodes) );
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

	function rollback() {
		inline function epsilon(seg) return table.exits(seg);
		var alt = new haxe.ds.Vector<Bool>(this.perRB);
		for (i in 0...alt.length) alt[i] = false;
		var INVALID = this.invalid;
		var rollpos = this.posRB();
		var rlenpos = this.posRBL();
		var que = new List<{exit: Int, nxt: Int, len: Int}>(); // FIFO
		function loop(exit, seg, length) {
			alt[seg] = true;
			var base = seg * this.per;
			for (lv in 0...this.per) {
				var nxt = table.get(lv + base);
				if (nxt == INVALID || alt[nxt] || lv >= maxValue) continue;
				table.set(rollpos + nxt, exit);
				table.set(rlenpos + nxt, length);
				if (epsilon(nxt) == INVALID)
					que.add({exit: exit, nxt: nxt, len: length + 1});
			}
		}
		for (seg in 0...this.segs) {
			var exit = epsilon(seg);
			if (exit == INVALID) continue;
			loop(exit, seg, 1);
		}
		while (true) {
			var q = que.pop();
			if (q == null) break;
			loop(q.exit, q.nxt, q.len);
		}
	}

	function write(out: haxe.io.Output, split = false) {
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

	function doPrecedence() {
		if (assoc.length == 0) return;
		inline function segStart(i) return i * this.per;
		var INVALID = this.invalid;
		var lvlMap = new Map<Int, Array<OpAssocExt>>(); // lvl => []
		var fidMap = new Map<Int, Array<OpAssocExt>>(); // fid => []
		function parse(tmp: Table, begin:Int, max:Int) {
			for (i in begin...max) {
				var start = segStart(i);
				for (op in assoc) {
					var nxt = tmp.get(start + op.value);
					if (nxt >= this.segs) continue;
					var fi = segStart(nxt);
					for (p in (fi + maxValue)...(fi + maxValue + lhsA.length)) { // only non-terminal
						var fid = tmp.get(p);
						if (fid == INVALID || fid < this.segs) continue;
						// now check which stage can jump to "i"
						var hit = p - fi;
						for (j in begin...max) {
							if (tmp.get(segStart(j) + hit) == i) {
								var col = lvlMap.get(i);
								if (col == null) {
									col = [];
									lvlMap.set(i, col);
								}
								var ex = fidMap.get(fid);
								if (ex == null) {
									ex = [];
									fidMap.set(fid, ex);
								}
								var x = {lvl: i, hit: hit, nxt: nxt, fid: fid, prio: op.prio, value: op.value, left: op.left};
								col.push(x);
								ex.push(x);
								break;
							}
						}
					}
				}
			}
		}
		function swap(dst, src) {
			if (dst == src) return;
			var x = this.states[dst]; // x.id == dst
			var y = this.states[src]; // y.id == src;
			// swap targets
			for (s in this.states) {
				for (i in 0...s.targets.length) {
					var t = s.targets[i];
					if (t == dst) {
						s.targets[i] = src;
					} else if (t == src) {
						s.targets[i] = dst;
					}
				}
			}
			// swap states
			x.id = src;
			y.id = dst;
			this.states[dst] = y;
			this.states[src] = x;
			// swap opAssocEx.
			var a = fidMap.get(dst);
			var b = fidMap.get(src);
			if (a != null) {
				for (op in a)
					op.fid = src;
				fidMap.set(src, a);
			} else {
				fidMap.remove(src);
			}
			for (op in b)
				op.fid = dst;
			fidMap.set(dst, b);
		}

		// need a tmp table for analysis
		var tmp = new Table( segStart(this.segs) );
		for (i in 0...tmp.length)
			tmp.set(i, INVALID);
		for (s in this.states) {
			if (s.id >= this.segs) continue;
			@:privateAccess LexEngine.makeTrans(tmp, segStart(s.id), s.trans, s.targets);
		}
		// parse
		for (i in 0...this.segs)
			parse(tmp, 0, this.segs);

		// If the same ".fid" has an inconsistent ".prio" then raise a conflict error.
		for (a in fidMap) {
			var prio = a[0].prio;
			for (i in 1...a.length)
				if (a[i].prio != prio)
					throw "Operator Precedence Conflict";
		}
		// If all of the left assoc op has same ".prio" in same ".lvl" then remove it
		for (lvl in lvlMap.keys()) {
			var a = lvlMap.get(lvl);
			var prio = a[0].prio;
			var find = false;
			for (i in 1...a.length)
				if (prio != a[i].prio || a[i].left == false) {
					find = true;
					break;
				}
			if (!find) lvlMap.remove(lvl);
		}
		// highest(maximum) precedence in lvl
		var larMap = new haxe.ds.Vector<{left:Int, right: Int}>(this.perRB);
		for (a in lvlMap) {
			var lar = {left: -1, right: -1};
			larMap.set(a[0].lvl, lar);
			for (op in a) {
				if (op.left) {
					if (op.prio > lar.left) lar.left = op.prio;
				} else {
					if (op.prio > lar.right) lar.right = op.prio;
				}
			}
		}
		// (for reduce the size of the table)
		var dst = this.segs;
		var skiped = 0;
		var count = 0;
		var dupMap = new haxe.ds.Vector<Bool>(this.perRB);
		for (a in lvlMap) {
			var lar = larMap.get(a[0].lvl);
			for (op in a) {
				if (dupMap.get(op.fid)) continue;
				count ++;
				if (op.left && lar.left > lar.right && op.prio == lar.left) {
					++ skiped;
				} else {
					swap(dst++, op.fid);
				}
				dupMap.set(op.fid, true);
			}
		}
		this.segsEx = this.segs + count - skiped;
		// write to s.targets and this.trans
		for (fid in this.segs...this.segsEx) {
			var s = this.states[fid];
			var own = fidMap.get(fid)[0];
			var tar = [];  // targets
			var cset = []; // trans
			var i = 0;
			var a = lvlMap.get(own.lvl);
			for (op in a) {
				if (own.left) {
					if (op.prio > own.prio) {
						tar[i] = op.nxt;
						cset[i] = Char.c3(op.value, op.value, i);
						++ i;
					}
				} else {
					if (op.prio >= own.prio) {
						tar[i] = op.nxt;
						cset[i] = Char.c3(op.value, op.value, i);
						++ i;
					}
				}
			}
			s.trans = cset;
			s.targets = tar;
		}
	}

	function checking() {
		var INVALID = this.invalid;
		// init exitN/NRule => final_state rules
		var rules = new haxe.ds.Vector<Int>(this.nrules);
		for (n in 0...this.nrules)
			rules[n] = INVALID;
		for (i in table.length-this.perRB...table.length) {
			var n = table.get(i);
			if (n == INVALID) continue;
			// since: i = table.length - 1 - state; // table.exitpos
			// so   : state = table.length - 1 - i;
			rules[n] = table.length - 1 - i;
		}
		// 1. Is switch case unreachable?
		for (n in 0...this.nrules)
			if (rules[n] == INVALID)
				Context.fatalError("Unreachable switch case", this.ruleToCase(n).pos);
		// 2. each state
		for (i in 0...this.segs) {
			if (table.exits(i) != INVALID) continue; // epsilon
			var base = i * this.per;
			var find = false;
			for (p in base ... base + this.maxValue) {
				var c = table.get(p);
				if (c != INVALID) {
					find = true;
					break;
				}
			}
			if (!find) throw("Missing terminator in S" + i);
		}
		// 3. switch guard
		var n = 0;
		for (lhs in this.lhsA) {
			for (li in lhs.cases) {
				if (li.guard != null) {
					var final_state = rules[n];
					if (this.table.get(this.posRB() + final_state) == INVALID)
						Context.fatalError("No switch case that can be rollback from here", li.guard.pos);
				}
				++ n;
			}
		}
		// more?
	}

	public static function build() {
		var allFields = new Map<String, Field>();
		var slr = new SLRBuilder("lm.SLR", allFields);
		if (slr.lhsA.length == 0)
			return null;
		slr.make();

		#if lex_lr0table
		var f = sys.io.File.write("slr-table.txt");
		f.writeString("\nProduction:\n");
		f.writeString(debug.Print.production(slr));
		f.writeString(debug.Print.slrTable(slr));
		f.writeString("\n\nRAW:\n");
		slr.write(f, true);
		f.close();
		#end
		return [];
	}
}
#else
extern class SLRBuilder{}

@:autoBuild(lm.SLRBuilder.build())
#end
@:remove interface SLR<LEX, LHS> {
}