package lm;

#if macro
import lm.Parser;
import lm.Charset;
import lm.LexEngine;
import haxe.macro.Type;
import haxe.macro.Expr;
import haxe.macro.Context;

@:access(lm.LexEngine)
class LR0Builder extends lm.Parser {

	public static inline var U8MAX = 0xFF;
	public static inline var U16MAX = 0xFFFF;

	var uid(default, null): Int;
	var h: Map<String, Int>;
	var final_counter: Int;
	var lstates: List<State>;
	var per: Int;
	var perExit: Int;
	var table: Table;
	var segs: Int;
	var nstates: Int;
	var invalid: Int;
	var entrys: Array<{index:Int, begin:Int, width:Int}>;
	var nfas: haxe.ds.Vector<Array<Node>>; // assoc with lhsA
	var used: haxe.ds.Vector<Bool>;        // Does not perform reachable detection for unused LHS
	var hasSide: Bool;                     // if have @:side on LHS

	public inline function debugWrite(out) this.table.debugWrite(per, perExit, isBit16(), out);
	inline function isBit16() return this.invalid == U16MAX;
	inline function isFinal(n: Node) return n.id < this.nrules;

	function new(s_it, rest) {
		super(s_it, rest);
		this.h = new Map();
		this.lstates = new List();
		this.segs = 0;      // state_counter
		this.invalid = U16MAX;
		this.entrys = [];
		this.nfas = new haxe.ds.Vector(lhsA.length);
		this.used = new haxe.ds.Vector(lhsA.length);
		this.final_counter = U16MAX - 1; // will compress it later.
		this.per = (this.width() - 1 | 15 ) + 1;
		this.uid = nrules;  // so all the finalsID must be less then nrules.
	}

	function dump(file:String) {
		var f = sys.io.File.write(file);
		f.writeString("\nProduction:\n");
		f.writeString(debug.Print.production(this));
		f.writeString(debug.Print.lr0Table(this));
		f.writeString("\n\nRAW:\n");
		this.debugWrite(f);
		f.close();
	}

	function make() {
		if ( isEmpty() ) return [];
		// Pattern -> NFA(nodes)
		var i = 0;
		var a = this.toPartern();
		for (p in 0...a.length) {
			var pats = a[p];
			var len = pats.length;
			var nodes = [];
			for (j in 0...len) {
				var f = new Node(i++);
				var n = initNode(pats[j], f);
				nodes[j] = n;
			}
			nfas[p] = nodes;
		}
		var begin = 0;
		function addEntry(i) {
			this.entrys.push({index: i, begin: begin, width: this.segs - begin});
			if (begin == this.segs)
				Context.fatalError("Empty: " + lhsA[i].name, lhsA[i].pos);
			begin = this.segs;
			this.used[i] = true;
		}
		// NFA -> DFA
		// main entry
		compile( LexEngine.addNodes([], this.nfas[0]) );
		addEntry(0);
		// side entrys
		for (i in 1...this.lhsA.length) {
			if (this.lhsA[i].side == false) continue;
			hasSide = true;
			this.h = new Map(); // reset
			compile( LexEngine.addNodes([], this.nfas[i]) );
			addEntry(i);
		}

		// properties
		this.nstates = lstates.length;
		this.invalid = nstates < U8MAX ? U8MAX : U16MAX;
		this.perExit = 1 + ((nstates - 1) | 15);
		// compress finalState
		var diff = final_counter + 1 - segs;
		for (s in lstates) {
			for (i in 0...s.targets.length)
				if (s.targets[i] > segs)
					s.targets[i] -= diff;
			if (s.id > segs) s.id -= diff;
		}
		// DFA -> Tables
		this.makeTable();

		#if lex_lr0table
		this.dump("lr0-table.txt");
		#end
		this.checking();
		return this.generate();
	}

	function makeTable() {
		var INVALID = this.invalid;
		var bytes = (segs * per) + perExit;
		var tbls = new Table(bytes);
		for (i in 0...bytes) tbls.set(i, INVALID);

		for (s in this.lstates) {
			tbls.set(tbls.exitpos(s.id), s.finalID == -1 ? INVALID: s.finalID);
			if (s.id < segs)
				LexEngine.makeTrans(tbls, s.id * per, s.trans, s.targets);
		}
		this.table = tbls;
	}

	function closure(nodes: Array<Node>) {
		function mixing(dst: Array<Node>, from:Int, lval: Int, rule: Int) {
			var nfa = this.nfas[from];
			var lhs = this.lhsA[from];
			var caze = ruleToCase(rule);
			if (caze.left == null) {          // [E] | [...,T,E] | [..., undefined_op, E]
				for (i in 0...nfa.length)
					if (caze != lhs.cases[i]) // Exclude only the rule itself
						LexEngine.addNode(dst, nfa[i]);
			} else {
				var left = caze.left;
				for (i in 0...nfa.length) {
					var right = lhs.cases[i].right;
					if ( right == null || right.own != lval || right.prio == -1
					|| (right.prio >= 0 && (left.prio < right.prio || left.type == Right && left.prio <= right.prio))
					) LexEngine.addNode(dst, nfa[i]);
				}
			}
		}
		inline function addAll(nodes, nfa) LexEngine.addNodes(nodes, nfa);
		var alt = new haxe.ds.Vector<Bool>(lhsA.length);
		for (n in nodes) {
			for (nc in n.trans) {
				for (c in nc.chars) {
					if ( isNonTerm(c.min) ) {
						var lval = c.min;     // the value of the last non-term
						var i = lval - maxValue;
						if (alt[i]) continue; // if already added to current "nodes".
						var atLast = isFinal(nc.n);
						var exit = nc.n.id;
						if (!atLast) {
							addAll(nodes, this.nfas[i]);
						} else {
							mixing(nodes, i, lval, exit);
						}
						alt[i] = true;
						used[i] = true;
						// left subs.
						for (s in lhsA[i].lsubs) {
							var j = s - maxValue;
							if (alt[j]) continue;
							if (!atLast) {
								addAll(nodes, this.nfas[j]);
							} else {
								mixing(nodes, j, lval, exit);
							}
							alt[j] = true;
							used[j] = true;
						}
					}
					break;
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

		var exits = [];
		for (n in nodes)
			if ( isFinal(n) )
				exits.push(n.id);
		var f = switch (exits.length) {
		case 0: -1;
		case 1: exits[0];
		default: // since LR(0) so the conflicts can not be resolved.
			for (r in exits)
				Context.warning("conflict case: " + r, ruleToCase(r).pos);
			Context.fatalError("conflict: " + exits.join(","), ruleToCase(exits[exits.length - 1]).pos);
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

	function checking() {
		var INVALID = this.invalid;
		// init exitN/NRule => final_state rules
		var rules = new haxe.ds.Vector<Int>(this.nrules);
		for (n in 0...this.nrules)
			rules[n] = INVALID;
		for (i in table.length - this.perExit...table.length) {
			var n = table.get(i);
			if (n == INVALID) continue;
			// since: i = table.length - 1 - state; // table.exitpos
			// so   : state = table.length - 1 - i;
			rules[n] = table.length - 1 - i;
		}
		// 1. Is switch case unreachable?
		for (n in 0...this.nrules)
			if (rules[n] == INVALID) {
				var lhs = byRule(n);
				if ( !this.used.get( index(lhs) ) ) continue;
				var msg = "Unreachable switch case";
				if (!lhs.side)
					msg += ' OR missing @:side on "' + lhs.name + '"';
				Context.fatalError(msg, this.ruleToCase(n).pos);
			}
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
			if (!find) throw("Missing terminator in STATE: " + i);
		}
		// more?
	}

	function generate(): Array<Field> {
		var force_bytes = !Context.defined("js") || Context.defined("lex_rawtable");
		// force string as table format if `-D lex_strtable` and ucs2
		if (Context.defined("lex_strtable") && Context.defined("utf16")) force_bytes = false;

		var getU:Expr = null;
		var raw:Expr = null;
		if (force_bytes) {
			var bytes = this.table.toByte(isBit16());
			var resname = "_" + StringTools.hex(haxe.crypto.Crc32.make(bytes)).toLowerCase();
			Context.addResource(resname, bytes);
		#if hl
			getU = isBit16() ? macro raw.getUI16(i << 1) : macro raw.get(i);
			raw = macro haxe.Resource.getBytes($v{resname}).getData().bytes;
		#else
			getU = isBit16() ? macro raw.getUInt16(i << 1) : macro raw.get(i);
			raw = macro haxe.Resource.getBytes($v{resname});
		#end
		} else {
			raw = macro $v{ this.table.map(i -> String.fromCharCode(i)).join("") };
			getU = macro StringTools.fastCodeAt(raw, i);
		}
		var lva = this.reduceDetail.map(n -> macro $v{n}).toArray(); // (lvalue << 8 | length)
		var defs = macro class {
			static var raw = $raw;
			static var lva:Array<Int> = [$a{lva}];
			static inline var INVALID = $v{this.invalid};
			static inline var NRULES  = $v{this.nrules};
			static inline var NSEGS   = $v{this.segs};
			static inline var MAXVALUE   = $v{this.maxValue};        // see .isNonTerm(v)/.isTerm(v)
			static inline function getU(i:Int):Int return $getU;
			static inline function trans(s:Int, c:Int):Int return getU($v{this.per} * s + c);
			static inline function exits(s:Int):Int return getU($v{this.table.length - 1} - s);
			static inline function gotos(fid:Int, s:$ct_stream):$ct_lval return cases(fid, s);
			var stream: $ct_stream;
			public function new(lex: lm.Lexer<Int>) {
				this.stream = new lm.Stream<$ct_lval>(lex, 0);
			}
			@:access(lm.Stream, lm.Tok)
			static function _entry(stream: $ct_stream, state:Int, exp:Int, until:Bool):$ct_lval {
				var prev = state;
				var t: lm.Stream.Tok<$ct_lval> = null;
				var dx = 0;
				var keep = stream.pos; // used for _side.
				while (true) {
					while (true) {
						t = stream.next();
						state = trans(prev, t.term);
						t.state = state;
						if (state >= NSEGS)
							break;
						prev = state;
					}
					if (state == INVALID) {
						state = prev;
						dx = 1;
					}
					var q = exits(state);
					if (q < NRULES) {
						stream.pos -= dx;
					} else {
						break;  // throw error.
					}
					dx = 0;
					while (true) {
						var value:$ct_lval = gotos(q, stream);
						t = stream.reduce( lva[q] );
						if ($e{ hasSide ? macro (t.term == exp && !until) : macro (t.term == exp) }) {
							-- stream.pos;      // discard the last token
							stream.junk(1);
							return value;
						}
						t.val = value;
						t.state = trans(stream.offset( -2).state, t.term);
						prev = t.state;
						if (prev < NSEGS)
							break;
						$e{ // Macro selection
							if (hasSide) {
								macro
								if (prev == INVALID) {
									if (until && exp == t.term)
									return value;
									throw stream.UnExpected(t);
								}
							} else {
								macro {};
							}
						}
						q = exits(prev);
					}
				}
				$e{ // Macro selection
					if (hasSide) {
						macro if ( until && (stream.pos - dx == keep + 1) && (exp == stream.cached[keep].term) ) // hasSide 3
								return stream.cached[keep].val;
					} else {
						macro {};
					}
				}
				t = stream.offset( -1);
				throw stream.error('Unexpected "' + (t.term != $i{sEof} ? stream.str(t): $v{sEof}) + '"', t);
			}
			@:dce @:access(lm.Stream, lm.Tok)
			static function _side(stream: $ct_stream, state:Int, lv: Int):$ct_lval {
				var keep = stream.pos;
				var prev = stream.offset( -1);
				var t = stream.reqTok(lv, prev.pmax, prev.pmax);
				t.state = state;
				stream.shift(t);
				var value = _entry(stream, state, lv, true);
				stream.pos = keep;
				stream.junk(2);
				return value;
			}
		}
		// build switch
		var actions = [];
		actions.resize(this.nrules);
		var i = 0;
		for (c in this.vcases)
			actions[i++] = c.action;
		var here = Context.currentPos();
		var defCase = actions.pop();
		var liCase:Array<Case> = [];
		liCase.resize(actions.length);
		for (i in 0...actions.length) {
			liCase[i] = {values: [macro $v{i}], expr: actions[i]};
		}
		var eSwitch = {expr: ESwitch(macro (q), liCase, defCase), pos: here};
		defs.fields.push({
			name: "cases",
			access: [AStatic],
			kind: FFun({
				args: [{name: "q", type: macro: Int}, {name: "s", type: ct_stream}],
				ret: ct_lval,
				expr: macro {
					@:mergeBlock $b{preDefs};
					return $eSwitch;
				}
			}),
			pos: here,
		});

		// main entry
		var en = this.entrys[0];
		var lhs = lhsA[en.index];
		defs.fields.push({
			name: lhs.name,
			access: [APublic, AInline],
			kind: FFun({
				args: [],
				ret: lhs.ctype,
				expr: macro return _entry(stream, $v{en.begin}, $v{lhs.value}, false)
			}),
			pos: lhs.pos,
		});
		// other entrys =>
		for (i in 1...this.entrys.length) {
			var en = this.entrys[i];
			var lhs = lhsA[en.index];
			defs.fields.push({
				name: lhs.name,
				access: [AStatic, AInline],
				kind: FFun({
					args: [{name: "s", type: ct_stream}],
					ret: lhs.ctype,
					expr: macro return _side(s, $v{en.begin}, $v{lhs.value})
				}),
				pos: lhs.pos,
			});
		}
		return defs.fields;
	}

	public static function build() {
		var allFields = new Map<String, Field>();
		var lrb = new LR0Builder("lm.LR0", allFields);
		var defs = lrb.make();
		// combine
		var ret = [];
		for (f in defs)
			if (!allFields.exists(f.name))
				ret.push(f);
		for (f in allFields) ret.push(f);
		return ret;
	}
}
#else
extern class LR0Builder{}

@:autoBuild(lm.LR0Builder.build())
#end
@:remove interface LR0<LEX, LHS> {
}