package lm;

#if macro
import lm.Charset;
import lm.LexEngine;
import haxe.macro.Type;
import haxe.macro.Expr;
import haxe.macro.Context;

@:access(lm.LexEngine)
class LR0Builder extends lm.LR0Base {

	public static inline var U8MAX = 0xFF;
	public static inline var U16MAX = 0xFFFF;

	var generator(default, null): NodeGenerator;
	var h: Map<String, Int>;
	var final_counter: Int;
	var lstates: List<State>;
	var per: Int;
	var perExit: Int;
	var table: Table;
	var segs: Int;
	var nstates: Int;
	var invalid: Int;
	var nfas: haxe.ds.Vector<Array<Node>>; // assoc with lhsA

	public inline function debugWrite(out) this.table.debugWrite(per, perExit, isBit16(), out);
	inline function isBit16() return this.invalid == U16MAX;

	function new(s_it, rest) {
		super(s_it, rest);
		this.h = new Map();
		this.lstates = new List();
		this.segs = 0;      // state_counter
		this.invalid = U16MAX;
		this.nfas = new haxe.ds.Vector(lhsA.length);
		this.final_counter = U16MAX - 1; // will compress it later.
		this.per = (this.width() - 1 | 15 ) + 1;
		this.generator = new NodeGenerator(nrules);
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
		// String -> Pattern
		var a = this.toPartern();
		// Pattern -> NFA(nodes)
		for (i in 0...a.length) {
			var pats = a[i];
			var len = pats.length;
			var nodes = [];
			for (j in 0...len) {
				var f = generator.newFinal();
				var n = generator.normalize(pats[j], f);
				nodes[j] = n;
			}
			nfas[i] = nodes;
		}
		// NFA -> DFA
		var begin = 0;
		for (e in this.starts) {
			compile( LexEngine.addNodes([], this.nfas[ e.index ]) );
			e.begin = begin;
			e.width = this.segs - begin;
			begin = this.segs;
			this.h = new Map(); // reset for next
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
		for (i in 0...bytes)
			tbls.set(i, INVALID);

		for (s in this.lstates) {
			tbls.set(tbls.exitpos(s.id), s.finalID == -1 ? INVALID: s.finalID);
			if (s.id < segs)
				LexEngine.makeTrans(tbls, s.id * per, s.sets, s.targets);
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
		for (p in 0...nodes.length) { // no need to iterate the nodes added later, because "::lhsClosure()" has already done it.
			for (nc in nodes[p].arrows) {
				for (c in nc.chars) {
					if ( isNonTerm(c.min) ) {
						var lval = c.min;     // the value of the last non-term
						var i = lval - maxValue;
						if (alt[i]) continue; // if already added to current "nodes".
						var atLast = generator.isFinal(nc.n);
						var exit = nc.n.id;
						if (!atLast) {
							addAll(nodes, this.nfas[i]);
						} else {
							mixing(nodes, i, lval, exit);
						}
						alt[i] = true;
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
		var ta = LexEngine.getTransitions( closure(nodes) );
		id = if (ta.length == 0) {
			final_counter--;
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

		var exits = [];
		for (n in nodes)
			if ( generator.isFinal(n) )
				exits.push(n.id);
		var f = switch (exits.length) {
		case 0: -1;
		case 1: exits[0];
		default:
			for (r in exits)
				Context.reportError("conflict case: " + r, ruleToCase(r).pos);
			throw new Error("conflict: " + exits.join(","), ruleToCase(exits[exits.length - 1]).pos);
		}
		lstates.push(new State(id, sets, targets, f));
		return id;
	}

	function checking() {
		var VALID = 1;
		var INVALID = this.invalid;
		// 1. Is switch case unreachable?
		var exits = haxe.io.Bytes.alloc(this.nrules);
		for (i in table.length - this.perExit...table.length) {
			var n = table.get(i);
			if (n == INVALID)
				continue;
			exits.set(n, VALID);
		}
		for (n in 0...this.nrules)
			if (exits.get(n) != VALID)
				throw new Error("Unreachable switch case", this.ruleToCase(n).pos);

		// 2. The "entry" is not allowed to be epsilon
		for (e in this.starts)
			if (this.table.exits(e.begin) != INVALID)
				throw new Error(lhsA[e.index].name + " is not allow to be EPSILON.", lhsA[e.index].pos);
	}

	function generate(): Array<Field> {
		var forceBytes = !Context.defined("js") || Context.defined("lex_rawtable");
		// force string as table format if `-D lex_strtable` and ucs2
		if (Context.defined("lex_strtable") && Context.defined("utf16")) forceBytes = false;

		var getU:Expr = null;
		var raw:Expr = null;
		if (forceBytes) {
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
		var lvs = this.lvalues.map(n -> macro $v{n}).toArray(); // (lvalue << 8 | length)
		var fields = (macro class {
			var s(get, never) : $ct_stream;
			@:dce inline function get_s() return stream;
			static var raw = $raw;
			static var lvs:Array<Int> = [$a{lvs}];
			static inline var INVALID = $v{this.invalid};
			static inline var NRULES  = $v{this.nrules};
			static inline var NSEGS   = $v{this.segs};
			static inline function getU(raw, i) return $getU;
			static inline function trans(r, s, t) return getU(r, $v{this.per} * s + t.term);
			static inline function exits(r, s) return getU(r, $v{this.table.length - 1} - s);
			inline function gotos(fid:Int):$ct_lval return cases(fid);
			final stream: $ct_stream;
			public function new(lex: lm.Lexer<Int>) {
				this.stream = @:privateAccess new lm.Stream<$ct_lval>(lex);
			}
			@:access(lm.Stream, lm.Tok)
			function _entry(state:Int, exp:Int):$ct_lval {
				var t = stream.newTok($i{sEof}, 0, 0);
				t.state = state;
				stream.unshift(t); // should be removed when returning
				var raw = raw;     // fast reference
				while (true) {
					while (true) {
						t = stream.next();   // next token
						state = trans(raw, state, t);
						if (state >= NSEGS)
							break;
						t.state = state;     // update state to stream-token
					}
					if (state == INVALID) {
						stream.pos -= 1;     // reverts if INVALID
						state = stream.offset( -1).state;
					}
					while (true) {
						var q = exits(raw, state);
						var value : $ct_lval = gotos(q);
						if (q >= NRULES)
							return value;    // error exiting
						t = stream.reduce( lvs[q] );
						if (t.term == exp) {
							stream.pos -= 2; // ready to discard
							stream.junk(2);  // commit
							return value;
						}
						t.val = value;
						state = trans(raw, stream.offset( -2).state, t);
						t.state = state;
						if (state < NSEGS)
							break;
					}
				}
				return gotos(INVALID); // goto switch default:
			}
		}).fields;
		// build siwtch
		var edef = this.unMatched != null ? this.unMatched : (macro {
			var t = stream.peek(0);
			throw stream.error('Unexpected "' + (t.term != $i{sEof} ? stream.str(t): $v{sEof}) + '"', t);
		});
		var ecases:Array<Case> = [];
		ecases.resize(this.nrules);
		for (i in 0...this.nrules) {
			ecases[i] = {values: [macro $v{i}], expr: this.vcases[i].action};
		}
		var here = Context.currentPos();
		var eswitch = {expr: ESwitch(macro (q), ecases, edef), pos: here};
		fields.push({
			name: "cases",
			access: [],
			kind: FFun({
				args: [{name: "q", type: macro: Int}],
				ret: ct_lval,
				expr: macro { final __s = stream; return $eswitch; },
			}),
			pos: here,
		});
		// starts =>
		for (i in 0...this.starts.length) {
			var en = this.starts[i];
			var lhs = lhsA[en.index];
			fields.push({
				name: lhs.name,
				access: [APublic, AInline],
				kind: FFun({
					args: [],
					ret: lhs.ctype,
					expr: macro return _entry($v{en.begin}, $v{lhs.value})
				}),
				pos: lhs.pos,
			});
		}
		return fields;
	}

	public static function build() {
		var ret = [];
		var reserve = new Map<String, Field>();
		try {
			var lrb = new LR0Builder("lm.LR0", reserve);
			var fields = lrb.make();
			// combine
			for (f in fields)
				if (!reserve.exists(f.name))
					ret.push(f);
			for (f in reserve)
				ret.push(f);
		} catch ( e : Error ) {
			Context.fatalError("[LR0 build] " + e.message , e.pos);
		}
		return ret;
	}
}
#else
extern class LR0Builder{}

@:autoBuild(lm.LR0Builder.build())
#end
@:remove interface LR0<LEX, LHS> {
}