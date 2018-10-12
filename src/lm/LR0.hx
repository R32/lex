package lm;

#if macro
import haxe.macro.Expr;
import haxe.macro.Type;
import haxe.macro.Context;
import haxe.macro.Expr.Position;
import lm.Charset;
import lm.LexEngine;

using haxe.macro.Tools;

typedef Udt = {    // user defined token
	t: Bool,       // terminal or not
	name: String,  // ident
	value: Int,    // must be `enum abstract (Int)`
	cset: Charset, // CSet.single(value)
	pos: Position, // haxe.macro.Position
}

typedef Symbol = { // from switch cases
	t: Bool,
	name: String,  // ident name
	cset: Charset,
	ex: String,    // extract name. CStr(s)| then ex = "s" for terminal   | e = expr then ex = "e" for non-terminal
	pos: Position,
}

typedef SymSwitch = {
	name: String,        // Associated var name
	value: Int,          // Automatic growth
	cases: Array<Case>,
	pos: Position,
}

typedef SymbolSet = {
	expr: Expr,
	guard: Null<Expr>,
	syms: Array<Symbol>,
	pos: Position, // case pos
}

typedef Lhs = {    // one switch == one Lhs
	name: String,  // associated var name(non-terminals name)
	value: Int,    // automatic increase from "maxValue"
	epsilon: Bool, // if have switch "default:" or "case []:"
	cases: Array<SymbolSet>,
	cbegin: Int,   // cases begin
	pos: Position,
}

typedef LhsArray = Array<Lhs>;   // all switches.

typedef OpAssoc = Array<{left: Bool, prio: Int, value: Int}>

private typedef OpAssocExt = {
	var lvl: Int;    // which state is op value found
	var nxt: Int;    // in lvl state, when hit (a op value) then goto nxt.
	var hit: Int;    // in nxt state, a hit value
	var fid: Int;    // in nxt state, when hit (a hit value) then goto final state.
	var prio: Int;   // op precedence. The higher the value, the higher the priority
	var value: Int;  // op value
	var left: Bool;  // is left assoc?
}

class LR0Builder {

	var termls: Array<Udt>;      // all Terminal
	var termlsC_All: Charset;    // Terminal Universal Set.
	var udtMap: Map<String,Udt>; // User Defined Terminal + Non-Terminal
	var maxValue: Int;           // if value >= maxValue then it must be a non-terminal
	var lhsA: LhsArray;
	var lhsMap: Map<String,Lhs>;
	var sEof: String;            // by @:rule from Lexer
	var funMap: Map<String, {name: String, ct: ComplexType, args: Int}>; //  TokenName => FunctionName
	var ct_terms: ComplexType;   // token completeType
	var ct_lhs: ComplexType;     // unify all type of lhsA.
	var ct_stream: ComplexType;  //
	var ct_stream_tok: ComplexType;
	var preDefs: Array<Expr>;    // for function cases()
	var assoc: OpAssoc;
	var n2Lhs: haxe.ds.Vector<Int>;  // NRule => (lvalue << 8) | syms.length. (for reduction)
	var s2Lhs: haxe.ds.Vector<Lhs>;  // State belone which Lhs. available after lexData()
	public function new(t_tok, t_lhs, es) {
		maxValue = 0;
		termls = [];
		termlsC_All = [];
		lhsA = [];
		preDefs = [];

		udtMap = new Map();
		lhsMap = new Map();
		funMap = new Map();

		sEof = es;
		ct_terms = Context.toComplexType(t_tok);
		ct_lhs = Context.toComplexType(t_lhs);
		ct_stream = macro :lm.Stream<$ct_lhs>;
		ct_stream_tok = macro :lm.Stream.Tok<$ct_lhs>;
		parseToken(t_tok);
	}

	function parsePrecedence(cls:ClassType) {
		function extract(a: Array<ObjectField>, assoc: OpAssoc) {
			var dup = new Map<String, Bool>();
			for (i in 0...a.length) {
				var li = a[i].expr;
				var left = if (a[i].field == "left") {
					true;
				} else if (a[i].field == "right") {
					false;
				} else {
					continue;
				}
				switch (li.expr) {
				case EArrayDecl(a):
					for (e in a) {
						var term = null;
						switch (e.expr) {
						case EConst(CIdent(s)) if ((term = this.udtMap.get(s)) != null && term.t):
							if (dup.exists(s))
								Context.fatalError("Duplicate Token " + e.toString(), e.pos);
							dup.set(s, true);
							assoc.push( {left: left, prio: i, value: term.value} );
						case _:
							Context.fatalError("UnSupported Token: " + e.toString(), e.pos);
						}
					}
				default:
					Context.fatalError("UnSupported Type",li.pos);
				}
			}
		}
		assoc = [];
		var rule = cls.meta.extract(":rule");
		if (rule.length > 0) {
			var obj = rule[0].params[0];
			switch (obj.expr) {
			case EObjectDecl(a):
				extract(a, assoc);
			default:
			}
		}
	}

	function parseToken(tk: Type) {
		switch (tk) {
		case TAbstract(_.get() => ab, _):
			for (field in ab.impl.get().statics.get()) {
				for (meta in field.meta.get()) {
					if (meta.name != ":value") continue;
					switch(meta.params[0].expr) {
					case ECast({expr: EConst(CInt(i))}, _):
						var n = Std.parseInt(i);
						if (n < 0 || n > 99) // TODO: limited token value
							Context.fatalError("Value should be [0-99]", field.pos);
						if (n > maxValue) maxValue = n;
						firstCharChecking(field.name, UPPER, field.pos);
						var t = {t: true, name: field.name, value: n, cset: CSet.single(n), pos: field.pos};
						termls.push(t);
						udtMap.set(t.name, t);
						if (t.name != this.sEof)
							termlsC_All = CSet.union(termlsC_All, t.cset);
					case _:
					}
				}
			}
		case _:
		}
		if (udtMap.get(this.sEof) == null) throw "Invalid EOF value: " + this.sEof; //
		++ maxValue;
	}

	function getTermlCSet(name: String) {
		if (name == "_") return termlsC_All;
		var t = udtMap.get(name);
		if (t != null) {
			return t.t ? t.cset : null; // Non-Termls if .t == false
		}
		if (name.length >= 2) {         // if name == "Op" then OpPlus, OpMinus, OpXxxx ....
			var cset = [];
			for (t in termls)
				if (StringTools.startsWith(t.name, name))
					cset = CSet.union(cset, t.cset);
			// to prevent abuse
			if (cset.length > 1 || (cset.length > 0 && (cset[0].max > cset[0].min)))
				return cset;
		}
		return null;
	}

	function getNonTermlCSet(name: String) {
		var t = udtMap.get(name);
		return t == null || t.t ? null : t.cset;
	}

	// final state => rule
	inline function byRule(n):Lhs return lhsA[(this.n2Lhs[n] >> 8) - maxValue]; // index = lhs.value - maxValue
	inline function byState(s):Lhs return s2Lhs[s];

	function ruleToCase(n: Int): SymbolSet {
		var lhs = byRule(n);
		return lhs.cases[n - lhs.cbegin];
	}

	function transform(swa: Array<SymSwitch>) {
		for (sw in swa) { // init udtMap first..
			this.udtMap.set(sw.name, {t: false, name: sw.name, value: sw.value, cset: CSet.single(sw.value), pos: sw.pos});
		}
		inline function setEpsilon(lhs, pos) {
			if (lhs.epsilon) Context.fatalError('Duplicate "default:" or "case _:"', pos);
			lhs.epsilon = true;
		}
		function getCSet(name, pos) {
			var cset = getTermlCSet(name);
			if (cset == null) Context.fatalError("Undefined: " + name, pos);
			return cset;
		}
		var cbegin = 0;
		for (sw in swa) {
			var lhs: Lhs = { name: sw.name, value: sw.value, cases: [], cbegin: cbegin, epsilon: false, pos: sw.pos};
			for (c in sw.cases) {
				switch(c.values) {
				case [{expr:EArrayDecl(el), pos: pos}]:
					if (lhs.epsilon)
						Context.fatalError('This case is unused', c.values[0].pos);
					var g: SymbolSet = {expr: c.expr, guard: c.guard, syms: [], pos: pos};
					for (e in el) {
						switch (e.expr) {
						case EConst(CIdent(i)):
							firstCharChecking(i, UPPER, e.pos);            // e.g: CInt
							g.syms.push( {t: true, name: i,    cset: getCSet(i, e.pos), ex: null, pos: e.pos} );
						// TODO: maybe it's not good..
						case EParenthesis(macro $i{i}):
							firstCharChecking(i, LOWER, e.pos);            // all termls but no Eof
							g.syms.push( {t: true, name: "_",  cset: termlsC_All,       ex: i,    pos: e.pos} );

						case ECall(macro $i{i}, [macro $i{v}]):            // e.g: CInt(n)
							firstCharChecking(i, UPPER, e.pos);
							firstCharChecking(v, LOWER, e.pos);
							g.syms.push( {t: true, name: i,    cset: getCSet(i, e.pos), ex: v,    pos: e.pos} );

						case EBinop(OpAssign, macro $i{v}, macro $i{nt}):  // e.g: e = expr
							var udt = udtMap.get(nt);
							if (udt == null || udt.t == true)
								Context.fatalError("Undefined non-terminal: " + nt, e.pos);
							if (el.length == 1 && nt == sw.name)
								Context.fatalError("Infinite recursion", e.pos);
							g.syms.push( {t: false, name: nt, cset: udt.cset , ex: v , pos: e.pos} );

						case EBinop(OpAssign, macro $i{v}, macro $a{a}):   // e.g: t = [OpPlus, OpMinus]
							var cset = CSet.C_EMPTY;
							for (t in a) {
								switch (t.expr) {
								case EConst(CIdent(s)):
									firstCharChecking(s, UPPER, t.pos);
									cset = CSet.union(cset, getCSet(s, t.pos));
								default:
									Context.fatalError("Unsupported: " + t.toString(), t.pos);
								}
							}
							if (cset == CSet.C_EMPTY)
								Context.fatalError("Empty", pos);
							g.syms.push( {t: true, name: "_", cset: cset, ex: v, pos: e.pos} );

						case _:
							Context.fatalError("Unsupported: " + e.toString(), e.pos);
						}
					}
					if (g.syms.length == 0)
						setEpsilon(lhs, pos);
					lhs.cases.push(g);
				case [{expr:EConst(CIdent("_")), pos: pos}]: // case _: || defualt:
					setEpsilon(lhs, pos);
					lhs.cases.push({expr: c.expr, guard: null, syms: [], pos: pos});
				case [e]:
					Context.fatalError("Expected [ patterns ]", e.pos);
				case _:
					Context.fatalError("Comma notation is not allowed while matching streams", c.values[0].pos);
				}
			}
			cbegin += lhs.cases.length;
			this.lhsA.push(lhs);
		}
		organize();
	}

	function organize() {
		// add to lhsMap.
		for (lhs in lhsA) {
			if (lhsMap.exists(lhs.name))
				Context.fatalError("Duplicate rule field declaration: " + lhs.name, lhs.pos);
			lhsMap.set(lhs.name, lhs);
		}

		// the "entry" must place  "EOF " at the end
		var entry = lhsA[0];
		for (li in entry.cases) {
			var last = li.syms.length > 0 ? li.syms[li.syms.length - 1] : null;
			if (last == null || last.name == null || last.name != this.sEof)
				Context.fatalError("for entry you must place *"+ this.sEof +"* at the end", li.pos);
		}

		// find max and second largest
		var lsecond = 0, lmax = 0, lcases = 0;
		for (lhs in lhsA) {
			lcases += lhs.cases.length;
			for (li in lhs.cases) {
				var len = li.syms.length;
				if (len > lmax) {
					lsecond = lmax;
					lmax = len;
				}
			}
		}

		if (lsecond == 0) lsecond = lmax;
		for (i in 0...lsecond) {
			var stok = "_t" + (i + 1);
			preDefs.push(macro var $stok: $ct_stream_tok);
		}
		// Scanning for reduce the temp varialbes of _t1~_tN
		var tmp: Array<Null<Bool>>;
		function loop(e: Expr) {
			switch(e.expr) {
			case EConst(CIdent(s)):
				if (s.substr(0, 2) == "_t") {
					var i = Std.parseInt(s.substr(2, s.length - 2));
					if (i != null)
						tmp[i - 1] = true;
				}
			default:
				e.iter(loop);
			}
		}
		var toks:Array<Array<Null<Bool>>> = [];
		toks.resize(lcases);
		var ti = 0;
		for (lhs in lhsA) {
			for (li in lhs.cases) {
				tmp = [];
				tmp.resize(li.syms.length);
				if (li.expr == null)
					Context.fatalError("Need return *" + ct_lhs.toString() + "*", li.pos);
				li.expr.iter(loop);
				toks[ti++] = tmp;
			}
		}
		this.n2Lhs = new haxe.ds.Vector<Int>(lcases);
		// duplicate var checking. & transform expr
		ti = 0;
		for (lhs in lhsA) {
			for (li in lhs.cases) {
				var row = ["s" => true, "_q" => true]; // reserve "s" as stream
				var a:Array<Expr> = [];
				var len = li.syms.length;
				this.n2Lhs[ti] = lhs.value << 8 | len;
				for (i in 0...len) {
					var s = li.syms[i];
					var dx = -(len - i);
					if (toks[ti][i]) {
						var stok = "_t" + (i + 1);
						if (i < lsecond)
							a.push( macro $i{stok} = @:privateAccess s.offset($v{dx}) );
						else
							a.push( macro var $stok: $ct_stream_tok = @:privateAccess s.offset($v{dx}) );
					}
					// checking...
					if (s.t == false && s.name == entry.name)
						Context.fatalError("the entry LHS(\"" + s.name +"\") is not allowed on the right side", s.pos);
					if (s.ex == null)
						continue;
					if (row.exists(s.ex))
						Context.fatalError("duplicate var: " + s.ex, s.pos);
					row.set(s.ex, true);

					// transform expr
					var name = s.ex;  // variable name
					if (s.t) {
						var ofstr = funMap.get(s.name); //
						if (ofstr == null) {
							a.push(macro var $name: $ct_terms = cast @:privateAccess s.offset($v{dx}).term);
						} else {
							var ct = ofstr.ct;
							switch(ofstr.args) {
							case 1:  // (string)
								a.push(macro var $name: $ct = $i{ofstr.name}( @:privateAccess s.stri($v{dx}) ));
							case 2:  // (input, tok)
								a.push(macro var $name: $ct = @:privateAccess ($i{ofstr.name}(s.lex.input, s.offset($v{dx}))));
							default: // (input, pmin, pmax)
								a.push(macro var $name: $ct = @:privateAccess ($i{ofstr.name}(s.lex.input, s.offset($v{dx}).pmin, s.offset($v{dx}).pmax)));
							}
						}
					} else if (name != "_") {
						a.push( macro var $name: $ct_lhs = cast @:privateAccess s.offset($v{dx}).val );
					}
				}
				var reduce = len > 0 ? (macro null) : (macro @:privateAccess s.reduceEP($v{lhs.value}));
				if (len == 0) // if epsilon then return directly
					li.expr = macro @:pos(li.expr.pos) return $e{li.expr};
				li.expr = if (li.guard == null) {
					macro @:pos(li.expr.pos) @:mergeBlock {
						$reduce;
						@:mergeBlock $b{a};
						@:mergeBlock $e{li.expr}
					}
				} else {
					macro @:pos(li.expr.pos) @:mergeBlock {
						$reduce;
						@:mergeBlock $b{a};
						if ($e{li.guard}) {
							@:mergeBlock $e{li.expr}
						} else {
							var _1 = @:privateAccess s.offset( -1).state;
							@:privateAccess s.rollback( rollL(_1), $v{maxValue} );
							return gotos(rollB(_1), s);
						}
					}
				} // end if else
				++ ti;
			}
		}
	}

	// called by LexEngine, NOTICE: the lex.table is invalid atm.
	@:allow(lm.LexEngine)
	function doPrecedence(lex: LexEngine) {
		if (assoc.length == 0) return;
		inline function segStart(i) return i * lex.per;
		var INVALID = lex.invalid;
		var lvlMap = new Map<Int, Array<OpAssocExt>>(); // lvl => []
		var fidMap = new Map<Int, Array<OpAssocExt>>(); // fid => []
		function parse(table: Table, begin, max) {
			for (i in begin...max) {
				var start = segStart(i);
				for (op in assoc) {
					var nxt = table.get(start + op.value);
					if (nxt >= lex.segs) continue;
					var fi = segStart(nxt);
					for (p in (fi + maxValue)...(fi + lex.per)) { // only non-terminal
						var fid = table.get(p);
						if (fid == INVALID || fid < lex.segs) continue;
						// now check which stage can jump to "i"
						var hit = p - fi;
						for (j in begin...max) {
							if (table.get(segStart(j) + hit) == i) {
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
			var x = lex.states[dst]; // x.id == dst
			var y = lex.states[src]; // y.id == src;
			// swap targets
			for (s in lex.states) {
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
			lex.states[dst] = y;
			lex.states[src] = x;
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
		var tmp = new Table( segStart(lex.segs) );
		for (i in 0...tmp.length)
			tmp.set(i, INVALID);
		for (s in lex.states) {
			if (s.id >= lex.segs) continue;
			@:privateAccess LexEngine.makeTrans(tmp, segStart(s.id), lex.trans[s.part], s.targets);
		}
		// analysis
		for (e in lex.entrys)
			parse(tmp, e.begin, e.begin + e.segs);

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
		var larMap = new Map<Int, {left:Int, right: Int}>();
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
		var dst = lex.segs;
		var hight = 0;
		var count = 0;
		var dupMap = new Map<Int, Bool>();
		for (a in lvlMap) {
			var lar = larMap.get(a[0].lvl);
			for (op in a) {
				if (dupMap.exists(op.fid)) continue;
				count ++;
				if (op.left && lar.left > lar.right && op.prio == lar.left) {
					++ hight;
				} else {
					swap(dst++, op.fid);
				}
				dupMap.set(op.fid, true);
			}
		}
		@:privateAccess lex.segsEx = lex.segs + count - hight;
		// write to s.targets and this.trans
		for (fid in lex.segs...lex.segsEx) {
			var s = lex.states[fid];
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
			s.part = lex.trans.length;
			s.targets = tar;
			lex.trans.push(cset);
		}
	}

	function lexData(lex: LexEngine) {
		this.s2Lhs = new haxe.ds.Vector<Lhs>(lex.nstates);
		for (i in 0...lex.entrys.length) {
			var e = lex.entrys[i];
			var l = this.lhsA[i];
			for (i in e.begin...e.begin + e.segs)
				this.s2Lhs[i] = l;
		}
		// final states
		for (i in lex.segs...lex.nstates) {
			var n = lex.table.exits(i);
			this.s2Lhs[i] = byRule(n);
		}
	}

	function checking(lex: LexEngine) {
		var table = lex.table;
		var INVALID = lex.invalid;
		// init exitN/NRule => final_state rules
		var rules = new haxe.ds.Vector<Int>(lex.nrules);
		for (n in 0...lex.nrules)
			rules[n] = INVALID;
		for (i in table.length-lex.perRB...table.length) {
			var n = table.get(i);
			if (n == INVALID) continue;
			// since: i = table.length - 1 - state; // table.exitpos
			// so   : state = table.length - 1 - i;
			rules[n] = table.length - 1 - i;
		}
		// 1. Is switch case unreachable?
		for (n in 0...lex.nrules)
			if (rules[n] == INVALID)
				Context.fatalError("Unreachable switch case", this.ruleToCase(n).pos);

		// 2. A non-terminator(lhs) must be able to derive at least one terminator directly or indirectly or be epsilon.
		for (index in 0...lex.entrys.length) {
			var entry = lex.entrys[index];
			var base = entry.begin * lex.per;
			var find = false;
			if (table.exits(entry.begin) != INVALID) // epsilon
				continue;
			for (i in base ... base + this.maxValue) {
				var c = table.get(i);
				if (c != INVALID) {
					find = true;
					break;
				}
			}
			if (!find) Context.fatalError("There must be at least one terminator.", lhsA[index].pos);
		}

		// 3. switch guard
		var n = 0;
		for (lhs in this.lhsA) {
			for (li in lhs.cases) {
				if (li.guard != null) {
					var final_state = rules[n];
					if (lex.table.get(lex.posRB() + final_state) == INVALID)
						Context.fatalError("No switch case that can be rollback from here", li.guard.pos);
				}
				++ n;
			}
		}
		// more?
	}

	function toPartern(): Array<PatternSet> {
		inline function add(r, cur) return @:privateAccess LexEngine.next(r, Match(cur));
		var ret = [];
		for (lhs in this.lhsA) {
			var a = [];
			for (c in lhs.cases) {
				var p = Empty;
				for (s in c.syms)
					p = add(p, s.cset);
				a.push(p);
			}
			ret.push(a);
		}
		return ret;
	}

	function mixing(lex: LexEngine, src: Int, dst: Int, lvalue: Int, lpos) {
		var state = lex.states[src];
		var targets = state.targets;
		var dstStart = dst * lex.per;
		for (c in lex.trans[state.part]) {
			var i = c.min;
			var max = c.max;
			var s = targets[c.ext];
			while ( i <= max ) {
				var dst_nxt = lex.table.get(dstStart + i);
				if (i == lvalue) {
					var src_nxt = lex.table.get(src * lex.per + lvalue);
					if (dst_nxt < lex.segs && src_nxt < lex.segs) {
						mixing(lex, src_nxt, dst_nxt, -1, lpos);
					}
				} else {
					if (dst_nxt != lex.invalid) {
						if (s != dst_nxt)
							Context.fatalError("rewrite conflict: " + Lambda.find(udtMap, u -> u.value == i).name, lpos);
					} else {
						lex.table.set(dstStart + i, s);
					}
				}
				++ i;
			}
		}
	}

	function modify(lex: LexEngine) {
		var b = lex.table;
		var INVALID = lex.invalid;
		var per = lex.per;
		// sort
		var h = lhsA.copy();
		h.sort(function(L1, L2) {
			var I1 = L1.value - this.maxValue;
			var I2 = L2.value - this.maxValue;
			var S1 = lex.entrys[I1].begin;
			var S2 = lex.entrys[I2].begin;
			var E1 = b.get(S2 * per + L1.value);
			var E2 = b.get(S1 * per + L2.value);
			if (E1 != INVALID) {
				if (E2 != INVALID)
					Context.fatalError("conflict: " + I1 + " <-> " + I2, L1.pos);
				return 1;
			}
			if (E2 != INVALID)
				return -1;
			return L1.value - L2.value;
		});
		for (seg in 0...lex.segs) {
			var base = seg * per;
			for (l in h) {
				if (b.get(base + l.value) == INVALID)
					continue;
				var entry = lex.entrys[l.value - this.maxValue]; // the entry Associated with lhs
				if (entry.begin == seg) continue; // skip self.
				mixing(lex, entry.begin, seg, l.value, l.pos);
				if (l.epsilon) {
					var dst = b.exitpos(seg);
					if (b.get(dst) != INVALID)
						Context.fatalError("epsilon conflict with " + l.name, l.pos);
					b.set(dst, b.exits(entry.begin));
				}
			}
		}
		rollback(lex);
	}

	function rollback(lex: LexEngine) {
		var table = lex.table;
		inline function epsilon(seg) return table.exits(seg);
		var alt = new haxe.ds.Vector<Bool>(lex.perRB);
		for (i in 0...alt.length) alt[i] = false;
		var INVALID = lex.invalid;
		var rollpos = lex.posRB();
		var rlenpos = lex.posRBL();
		var que = new List<{exit: Int, nxt: Int, len: Int}>(); // FIFO
		function loop(exit, seg, length) {
			alt[seg] = true;
			var base = seg * lex.per;
			for (lv in 0...lex.per) {
				var nxt = table.get(lv + base);
				if (nxt == INVALID || alt[nxt] || lv >= maxValue) continue;
				table.set(rollpos + nxt, exit);
				table.set(rlenpos + nxt, length);
				if (epsilon(nxt) == INVALID)
					que.add({exit: exit, nxt: nxt, len: length + 1});
			}
		}
		for (seg in 0...lex.segs) {
			var exit = epsilon(seg);
			if (exit == INVALID) continue;
			loop(exit, seg, 1);
		}
		while (true) {
			var x = que.pop();
			if (x == null) break;
			loop(x.exit, x.nxt, x.len);
		}
	}

	static public function build() {
		var cls = Context.getLocalClass().get();
		var ct_lr0 = TPath({pack: cls.pack, name: cls.name});
		var t_tok:Type = null;
		var t_lhs: Type = null;
		var eof: Expr = null;
		for (it in cls.interfaces) {
			if (it.t.toString() == "lm.LR0") {
				switch(it.params[0]) {
				case TInst(_.get() => lex, _):
					for (it in lex.interfaces) {
						if (it.t.toString() == "lm.Lexer") {
							t_tok = it.params[0];
							eof = @:privateAccess LexBuilder.getMeta(lex.meta.extract(":rule")).eof;
							if (eof == null || eof.toString() == "null") // "null" is not allowed as an EOF in parser
								Context.fatalError("Invalid EOF value " + eof.toString(), lex.pos);
							break;
						}
					}
				default:
				}
				t_lhs = it.params[1];
				break;
			}
		}
		if (t_tok == null || !Context.unify(t_tok, Context.getType("Int")))
			Context.fatalError("Wrong generic Type for lm.LR0<?>", cls.pos);
		// begin
		var lrb = new LR0Builder(t_tok, t_lhs, eof.toString());
		lrb.parsePrecedence(cls);
		var allFields = new haxe.ds.StringMap<Field>();
		var switches = filter(Context.getBuildFields(), allFields, lrb);
		if (switches.length == 0)
			return null;
		lrb.transform(switches);
		var pats = lrb.toPartern();
		var lex = new LexEngine(pats, lrb.maxValue + lrb.lhsA.length | 15, lrb);
		lrb.lexData(lex);

		// modify lex.table as LR0
		lrb.modify(lex);

	#if lex_lr0table
		var f = sys.io.File.write("lr0-table.txt");
		f.writeString("\nProduction:\n");
		f.writeString(debug.Print.lr0Production(lrb));
		f.writeString(debug.Print.lr0Table(lrb, lex));
		f.writeString("\n\nRAW:\n");
		lex.write(f, true);
		f.close();
	#end

		lrb.checking(lex); // checking.

		var ret = [];
		var defs = lrb.generate(lex);
		for (f in defs)
			if (!allFields.exists(f.name))
				ret.push(f);
		for (f in allFields) ret.push(f);
		return ret;
	}

	function generate(lex: lm.LexEngine) {
		var force_bytes = !Context.defined("js") || Context.defined("lex_rawtable");
		if (Context.defined("lex_strtable")) force_bytes = false; // force string as table format
		if (false == force_bytes && !Context.defined("utf16")) force_bytes = true;
		var getU:Expr = null;
		var raw:Expr = null;
		if (force_bytes) {
			var bytes = lex.bytesTable();
			var resname = "_" + StringTools.hex(haxe.crypto.Crc32.make(bytes)).toLowerCase();
			Context.addResource(resname, bytes);
			#if hl
			getU = lex.invalid == LexEngine.U16MAX ? macro raw.getUI16(i << 1) : macro raw.get(i);
			raw = macro haxe.Resource.getBytes($v{resname}).getData().bytes;
			#else
			getU = lex.invalid == LexEngine.U16MAX ? macro raw.getUInt16(i << 1) : macro raw.get(i);
			raw = macro haxe.Resource.getBytes($v{resname});
			#end
		} else {
			var out = haxe.macro.Compiler.getOutput() + ".lr0-table";
			var dir = haxe.io.Path.directory(out);
			if (!sys.FileSystem.exists(dir))
				sys.FileSystem.createDirectory(dir);
			var f = sys.io.File.write(out);
			lex.write(f);
			f.close();
			getU = macro StringTools.fastCodeAt(raw, i);
			raw = macro ($e{haxe.macro.Compiler.includeFile(out, Inline)});
		}
		var lva = n2Lhs.map(n -> macro $v{n}).toArray();
		var defs = macro class {
			static var raw = $raw;
			static var lva:Array<Int> = [$a{lva}];
			static inline var INVALID = $v{lex.invalid};
			static inline var NRULES  = $v{lex.nrules};
			static inline var NSEGS   = $v{lex.segs};
			static inline var NSEGSEX = $v{lex.segsEx};
			static inline function getU(i:Int):Int return $getU;
			static inline function trans(s:Int, c:Int):Int return getU($v{lex.per} * s + c);
			static inline function exits(s:Int):Int return getU($v{lex.table.length - 1} - s);
			static inline function rollB(s:Int):Int return getU(s + $v{lex.posRB()});
			static inline function rollL(s:Int):Int return getU(s + $v{lex.posRBL()});
			static inline function gotos(fid:Int, s:$ct_stream) return cases(fid, s);
			var stream: $ct_stream;
			public function new(lex: lm.Lexer<Int>) {
				this.stream = new lm.Stream<$ct_lhs>(lex, $v{lex.entrys[0].begin});
			}
			@:access(lm.Stream, lm.Tok)
			static function _entry(stream: $ct_stream, state:Int, exp:Int):$ct_lhs {
				var prev = state;
				var t: lm.Stream.Tok<$ct_lhs> = null;
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
						q = rollB(state);
						if (q < NRULES) {
							var dy = dx + rollL(state);
							t = stream.offset(-1 - dy);
							if ( trans(t.state, lva[q] >> 8) == INVALID ) {
								stream.pos -= dx;
								exp = 0;
								break;
							}
							stream.rollback(dy, $v{maxValue});
						} else {
							break;  // throw error.
						}
					}
					dx = 0;         // reset dx after rollback
					while (true) {
						var value:$ct_lhs = gotos(q, stream);
						t = stream.offset( -1); // last token
						if (t.term == exp) {
							-- stream.pos;      // discard the last token
							stream.junk(1);
							return value;
						}
						t.val = value;
						t.state = trans(stream.offset( -2).state, t.term);
						prev = t.state;
						if (prev < NSEGSEX) break;
						if (prev == INVALID) {
							if (exp == -1)
								return stream.cached[keep].val;
							throw lm.Utils.error('Unexpected "' + stream.str(t) + '"' + stream.errpos(t.pmin));
						}
						q = exits(prev);
					}
				}
				if (exp == -1 && (stream.pos - dx) > keep)
					return stream.cached[keep].val;
				t = stream.offset( -1);
				throw lm.Utils.error('Unexpected "' + (t.term != $i{sEof} ? stream.str(t): $v{sEof}) + '"' + stream.errpos(t.pmin));
			}
			@:access(lm.Stream, lm.Tok)
			static function _side(stream: $ct_stream, state:Int, lv: Int):$ct_lhs {
				var keep = stream.pos;
				var prev = stream.offset( -1);
				var t = new lm.Stream.Tok<$ct_lhs>(lv, prev.pmax, prev.pmax);
				t.state = state;
				stream.shift(t);
				var value = _entry(stream, state, -1); // -1 then until to match failed
				stream.pos = keep;
				stream.junk(2);
				return value;
			}
		}
		// build switch
		var exprs = Lambda.flatten( lhsA.map(l -> l.cases) ).map( s -> s.expr );
		var here = Context.currentPos();
		var defCase = exprs.pop();
		var liCase = Lambda.mapi( exprs, (i, e)->({values: [macro $v{i}], expr: e}: Case) );
		var eSwitch = {expr: ESwitch(macro (_q), liCase, defCase), pos: here};
		defs.fields.push({
			name: "cases",
			access: [AStatic],
			kind: FFun({
				args: [{name: "_q", type: macro: Int}, {name: "s", type: ct_stream}],
				ret: ct_lhs,
				expr: macro {
					@:mergeBlock $b{preDefs};
					var _v = $eSwitch;
					@:privateAccess s.reduce(lva[_q]);
					return _v;
				}
			}),
			pos: here,
		});

		// main entry => member inline
		var entry = lex.entrys[0];
		var lhs = lhsA[0];
		defs.fields.push({
			name: lhs.name,
			access: [APublic, AInline],
			kind: FFun({
				args: [],
				ret: ct_lhs,
				expr: macro return _entry(stream, $v{entry.begin}, $v{lhs.value})
			}),
			pos: lhs.pos,
		});
		// other entrys => static inline
		for (i in 1...lex.entrys.length) {
			var entry = lex.entrys[i];
			var lhs = lhsA[i];
			defs.fields.push({
				name: lhs.name,
				access: [AStatic, AInline],
				kind: FFun({
					args: [{name: "s", type: ct_stream}],
					ret: ct_lhs,
					expr: macro return _side(s, $v{entry.begin}, $v{lhs.value})
				}),
				pos: lhs.pos,
			});
		}
		return defs.fields;
	}

	////////

	static function filter(fields: Array<Field>, allFields, lrb: LR0Builder) {
		var lvalue = lrb.maxValue;
		var ret = [];
		for (f in fields) {
			if (f.access.indexOf(AStatic) > -1) {
				switch (f.kind) {
				case FVar(ct, e) if (e != null):
					switch(e.expr) {
					case ESwitch(macro ($i{"s"}), cl, edef):
						if (cl.length == 0 && edef == null) continue;
						if (edef != null)
							cl.push({values: [macro @:pos(edef.pos) _], expr: edef, guard: null});
						firstCharChecking(f.name, LOWER, f.pos);
						if ( ct != null && !Context.unify(ct.toType(), lrb.ct_lhs.toType()) )
							Context.fatalError("All types of lhs must be uniform.", f.pos);
						ret.push({name: f.name, value: lvalue++, cases: cl, pos: f.pos});
						continue;
					case _:
					}
				case FFun(fun):
					var ofstr = Lambda.find(f.meta, m->m.name == ":rule");
					if (ofstr != null && ofstr.params.length > 0) {
						var p0 = ofstr.params[0];
						switch(p0.expr){
						case EConst(CIdent(s)) | EConst(CString(s)):
							lrb.funMap.set(s, {name: f.name, ct: fun.ret, args: fun.args.length});
						default:
							Context.fatalError("UnSupperted value for @:rule: " + p0.toString(), p0.pos);
						}
						if (fun.args.length == 2 && fun.args[1].type == null) { // improved for display
							fun.args[1].type = lrb.ct_stream_tok;
						}
					} else {
						for (arg in fun.args) {
							if (arg.type != null) continue;
							switch(arg.name) {
							case "t": arg.type = lrb.ct_stream_tok;
							case "s": arg.type = lrb.ct_stream;
							default:
							}
						}
					}
				default:
				}
			}
			if (allFields.exists(f.name))
				Context.fatalError("Duplicate field: " + f.name, f.pos);
			allFields.set(f.name, f);
		}
		return ret;
	}

	static inline var UPPER = true;
	static inline var LOWER = false;
	static function firstCharChecking(s: String, upper: Bool, pos: Position) {
		var c = s.charCodeAt(0);
		if (upper) {
			if ( !(c >= "A".code && c <= "Z".code) )
				Context.fatalError("Should be start with a capital letter: " + s, pos);
		} else {
			if ( !(c >= "a".code && c <= "z".code || c == "_".code) )
				Context.fatalError("Should be start with a lowercase letter: " + s, pos);
		}
	}
}

#else
class LR0Builder{}

@:autoBuild(lm.LR0Builder.build())
#end
@:remove interface LR0<LEX, LHS> {
}
