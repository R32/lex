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
	value: Int,
	cset: Charset, // CSet.single(value)
	pos: Position, // haxe.macro.Position
}

typedef Symbol = { // from switch cases
	t: Bool,
	name: String,  // ident name
	cset: Charset,
	ex: String,    // extract name. if "CStr(s)" then ex = "s" for terml, if "e = expr" then ex = "e" for non-terml
	pos: Position,
}

typedef SymbolSet = {
	action: Expr,
	guard: Null<Expr>,
	syms: Array<Symbol>,
	prec: Null<OpRHS>,
	pos: Position, // case pos
}

typedef Lhs = {    // one switch == one Lhs
	name: String,  // associated var name(non-terminals name)
	value: Int,    // automatic increase from "maxValue"
	side: Bool,    // if @:side
	epsilon: Bool, // if have switch "default:" or "case []:"
	cases: Array<SymbolSet>,
	lsubs: Array<Int>, // left subs, Array<Lhs::value>
	ctype: ComplexType,
	pos: Position,
}

enum OpAssocType {
	Left;
	Right;
	NonAssoc;
}

typedef OpAssoc = {
	type: OpAssocType,
	prio: Int,
	tval: Int,   // terml value, if the name is no exists then -1
}

typedef OpRHS = {
	type: OpAssocType,
	prio: Int,
	lval: Int,   // last "non-terml" of a case
}

/**
 parser datas
*/
class Parser {

	var termls: Array<Udt>;      // all Terminal
	var termlsC_All: Charset;    // Terminal Universal Set.
	var udtMap: Map<String,Udt>; // User Defined Terminal + Non-Terminal
	var maxValue: Int;           // if value >= maxValue then it must be a non-terminal
	var lhsA: Array<Lhs>;
	var sEof: String;            // by @:rule from Lexer
	var funMap: Map<String, {name: String, ct: ComplexType, args: Int}>; //  TokenName => FunctionName
	var ct_terms: ComplexType;   // the ctype of tokens
	var ct_ldef: ComplexType;    // the default ctype of LHS(if not specified)
	var ct_lval: ComplexType;    // if "ct_lhs" can no be unified with any "LHS.ctype" then its value is ":Dynamic"
	var ct_stream: ComplexType;  // :lm.Stream<ct_lval>
	var ct_stream_tok: ComplexType;
	var preDefs: Array<Expr>;    // for function cases()
	var opIMap: haxe.ds.Vector<OpAssoc>; //
	var opSMap: Map<String, OpAssoc>;    // e.g: %prec UMINUS
	var n2Lhs: haxe.ds.Vector<Int>;      // NRule => (lvalue << 8) | syms.length. (for reduction)
	var p2t: Map<String, String>;        // PatternString => TokenString for reflect

	public inline function isEmpty() return this.lhsA.length == 0;
	public inline function isNonTerm(v) return v >= this.maxValue;
	public inline function isTerm(v) return v < this.maxValue;
	public inline function index(l:Lhs):Int return l.value - this.maxValue;
	public inline function width():Int return this.maxValue + this.lhsA.length;

	public function new(s_it: String, rest: Map<String, Field>) {
		var cls = Context.getLocalClass().get();
		var t_terms:Type = null;
		var t_lhs: Type = null;
		for (it in cls.interfaces) {
			if (it.t.toString() == s_it) {
				switch(it.params[0]) {
				case TInst(_.get() => lex, _):
					for (it in lex.interfaces) {
						if (it.t.toString() == "lm.Lexer") {
							t_terms = it.params[0];
							var eof = @:privateAccess LexBuilder.getMeta(lex.meta.extract(":rule")).eof;
							if (eof == null || eof.toString() == "null") // "null" is not allowed as an EOF in parser
								Context.fatalError("Invalid EOF value " + eof.toString(), lex.pos);
							this.sEof = eof.toString();
							p2t = LexBuilder.lmap.get(Utils.getClsFullName(lex));
							break;
						}
					}
				default:
				}
				t_lhs = it.params[1];
				break;
			}
		}
		if (t_terms == null || !Context.unify(t_terms, Context.getType("Int")))
			Context.fatalError("Wrong generic Type for "+ s_it + "<?>", cls.pos);
		maxValue = 0;
		termls = [];
		termlsC_All = [];
		lhsA = [];
		preDefs = [];

		udtMap = new Map();
		funMap = new Map();

		ct_terms = Context.toComplexType(t_terms);
		ct_ldef = Context.toComplexType(t_lhs);
		ct_lval = ct_ldef;

		readTerms(t_terms);
		readPrecedence(cls);
		transform(rest);
	}

	function readPrecedence(cls:ClassType) {
		function extract(a: Array<ObjectField>, vec: haxe.ds.Vector<OpAssoc>, map:Map<String, OpAssoc>) {
			var R_UPPER:EReg = ~/^[A-Z][A-Z0-9_]*$/;
			for (i in 0...a.length) {
				var li = a[i].expr;
				var stype = a[i].field.toLowerCase();
				var type = switch( stype ){
				case "left": Left;
				case "right": Right;
				case "nonassoc": NonAssoc;
				case _: Context.fatalError("UnSupported type of op assoc: " + stype, li.pos);
				}
				switch (li.expr) {
				case EArrayDecl(a):
					for (e in a) {
						var name = switch (e.expr) {
						case EConst(CIdent(i)): i;
						case EConst(CString(p)):
							var i = this.p2t.get(p); // reflect
							if (i == null)
								Context.fatalError('No reflect for "' + p + '"', e.pos);
							i;
						case _: "";
						}
						if ( map.exists(name) )
							Context.fatalError("Duplicate Token: " + name, e.pos);
						var term = udtMap.get(name);
						if (term != null) {
							if (term.t == false)
								Context.fatalError("UnSupported Token: " + name, e.pos);
						} else if ( !R_UPPER.match(name) ) {
							Context.fatalError("Only accept '/[A-Z][A-Z0-9_]*/' as placeholder: " + name, e.pos);
						}
						var op = {type: type, prio: i, tval: term != null ? term.value : -1};
						map.set(name, op);
						if (op.tval != -1)
							vec.set(op.tval, op);
					}
				default:
					Context.fatalError("UnSupported Type", li.pos);
				}
			}
		}
		this.opIMap = new haxe.ds.Vector(this.maxValue);
		this.opSMap = new Map();
		var rule = cls.meta.extract(":rule");
		if (rule.length > 0) {
			var obj = rule[0].params[0];
			switch (obj.expr) {
			case EObjectDecl(a):
				extract(a, opIMap, opSMap);
			default:
			}
		}
	}

	function readTerms(tk: Type) {
		switch (tk) {
		case TAbstract(_.get() => ab, _):
			for (field in ab.impl.get().statics.get()) {
				for (meta in field.meta.get()) {
					if (meta.name != ":value") continue;
					switch(meta.params[0].expr) {
					case ECast({expr: EConst(CInt(i))}, _):
						var n = Std.parseInt(i);
						if (n >= maxValue)
							maxValue = n + 1;
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
			if ( !CSet.isSingle(cset) ) // to prevent abuse
				return cset;
		}
		return null;
	}

	function getNonTermlCSet(name: String) {
		var t = udtMap.get(name);
		return t == null || t.t ? null : t.cset;
	}

	inline function byRule(n):Lhs return lhsA[(this.n2Lhs[n] >> 8) - maxValue]; // index = lhs.value - maxValue

	function ruleToCase(n: Int): SymbolSet {
		for (lhs in lhsA) {
			var len = lhs.cases.length;
			if (n < len) return lhs.cases[n];
			n -= len;
		}
		throw "NotFound";
	}

	function transform(out) {
		var cases:Array<Array<Case>> = preProcess(out);
		if (cases.length == 0) return;
		for (lhs in lhsA) { // init udtMap first..
			if (this.udtMap.exists(lhs.name))
				Context.fatalError("Duplicate LHS: " + lhs.name, lhs.pos);
			this.udtMap.set(lhs.name, {t: false, name: lhs.name, value: lhs.value, cset: CSet.single(lhs.value), pos: lhs.pos});
		}
		function setEpsilon(lhs, pos) {
			if (lhs.epsilon) Context.fatalError('Duplicate "default:" or "case _:"', pos);
			lhs.epsilon = true;
		}
		function getCSet(name, pos) {
			var cset = getTermlCSet(name);
			if (cset == null) Context.fatalError("Undefined: " + name, pos);
			return cset;
		}
		function opVerify(sym: Symbol):Null<OpRHS> {
			var prio = -1;
			var type = Left;
			var got = false;
			for (c in sym.cset) {
				for (i in c.min...c.max + 1) {
					var op = this.opIMap.get(i);
					if (op == null) {
						if (prio != -1)
							Context.fatalError("Mixed non-operator", sym.pos);
					} else {
						if (!got) {
							prio = op.prio;
							type = op.type;
						} else {
							if (prio == -1)
								Context.fatalError("Mixed non-operator", sym.pos);
							else if (prio != op.prio)
								Context.fatalError("Different priority", sym.pos);
						}
					}
					got = true;
				}
			}
			return prio == -1 ? null : {type: type, prio: prio, lval: -1};
		}
		for (lhs in this.lhsA) {
			for (c in cases[ index(lhs) ]) {
				switch(c.values) {
				case [{expr:EArrayDecl(el), pos: pos}]:
					if (lhs.epsilon)
						Context.fatalError('This case is unused', pos);
					var g: SymbolSet = {action: c.expr, guard: c.guard, syms: [], prec: null, pos: pos};
					var len = el.length;
					if (len == 0) {
						setEpsilon(lhs, pos);
						lhs.cases.push(g);
						continue;
					}
					var prec:Null<OpRHS> = null;
					var ei = 0;
					var e = el[0];
					while (true) {
						switch (e.expr) {
						case EConst(CIdent(i)):
							firstCharChecking(i, UPPER, e.pos);            // e.g: CInt
							g.syms.push( {t: true, name: i,    cset: getCSet(i, e.pos), ex: null, pos: e.pos} );

						case EConst(CString(s)):
							var i = this.p2t.get(s);
							if (i == null)
								Context.fatalError("No associated token: " + s, e.pos);
							firstCharChecking(i, UPPER, e.pos);
							g.syms.push( {t: true, name: i,    cset: getCSet(i, e.pos), ex: null, pos: e.pos} );

						// TODO: maybe it's not good..
						case EParenthesis(macro $i{i}):
							firstCharChecking(i, LOWER, e.pos);            // all termls but no Eof
							g.syms.push( {t: true, name: "_",  cset: termlsC_All,       ex: i,    pos: e.pos} );

						case ECall(macro $i{i}, [macro $i{v}]):            // e.g: CInt(n) or Op(op)
							firstCharChecking(i, UPPER, e.pos);
							firstCharChecking(v, LOWER, e.pos);
							g.syms.push( {t: true, name: i,    cset: getCSet(i, e.pos), ex: v,    pos: e.pos} );

						case EBinop(OpAssign, macro $i{v}, macro $i{nt}):  // e.g: e = expr
							var udt = udtMap.get(nt);
							if (udt == null || udt.t == true)
								Context.fatalError("Undefined non-terminal: " + nt, e.pos);
							if (el.length == 1 && nt == lhs.name)
								Context.fatalError("Infinite recursion", e.pos);
							if (nt != lhs.name && el[0] == e)
								lhs.lsubs.push(udt.value);
							g.syms.push( {t: false, name: nt, cset: udt.cset , ex: v , pos: e.pos} );

						case EBinop(OpAssign, macro $i{v}, macro $a{a}):   // e.g: t = [OpPlus, OpMinus]
							var cset = CSet.C_EMPTY;
							for (t in a) {
								var i = switch (t.expr) {
								case EConst(CIdent(i)): i;
								case EConst(CString(s)): this.p2t.get(s);
								default: null;
								}
								if (i == null)
									Context.fatalError("Unsupported: " + t.toString(), t.pos);
								firstCharChecking(i, UPPER, t.pos);
								cset = CSet.union(cset, getCSet(i, t.pos));
							}
							if (cset == CSet.C_EMPTY)
								Context.fatalError("Empty", pos);
							g.syms.push( {t: true, name: "_", cset: cset, ex: v, pos: e.pos} );
						case EMeta({name: ":prec", params: [{expr: EConst(c), pos: p}]}, e2): // e.g: @:prec(UMINUS)
							var i = switch(c) {
							case CIdent(i): i;
							case CString(s):
								var i = this.p2t.get(s);
								if (i == null)
									Context.fatalError("No associated token: " + s, p);
								i;
							case _: Context.fatalError("Unsupported: " + c, p);
							}
							var op = this.opSMap.get(i);
							if (op == null)
								Context.fatalError("Undefined :" + i, p);
							prec = {type: op.type, prio: op.prio, lval: -1};
							e = e2;
							continue;
						case _:
							Context.fatalError("Unsupported: " + e.toString(), e.pos);
						}
						if (++ei >= len) break;
						e = el[ei];
					}
					// op prec
					if (len >= 2) {
						var x1:Symbol = g.syms[len - 1];
						var x2:Symbol = g.syms[len - 2];
						if (x2.t && x1.t == false) { // case [..., Terml, NON-Terml]:
							if (prec == null)
								prec = opVerify(x2);
							if (prec != null) {
								prec.lval = x1.cset[0].min;
								g.prec = prec;
							}
						}
					}
					lhs.cases.push(g);
				case [{expr:EConst(CIdent("_")), pos: pos}]: // case _: || defualt:
					setEpsilon(lhs, pos);
					lhs.cases.push({action: c.expr, guard: null, syms: [], prec: null, pos: pos});
				case [e]:
					Context.fatalError("Expected [ patterns ]", e.pos);
				case _:
					Context.fatalError("Comma notation is not allowed while matching streams", c.values[0].pos);
				}
			}
		}
		// closure(lsubs)
		var added = true;
		while (added) {
			added = false;
			for (top in lhsA) {
				var i = 0;
				var len = top.lsubs.length;
				while (i < len) {
					var sub: Lhs = this.lhsA[top.lsubs[i++] - this.maxValue];
					for (slv in sub.lsubs) {
						if (top.lsubs.indexOf(slv) == -1) {
							top.lsubs.push(slv);
							added = true;
						}
					}
				}
			}
		}
		organize();
	}

	function organize() {
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
				if (li.action == null)
					Context.fatalError("Need return *" + lhs.ctype.toString() + "*", li.pos);
				li.action.iter(loop);
				toks[ti++] = tmp;
			}
		}
		this.n2Lhs = new haxe.ds.Vector<Int>(lcases);
		// duplicate var checking. & transform expr
		ti = 0;
		for (lhs in lhsA) {
			for (li in lhs.cases) {
				var row = ["s" => true, "_q" => true]; // reserve "s" as stream, "_q" for reduction
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
					if (s.ex == null || s.ex == "_")
						continue;
					if (row.exists(s.ex))
						Context.fatalError("duplicate var: " + s.ex, s.pos);
					row.set(s.ex, true);

					// transform expr
					var name = s.ex;  // variable name
					if (s.t) {
						var ofstr = funMap.get(s.name);
						if (ofstr == null) {
							if ( s.name != "_" && CSet.isSingle(s.cset) ) // If you forget to add an extract function
								Context.fatalError("Required a static function with @:rule("+ s.name +")", s.pos);
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
					} else {
						var lvalue = s.cset[0].min; // NON-TERML
						var ct = lhsA[lvalue - maxValue].ctype;
						a.push( macro @:pos(li.action.pos) var $name = @:privateAccess (s.offset($v{dx}).val: $ct) );
					}
				}
				switch(li.action.expr) {
				case EBlock(a) if (a.length > 0):
					var ct = lhs.ctype;
					var j = a.length - 1;
					a[j] = macro @:pos(a[j].pos) ($e{a[j]}: $ct);  // Type safe and accurate error postion
					li.action = {expr: EBlock(a), pos: li.action.pos};
				case _:
				}
				var reduceEp = len > 0 ? (macro null) : (macro @:privateAccess s.reduceEP($v{lhs.value}));
				if (len == 0) // if epsilon then return directly
					li.action = macro @:pos(li.action.pos) return $e{li.action};
				li.action = if (li.guard == null) {
					macro @:pos(li.action.pos) @:mergeBlock {
						$reduceEp;
						@:mergeBlock $b{a};
						@:mergeBlock $e{li.action};
					}
				} else {
					macro @:pos(li.action.pos) @:mergeBlock {
						@:mergeBlock $b{a};
						if ($e{li.guard}) {
							$reduceEp;
							@:mergeBlock $e{li.action};
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

	function preProcess(out: Map<String, Field>): Array<Array<Case>> {
		var lvalue = this.maxValue;
		var t_lhs = this.ct_ldef.toType();
		var fields: Array<Field> = Context.getBuildFields();
		var flazy = new List<{f: Field, fun: Function}>();
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
						if ( ct != null && this.ct_ldef == this.ct_lval && !Context.unify(ct.toType(), t_lhs) )
							this.ct_lval = macro :Dynamic; // use Dynamic if .unify() == false
						ret.push(cl);
						this.lhsA.push({
							name: f.name,
							value: lvalue++,
							side: Lambda.exists(f.meta, e-> e.name == ":side"),
							epsilon: false,
							cases: [],
							lsubs: [],
							ctype: ct == null ? this.ct_ldef : ct,
							pos: f.pos,
						});
						continue;
					case _:
					}
				case FFun(fun):
					flazy.add({f: f, fun: fun});
				default:
				}
			}
			if (out.exists(f.name))
				Context.fatalError("Duplicate field: " + f.name, f.pos);
			out.set(f.name, f);
		}

		if (lvalue > 255) throw "Too Many Termls/NoN-Termls"; // TODO: or force the invalid value up to 16bit??

		// Waiting for "ct_lval" available
		this.ct_stream = macro :lm.Stream<$ct_lval>;
		this.ct_stream_tok = macro :lm.Stream.Tok<$ct_lval>;

		for (x in flazy) {
			var f = x.f;
			var fun = x.fun;
			var ofstr = Lambda.find(f.meta, m->m.name == ":rule");
			if (ofstr != null && ofstr.params.length > 0) {
				var len = fun.args.length;
				for (t in ofstr.params) {
					switch(t.expr){
					case EConst(CIdent(s)) | EConst(CString(s)):
						this.funMap.set(s, {name: f.name, ct: fun.ret, args: len});
					default:
						Context.fatalError("UnSupperted value for @:rule: " + t.toString(), t.pos);
					}
				}
				switch(len) {
				case 1:
					if (fun.args[0].type == null)
						fun.args[0].type = macro :String;
				case 2:
					if (fun.args[0].type == null)
						fun.args[0].type = macro :lms.ByteData;
					if (fun.args[1].type == null)
						fun.args[1].type = ct_stream_tok;
				default:
				}
			}
			// auto convert the type of args(If not specified)
			for (arg in fun.args) {
				if (arg.type != null) continue;
				switch(arg.name) {
				case "t": arg.type = ct_stream_tok;
				case "s": arg.type = ct_stream;
				default:
				}
			}
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
extern class Parser {}
#end
