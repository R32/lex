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
	t: Bool,       // terml or non-terml
	name: String,
	value: Int,
	cset: Charset, // CSet.single(value)
	pos: Position, // haxe.macro.Position
}

typedef Symbol = { // from switch cases
	t: Bool,
	name: String,  // ident name
	cset: Charset,
	ex: String,    // extract name. if "CStr(str)" then ex = "str" for terml, if "e = expr" then ex = "e" for non-terml
	pos: Position,
}

typedef SymbolSet = {
	action: Expr,
	syms: Array<Symbol>,
	left: Null<OpLeft>,
	right: Null<OpRight>,
	pos: Position, // case pos
}

typedef Lhs = {    // left hand side of the production which is generated by a switch expr
	name: String,  // associated non-terml name(The first letter must be lowercase)
	value: Int,    // automatic increase from "maxValue"
	epsilon: Bool, // if the switch expr have "default:" or "case []:"
	cases: Array<SymbolSet>,
	lsubs: List<Int>, // List<lhsValue>
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
	tval: Int,      // terml value, if the name is no exists then -1
}

typedef OpLeft = {  // for stream match: [ ..., termls, E], if ( query(termls) ) then .lval = E.value, ...
	type: OpAssocType,
	prio: Int,
	lval: Int,      // last "non-terml" of a case
}

typedef OpRight = { // for stream match: [E, termls, ... ], if ( query(termls) ) then  .own = E.value, ...
	type: OpAssocType,
	prio: Int,      // could be (-2: singular, (-1: undefined, (0~n: competing with OpLeft
	own: Int,       // lhs.value
}
/**
 parserInfo
*/
class LR0Base {

	var termls: Array<Udt>;      // all Terminators
	var termlsAll: Charset;      // Terminators Universal Set but without EOF
	var udtMap: Map<String,Udt>; // User Defined Terminators and Non-Terminators
	var maxValue: Int;           // if value >= maxValue then it must be a non-terminator
	var lhsA: Array<Lhs>;
	var sEof: String;            // by @:rule from Lexer
	var funMap: Map<String, {name: String, ct: ComplexType, args: Int}>; //  TokenName => FunctionName
	var ct_termls: ComplexType;  // the ctype of tokens
	var ct_ldef: ComplexType;    // the default ctype of LHS.
	var ct_lval: ComplexType;    // if "ct_lhs" can no be unified with any "LHS.ctype" then its value is ":Dynamic"
	var ct_stream: ComplexType;  // :lm.Stream<ct_lval>
	var ct_stream_tok: ComplexType;
	var opIMap: haxe.ds.Vector<OpAssoc>;   // term value => OpAssoc
	var opSMap: Map<String, OpAssoc>;      // (term name | placeholder) => OpAssoc
	var lvalues: haxe.ds.Vector<Int>;      // [(lhsValue << 8) | syms.length]. (used for stream.reduce)
	var reflect: Map<String, String>;      // PatternString => TokenString
	var nrules: Int;                       // length(all switch cases)
	var vcases: haxe.ds.Vector<SymbolSet>; // flatten(all lhs.cases)
	var starts: Array<{index:Int, begin:Int, width:Int}>; //
	var unMatched : Null<Expr>;

	public inline function isEmpty() return this.lhsA.length == 0;
	public inline function isNonTerm(v) return v >= this.maxValue;
	public inline function isTerm(v) return v < this.maxValue;
	public inline function index(l:Lhs):Int return l.value - this.maxValue;
	public inline function width():Int return this.maxValue + this.lhsA.length;

	static function fatalError(msg, p) return Context.fatalError("[LR0 build] " + msg , p);

	public function new(s_interface: String, allFields: Map<String, Field>) {
		var cl = Context.getLocalClass().get();
		var termlsType:Type = null;
		var lhsType:Type = null;
		for (it in cl.interfaces)
			if (it.t.toString() == s_interface) {
				termlsType = readLexerMeta( it.params[0] );
				lhsType = it.params[1];
				break;
			}
		if (termlsType == null || !Context.unify(termlsType, Context.getType("Int")))
			fatalError("Wrong generic Type for "+ s_interface + "<?>", cl.pos);
		maxValue = 0;
		termls = [];
		termlsAll = [];
		lhsA = [];
		starts = [];
		udtMap = new Map();
		funMap = new Map();

		ct_termls = Context.toComplexType(termlsType);
		ct_ldef = Context.toComplexType(lhsType);
		ct_lval = ct_ldef;
		readLexerTermls(termlsType);    // termals
		var cases = readLhs(allFields); // non-termls
		readPrecedence(cl);
		transform( cases );
	}

	function readLexerMeta(t: Type) {
		switch(t) {
		case TInst(_.get() => lex, _):
			this.sEof = "null";
			for (it in lex.interfaces) {
				if (it.t.toString() == "lm.Lexer") {
					var eof = @:privateAccess LexBuilder.getMeta(lex.meta.extract(":rule")).eof;
					if (eof != null)
						this.sEof = eof.toString();
					if (this.sEof == "null" || this.sEof == "Void")
						fatalError("Invalid EOF value " + this.sEof, lex.pos);
					this.reflect = LexBuilder.lmap.get( ExprHelps.classFullName(lex) );
					return it.params[0];
				}
			}
		default:
		}
		return null;
	}

	function readPrecedence(cl:ClassType) {
		function readStarts(a) {
			switch(a.expr) {
			case EArrayDecl(a):
				for (e in a) {
					switch (e.expr) {
					case EConst(CIdent(i)):
						var lhs = Lambda.find(this.lhsA, lhs -> lhs.name == i);
						if (lhs == null)
							fatalError("UnSupported %start: " + i, e.pos);
						this.starts.push({index: this.index(lhs), begin: -1, width: -1});
					case _:
						fatalError("UnSupported :" + TExprTools.toString(e), e.pos);
					}
				}
			case _:
				fatalError("UnSupported :" + TExprTools.toString(a), a.pos);
			}
		}
		function extract(a: Array<ObjectField>, vec: haxe.ds.Vector<OpAssoc>, map:Map<String, OpAssoc>) {
			var R_UPPER:EReg = ~/^[A-Z][A-Z0-9_]*$/;
			for (i in 0...a.length) {
				var li = a[i].expr;
				var stype = a[i].field.toLowerCase();
				var type = Left;
				switch( stype ){
				case "left":
				case "right":
					type = Right;
				case "nonassoc":
					type = NonAssoc;
				case "start":
					readStarts(li);
					continue;
				case _:
					fatalError("UnSupported type of op assoc: " + stype, li.pos);
				}
				switch (li.expr) {
				case EArrayDecl(a):
					for (e in a) {
						var name = switch (e.expr) {
						case EConst(CIdent(i)): i;
						case EConst(CString(p)):
							var i = this.reflect.get(p); // reflect
							if (i == null)
								fatalError('No reflect for "' + p + '"', e.pos);
							i;
						case _: "";
						}
						if ( map.exists(name) )
							fatalError("Duplicate Token: " + name, e.pos);
						var term = udtMap.get(name);
						if (term != null) {
							if (term.t == false)
								fatalError("UnSupported Token: " + name, e.pos);
						} else if ( !R_UPPER.match(name) ) {
							fatalError("Only accept '/[A-Z][A-Z0-9_]*/' as placeholder: " + name, e.pos);
						}
						var op = {type: type, prio: i, tval: term != null ? term.value : -1};
						map.set(name, op);
						if (op.tval != -1)
							vec.set(op.tval, op);
					}
				default:
					fatalError("UnSupported Type", li.pos);
				}
			}
		}
		this.opIMap = new haxe.ds.Vector(this.maxValue);
		this.opSMap = new Map();
		var rule = cl.meta.extract(":rule");
		if (rule.length > 0) {
			var obj = rule[0].params[0];
			switch (obj.expr) {
			case EObjectDecl(a):
				extract(a, opIMap, opSMap);
			default:
			}
		}
		// if no entry then the first one
		if (this.starts.length == 0) this.starts.push({index: this.index(this.lhsA[0]), begin: -1, width: -1});
	}

	function stripECast( e : Expr ) {
		return switch(e.expr) {
		case ECast(e, _):
			stripECast(e);
		case _:
			e;
		}
	}

	function readLexerTermls(tk: Type) {
		switch (tk.follow()) {
		case TAbstract(_.get() => ab, _):
			for (field in ab.impl.get().statics.get()) {
				for (meta in field.meta.get()) {
					if (meta.name != ":value") continue;
					switch( stripECast(meta.params[0]).expr ) {
					case EConst(CInt(i)):
						var n = Std.parseInt(i);
						if (n >= maxValue)
							maxValue = n + 1;
						firstCharChecking(field.name, UPPER, field.pos);
						var t:Udt = {t: true, name: field.name, value: n, cset: CSet.single(n), pos: field.pos};
						termls.push(t);
						udtMap.set(t.name, t);
						if (t.name != this.sEof)
							termlsAll = CSet.union(termlsAll, t.cset);
					case _:
					}
				}
			}
		case _:
		}
		if (udtMap.get(this.sEof) == null) throw "Invalid EOF value: " + this.sEof; //
	}

	function getTermlCSet(name: String) {
		if (name == "_") return termlsAll;
		var t = udtMap.get(name);
		if (t != null) {
			return t.t ? t.cset : null; // Non-Termls if .t == false
		}
		if (name.length >= 2) {         // if name == "Op" then OpPlus, OpMinus, OpXxxx ....
			var cset = [];
			for (t in termls)
				if (StringTools.startsWith(t.name, name))
					cset = CSet.union(cset, t.cset);
			if ( cset.length > 0 && !CSet.isSingle(cset) ) // to prevent abuse
				return cset;
		}
		return null;
	}

	function getNonTermlCSet(name: String) {
		var t = udtMap.get(name);
		return t == null || t.t ? null : t.cset;
	}

	function byRule(n):Lhs {
		for (lhs in lhsA) {
			var len = lhs.cases.length;
			if (n < len) return lhs;
			n -= len;
		}
		throw "NotFound";
	}

	inline function ruleToCase(n: Int):SymbolSet return this.vcases.get(n);

	function transform(cases:Array<Array<Case>>) {
		if (cases.length == 0) return;
		for (lhs in lhsA) { // init udtMap first..
			if (this.udtMap.exists(lhs.name))
				fatalError("Duplicate LHS: " + lhs.name, lhs.pos);
			this.udtMap.set(lhs.name, {t: false, name: lhs.name, value: lhs.value, cset: CSet.single(lhs.value), pos: lhs.pos});
		}
		function setEpsilon(lhs, pos) {
			if (lhs.epsilon) fatalError('Duplicate "default:" or "case _:"', pos);
			lhs.epsilon = true;
		}
		function getCSet(name, pos) {
			var cset = getTermlCSet(name);
			if (cset == null) fatalError("Undefined: " + name, pos);
			return cset;
		}
		function opVerify(sym: Symbol):{type:OpAssocType, prio:Int} {
			var prio = -1;
			var type = Left;
			for (c in sym.cset) {
				for (i in c.min...c.max + 1) {
					var op = this.opIMap.get(i);
					if (op == null) {
						if (prio != -1)
							fatalError("Mixed non-operator", sym.pos);
					} else {
						if (i == c.min) { // if first item
							prio = op.prio;
							type = op.type;
						} else {
							if (prio == -1)
								fatalError("Mixed non-operator", sym.pos);
							else if (prio != op.prio)
								fatalError("Different priority", sym.pos);
						}
					}
				}
			}
			return {type: type, prio: prio};
		}
		for (lhs in this.lhsA) {
			for (c in cases[ index(lhs) ]) {
				if (c.guard != null)
					fatalError("Unsupported: " + "guard", c.guard.pos);
				switch(c.values) {
				case [{expr:EArrayDecl(el), pos: pos}]:
					if (lhs.epsilon)
						fatalError('This case is unused', pos);
					var g: SymbolSet = {action: c.expr, syms: [], left: null, right: null, pos: pos};
					var len = el.length;
					if (len == 0) {
						setEpsilon(lhs, pos);
						lhs.cases.push(g);
						continue;
					}
					var prec:{left:OpLeft, right:OpRight} = {left: null, right: null};
					var ei = 0;
					var e = el[0];
					while (true) {
						switch (e.expr) {
						case EConst(CIdent(i)):
							firstCharChecking(i, UPPER, e.pos);            // e.g: CInt
							g.syms.push( {t: true, name: i,    cset: getCSet(i, e.pos), ex: null, pos: e.pos} );

						case EConst(CString(s)):
							var i = this.reflect.get(s);
							if (i == null)
								fatalError("No associated token: " + s, e.pos);
							firstCharChecking(i, UPPER, e.pos);
							g.syms.push( {t: true, name: i,    cset: getCSet(i, e.pos), ex: null, pos: e.pos} );

						// TODO: maybe it's not good..
						case EParenthesis(macro $i{i}):
							firstCharChecking(i, LOWER, e.pos);            // all termls but no Eof
							g.syms.push( {t: true, name: "_",  cset: termlsAll,         ex: i,    pos: e.pos} );

						case ECall(macro $i{i}, [macro $i{v}]):            // e.g: CInt(n) or Op(op)
							firstCharChecking(i, UPPER, e.pos);
							firstCharChecking(v, LOWER, e.pos);
							g.syms.push( {t: true, name: i,    cset: getCSet(i, e.pos), ex: v,    pos: e.pos} );

						case EBinop(OpAssign, macro $i{v}, macro $i{nt}):  // e.g: e = expr
							var udt = udtMap.get(nt);
							if (udt == null || udt.t == true)
								fatalError("Undefined non-terminator: " + nt, e.pos);
							if (len == 1 && nt == lhs.name)
								fatalError("Infinite recursion", e.pos);
							if (ei == 0) { // the first item
								if (nt != lhs.name)
									lhs.lsubs.add(udt.value);
								if (len == 1)
									g.right = {type: Left, prio: -2, own: udt.value};
							}
							g.syms.push( {t: false, name: nt, cset: udt.cset , ex: v , pos: e.pos} );

						case EBinop(OpAssign, macro $i{v}, macro $a{a}):   // e.g: t = [OpPlus, OpMinus]
							var cset = CSet.C_EMPTY;
							for (t in a) {
								var i = switch (t.expr) {
								case EConst(CIdent(i)): i;
								case EConst(CString(s)): this.reflect.get(s);
								default: null;
								}
								if (i == null)
									fatalError("Unsupported: " + t.toString(), t.pos);
								firstCharChecking(i, UPPER, t.pos);
								cset = CSet.union(cset, getCSet(i, t.pos));
							}
							if (cset == CSet.C_EMPTY)
								fatalError("Empty", pos);
							g.syms.push( {t: true, name: "_", cset: cset, ex: v, pos: e.pos} );
						case EMeta({name: ":prec", params: [{expr: EConst(c), pos: p}]}, e2): // e.g: @:prec(FOLLOW)
							var i = switch(c) {
							case CIdent(i): i;
							case CString(s):
								var i = this.reflect.get(s);
								if (i == null)
									fatalError("No associated token: " + s, p);
								i;
							case _: fatalError("Unsupported: " + c, p);
							}
							var op = this.opSMap.get(i);
							if (op == null)
								fatalError("Undefined :" + i, p);
							prec.left  = {type: op.type, prio: op.prio, lval: -1};
							prec.right = {type: op.type, prio: op.prio, own: -1};
							e = e2;
							continue;
						case _:
							fatalError("Unsupported: " + e.toString(), e.pos);
						}
						if (++ei >= len) break;
						e = el[ei];
					}
					if (len >= 2) {
						// op prec
						var x1:Symbol = g.syms[len - 1];
						var x2:Symbol = g.syms[len - 2];
						if (x2.t && x1.t == false) { // case [..., termls, non-terml]:
							if (prec.left == null) {
								var op = opVerify(x2);
								if (op.prio != -1)
									g.left = {type: op.type, prio: op.prio, lval: x1.cset[0].min};
							} else {  // by @:prec(FOLLOW)
								prec.left.lval = x1.cset[0].min;
								g.left = prec.left;
							}
						}
						// op follows for each LHS
						var x1:Symbol = g.syms[0];
						var x2:Symbol = g.syms[1];
						if (x2.t && x1.t == false) { // case [non-term, termls, ...]:
							if (prec.right == null) {
								var op = opVerify(x2);
								if (op.prio != -1)
									g.right = {type: op.type, prio: op.prio, own: x1.cset[0].min}
							} else { // by @:prec(FOLLOW)
								prec.right.own = x1.cset[0].min;
								g.right = prec.right;
							}
						}
					}
					lhs.cases.push(g);
				case [{expr:EConst(CIdent("_")), pos: pos}]: // case _: || defualt:
					setEpsilon(lhs, pos);
					lhs.cases.push({action: c.expr, syms: [], left: null, right: null, pos: pos});
				case [e]:
					fatalError("Expected [ patterns ]", e.pos);
				case _:
					fatalError("Comma notation is not allowed while matching streams", c.values[0].pos);
				}
			}
		}
		lhsClosure();
		organize();
	}

	function lhsClosure() {
		var added = true;
		while (added) {
			added = false;
			for (top in lhsA) {
				var i = 0;
				var len = top.lsubs.length;
				for (tlv in top.lsubs) {
					if (i++ == len) break; // prevent iterating on added items
					var sub = this.lhsA[tlv - this.maxValue];
					for (slv in sub.lsubs) {
						if (Lambda.indexOf(top.lsubs, slv) == -1) {
							top.lsubs.add(slv);
							added = true;
						}
					}
				}
			}
		}
	}

	function organize() {
		// Scanning _t1~_tN from action
		var thas = [];
		function loop(e: Expr) {
			switch(e.expr) {
			case EConst(CIdent(s)) | EField({expr: EConst(CIdent(s))},_):
				if ( StringTools.startsWith(s, "_t") ) {
					var i = Std.parseInt( s.substr(2, s.length - 2) );
					if (i != null && i >= 1 && i <= thas.length)
						thas[i-1] = true;
				}
			case EField(_,_):
			default:
				e.iter(loop);
			}
		}
		var index = 0;
		var thasAll = new haxe.ds.Vector(nrules);
		for (lhs in lhsA) {
			for (li in lhs.cases) {
				if (li.action == null)
					fatalError("Need return *" + lhs.ctype.toString() + "*", li.pos);
				thas = [];
				thas.resize(li.syms.length);
				thasAll[index++] = thas;
				li.action.iter(loop);
			}
		}
		// duplicate var checking. & transform expr
		this.vcases = new haxe.ds.Vector(nrules);
		this.lvalues = new haxe.ds.Vector<Int>(this.nrules);
		var index = 0;
		for (lhs in lhsA) {
			for (li in lhs.cases) {
				this.vcases[index] = li; // init for ruleToCase()
				var row = new Map();
				var a:Array<Expr> = [];
				var len = li.syms.length;
				this.lvalues[index] = lhs.value << 8 | len;
				for (i in 0...len) {
					var dx = -(len - i); // negative
					if ( thasAll[index][i] ) {
						var stok = "_t" + (i + 1);
						var expr = macro @:privateAccess __s.offset($v{dx});
						a.push( macro var $stok: $ct_stream_tok = $expr );
					}
					var sym = li.syms[i];
					// checking...
					if (sym.ex == null || sym.ex == "_")
						continue;
					if (row.exists(sym.ex))
						fatalError("duplicate var: " + sym.ex, sym.pos);
					row.set(sym.ex, true);

					// transform expr
					var name = sym.ex;  // variable name
					if (sym.t) {
						var ofstr = funMap.get(sym.name);
						if (ofstr == null) {
							if ( sym.name != "_" && CSet.isSingle(sym.cset) ) // If you forget to add an extract function
								fatalError("Required a static function with @:rule("+ sym.name +")", sym.pos);
							a.push(macro @:pos(sym.pos) var $name: $ct_termls = cast @:privateAccess __s.offset($v{dx}).term);
						} else {
							var ct = ofstr.ct;
							switch(ofstr.args) {
							case 1:  // (string)
								a.push(macro @:pos(sym.pos) var $name: $ct = $i{ofstr.name}( @:privateAccess __s.stri($v{dx}) ));
							case 2:  // (input, tok)
								a.push(macro @:pos(sym.pos) var $name: $ct = @:privateAccess ($i{ofstr.name}(__s.lex.input, __s.offset($v{dx}))));
							default: // (input, pmin, pmax)
								a.push(macro @:pos(sym.pos) var $name: $ct = @:privateAccess ($i{ofstr.name}(__s.lex.input, __s.offset($v{dx}).pmin, __s.offset($v{dx}).pmax)));
							}
						}
					} else {
						var lhsValue = sym.cset[0].min; // NON-TERML
						var ct = lhsA[lhsValue - maxValue].ctype;
						a.push( macro @:pos(sym.pos) var $name: $ct = @:privateAccess (cast __s.offset($v{dx}).val: $ct) );
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
				li.action = macro @:pos(li.action.pos) @:mergeBlock {
					@:mergeBlock $b{a};
					@:mergeBlock $e{li.action};
				}
				++ index;
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
				for (sym in c.syms)
					p = add(p, sym.cset);
				a.push(p);
			}
			ret.push(a);
		}
		return ret;
	}

	function readLhs(allFields: Map<String, Field>): Array<Array<Case>> {
		var lhsValue = this.maxValue;
		var lhsType = this.ct_ldef.toType();
		var fields: Array<Field> = Context.getBuildFields();
		var flazy = new List<{f: Field, fun: Function}>();
		var ret = [];
		this.nrules = 0;
		for (f in fields) {
			switch (f.kind) {
			case FVar(ct, e) if (e != null):
				switch(e.expr) {
				case ESwitch(_, ecases, edef):
					if (ecases.length == 0 && edef == null) continue;
					if (edef != null)
						ecases.push({values: [macro @:pos(edef.pos) _], expr: edef});
					firstCharChecking(f.name, LOWER, f.pos);
					if ( ct != null && this.ct_ldef == this.ct_lval && !Context.unify(ct.toType(), lhsType) )
						this.ct_lval = macro :Dynamic; // use Dynamic if .unify() == false
					ret.push(ecases);
					this.nrules += ecases.length;
					this.lhsA.push({
						name: f.name,
						value: lhsValue++,
						epsilon: false,
						cases: [],
						lsubs: new List(),
						ctype: ct == null ? this.ct_ldef : ct,
						pos: f.pos,
					});
					continue;
				case _:
					if (f.name == "__default__") {
						this.unMatched = e;
						continue;
					}
				}
			case FFun(fun):
				flazy.add({f: f, fun: fun});
			default:
			}
			if (allFields.exists(f.name))
				fatalError("Duplicate field: " + f.name, f.pos);
			allFields.set(f.name, f);
		}

		if (lhsValue > 255) throw "Too Many Termls/NoN-Termls"; // TODO: or force the invalid value up to 16bit??

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
						fatalError("UnSupperted value for @:rule: " + t.toString(), t.pos);
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
				fatalError("Should be start with a capital letter: " + s, pos);
		} else {
			if ( !(c >= "a".code && c <= "z".code || c == "_".code) )
				fatalError("Should be start with a lowercase letter: " + s, pos);
		}
	}
}
#else
extern class LR0Base {}
#end
