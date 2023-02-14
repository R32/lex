package lm;

#if (macro || eval || display)

import haxe.macro.Expr;
import haxe.macro.Type;
import lm.Charset;
import lm.LexEngine;
import lm.ExprHelps;
import lm.Utils.isUpperCaseFirst;
import lm.Utils.isLowerCaseFirst;
 using haxe.macro.ExprTools;

typedef RuleCaseGroupExtend = {
	>RuleCaseGroup,
	ctype : ComplexType,
	pos : Position,
}

/*
 * enum term
 */
typedef EmuTerm = {
	t : Bool, // term or non-term
	name : String,
	value : Int,
	cset : Charset,
	pos : Position,
}

/*
 * stream token
 */
typedef StreamToken = {
	t    : Bool,
	name : String,
	cset : Charset,
	pos  : Position,
	?extract : String, // if "CStr(str)" then "str", elseif "e = expr" then "e"
}
typedef StreamTokenSets = {
	action : Expr,
	sets   : Array<StreamToken>,
	prec  : PrecCase,
}

typedef LeftHandSide = {
	name  : String,
	value : Int,            // automatic increase
	edef  : Expr,           // if "default:", "case []" or "case _"
	cases : Array<StreamTokenSets>,
	lsubs : Array<Int>,     // <sub_lhs.value>
	ctype : ComplexType,
	entry : Bool,
	pos   : Position,
}

enum PrecType {
	Left;
	Right;
	Nonassoc;
}
typedef Prec = {
	type : PrecType,
	prio : Int,
	value : Int,
}
typedef PrecCase = {
	?left : Prec,
	?right : Prec,
}

/*
 *
 */
class ParserBase {

	/*
	 * The union set of terms(no "eof" and no "non-terms")
	 */
	public var terms_union_cset : Charset; // terms_union_cset

	public var terms_ct : ComplexType;

	/*
	 * enum terms map(without non-terms), The first letter of all keys is UPPERCASE.
	 */
	public var terms_map : Map<String, EmuTerm>;

	/*
	 * non-terms map, The first letter of all keys is LOWERCASE.
	 */
	public var non_terms_map : Map<String, EmuTerm>;

	public var lhsides : Array<LeftHandSide>;

	public var rule_groups : Array<RuleCaseGroupExtend>;

	public var funmap : Map<String, {name : String, ct : ComplexType, args : Int, pos : Position}>; //  TokenName => FunctionName

	public var precs_map : Map<String, Prec>;

	public var starts : Array<String>;

	var precs_idx : haxe.ds.Vector<Prec>; // precs_idx[term.value]

	public var nrules : Int;

	public var reduce_data : Array<Int>; // for lm.Stream.reduce

	public var eof : String;

	public var reflect : Map<String, String>; // PatternString => TokenString

	public var max_term_value(default, null) : Int; // terms | non-terms Separator

	public function is_term( v : Int ) return v < max_term_value;

	public function is_non_term( v : Int ) return v >= max_term_value;

	public function new () {
		terms_union_cset = [];
		rule_groups = [];
		lhsides = [];
		non_terms_map = new Map();
		terms_map = new Map();
		precs_map = new Map();
		funmap = new Map();
		reduce_data = [];
		eof = null;
		nrules = 0;
		reflect = null;
		max_term_value = 0;
	}

	public function lhsFromRule( n : Int ) : LeftHandSide {
		for (lhs in this.lhsides) {
			var len = lhs.cases.length;
			if (n < len)
				return lhs;
			n -= len;
		}
		throw "NotFound";
	}

	public function caseFromRule( n : Int ) : StreamTokenSets {
		for (lhs in this.lhsides) {
			var len = lhs.cases.length;
			if (n < len)
				return lhs.cases[n];
			n -= len;
		}
		throw "NotFound";
	}

	public function addTerm( name : String, value : Int, pos : Position ) {
		if (!isUpperCaseFirst(name))
			throw new Error("The first char must be UPPERCASE: " + name, pos);
		var term = {t : true, name : name, value : value, cset : CSet.single(value), pos : pos};
		this.terms_map.set(name, term);
		if (value >= this.max_term_value)
			this.max_term_value = value + 1;
		if (name != this.eof)
			this.terms_union_cset = CSet.union(this.terms_union_cset, term.cset);
	}

	public function initPrecMap( precs : Array<{type : PrecType, sets : Array<Expr>}> ) {
		this.precs_idx = new haxe.ds.Vector(this.max_term_value);
		var prio = 1;
		for (prec in precs) {
			for (e in prec.sets) {
				var tm = this.expectTerm(e, true);
				var id = expectIdent(e);
				var value = -1;
				if (tm != null) {
					id = tm.name;
					value = tm.value;
				} else if (id == null || id != id.toUpperCase()) {
					throw new Error("Undefined token: " + e.toString(), e.pos);
				}
				if (this.precs_map.exists(id))
					throw new Error("Duplicate token: " + id, e.pos);
				var prec = {type : prec.type, prio : prio, value : value};
				this.precs_map.set(id, prec);
				if (value >= 0)
					this.precs_idx.set(value, prec);
			}
			prio++;
		}
	}

	// add non-term
	public function addNonTerm( name : String, value : Int, ct : ComplexType, cases : Array<Case>, edef : Null<Expr>, pos : Position ) {
		if (!isLowerCaseFirst(name))
			throw new Error("The first char must be LOWERCASE: " + name, pos);
		if (value >= 255)
			throw new Error("The number of (tokens + LHS) exceeds 255", pos);

		if (this.starts.length == 0)
			this.starts.push(name);
		// raw term
		var term = {t : false, name : name, value : value, cset : CSet.single(value), pos : pos};
		this.non_terms_map.set(name, term);

		// transform edef to case "_" if non entry
		var isEntry = this.starts.indexOf(name) >= 0;
		var unmatch = null;
		if (edef != null) {
			var pattern = {expr : EConst(CIdent("_")), pos : edef.pos};
			if (isEntry) {
				unmatch = {pattern : pattern, action : edef};
			} else {
				cases.push({values : [pattern], expr : edef});
			}
		}

		this.nrules += cases.length;
		// origin switch expr
		this.rule_groups.push({name : name, ctype : ct, rules : cases.map(caseToPattern), unmatch : unmatch , pos : pos});
		// transformed switch expr
		this.lhsides.push({name : name, ctype : ct, cases : [], edef : edef, value : value, lsubs : [], pos : pos, entry : isEntry});
	}
	function caseToPattern( c : Case ) {
		if (c.guard != null)
			throw new Error("Unsupported: " + c.guard.toString(), c.guard.pos);
		if (c.values.length > 1)
			throw new Error("Multi-match is not supported", c.values[1].pos);
		return {pattern : c.values[0], action : c.expr};
	}

	public function expectIdent( e : Expr ) : String {
		return switch(e.expr) {
		case EConst(CIdent(i)): i;
		default: null;
		}
	}

	public function expectTerm( e : Expr, skipError = false ) {
		var name = switch(e.expr) {
		case EConst(CIdent(i)):
			i;
		case EConst(CString(s)):
			var t = this.reflect.get(s);
			if (t == null)
				throw new Error("No associated token: " + s, e.pos);
			t;
		case _:
			"";
		}
		var tm = terms_map.get(name);
		if (tm == null && !skipError)
			throw new Error("Undefined token: " + name, e.pos);
		return tm;
	}

	public function toPatternSets() : Array<PatternSet> {
		inline function add(rule, cur)
			return @:privateAccess LexEngine.next(rule, Match(cur));
		var result = [];
		for (lhs in this.lhsides) {
			var line = [];
			for (kase in lhs.cases) {
				var pattern = Empty;
				for (tk in kase.sets)
					pattern = add(pattern, tk.cset);
				line.push(pattern);
			}
			result.push(line);
		}
		return result;
	}

	public function transform( extra = true ) {
		for (i in 0...this.rule_groups.length) {
			var lhs = this.lhsides[i];
			for (rule in this.rule_groups[i].rules) {
				var stsets = {action : rule.action, sets : [], prec : {}};
				switch(rule.pattern.expr) {
				case EArrayDecl([]), EConst(CIdent("_")):
					if (lhs.edef != null)
						throw new Error("Duplicate empty matche", rule.pattern.pos);
				case EArrayDecl(el):
					stsetsRead(lhs, el, stsets);
				case EMeta({name: ":prec", params: [c]}, {expr : EArrayDecl(el)}) if (el.length > 0):
					stsets.prec = casePrecMeta(c);
					stsetsRead(lhs, el, stsets);
				default:
					throw new Error("Unsupported: " + rule.pattern.toString(), rule.pattern.pos);
				};
				lhs.cases.push(stsets);
				this.reduce_data.push(lhs.value << 8 | stsets.sets.length);
			}
		}
		this.lhsClosure();
		if (extra)
			this.AddExtraHaxeCode(); // T1~TN ...
	}

	function stsetsRead( owner : LeftHandSide, el : Array<Expr>, result : StreamTokenSets ) {
		var index = 0;
		var indexMax = el.length;
		var e = el[0];
		var stoken : StreamToken = null;
		while (true) {
			switch(e.expr) {
			case EConst(CIdent(i)):  // CInt, OpAdd
				if (isUpperCaseFirst(i)) {
					stoken = {t : true, name : i  , cset : getCSet(i) , pos : e.pos};
				} else if (indexMax == 1 && index == 0) { // e.g: [t]
					stoken = {t : true, name : "*", cset : terms_union_cset, pos : e.pos, extract : i};
				} else {
					throw new Error("The first char must be UPPERCASE: " + i, e.pos);
				}
			case EConst(CString(s)): // ["+", "-", "*"]
				var i = this.reflect.get(s);
				if (i == null)
					throw new Error("No associated token: " + s, e.pos);
				stoken = {t : true, name : i,  cset : getCSet(i), pos : e.pos};
			case EParenthesis(sub):  // (pattern)
				e = sub;
				continue;
			case ECall({expr : EConst(CIdent(i)), pos : pos}, [v]): // CInt(n)
				var sv = expectIdent(v);
				if (sv == null)
					throw new Error("Unsupported: " + v.toString() ,v.pos);
				stoken = {t : true, name : i, cset : getCSet(i), pos : e.pos, extract : sv};
			case EBinop(OpAssign, {expr : EConst(CIdent(v))}, {expr : EConst(CIdent(non)), pos : npos}): // [e = expr]
				var tm = non_terms_map.get(non);
				if (tm == null)
					throw new Error("Undefined non-terminator: " + non, npos);

				if (indexMax == 1 && non == owner.name)
					throw new Error("Infinite recursion", e.pos);

				if (index == 0 && non != owner.name)
						owner.lsubs.push(tm.value);

				stoken = {t : false, name: non, cset : tm.cset , extract : v , pos : e.pos};
			case EBinop(OpAssign, {expr : EConst(CIdent(v))}, {expr : EArrayDecl(a), pos : pos}): // t = [OpPlus, OpMinus]
				var cset = CSet.C_EMPTY;
				var prev = 0;
				var opts = [];
				for (i in 0...a.length) {
					var tm = expectTerm(a[i]);
					opts.push(tm.name);
					cset = CSet.union(cset, tm.cset);
				}
				if (result.prec.left == null && !precPrioDetect(opts))
					throw new Error("Inconsistent priorities: " + opts.join(", "), pos);
				if (cset == CSet.C_EMPTY)
					throw new Error("Empty", pos);
				stoken = {t : true, name : "*", cset : cset, extract : v, pos : e.pos};
			case EMeta({name : ":prec", params : [c]}, sub): // e.g: @:prec(FOLLOW) tokns
				result.prec = casePrecMeta(c);
				e = sub;
				continue;
			default:
				throw new Error("Unsupported: " + e.toString(), e.pos);
			}
			if (stoken.cset == CSet.C_EMPTY)
				throw new Error("Undefined: " + e.toString(), e.pos);
			result.sets.push(stoken);
			if (++index >= indexMax)
				break;
			// reset for next
			e = el[index];
			stoken = null;
		}
		// prec reads
		casePrecRead(result);
		return result;
	}
	function stripUnderscore( e : Expr ) {
		return switch(e.expr) {
		case EConst(CIdent(i)) if (i != "_"): e;
		default: null;
		}
	}
	function casePrecMeta( e : Expr ) {
		var tm = expectTerm(e, true);
		var name = tm == null ? expectIdent(e) : tm.name;
		var op = this.precs_map.get(name);
		if (op == null)
			throw new Error("Undefined :" + name, e.pos);
		return {
			left : {type : op.type, prio : op.prio, value : -1},
			right : {type : op.type, prio : op.prio, value : -1},
		}
	}
	function casePrecRead( stsets: StreamTokenSets ) {
		if (stsets.sets.length < 2 || stsets.prec.left != null)
			return;
		var sets = stsets.sets;
		var prec = stsets.prec;
		// left
		var t1 = sets[sets.length - 1];
		var t2 = sets[sets.length - 2];
		if (prec.left == null && t2.t && t1.t == false) {// [..., Token, e = expr]
			var def = getPrecByCSet(t2.cset);
			if (def != null) {
				prec.left = {type : def.type, prio : def.prio, value : t1.cset[0].min}
			}
		}
		// right
		var t1 = sets[0];
		var t2 = sets[1];
		if (prec.right == null && t2.t && t1.t == false) {// [expr, Token, ....]
			var def = getPrecByCSet(t2.cset);
			if (def != null) {
				prec.right = {type : def.type, prio : def.prio, value : t1.cset[0].min}
			}
		}
	}
	function getPrecByCSet( cset : Charset ) {
		for (c in cset) {
			for (i in c.min ... c.max + 1) {
				// The priority has been verified in function "transform"
				return precs_idx.get(i);
			}
		}
		return null;
	}

	function getCSet( name : String ) {
		if (name == "*")
			return this.terms_union_cset;
		var t = terms_map.get(name);
		if (t != null)
			return t.cset;
		return CSet.C_EMPTY;
	}

	// Are all operators the same priority?
	function precPrioDetect( names : Array<String> ) {
		var prev = 0;
		for (i in 0...names.length) {
			var op = precs_map.get(names[i]);
			var prio = op == null ? 0 : op.prio;
			if (i > 0 && prio != prev)
				return false;
			prev = prio;
		}
		return true;
	}

	function lhsClosure() {
		var added = true;
		while (added) {
			added = false;
			for (top in lhsides) {
				var i = 0;
				var len = top.lsubs.length;
				while (i < len) { // prevent iterating on added items
					var sub = this.lhsides[top.lsubs[i++] - this.max_term_value];
					for (v in sub.lsubs) {
						if (top.lsubs.indexOf(v) >= 0)
							continue;
						top.lsubs.push(v);
						added = true;
					}
				}
			}
		}
		// sorts
		for (lhs in lhsides) {
			lhs.lsubs.sort(lm.Utils.onIntCompar);
		}
	}

	// writing AST manually is complex, because the keyword "macro" cannot be used here,
	function AddExtraHaxeCode() {
		//// cast e
		function mk_cast( e : Expr) {
			return {
				expr : ECast(e, null),
				pos : e.pos
			}
		}
		//// e.field
		function mk_field( e : Expr, field : String, pos : Position ) {
			return {
				expr : EField(e, field),
				pos : pos
			}
		}
		//// @:privateAccess e.field
		function mk_field_pri( e : Expr, field : String, pos : Position ) {
			var expr = mk_field(e, field, pos);
			return {
				expr : EMeta({name : ":privateAccess", pos : pos}, expr),
				pos : pos
			}
		}
		//// (expr :ctype)
		function mk_check( e : Expr, ctype : ComplexType ) {
			if (ctype == null)
				return e;
			return {
				expr : ECheckType(e, ctype),
				pos : e.pos
			}
		}
		//// var name : ctype = expr;
		function mk_var( name : String, ctype : ComplexType, expr : Expr, pos : Position ) {
			return {
				expr : EVars([{
					name : name,
					expr : expr,
					type : ctype,
					isFinal : true,
				}]),
				pos : pos
			}
		}
		//// stream.offset(dx)
		function mk_stream_offset( dx : Int, pos : Position ) {
			var expr = mk_field_pri({expr : EConst(CIdent("stream")), pos : pos}, "offset", pos);
			return {
				expr : ECall(expr, [{expr : EConst(CInt("" + dx)), pos : pos}]),
				pos : pos
			}
		}

		for (lhs in this.lhsides) {
			for (stsets in lhs.cases) {
				var len = stsets.sets.length;
				var extra : Array<Expr> = [];
				for (i in 0...len) {
					var dx = -(len - i);
					var stoken = stsets.sets[i];
					var vname = stoken.extract;
					var pos = stoken.pos;
					//// final Tx = stream.offset(dx);
					extra.push(mk_var("T" + (i + 1), null, mk_stream_offset(dx, pos), pos));
					if (vname == null || vname == "_")
						continue;
					var CURRENT = {expr : EConst(CIdent("T" + (i + 1))), pos : pos};
					// non-term
					if (stoken.t == false) {
						var lvalue = stoken.cset[0].min;
						var ctype = lhsides[lvalue - max_term_value].ctype;
						var expr = mk_field(CURRENT, "val", pos);
						var stsm = mk_var(vname, ctype, expr, pos);
						//// var vname = stream.offset(dx).val;
						extra.push(stsm);
						continue;
					}
					// term
					var tkname = stoken.name;
					var func = this.funmap.get(tkname);
					if (func == null) {
						if (tkname != "*" && CSet.isSingle(stoken.cset))
							throw new Error("Required a function with @:rule(" + tkname + ")", pos);
						var expr = mk_field(CURRENT, "term", pos);
						var stsm = mk_var(vname, terms_ct, mk_cast(expr), pos);
						//// var vname : Token = cast stream.offset(dx).term;
						extra.push(stsm);
						continue;
					}
					var fexpr = {expr : EConst(CIdent(func.name)), pos : pos};
					var args = switch(func.args) {
					case 1:
						//// [stream.str(stream.offset(dx))]
						var expr = mk_field({expr : EConst(CIdent("stream")), pos : pos}, "str", pos);
						var expr = {expr : ECall(expr, [CURRENT]), pos : pos};
						[expr];
					case 2:
						//// [stream.lex.input, stream.offset(dx)]
						var expr = mk_field_pri({expr : EConst(CIdent("stream")), pos : pos}, "lex", pos);
						var expr = mk_field(expr, "input", pos);
						[expr, CURRENT];
					default:
						throw new Error("Unsupported: " + func.name, func.pos);
					}
					var call = {expr : ECall(fexpr, args), pos : pos};
					var stsm = mk_var(vname, func.ct, call, pos);
					extra.push(stsm);
				}
				if (extra.length == 0)
					continue;
				// update action
				extra.push(mk_check(stsets.action, lhs.ctype));
				stsets.action = {
					expr : EBlock(extra),
					pos : stsets.action.pos
				}
			}
		}
	}
}
#end
