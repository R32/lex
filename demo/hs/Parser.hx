package hs;

import hs.Lexer;
import hscript.Expr;

/**
* The Parser for hscript (As an example only that passed the Test.hx of hscript.)
* - 1. (HACK 1) for arrow-functions. since `(x)->expr` will be conflict with `(expr)` which is annoying.
* - 2. using ">",">","=" instead of ">>=" for parsing, and also ">>", ">>>", ">=", ">>>="
*/
@:rule({
	right: ["=", "+=", "-=", "*=", "/=", "%=", "<<=", "|=", "&=", "^=", "=>", "->", "?", /* ">>=", ">>>=" */],
	left: ["||"],
	left: ["&&"],
	left: ["..."],
	left: ["==", "!=", ">", "<", "<=", /* >= */],
	left: ["|", "&", "^"],
	left: ["<<", /* ">>", ">>>" */],
	left: ["+", "-"],
	left: ["*", "/"],
	left: ["%"],
	nonassoc: ["!", "~" , "++", "--"],
	left: ["[", "(", "."],  // ++a[0] => ++(a[0])
}) class Parser implements lm.SLR<Lexer> {

	// ofString
	@:rule(CIdent) static inline function id_ofstr(s:String):String return s;
	@:rule(CInt) static inline function int_ofstr(s:String):Int return Std.parseInt(s);
	@:rule(CFloat) static inline function float_ofstr(s:String):Float return Std.parseFloat(s);
	@:rule(Meta) static inline function meta_ofstr(input:lms.ByteData, t):String return input.readString(t.pmin + 1, t.pmax - 1 - t.pmin);
	@:rule(CString) @:access(hs.Lexer) static inline function s_ofstr(input:lms.ByteData, t):String return Lexer.smap.get(t.pmin);

	static function optSemicolon(s) {
		if (s.peek(0).term == cast Semicolon)
			s.junk(1);
	}

	static function autoSemicolon( s : lm.Stream, e : Expr) {
		var t = s.peek(0);
		if (t.term == cast Semicolon) {
			s.junk(1);
		} else {
			switch (e) {
			case EIf(_)
			| EBlock(_)
			| EObject(_)
			| EFor(_)
			| EWhile(_)
			| EDoWhile(_)
			| EFunction(_)
			| ESwitch(_)
			| ETry(_):
			default:
				if (t.term != cast Eof)
					throw s.UnExpected(t);
			}
		}
	}

	static function conpos( s : lm.Stream, a : Array<lm.Stream.Tok> ) : Void {
		var i = 0;
		var len = a.length;
		var t1 = a[i++];
		while (i < len) {
			var t2 = a[i++];
			if (t1.pmax != t2.pmin)
				throw s.UnExpected(t2);
			t1 = t2;
		}
	}

	// %start main
	static var main : Expr = switch (s) {
		case [l = list, Eof]: l.length == 1 ? l[0]: EBlock(l);
		default:
			var t = stream.peek(0);
			switch(t.term) {
			case Eof:
				EBlock([]);
			default:
				throw stream.UnExpected(t);
			}
	}

	static var list: Array<Expr> = switch(s) {
		case [l = list, i = item]:
			autoSemicolon(stream, i);
			l.push(i);
			l;
		case [i = item]:
			autoSemicolon(stream, i);
			[i];
	}

	static var item = switch(s) {
		case [e = expr]: e;
		case [v = vdef]: v;
		case ["break"]:            EBreak;
		case ["continue"]:         EContinue;
		case ["return"]:           EReturn(null);
		case ["return", e = expr]: EReturn(e);
	}

	static var vdef = switch(s) {
		case ["var", CIdent(i)]:                                                     EVar(i, null, null);
		case ["var", CIdent(i), ":", t = ctype]:                                     EVar(i, t, null);
		case ["var", CIdent(i), "=", e = expr]:                                      EVar(i, null, e);
		case ["var", CIdent(i), ":", t = ctype, "=", e = expr]:                      EVar(i, t, e);
		case ["function", CIdent(i), "(", a = args , ")", e = item]:                 EFunction(a, e, i, null);
		case ["function", CIdent(i), "(", a = args , ")", ":", t = ctype, e = item]: EFunction(a, e, i, t);
		case ["function", CIdent(i), "(", ")", e = item]:                            EFunction([], e, i, null);
		case ["function", CIdent(i), "(", ")", ":", t = ctype, e = item]:            EFunction([], e, i, t);
	}

	static var anon = switch(s) {
		case ["{", "}"]:                            EObject([]);
		case ["{", o = anon_list , "}"]:            EObject(o);        // {a:1}
		case ["{", o = anon_list , ",", "}"]:       EObject(o);        // {a:1,} , could be end with extra ","
	}
	static var anon_list: Array<{name:String, e:Expr}> = switch(s) {
		case [o1 = anon_list, ",", o2 = anon_list]: o1.push( o2[0] ); o1;
		case [CIdent(i), ":", e = expr]:            [{name: i, e: e}];
		case [CString(i), ":", e = expr]:           [{name: i, e: e}]; // allowJSON
	}

	static var expr : Expr = switch (s) {
		case [e1 = expr, op = ["=", "+=", "-=", "*=", "/=", "%=", "<<=", "|=", "&=", "^="], e2 = expr]:     EBinop(stream.str(T2), e1, e2);
		case [@:prec("<<=") e1 = expr, ">", ">", "=", e2 = expr]:        conpos(stream, [T2, T3, T4]);      EBinop(">>=", e1, e2);
		case [@:prec("<<=") e1 = expr, ">", ">", ">", "=", e2 = expr]:   conpos(stream, [T2, T3, T4, T5]);  EBinop(">>>=", e1, e2);
		case [@:prec("<=") e1 = expr, ">", "=", e2 = expr]:              conpos(stream, [T2, T3]);          EBinop(">=", e1, e2);
		case [@:prec("<<") e1 = expr, ">", ">", e2 = expr]:              conpos(stream, [T2, T3]);          EBinop(">>", e1, e2);
		case [@:prec("<<") e1 = expr, ">", ">", ">", e2 = expr]:         conpos(stream, [T2, T3, T4]);      EBinop(">>>", e1, e2);
		case [e1 = expr, op = ["==", "!=", ">", "<", "<="], e2 = expr]:  EBinop(stream.str(T2), e1, e2);
		case [e1 = expr, "<<", e2 = expr]:                     EBinop("<<", e1, e2);
		case [e1 = expr, "||", e2 = expr]:                     EBinop("||", e1, e2);
		case [e1 = expr, "&&", e2 = expr]:                     EBinop("&&", e1, e2);
		case [e1 = expr, op = ["|", "&", "^"], e2 = expr]:     EBinop(stream.str(T2), e1, e2);
		case [e1 = expr, op = ["+", "-"], e2 = expr]:          EBinop(stream.str(T2), e1, e2);
		case [e1 = expr, op = ["*", "/"], e2 = expr]:          EBinop(stream.str(T2), e1, e2);
		case [e1 = expr, "%", e2 = expr]:                      EBinop("%", e1, e2);
		case [@:prec("~") op = ["-", "~", "!"], e = expr]:     EUnop(stream.str(T1), true, e);
		case [l = left]:     l;
		case [b = block]:    b;
		case [c = constant]: c;
	}

	static var constant = switch(s) {
		case [CInt(i)]:     EConst(CInt(i));     // int
		case [CFloat(f)]:   EConst(CFloat(f));   // float
		case [CString(cs)]: EConst(CString(cs)); // string
		case ["true"]:      EIdent("true");      // true
		case ["false"]:     EIdent("false");     // false
		case ["null"]:      EIdent("null");      // null
	}

	static var left = switch(s) {
		case [op = ["++", "--"], l = left]:         EUnop(stream.str(T1), true, l);
		case [l = left, op = ["++", "--"]]:         EUnop(stream.str(T2), false, l);
		case [l = left, ".", CIdent(i)]:            EField(l, i);
		case [l = left, "[", e = expr , "]"]:       EArray(l, e);
		case ["[", "]"]:                            EArrayDecl([]);  // []
		case ["[", a = a_expr , "]"]:               EArrayDecl(a);   // [1, 2]
		case ["[", a = a_expr , Comma, "]"]:        EArrayDecl(a);   // [1, 2,]
		case ["[", a = map_item, "]"]:              EArrayDecl(a);   // [a=>1, b=>2]
		case ["[", a = map_item , Comma, "]"]:      EArrayDecl(a);   // [a=>1, b=>2,]
		case [b = anon]:                            b;               // {}, {a:1, b:2}
		case ["(", e = expr, ")"]:                  EParent(e);
		case ["(", e = expr, ":", t = ctype, ")"]:  ECheckType(e, t);
		case ["(", CIdent(i),":", t = ctype, ")"]:  ECheckType(EIdent(i), t);  // HACK 1
		case ["(", CIdent(i), ")"]:                 EParent(EIdent(i));        // HACK 1.
		case [l = left, "(", p = a_expr,")"]:       ECall(l, p);     // call(a,b...);
		case [l = left, "(", ")"]:                  ECall(l, []);    // call()
		case ["new", f = s_field, "(", ")"]:        ENew(f, []);     // new Instance()
		case ["new", f = s_field, "(", p = a_expr, ")"]: ENew(f, p); // new Instance(a, b)
		case [CIdent(i)]:                           EIdent(i);
	}

	static var s_field:String = switch(s) {
		case [f = s_field, ".", CIdent(i)]: f + "." + i;
		case [CIdent(i)]: i;
	}

	static var block = switch(s) {
		case [f = ifelse]:           f;  // if else
		case ["{", l = list , "}"]:  EBlock(l);   // {expr1; expr2;}
		case ["try", e1 = expr, "catch", "(", CIdent(v), ":", t = ctype, ")", e2 = expr]: ETry(e1, v, t, e2);
		case ["for", "(", CIdent(i), "in", e1 = expr, "...", e2 = expr , ")", e = item]:  EFor(i, EBinop("...", e1, e2), e);
		case ["for", "(", CIdent(i), "in", l=left, ")", e = item]:       EFor(i, l, e);
		case ["switch", "(", c = expr, ")", "{", b = switch_block, "}"]: ESwitch(EParent(c), b.cases, b.def);
		case ["do", e = item, "while", "(", c = expr, ")"]:              EDoWhile(EParent(c), e);
		case ["while", "(", c = expr, ")", e = item]:                    EWhile(EParent(c), e);
		case [c = expr, "?", e1 = expr, ":", e2 = expr]:                 ETernary(c, e1, e2);
		case ["throw", e = expr]:                                        EThrow(e);
		// Closure functions
		case ["function", "(", a = args, ")", e = item]:                 EFunction(a, e, null, null);
		case ["function", "(", a = args, ")", ":", t = ctype, e = item]: EFunction(a, e, null, t);
		case ["function", "(", ")", e = item]:                           EFunction([], e, null, null);
		case ["function", "(", ")", ":", t = ctype, e = item]:           EFunction([], e, null, t);
		// Arrow Functions,
		case [CIdent(i), "->", e = expr]:                                EFunction([{name: i}], EReturn(e));
		case ["(", ")", "->", e = expr]:                                 EFunction([], EReturn(e));
		case ["(", x = hack_first, ")", "->", e = expr]:                 EFunction([x], EReturn(e));                    // HACK 1
		case ["(", x = hack_first, ",", a = args, ")", "->", e = expr]:  a.unshift(x); EFunction(a, EReturn(e));        // HACK 1
		case ["(", CIdent(i), ")", "->", e = expr]:                      EFunction([{name: i}], EReturn(e));            // HACK 1
		case ["(", CIdent(i), ":", t = ctype,")", "->", e = expr]:       EFunction([{name: i, t:t}], EReturn(e));       // HACK 1
		case ["(", CIdent(i), ",", a = args, ")", "->", e = expr]:       a.unshift({name: i}); EFunction(a, EReturn(e));// HACK 1
	}
	static var args: Array<Argument> = switch(s) {
		case [a = args, ",", i = args_item]: a.push(i); a;
		case [i = args_item]: [i];
	}
	static var args_item: Argument = switch(s) {
		case ["?", CIdent(i), ":", t = ctype]:    {name: i, t:t, opt: true};      // ?arg:Type
		case ["?", CIdent(i)]:                    {name: i, opt: true};           // ?arg
		case [CIdent(i), ":", t = ctype]:         {name: i, t:t};                 // arg:Type
		case [CIdent(i), "=", c = constant]:      {name: i, opt: true, value: c}; // arg = const
		case [CIdent(i)]:                         {name: i};                      // arg,
	}
	static var hack_first: Argument = switch (s) {                                // HACK 1, no single [CIdent(i)] here
		case ["?", CIdent(i), ":", t = ctype]:    {name: i, t:t, opt: true};
		case ["?", CIdent(i)]:                    {name: i, opt: true};
		case [CIdent(i), ":", t = ctype]:         {name: i, t:t};
		case [CIdent(i), "=", c = constant]:      {name: i, opt: true, value: c};
	}

	static var a_expr: Array<Expr> = switch(s) {
		case [a = a_expr, ",", e = expr]:         a.push(e); a;
		case [e = expr]:                          [e];
	}

	static var map_item: Array<Expr> = switch(s) {
		case [a1 = map_item, ",", a2 = map_item]: a1.push( a2[0] ); a1;
		case [e1 = expr, "=>", e2 = expr]:        [EBinop("=>", e1, e2)];
	}

	static var ifelse = switch(s) {
		case [f = ifelse, "else", se = item]:
			optSemicolon(stream);
			switch(f) {
			case EIf(c, e, _): EIf(c, e, se);
			case _: throw("TODO");
			}
		case ["if", "(", c = expr, ")", e = item]:
			optSemicolon(stream);
			EIf(c, e);
	}

	static var metas: Array<{name:String, params:Array<Expr>}> = switch(s) {
		case [a = metas, m = metas_item]:         a.push(m);  a;
		case [m = metas_item]:                    [m];
	}
	static var metas_item:{name:String, params:Array<Expr>} = switch(s) {
		case [Meta(m), "(", p = a_expr, ")"]:     {name: m, params: p};   // @:meta(expr)
		case [Meta(m)]:                           {name: m, params: []};  // @:meta
	}

	static var ctype: CType = switch(s) {
		case [c = ctype, "->", t = ctype_item]:
			switch(c) {
			case CTFun(a, r):
				a.push(r);
				CTFun(a, t);
			case _:
				CTFun([c], t);
			}
		case [t = ctype_item]: t;
	}
	static var ctype_item: CType = switch(s) {
		case ["(", c = ctype, ")"]:                       CTParent(c);
		case ["{", a = ctype_anon_1 , "}"]:               CTAnon(a);
		case ["{", a = ctype_anon_1 , Comma, "}"]:        CTAnon(a);
		case ["{", a = ctype_anon_2 , "}"]:               CTAnon(a);
		case ["(", a = ctype_args, ")", "->", r = ctype]: CTFun(a, r);
		case [f = s_field, "<", a = ctype_params, ">"]:   CTPath(f.split("."), a);
		case [f = s_field]:                               CTPath(f.split("."), null);
	}
	static var ctype_params: Array<CType> = switch(s) {
		case [a = ctype_params, ",", c = ctype]:          a.push(c); a;
		case [c = ctype]:                                 [c];
	}

	// {a:Int, b:Int}
	static var ctype_anon_1: Array<{name:String, t:CType, ?meta:Metadata}> = switch(s) {
		case [a1 = ctype_anon_1, ",", a2 = ctype_anon_1]: a1.push( a2[0] ); a1;
		case [m = metas, CIdent(i), ":", t = ctype]:      [{name:i, t:t, meta:m}];
		case [CIdent(i), ":", t = ctype]:                 [{name:i, t:t, meta:null}];
	}
	// {var a:Int; var b:Int;}
	static var ctype_anon_2: Array<{name:String, t:CType, ?meta:Metadata}> = switch(s) {
		case [a = ctype_anon_2, i = ctype_anon_2_item]:   a.push( i ); a;
		case [i = ctype_anon_2_item]:                     [i];
	}
	static var ctype_anon_2_item: {name:String, t:CType, ?meta:Metadata} = switch(s) {
		case [m = metas, "var", CIdent(i), ":", t = ctype, ";"]: {name:i, t:t, meta:m};
		case ["var", CIdent(i), ":", t = ctype, ";"]:            {name:i, t:t, meta:null};
	}
	// (Int, Int)->(x:Int, y:Int)-> Int
	static var ctype_args: Array<CType> = switch(s) {
		case [a = ctype_args, ",", c = ctype_args_item]:  a.push(c); a;
		case [c = ctype_args_item]:                       [c];
	}
	static var ctype_args_item: CType = switch(s) {
		case [CIdent(i), ":", c = ctype]:                 CTNamed(i, c);
		case ["?", CIdent(i), ":", c = ctype]:            CTNamed(i, CTOpt(c));
		case [c = ctype]:                                 c;
	}

	static var switch_block: {cases:Array<{values : Array<Expr>, expr : Expr}>, ?def: Expr} = switch (s) {
		case [b = switch_block, x = switch_case]:
			if (x.v.length == 0) {
				if (b.def != null)
					throw stream.UnExpected(T2);
				b.def = x.e;
			} else {
				b.cases.push({values: x.v, expr: x.e});
			}
			b;
		case [x = switch_case]:
			if (x.v.length == 0) {
				{cases: [], def: x.e};
			} else {
				{cases: [{values: x.v, expr: x.e}], def: null};
			}
	}
	static var switch_case: {v:Array<Expr>, e:Expr} = switch(s) {
		case ["case", v = a_expr, ":"]:           {v: v,  e: EBlock([])};
		case ["case", v = a_expr, ":", e = list]: {v: v,  e: EBlock(e)};
		case ["default", ":"]:                    {v: [], e: EBlock([])};
		case ["default", ":", e = list]:          {v: [], e: EBlock(e)};
	}
}
