package tools.generator;

import lm.LineColumn;
import haxe.macro.Expr;
 using haxe.macro.ExprTools;

private enum TPExprDef {
	EEof( id : String );
	ESrc( id : String );
	EMax( i : Int );
	EName( name : String );
	EAssign( name : String, value : Expr );
	ERuleSet( r : RuleCaseGroup );
	EToken( name : String, ?file : String);
}

private typedef TPExpr = {
	texpr : TPExprDef,
	pos : Position,
}

#if macro
private class LexParser{}
#else
@:rule({
	start : [begin, cenum],
	left  : ["|"],
	left  : ["+"],
}) private class LexParser implements lm.SLR<Lexer> {

	public var lexer : Lexer;

	function mp( t1 : lm.Lexer.Position, t2 : lm.Lexer.Position ) {
		return lexer.mkpos(t1.pmin, t2.pmax);
	}

	function mk( def : ExprDef, t1, t2 ) {
		return {expr : def, pos : mp(t1, t2)};
	}

	function mkc( c : Constant, t1, t2 ) {
		return {expr : EConst(c), pos : mp(t1, t2)};
	}

	function mk_tpexpr( def : TPExprDef, t1, t2 ) {
		return {texpr : def, pos : mp(t1, t2)};
	}

	public function new( lex : Lexer ) {
		this.lexer = lex;
		this.stream = @:privateAccess new lm.Stream(lex);
	}

	// start
	var begin : Array<TPExpr> = switch(s) {
	case [a = list, Eof] :
		a;
	default:
		var t = stream.peek(0);
		if (t.term == Eof)
			return [];
		throw new Error('Unexpected "' + stream.str(t) + '"', mp(t, t));
	}

	var list : Array<TPExpr> = switch(s) {
	case [a = list, e = tpexpr]                         : a.push(e); a;
	case [e = tpexpr]                                   : [e];
	}

	var tpexpr : TPExpr = switch(s) {
	case [DName, "(", CIdent(n), ")"]                   : mk_tpexpr(EName (n   ), T1, T4);
	case [DToken, "(", CIdent(n), ")"]                  : mk_tpexpr(EToken(n   ), T1, T4);
	case [DToken, "(", CIdent(n), ",", CString(f), ")"] : mk_tpexpr(EToken(n, f), T1, T5);
	case [DEoF, "(", CIdent(c), ")"]                    : mk_tpexpr(EEof(c), T1, T4);
	case [DSrc, "(", CIdent(c), ")"]                    : mk_tpexpr(ESrc(c), T1, T4);
	case [DMax, "(", CInt(i), ")"]                      : mk_tpexpr(EMax(i), T1, T4);
	case [KLet, CIdent(c), "=", e = expr]               : mk_tpexpr(EAssign(c, e), T1, T4);
	case [KLet, CIdent(c), "=", KFun, r = cases]        : r.name = c; mk_tpexpr(ERuleSet(r), T1, T5);
	}

	var expr : Expr = switch(s) {
	case [CIdent(name), "(", e = expr , ")"] :
		switch (name) {
		case "Opt", "Plus", "Star":
		default:
			throw new Error("Duplicated null matching: _", mp(T1, T1));
		}
		var fname = mkc(CIdent(name), T1, T1);
		mk(ECall(fname, [e]), T1, T4);

	case [e1 = expr, "+", e2 = expr] :
		mk(EBinop(OpAdd, e1, e2), T1, T3);

	case [e1 = expr, "|", e2 = expr] :
		mk(EBinop(OpOr, e1, e2), T1, T3);

	case ["(", e = expr , ")"]:
		mk(EParenthesis(e), T1, T3);

	case [CString(s)] :
		mkc(CString(s), T1, T1);

	case [CIdent(i)] :
		mkc(CIdent(i), T1, T1);
	}

	var cases : RuleCaseGroup = switch(s) {
	case [c = cases, r = line]:
		if (is_nullcase(r.pattern)) {
			if (c.unmatch != null)
				throw new Error("Duplicated null matching: _", c.unmatch.pattern.pos);
			c.unmatch = r;
		} else {
			c.rules.push(r);
		}
		c;
	case [r = line]:
		if (is_nullcase(r.pattern)) {
			{ name : "", rules : [ ], unmatch : r }
		} else {
			{ name : "", rules : [r], unmatch : null }
		}
	}

	var line : RuleCase = switch(s) {
	case [OpOr, pat = expr, OpArrow]:
		stream.junk(stream.rest);
		{ pattern : pat, action : this.lexer.copyAction(T3.pmax) }
	}

	//// parse c enum
	var cenum : Expr = switch(s) {
	case ["{", e = enum_item, "}"]:
		e;
	default:
		var t = stream.peek(0);
		throw new Error("UnExpected: " + stream.str(t) , mp(t, t));
	}

	var enum_item : Expr = switch(s) {
	case [e1 = enum_item, ",", e2 = enum_item]:
		switch(e1.expr) {
		case EBlock(a):
			a.push(e2);
			mk(EBlock(a), T1, T3);
		case _:
			mk(EBlock([e1, e2]), T1, T3);
		}
	case [e = enum_item, ","]:
		e;
	case [CIdent(id) , "=", CInt(n)]:
		var num = mk(EConst(CInt("" + n)), T3, T3);
		mk(EMeta({"name" : id, pos : mp(T1, T1)}, num), T1, T3);
	case [CIdent(id)]:
		mk(EMeta({"name" : id, pos : mp(T1, T1)}, null), T1, T1);
	}

	// custom extract function
	@:rule(CInt) inline function __s1( s : String ) return Std.parseInt(s);
	@:rule(CIdent) inline function __s2( s : String ) return s;
	@:rule(CString) function __s3( input, t ) {
		var s = this.lexer.parray.get(t.pmin + 1);
		if (s == null)
			throw new Error("CLexer __s3() TODO", mp(t, t));
		return s;
	}

	public function is_nullcase(e) {
		return switch(e.expr) {
		case EConst(CIdent("_")) : true;
		default: false;
		}
	}
}
#end

/*
 * build lex for c language
 */
class CLexer {

	static public var tokenjsons = new Map<String, TokenJson>(); // used for CSLR

	public var dargs : DArgs;
	public var lexer : Lexer;
	public var parser : LexParser;
	public var tplvar : TplVars;
	public var path : haxe.io.Path;

	public function new( mt : Template, args : DArgs ) {
		this.dargs = args;

		this.path = new haxe.io.Path(args.lexeme);

		this.tplvar = new TplVars(this.path.file);

		this.lexer = new Lexer(args.lexeme);
		this.parser = new LexParser(this.lexer);
		try {
			build(mt);
		} catch ( e : Error ) {
			fatalError(e.message, e.pos);
		}
	}

	function build( mt : Template ) {
		this.lexer.skipBegin();
		var pstart = lexer.pmax;
		var groups = [];
		var varmap = new Map<String, Expr>();
		// parse file
		var cenums :  {name : String, ?file : String} = null;
		var aexpr = this.parser.begin();
		for (e in aexpr) {
			switch(e.texpr) {
			case EEof(id):
				tplvar.eof = id;
			case ESrc(id):
				tplvar.utf8 = id == "UTF8";
			case EMax(i):
				tplvar.stride = (i | 0x7f) + 1; // 128|256
			case EToken(name, file):
				cenums = {name : name, file : file};
			case EAssign(name, value):
				varmap.set(name, value);
			case ERuleSet(r):
				groups.push(r);
			case EName(name):
				tplvar.prefix = name;
			}
		}
		if (tplvar.eof == null)
			throw new Error("%EOF() is required.", lexer.mkpos(pstart, pstart));
		if (groups.length == 0)
			return;
		generateToken(cenums, groups); // for CSLR
		var paterns = toPattens(groups, varmap);
		var lexe = new lm.LexEngine(paterns, tplvar.stride - 1);

		// checking
		ExprHelps.lexChecking(lexe, groups);

		// update data to tplvar
		var index = 0;
		for (g in groups) {
			for (r in g.rules) {
				tplvar.cases.push({ index : index++, action : finalizeActoin(r.action)});
			}
		}
		if (groups[0].unmatch != null) {
			tplvar.epsilon = finalizeActoin(groups[0].unmatch.action);
		} else {
			tplvar.epsilon = lexUnMatching();
		}
		// get extra case and update tplvar.cases
		var extra = ExprHelps.lexUnMatchedActions(lexe, groups);
		for (c in extra) {
			tplvar.cases.push({ index : index++, action : finalizeActoin(c.expr) });
		}
		tplvar.update(lexe);
		for (i in 0...lexe.entrys.length) {
			var name = groups[i].name;
			tplvar.entrys.push({name : name, uname : name.toUpperCase(), begin : lexe.entrys[i].begin});
		}

		// write
		var text = mt.execute(tplvar);
		// path
		var fullpath = dargs.normalizePath(this.path);
		var out = sys.io.File.write(fullpath);
		out.writeString(tokenPreDefines(tplvar.utf8));
		out.writeString(lexer.header);
		out.writeString(text);
		out.writeString(lexer.footer);
		out.close();
		Sys.print("> " + fullpath + "\n");
	}

	function toPattens( groups : Array<RuleCaseGroup>, varmap : Map<String, Expr> ) {
		var cset = [new lm.Charset.Char(0, tplvar.stride - 1)];
		var ret = [];
		var index = 0;
		for (g in groups) {
			var a = [];
			for (r in g.rules) {
				if (parser.is_nullcase(r.pattern))
					throw new Error("TODO", r.pattern.pos);
				a.push(ExprHelps.parsePaterns(r.pattern, varmap, cset));
			}
			ret.push(a);
		}
		return ret;
	}

	function generateToken( cenum : {name : String, ?file : String}, groups : Array<RuleCaseGroup> ) {
		if (cenum == null)
			return;
		var prevstate : {lines : LineCounter, parray : lm.PosArray} = null;
		var lex = this.lexer;
		var par = this.parser;

		if (string_has(cenum.file)) {
			lex = new Lexer(cenum.file);
			par = new LexParser(lex);
		} else {
			// save states to reuse current file
			prevstate = {
				lines  : this.lexer.lines,
				parray : this.lexer.parray,
			}
			lex.lines = new LineCounter(lex.lines.owner);
			lex.parray = new lm.PosArray();
		}
		var terms = new haxe.DynamicAccess<{value : Int, pmin : Int, pmax : Int}>();
		var i = 0;
		var source = lex.input;
		var len = source.length;
		inline function char(i) return source.readByte(i);
		while (i < len) {
			var c = char(i++);
			if (c == "e".code && char(i) == "n".code // "enum"
			&& char(i + 1) == "u".code && char(i + 2) == "m".code) {
				@:privateAccess lex.pmax = i + 3;
				var t = lex.token();
				if (!(t == CIdent && lex.current == cenum.name))
					continue;
				var expr = par.cenum();
				readCEnumToken(expr, terms);
				break;
			} else if (c == "\n".code) {
				lex.lines.add(i);
			}
		}
		// reflect
		function loop(e) {
			switch (e.expr) {
			case EBlock(a):
				for (e in a)
					loop(e);
			case EMeta({name : name}, e):
			default:
			}
		}
		var reflect = new haxe.DynamicAccess<String>();
		for (g in groups) {
			for (r in g.rules) {
				switch(r.action.expr) {
				case EBlock(a) if (a.length > 0):
					var stsm = ExprHelps.expectCString(a[a.length - 1]);
					if (!terms.exists(stsm))
						continue;
					switch (r.pattern.expr) {
					case EConst(CString(s)):
						var spat = lm.Utils.unescape(s);
						reflect.set(spat, stsm);
					default:
					}
				default:
				}
			}
		}
		reflect.remove('"');
		reflect.remove("'");
		var json : TokenJson = {
			file : lex.lines.owner,
			stype : "enum " + cenum.name,
			terms : terms,
			reflect : reflect,
			eof : this.tplvar.eof,
			utf8 : this.tplvar.utf8, // the input(source) format
		}

		// TODO: save to file?
		tokenjsons.set(dargs.lexeme, json);

		// restore
		if (prevstate != null) {
			this.lexer.lines = prevstate.lines;
			this.lexer.parray = prevstate.parray;
		}
	}

	function readCEnumToken( e : Expr, out : haxe.DynamicAccess<{value : Int, pmin : Int, pmax : Int}> ) {
		var terms = new Map<String, Int>();
		var acc = 0;
		function loop(e) {
			if (e == null)
				return;
			switch(e.expr) {
			case EBlock(a):
				for (e in a)
					loop(e);
			case EMeta({name : name}, v):
				var value = acc++;
				if (v != null) {
					value = ExprHelps.expectCInt(v);
					acc = value + 1;
				}
				out.set(name, {value : value, pmin : e.pos.min, pmax : e.pos.max});
			default:
			}
		}
		loop(e);
	}

	function fatalError( msg : String, pos : Position ) : Dynamic {
		var lfcount = lexer.lines;
		var lmin = lfcount.get(pos.min);
		var spos = '${lmin.line}: characters ${lmin.column}-${pos.max - lmin.base + 1}';
		Sys.println(lfcount.owner + ":" + spos + " : " + msg);
		Sys.exit( -1);
		return null;
	}
}
