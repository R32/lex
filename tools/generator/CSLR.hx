package tools.generator;

import lm.Charset;
import lm.ParserBase;
import lm.ParserEngine;
import haxe.Template;
import haxe.macro.Expr;
 using haxe.macro.ExprTools;

#if macro
private class SLRParser{}
#else
@:rule({
	start :[main, predef]
})
private class SLRParser implements lm.SLR<Lexer> {

	public var lexer : Lexer;

	var lhsctype : Null<ComplexType>;

	public function new( lex : Lexer ) {
		this.lexer = lex;
		this.stream = @:privateAccess new lm.Stream(lex);
	}

	var main : Array<RuleCaseGroupExtend> = switch(_) {
	case [a = lstsm, Eof]: a;
	default:
		var t = stream.peek(0);
		throw new Error("UnExpected: " + stream.str(t), mp(t, t));
	}

	var lstsm = switch(_) {
	case [a = lstsm, b = stsmlet]:
		a.push(b);
		a;
	case [b = stsmlet] :
		[b];
	}

	var stsmlet : RuleCaseGroupExtend = switch(_) {
	case [KLet, CIdent(i), "=", KFun, t = ctype, c = cases]:
		c.name = i;
		c.ctype = t ?? this.lhsctype;
		c;
	}

	var cases : RuleCaseGroupExtend = switch(_) {
	case [c = cases, r = one]:
		if (is_default(r.pattern)) {
			if (c.unmatch != null)
				throw new Error("Duplicated null matching: _", c.unmatch.pattern.pos);
			c.unmatch = r;
		} else {
			c.rules.push(r);
		}
		c;
	case [r = one]:
		var pos = mp(T1, T1);
		if (is_default(r.pattern)) {
			{ rules : [ ], unmatch : r   , pos : pos, name : "", ctype : null };
		} else {
			{ rules : [r], unmatch : null, pos : pos, name : "", ctype : null };
		}
	}

	// TODO:  [p1] | [p2...]... =>
	var one : RuleCase = switch(_) {
	case [OpOr, CIdent(i), OpArrow]:
		var e = mk_id(i, mp(T2, T2));
		if (i != "_")
			ExprHelps.UnExpected(e);
		stream.junk(stream.rest);
		{ pattern : e, action : this.lexer.copyAction(T3.pmax) };
	case [OpOr, m = prec, "[", p = patterns, "]", OpArrow]:
		stream.junk(stream.rest);
		var p = m == null ? p : mk(EMeta(m, p), mp(T2, T5));
		{ pattern : p, action : this.lexer.copyAction(T6.pmax) };
	}

	var prec : MetadataEntry = switch (_) {
	case [DPrec, "(", CIdent(i) , ")"]:
		{ name : ":prec", params: [mk_id(i, mp(T3, T3))], pos : mp(T1, T1) };
	case []:
		null;
	}

	var patterns : Expr = switch(_) {
	case [p = patterns, ",", e = item]:
		switch(p.expr) {
		case EArrayDecl(a):
			a.push(e);
			mk(EArrayDecl(a), mp(T1, T3));
		default:
			throw new Error("Werid", mp(T1, T1));
		}
	case [e = item]:
		mk(EArrayDecl([e]), mp(T1, T1));
	case []:
		var i = stream.peek(0).pmin;
		mk(EArrayDecl([]), this.lexer.mkpos(i, i));
	}

	var item = switch (_) {
	case [CIdent(i), "=", "[", a = lstrid , "]"]: // op = [a, b, c]
		var name = mk_id(i, mp(T1, T1));
		var array = mk(EArrayDecl(a), mp(T3, T5));
		mk(EBinop(OpAssign, name, array), mp(T1, T5));

	case [CIdent(i), "=", CIdent(e)]:             // e = expr
		var name = mk_id(i, mp(T1, T1));
		var expr = mk_id(e, mp(T3, T3));
		mk(EBinop(OpAssign, name, expr), mp(T1, T3));

	case [CIdent(i), "(", CIdent(e), ")"]:        // CInt(n)
		var name = mk_id(i, mp(T1, T1));
		var expr = mk_id(e, mp(T3, T3));
		mk(ECall(name, [expr]), mp(T1, T4));

	case [CIdent(i)]:
		mk_id(i, mp(T1, T1));

	case [CString(s)]:
		mk_str(s, mp(T1, T1));
	}

	// struct type **, unsigned int **
	var ctype : ComplexType = switch (_) {
	case [":", t = multiid, s = star]:
		TPath({name : s != "" ? t + " " + s : t, pack : []});
	case []:
		null;
	}

	var multiid : String = switch(_) {
	case [t = multiid, CIdent(i)] : t + " " + i;
	case [CIdent(i)] : i;
	}

	var star : String = switch(_) {
	case [s = star, "*"] : s + "*";
	case ["*"] : "*";
	case [] : "";
	}

	//// pre defines

	var predef : Expr = switch(_) {
	case [e = pre_block]: e;
	}

	var pre_block = switch(_) {
	case [e1 = pre_block, e2 = pre_item]:
		switch(e1.expr) {
		case EBlock(a):
			a.push(e2);
			mk(EBlock(a), mp(T1, T2));
		default:
			mk(EBlock([e1, e2]), mp(T1, T2));
		}
	case [e = pre_item]:
		e;
	}

	var pre_item = switch(_) {
	case [DName, a = idargs]:
		mk_call("name"    , mp(T1, T1), a, mp(T1, T2));
	case [DStart, a = idargs]:
		mk_call("start"   , mp(T1, T1), a, mp(T1, T2));
	case [DLeft, a = idargs]:
		mk_call("left"    , mp(T1, T1), a, mp(T1, T2));
	case [DRight, a = idargs]:
		mk_call("right"   , mp(T1, T1), a, mp(T1, T2));
	case [DNonAssoc, a = idargs]:
		mk_call("nonassoc", mp(T1, T1), a, mp(T1, T2));
	case [DFunc, "(", CIdent(i), ",", t = multiid, s = star, ",", CIdent(f), ")"]:
		var t = s == "" ? t : t + " " + s;
		var a = [
			mk_id(i, mp(T3, T3)),
			mk_id(t, mp(T5, T6)),
			mk_id(f, mp(T8, T8))
		];
		mk_call("func"    , mp(T1, T1), a, mp(T1, T9));
	case [DDef, "(", t = multiid, s = star , ")"]:
		var stype = s == "" ? t : t + " " + s;
		this.lhsctype = TPath({name : stype, pack : []});
		mk_call("def"     , mp(T1, T1), [mk_id(stype, mp(T3, T4))], mp(T1, T5));
	}

	var idargs : Array<Expr> = switch(_) {
	case ["(", a = lstrid , ")"] : a;
	}

	var lstrid : Array<Expr> = switch(_) {
	case [a = lstrid, ",", a2 = lstrid]:
		a.push(a2[0]);
		a;
	case [CString(s)]:
		[mk_str(s, mp(T1, T1))];
	case [CIdent(i)]:
		[mk_id(i, mp(T1, T1))];
	}

	inline function mp(t1, t2) return mk_pos(t1, t2);

	function mk_pos(t1, t2) {
		return lexer.mkpos(t1.pmin, t2.pmax);
	}

	function mk( def : ExprDef, pos : Position ) {
		return {expr : def, pos : pos }
	}

	function mk_id( s : String, pos ) {
		return {expr : EConst(CIdent(s)), pos : pos}
	}

	function mk_str( s : String, pos ) {
		return {expr : EConst(CString(s)), pos : pos }
	}

	function mk_call( name : String, namepos, args : Array<Expr>, pos) {
		var fn = {expr : EConst(CIdent(name)), pos : namepos}
		return {expr : ECall(fn, args), pos : pos};
	}

	function is_default(e) {
		return switch(e.expr) { // [_]
		case EConst(CIdent("_")): true;
		default: false;
		}
	}

	@:rule(CIdent) inline function id_of_string( s ) : String return s;
	@:rule(CInt) inline function int_of_string( s ) : Int return Std.parseInt(s);
	@:rule(CString) function str_of_pos( input, t ) {
		var s = this.lexer.parray.get(t.pmin + 1);
		if (s == null)
			throw new Error("CSLR str_of_pos TODO", mp(t, t));
		return s;
	}
}
#end

class CSLR {

	public var lexer : Lexer;
	public var engine : ParserEngine;
	public var base : ParserBase;
	public var tplparser : SLRParser;
	public var tplvar : TplVarsExt;

	public var path : haxe.io.Path;
	public var dargs : DArgs;

	public function new( mt : Template, args : DArgs ) {
		this.dargs = args;
		this.path = new haxe.io.Path(args.simpleLR);
		this.tplvar = new TplVarsExt(path.file);
		this.base = new ParserBase();
		try {
			this.make();
			this.generate(mt);
		} catch (err : Error) {
			fatalError(err.message,  err.pos);
		}
	}

	function make() {
		this.getTerms();
		this.getPrecedence();
		this.base.rule_groups = this.tplparser.main();
		this.initNonTerms();
		this.base.transform(false);
		this.addExtraCodes(); // add extra codes
		this.engine = new ParserEngine(this.base);
	}

	function generate( mt : Template ) {
		if (this.engine.entrys.length == 0)
			return;
		// tplvars
		tplvar.reduce_data = this.base.reduce_data.join(", ");
		tplvar.error_ifelse = buildErrorIfElse();
		tplvar.update(engine);

		var lhsides = base.lhsides;
		var first = engine.entrys[0].index;
		var unmatch = lhsides[first].edef;
		var sret = "_ret = (void *)(size_t)";
		if (unmatch == null) {
			tplvar.epsilon = slrUnMatching();
		} else {
			tplvar.epsilon = finalizeActoin(unmatch, sret);
		}
		var epsilons : Array<Case> = ExprHelps.lexUnMatchedActions(engine, this.base.rule_groups, first + 1);
		var index = 0;
		for (lhs in lhsides) {
			for (c in lhs.cases) {
				tplvar.cases.push({ index : index++, action : finalizeActoin(c.action, sret) });
			}
		}
		for (c in epsilons) {
			tplvar.cases.push({ index : index++, action : finalizeActoin(c.expr, sret) });
		}
		for (en in engine.entrys) {
			var lhs = lhsides[en.index];
			tplvar.entrys.push({
				name : lhs.name,
				uname : lhs.name.toUpperCase(),
				begin : en.begin,
				value : lhs.value,
				stype : typecast(typedecl(lhs.ctype)),
			});
		}
		// template
		var text = mt.execute(tplvar);
		// path
		var fullpath = dargs.normalizePath(this.path);
		var out = sys.io.File.write(fullpath);
		out.writeString(tokenPreDefines(tplvar.utf8));
		out.writeString(tplvar.enum_tokens);
		out.writeString(lexer.header);
		out.writeString(text);
		out.writeString(lexer.footer);
		out.close();
		Sys.print("> " + fullpath + "\n");

		if (string_has(dargs.get("slrtable")))
			slrTablePrint(this.engine, this.base);
	}

	function buildErrorIfElse() {
		if (this.engine.entrys.length <= 1)
			return "";
		var ifelse = "{ }";
		var exit = this.engine.nrules;
		for (i in 1...engine.entrys.length) {
			var en = engine.entrys[i];
			ifelse =
			'if (state >= ${en.begin}) {
				q = ${exit};
			} else ' + ifelse;
			exit++;
		}
		return ifelse;
	}

	function addExtraCodes() {
		var extra : Array<Expr>;
		var pos : Position;
		inline function ADD( str : String ) {
			extra.push({expr : EConst(CString(str)), pos : pos});
		}
		var lhsides = this.base.lhsides;
		for (lhs in lhsides) {
			for (stsets in lhs.cases) {
				var len = stsets.sets.length;
				extra = [];
				for (i in 0...len) {
					var dx = -(len - i);
					var stoken = stsets.sets[i];
					var vname = stoken.extract;
					pos = stoken.pos;
					var CURRENT = "T" + (i + 1);
					ADD('const struct rstream_tok *$CURRENT = stream_offset($dx);');
					if (vname == null || vname == "_")
						continue;
					// E
					if (stoken.t == false) {
						var value = stoken.cset[0].min;
						var tdecl = typedecl(lhsides[value - this.base.max_term_value].ctype);
						var tcast = typecast(tdecl);
						ADD('const $tdecl $vname = $tcast $CURRENT->value;');
						continue;
					}
					// term
					var tkname = stoken.name;
					var func = this.base.funmap.get(tkname);
					if (func == null) {
						if (tkname != "*" && CSet.isSingle(stoken.cset))
							throw new Error("Required a %FUNC(" + tkname + ", type, function)", pos);
						var tdecl = typedecl(this.base.terms_ct);
						ADD('const $tdecl $vname = $CURRENT->term;');
						continue;
					}
					var fname = func.name;
					var tdecl = typedecl(func.ct);
					ADD('const $tdecl $vname = $fname(stream->lex->src, $CURRENT);');
				}
				if (extra.length == 0)
					continue;
				switch(stsets.action.expr) {
				case EBlock(a):
					for (e in a)
						extra.push(e);
				default:
					extra.push(stsets.action);
				}
				stsets.action = {
					expr : EBlock(extra),
					pos : stsets.action.pos,
				}
			}
		}
	}
	// override ParserBase.addNonTerm
	function initNonTerms() {
		var groups = this.base.rule_groups;
		if (groups.length == 0)
			return;
		if (this.base.starts.length == 0)
			this.base.starts.push(groups[0].name);
		var acc = base.max_term_value;
		for (g in groups) {
			var name = g.name;
			var term = {t : false, name : name, value : acc, cset : CSet.single(acc), pos : g.pos}
			this.base.non_terms_map.set(name, term);

			var entry = this.base.starts.indexOf(name) >= 0;
			var edef = g.unmatch?.action;
			if (!entry && edef != null) {
				g.rules.push({pattern : g.unmatch.pattern, action : edef});
				edef = null;
				g.unmatch = null;
			}

			this.base.nrules += g.rules.length;
			// lhs
			this.base.lhsides.push({
				name : name,
				ctype : g.ctype,
				cases : [],
				edef : edef,
				value : acc,
				lsubs : [],
				pos : g.pos,
				entry : entry,
			});
			acc++;
		}
	}

	function getTerms() {
		var json : TokenJson = CLexer.tokenjsons.get(dargs.lexeme);
		if (json == null)
			fatal("Missing data provided by Lexer.");
		// update this.base
		this.base.eof = json.eof;
		this.tplvar.eof = json.eof;
		for (k => v in json.terms) {
			this.base.addTerm(k, v.value, {file : json.file, min : v.pmin, max : v.pmax});
		}
		this.base.reflect = new Map();
		for (k => v in json.reflect) {
			this.base.reflect.set(k, v);
		}
		this.base.terms_ct = TPath({name : json.stype, pack : []});
		this.tplvar.utf8 = json.utf8;

		if (json.file == dargs.lexeme) {
			this.tplvar.initTerms(json.stype, json.terms);
		}
	}

	function getPrecedence() {
		this.lexer = new Lexer(this.dargs.simpleLR);
		this.tplparser = new SLRParser(lexer);
		this.lexer.skipBegin();
		var predefine = this.tplparser.predef();
		var starts = [];
		var precs = [];
		function metaloop( e : Expr ) {
			switch (e.expr) {
			case EBlock(a):
				for (e in a)
					metaloop(e);
			case ECall({expr : EConst(CIdent(name)), pos : pos}, args):
				switch (name) {
				case "left":
					precs.push({type : Left    , sets : args});
				case "right":
					precs.push({type : Right   , sets : args});
				case "nonassoc":
					precs.push({type : Nonassoc, sets : args});
				case "start":
					starts = starts.concat(args);
				case "name":
					this.tplvar.prefix = ExprHelps.expectCIdent(args[0]);
				case "func":
					var key = ExprHelps.expectCIdent(args[0]);
					var sct = ExprHelps.expectCIdent(args[1]);
					this.base.funmap.set(key, {
						name : ExprHelps.expectCIdent(args[2]),
						ct : TPath({name : sct, pack : []}),
						args : 2,
						pos : e.pos,
					});
				case "def":
					// already processed in SLRParser
				default:
					throw new Error("UnKnown: " + name, pos);
				}
			default:
				ExprHelps.UnExpected(e);
			}
		}
		metaloop(predefine);
		this.base.starts = starts.map(ExprHelps.stringOrId);
		this.base.initPrecMap(precs);
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

class TplVarsExt extends TplVars {

	public var reduce_data : String;

	public var error_ifelse : String;

	public var enum_tokens : String;

	public function new (name) {
		super(name);
		this.enum_tokens = "";
	}

	public function initTerms( name : String, map : haxe.DynamicAccess<{value : Int, pmin : Int, pmax : Int}> ) {
		var CRLF = "\r\n";
		var terms : Array<{key : String, value : Int}> = [];
		for (k => v in map) {
			terms.push({key : k, value : v.value});
		}
		terms.sort( (a, b)-> a.value - b.value );
		var buffer = new StringBuf();
		buffer.add(CRLF + name + " {" + CRLF);
		for (t in terms) {
			buffer.add("\t" + t.key + " = " + t.value + "," + CRLF);
		}
		buffer.add("};" + CRLF + CRLF);
		this.enum_tokens = buffer.toString();
	}
}
