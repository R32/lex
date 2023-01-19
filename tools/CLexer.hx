package tools;

import lm.LineColumn;
import lm.ExprHelps;
import haxe.Template;
import haxe.macro.Expr;
 using haxe.macro.ExprTools;

private enum abstract Token(Int) to Int {
	var Eof = 0;
	var Exit;
	var ActRollback;
	var CString;
	var CInt;
	var CIdent;
	var DEoF;      // %EOF
	var DSrc;      // %SRC
	var DMax;      // %MAX
	var KLet;      // let
	var KFun;      // function
	var OpOr;      // "|"
	var OpAdd;     // "+"
	var OpAssign;  // "="
	var OpArrow;   // "->"
	var LParen;    // "("
	var RParen;    // ")"
}

private enum TPExprDef {
	EEof( id : String );
	ESrc( id : String );
	EMax( i : Int );
	EAssign( name : String, value : Expr );
	ERuleSet( r : RuleCaseGroup );
}

private typedef TPExpr = {
	texpr : TPExprDef,
	pos : Position,
}

@:rule(Eof, 127) private class Lexer implements lm.Lexer<Token> {

	public var header : String;

	public var footer : String;

	public var lines : LineCounter;    // LF counter

	public var parray : lm.PosArray;   // Map<pos,string>

	public var ccodes : Array<String>; // c language codes

	var sbuf : StringBuf;

	public function new( file : String ) {
		var text = sys.io.File.getContent(file);
		this.input = lms.ByteData.ofString(text);
		this.parray = new lm.PosArray();
		this.lines = new LineCounter(file);
		this.pmin = 0;
		this.pmax = 0;
		footer = "";
	}

	inline function char(i) return this.input.readByte(i);

	public function skipBegin() {
		var i = 0;
		var max = input.length;
		while (i < max) {
			var c = char(i++);
			if (c == "\n".code) {
				this.lines.add(i);
			} else if (c == "%".code && char(i) == "%".code) {
				this.pmax = i - 1;
				this.header = this.current;
				this.pmax = i + 1;
				break;
			}
		}
	}

	public function copyAction( i : Int ) {
		this.pmax = i;
		this.ccodes = [];
		this.sbuf = new StringBuf();
		var t = this.clblock();
		if (t == ActRollback) {
			this.pmax = this.pmin;
		}
		this.pmin = i;
		// add "ret = ..." to
		var LN = "\n";
		var TAB = "\t\t";
		var i = ccodes.length - 1;
		if (i < 0)
			return "";
		ccodes[i] = "_ret = " + ccodes[i] + ";";
		return ccodes.join(LN + TAB);
	}

	public function mkpos(min, max) {
		return {min : min, max : max, file : lines.owner};
	}

	static var ident = "[a-zA-Z_][a-zA-Z0-9_]*";

	static var integer = "0|[1-9][0-9]*";

	static var CRLF = "\r?\n";

	static var token = [
		"%%" => {
			lex.footer = lex.getString(pmax, input.length - pmax);
			Eof;
		},

		CRLF          => { lines.add(pmax); lex.token(); },
		"[ \t]+"      => lex.token(),
		"//[^\n]*"    => lex.token(), // skip CommentLine

		"%EOF"        => DEoF,
		"%SRC"        => DSrc,
		"%MAX"        => DMax,
		"let"         => KLet,
		"function"    => KFun,
		"="           => OpAssign,
		"->"          => OpArrow,
		"|"           => OpOr,
		"+"           => OpAdd,
		"("           => LParen,
		")"           => RParen,
		ident         => CIdent,
		integer       => CInt,

		'"' => {
			var min = lex.pmin;
			sbuf = new StringBuf();
			var t = lex.str();
			if (t == Eof)
				throw new Error("Unclosed " + "string", mkpos(min, lex.pmax));
			lex.pmin = min; // pos union
			parray.add(min + 1, sbuf.toString());
			t;
		},
		"'" => {
			var min = lex.pmin;
			sbuf = new StringBuf();
			var t = lex.qstr();
			if (t == Eof)
				throw new Error("Unclosed " + "string", mkpos(min, lex.pmax));
			lex.pmin = min;
			parray.add(min + 1, sbuf.toString());
			t;
		},
		"/\\*" => {
			lex.commentblock();
			lex.token();
		},
		_ => {
			var s = "UnMatached: '" + lex.getString(pmax, pmin - pmax) + "'";
			throw new Error(s, mkpos(pmax, pmin));
		}
	];

	static var str = [
		'"'          => CString,
		'\\\\"'      => { // LexEngine will parse "\\\\" as a '\'
			sbuf.addChar('"'.code);
			lex.str();
		},
		"\\\\n"      => {
			sbuf.addChar("\n".code);
			lex.str();
		},
		"\\\\t"      => {
			sbuf.addChar("\t".code);
			lex.str();
		},
		"\\\\r"      => {
			sbuf.addChar("\r".code);
			lex.str();
		},
		"\\\\0"      => {
			sbuf.addChar(0);
			lex.str();
		},
		"\\\\[xX][0-9a-fA-F][0-9a-fA-F]" => {
			sbuf.addChar(Std.parseInt("0x" + input.readString(pmin + 2, pmax - pmin - 2)));
			lex.str();
		},
		"\\\\\\\\"   => {
			sbuf.addChar("\\".code);
			sbuf.addChar("\\".code);
			lex.str();
		},
		CRLF         => {
			lines.add(pmax);
			throw new Error("Unexpected: " + "\\n", mkpos(pmin, pmax));
		},
		'[^"\r\n\\\\]+'  => {
			sbuf.add(lex.current);
			lex.str();
		}
	];
	static var qstr = [
		"'"          => CString,
		"\\\\'"      => {
			sbuf.addChar("'".code);
			lex.qstr();
		},
		"\\\\n"      => {
			sbuf.addChar("\n".code);
			lex.qstr();
		},
		"\\\\t"      => {
			sbuf.addChar("\t".code);
			lex.qstr();
		},
		"\\\\r"      => {
			sbuf.addChar("\r".code);
			lex.qstr();
		},
		"\\\\0"      => {
			sbuf.addChar(0);
			lex.qstr();
		},
		"\\\\[xX][0-9a-fA-F][0-9a-fA-F]" => {
			sbuf.addChar(Std.parseInt("0x" + input.readString(pmin + 2, pmax - pmin - 2)));
			lex.qstr();
		},
		"\\\\\\\\"   => {
			sbuf.addChar("\\".code);
			sbuf.addChar("\\".code);
			lex.qstr();
		},
		CRLF         => {
			lines.add(pmax);
			throw new Error("Unexpected: " + "\\n", mkpos(pmin, pmax));
		},
		"[^'\r\n\\\\]+"  => {
			sbuf.add(lex.current);
			lex.qstr();
		},
	];

	static var commentblock = [
		"*/" => {
			Exit;
		},
		CRLF => {
			lines.add(pmax);
			lex.commentblock();
		},
		"*" => {
			lex.commentblock();
		},
		"[^*\n]+" => {
			lex.commentblock();
		}
	];

	// HACK
	static var clblock = [
		"let|%%" => {
			ActRollback; // Reserve for next
		},
		CRLF => {
			if (sbuf.length > 0) {
				ccodes.push(sbuf.toString());
				sbuf = new StringBuf();
			}
			lines.add(pmax);
			lex.trimbeginning();
			char(pmax) == "|".code ? Exit : lex.clblock();
		},
		"/\\*" => {
			lex.commentblock();
			clblock();
		},
		";" => {
			sbuf.addChar(";".code);
			lex.trimbeginning();
			lex.clblock();
		},
		"//[^\n]*" => {
			lex.clblock();
		},
		"[ \t]+" => {
			if (sbuf.length > 0)
				sbuf.addChar(" ".code);
			lex.clblock();
		},
		"[^ ;\r\t\n]+" => {
			sbuf.add(lex.current);
			lex.clblock();
		}
	];

	static var trimbeginning = [
		"[ \t]*" => Exit
	];
}

@:rule({
	left: ["|"],
	left: ["+"],
}) private class Parser implements lm.LR0<Lexer, Array<TPExpr>> {

	public var lexer : Lexer;

	function mkpos( min : Int, max : Int ) {
		return lexer.mkpos(min, max);
	}

	function mk_expr( def : ExprDef, min, max ) {
		return {expr : def, pos : mkpos(min, max)};
	}

	function mk_tpexpr( def : TPExprDef, min, max ) {
		return {texpr : def, pos : mkpos(min, max)};
	}

	public function new( lex : Lexer ) {
		this.lexer = lex;
		this.stream = @:privateAccess new lm.Stream<Dynamic>(lex);
	}

	// custom
	var __default__ = @:privateAccess {
		var t = stream.offset( -1);
		throw new Error('Unexpected "' + (t.term != Eof ? stream.str(t): "Eof") + '"', mkpos(t.pmin, t.pmax));
	}

	var begin = switch(s) {
	case [a = list, Eof]                         : a;
	case [Eof]                                   : [];
	}

	var list = switch(s) {
	case [a = list, e = tpexpr]                  : a.push(e); a;
	case [e = tpexpr]                            : [e];
	}

	var tpexpr : TPExpr = switch(s) {
	case [DEoF, "(", CIdent(c), ")"]             : mk_tpexpr(EEof(c), _t1.pmin, _t4.pmax);
	case [DSrc, "(", CIdent(c), ")"]             : mk_tpexpr(ESrc(c), _t1.pmin, _t4.pmax);
	case [DMax, "(", CInt(i), ")"]               : mk_tpexpr(EMax(i), _t1.pmin, _t4.pmax);
	case [KLet, CIdent(c), "=", e = expr]        : mk_tpexpr(EAssign(c, e), _t1.pmin, _t4.pmax);
	case [KLet, CIdent(c), "=", KFun, r = cases] : r.name = c; mk_tpexpr(ERuleSet(r), _t1.pmin, _t5.pmax);
	}

	var expr : Expr = switch(s) {
	case [CIdent(name), "(", e = expr , ")"] :
		switch (name) {
		case "Opt", "Plus", "Star":
		default:
			throw new Error("Duplicated null matching: _", mkpos(_t1.pmin, _t1.pmax));
		}
		var fname = mk_expr(EConst(CIdent(name)), _t1.pmin, _t1.pmax);
		mk_expr(ECall(fname, [e]), _t1.pmin, _t4.pmax);

	case [e1 = expr, "+", e2 = expr] :
		mk_expr(EBinop(OpAdd, e1, e2), _t1.pmin, _t3.pmax);

	case [e1 = expr, "|", e2 = expr] :
		mk_expr(EBinop(OpOr, e1, e2), _t1.pmin, _t3.pmax);

	case ["(", e = expr , ")"]:
		mk_expr(EParenthesis(e), _t1.pmin, _t3.pmax);

	case [CString(s)] :
		mk_expr(EConst(CString(s)), _t1.pmin, _t1.pmax);

	case [CIdent(i)] :
		mk_expr(EConst(CIdent(i)), _t1.pmin, _t1.pmax);
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
		var pmax = _t3.pmax;
		var act = this.lexer.copyAction(pmax);
		{ pattern : pat, action : {expr : EConst(CString(act)), pos : mkpos(_t1.pmin, _t3.pmin)} }
	}
	// custom extract function
	@:rule(CInt) inline function __s1( s : String ) return Std.parseInt(s);
	@:rule(CIdent) inline function __s2( s : String ) return s;
	@:rule(CString) function __s3( input, t ) {
		var s = this.lexer.parray.get(t.pmin + 1);
		if (s == null)
			throw new Error("CLexer __s3() TODO", mkpos(t.pmin, t.pmax));
		return s;
	}

	public function is_nullcase(e) {
		return switch(e.expr) {
		case EConst(CIdent("_")) : true;
		default: false;
		}
	}
}


private class Config {

	public var per     : Int;    // stride
	public var eof     : String;
	public var bit16   : Bool;   // the table format if nstates > 254
	public var utf8    : Bool;   // the input format
	public var nsegs   : Int;    // available states
	public var nrules  : Int;    // number of rules
	public var invalid : Int;    // invalid number, 0xFF for bytes
	public var cases   : Array<{ index : Int, action : String }>;
	public var epsilon : String; // case _: actions
	public var table   : String;
	public var tabsize : Int;
	public var entrys  : Array<{name : String, begin: Int}>;
	public var entrybegin : Int; // entrys[0].begin
	public var path    : haxe.io.Path;
	public var custom  : haxe.DynamicAccess<String>;

	public function new( file : String, cfg : haxe.DynamicAccess<String> ) {
		per = 128;
		eof = null;
		utf8 = true;
		cases = [];
		table = "";
		epsilon = "";
		entrys = [];
		custom = new haxe.DynamicAccess();
		path = new haxe.io.Path(file);
		path.ext = "c"; // change

		if (cfg == null)
			return;
		for (k => value in cfg)
			custom.set(k, value);
		var outdir = custom.get("outdir");
		if (outdir != null && outdir != "")
			path.dir = haxe.io.Path.removeTrailingSlashes(outdir);
	}

	public function update( lexe : lm.LexEngine, groups : Array<RuleCaseGroup> ) {
		bit16   = lexe.isBit16();
		nsegs   = lexe.segs;
		nrules  = lexe.nrules;
		invalid = lexe.invalid;
		tabsize = lexe.table.length;
		table   = stable(lexe);
		for(i in 0...lexe.entrys.length) {
			entrys.push({name: groups[i].name.toUpperCase(), begin: lexe.entrys[i].begin});
		}
		entrybegin = lexe.entrys[0].begin;
	}

	function stable( lexe : lm.LexEngine ) {
		var buff = new StringBuf();
		var table = lexe.table;
		var perExit = lexe.perExit;
		var left = table.length - perExit;
		var cmax = lexe.per - 1;
		var xpad = lexe.isBit16() ? 4 : 2;
		var state = 0;
		for (i in 0...left) {
			if ((i & cmax) == 0) {
				buff.add("// STATE " + (state++) + "\n");
			}
			buff.add("0x" + StringTools.hex(table.get(i), xpad) + ",");
			if (i > 0 && (i & 15) == 15) {
				buff.addChar("\n".code);
			}
		}
		buff.add("// EXIT \n");
		for (i in 0...perExit - 1) {
			buff.add("0x" + StringTools.hex(table.get(left + i), xpad));
			buff.addChar(",".code);
			if (i > 0 && (i & 15) == 15)
				buff.addChar("\n".code);
		}
		buff.add("0x" + StringTools.hex(table.get(left + perExit - 1), xpad));
		return buff.toString();
	}
}

/**
 build lex for c language
*/
class CLexer {

	public var lexer : Lexer;
	public var parser : Parser;
	public var cfg : Config;

	public function new( file : String, mt : Template, cfg : haxe.DynamicAccess<String> ) {
		this.cfg = new Config(file, cfg);
		this.lexer = new Lexer(file);
		this.parser = new Parser(this.lexer);
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
		var aexpr = this.parser.begin();
		for (e in aexpr) {
			switch(e.texpr) {
			case EEof(id):
				cfg.eof = id;
			case ESrc(id):
				cfg.utf8 = id == "UTF8";
			case EMax(i):
				cfg.per = (i | 0x7f) + 1; // 128|256
			case EAssign(name, value):
				varmap.set(name, value);
			case ERuleSet(r):
				groups.push(r);
			}
		}
		if (cfg.eof == null)
			throw new Error("%EOF() is required.", lexer.mkpos(pstart, pstart));
		if (groups.length == 0)
			return;
		var paterns = toPattens(groups, varmap);
		var lexe = new lm.LexEngine(paterns, cfg.per - 1);

		// checking
		ExprHelps.lexChecking(lexe, groups);

		// update cfg.cases + cfg.epsilon
		var index = 0;
		for (g in groups) {
			for (r in g.rules) {
				cfg.cases.push({ index : index++, action : cstring(r.action)});
			}
		}
		if (groups[0].unmatch != null)
			cfg.epsilon = cstring(groups[0].unmatch.action);
		// get extra case and update cfg.cases
		var extra = ExprHelps.lexUnMatchedActions(lexe, groups);
		for (c in extra) {
			cfg.cases.push({ index : index++, action : cstring(c.expr) });
		}

		cfg.update(lexe, groups);
		// write
		var text = mt.execute(cfg);
		var out = sys.io.File.write(cfg.path.toString());
		out.writeString("// Generated by haxelib lex\n");
		out.writeString(lexer.header);
		out.writeString(text);
		out.writeString(lexer.footer);
		out.close();
		Sys.print("> " + cfg.path.toString() + "\n");
	}

	function cstring( e : Expr ) {
		return switch (e.expr) {
		case EConst(CString(s)):
			s;
		default:
			throw new Error("Unsupported : " + e.toString(), e.pos);
		}
	}

	function toPattens( groups : Array<RuleCaseGroup>, varmap : Map<String, Expr> ) {
		var cset = [new lm.Charset.Char(0, cfg.per - 1)];
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

	function s_tpexpr(e) {
		return switch(e.tpexpr) {
		case EEof(id):
			"%EOF" + "(" + id + ")";
		case ESrc(id):
			"%SRC" + "(" + id + ")";
		case EMax(i):
			"%SRC" + "(" + i + ")";
		case EAssign(name, value):
			'let $name = ${value.toString()}';
		case ERuleSet(r):
			var s = 'let ${r.name} = function\n';
			for (c in r.rules) {
				var pat = c.pattern.toString();
				s += '| $pat -> ${c.action.toString()}';
			}
			if (r.unmatch != null) {
				s += '| _ -> ${r.unmatch.action.toString()}';
			}
			s;
		}
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
