package tools;

import haxe.Template;
import StringTools.ltrim;
import StringTools.rtrim;
import StringTools.trim;
import lm.LineColumn;

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
	var OpAssign;  // "="
	var OpArrow;   // "->"
	var LParen;    // "("
	var RParen;    // ")"
}
private enum Pattern {
	PNull;
	PIdent( id : String);
	PString( s : String);
}
private typedef PatternEx = {
	pat : Pattern,
	pos : Int,
}
private typedef RuleCase = {
	patten : PatternEx,
	action : String,
	faildown : Bool,
	pos : Int,   // the pos of action start
	index : Int, // for haxe.Template
}
private typedef RuleSet = {
	name     : String,
	rules    : Array<RuleCase>,
	epslon   : Null<RuleCase>,
}
private enum Expr {
	EEof( id : String );
	ESrc( id : String );
	EMax( i : Int );
	EAssign( name : String, value : String );
	ERuleSet( r : RuleSet );
}

@:rule(Eof, 127) private class Lexer implements lm.Lexer<Token> {

	public var header : String;

	public var footer : String;

	public var lines : LineCounter;

	public var parray : lm.PosArray;

	public var ccodes : Array<String>;

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
		"("           => LParen,
		")"           => RParen,
		ident         => CIdent,
		integer       => CInt,

		'"' => {
			var min = lex.pmin;
			sbuf = new StringBuf();
			var t = lex.str();
			if (t == Eof)
				CLexer.fatalError("Unclosed " + "string", min, lex.pmax);
			lex.pmin = min; // pos union
			parray.add(min + 1, sbuf.toString());
			t;
		},
		"'" => {
			var min = lex.pmin;
			sbuf = new StringBuf();
			var t = lex.qstr();
			if (t == Eof)
				CLexer.fatalError("Unclosed " + "string", min, lex.pmax);
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
			CLexer.fatalError(s, pmax, pmin);
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
			CLexer.fatalError("Unexpected: " + "\\n", pmin, pmax);
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
		CRLF         => { lines.add(pmax); CLexer.fatalError("Unexpected: " + "\\n", pmin, pmax); },
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

private class Parser implements lm.LR0<Lexer, Array<Expr>> {
	// custom
	static var __default__ = @:privateAccess {
		var t = stream.offset( -1);
		CLexer.fatalError('Unexpected "' + (t.term != Eof ? stream.str(t): "Eof") + '"', t.pmin, t.pmax);
	}

	static var begin = switch(s) {
	case [a = list, Eof]                         : a;
	case [Eof]                                   : [];
	}
	static var list = switch(s) {
	case [a = list, e = expr]                    : a.push(e); a;
	case [e = expr]                              : [e];
	}
	static var expr : Expr = switch(s) {
	case [DEoF, "(", CIdent(c), ")"]             : EEof(c);
	case [DSrc, "(", CIdent(c), ")"]             : ESrc(c);
	case [DMax, "(", CInt(i), ")"]               : EMax(i);
	case [KLet, CIdent(c), "=", CString(s) ]     : EAssign(c, s);
	case [KLet, CIdent(c), "=", KFun, r = cases] : r.name = c; ERuleSet(r);
	}

	static var cases : RuleSet = switch(s) {
	case [c = cases, r = line]:
		if (r.patten.pat == PNull) {
			if (c.epslon != null)
				throw "UnExpected: " + "_";
			c.epslon = r;
		} else {
			c.rules.push(r);
		}
		c;
	case [r = line]:
		if (r.patten.pat == PNull) {
			{ name : "", rules : [ ], epslon : r }
		} else {
			{ name : "", rules : [r], epslon : null }
		}
	}

	static var line : RuleCase = switch(s) {
	case [OpOr, p = pat, OpArrow]:
		stream.junk(stream.rest);
		var pmax = _t3.pmax;
		var lex = @:privateAccess stream.lex;
		var act = Std.downcast(lex, Lexer).copyAction(pmax);
		{ patten : p, action : act, faildown : false, pos : pmax, index : -1 }
	case [OpOr, p = pat]:
		var pmax = _t2.pmax;
		if (p.pat == PNull)
			CLexer.fatalError("Expected: " + "->", pmax, pmax);
		var tok = stream.peek(0);
		if (tok.term != OpOr)
			CLexer.fatalError("Unexpected: " + "actions", pmax, tok.pmin);
		{ patten : p, action : "\n", faildown : true, pos : pmax, index : -1 }
	}

	static var pat : PatternEx = switch(s) {
	case [CString(s)]: { pos: _t1.pmin, pat: PString(s)};
	case [CIdent(id)]: { pos: _t1.pmin, pat: id == "null" || id == "_" ? PNull : PIdent(id) };
	}

	// custom extract function
	@:rule(CInt) static inline function __s1( s : String ) return Std.parseInt(s);
	@:rule(CIdent) static inline function __s2( s : String ) return s;
	@:rule(CString) static function __s3( input, t ) {
		var s = CLexer.lastLexer.parray.get(t.pmin + 1);
		if (s == null)
			CLexer.fatalError("TODO", t.pmin, t.pmax);
		return s;
	}
}


private class Config {
	public var per     : Int;
	public var eof     : String;
	public var bit16   : Bool;   // the table format if nstates > 254
	public var utf8    : Bool;   // the input format
	public var nsegs   : Int;
	public var nrules  : Int;
	public var invalid : Int;
	public var cases   : Array<RuleCase>;
	public var epsilon : String; // case _: actions
	public var table   : String;
	public var tabsize : Int;
	public var entrys  : Array<{name : String, begin: Int}>;
	public var entrybegin : Int; // entrys[0].begin
	public var path    : haxe.io.Path;

	public function new( file : String, outdir : String ) {
		per = 128;
		eof = null;
		utf8 = true;
		cases = [];
		table = "";
		epsilon = "";
		entrys = [];
		path = new haxe.io.Path(file);
		if (outdir != null && outdir != "")
			path.dir = haxe.io.Path.removeTrailingSlashes(outdir);
		path.ext = "c"; // change
	}
	public function update( leg : lm.LexEngine, list : Array<RuleSet> ) {
		bit16   = leg.isBit16();
		nsegs   = leg.segs;
		nrules  = leg.nrules;
		invalid = leg.invalid;
		tabsize = leg.table.length;
		table   = stable(leg);
		for(i in 0...leg.entrys.length) {
			entrys.push({name: list[i].name.toUpperCase(), begin: leg.entrys[i].begin});
		}
		entrybegin = leg.entrys[0].begin;
	}
	function stable( leg : lm.LexEngine ) {
		var buff = new StringBuf();
		var table = leg.table;
		var perExit = leg.perExit;
		var left = table.length - perExit;
		var cmax = leg.per - 1;
		var xpad = leg.isBit16() ? 4 : 2;
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

	public function new( file : String, mt : Template, outdir : String ) {
		var lexer = new Lexer(file);
		lastLexer = lexer;
		lexer.skipBegin();
		var list = [];
		var idmap = new Map();
		var parser = new Parser(lexer);
		var cfg = new Config(file, outdir);
		// parse file
		var aexpr = parser.begin();
		for (e in aexpr) {
			switch(e) {
			case EEof(id):
				cfg.eof = id;
			case ESrc(id):
				cfg.utf8 = id == "UTF8";
			case EMax(i):
				cfg.per = (i | 0x7f) + 1; // 128|256
			case EAssign(name, value):
				idmap.set(name, value);
			case ERuleSet(r):
				list.push(r);
			}
		}
		if (cfg.eof == null)
			fatalError("%EOF() is required.", 0, 0);
		var paterns = toPattens(list, cfg, idmap);
		var leg = new lm.LexEngine(paterns, cfg.per - 1);
		checking(leg, list);
		hackNullActions(leg, cfg, list);
		cfg.update(leg, list);
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

	function parse( s , cset, min) {
		return try
			lm.LexEngine.parse(s, cset)
		catch (e : lm.LexEngine.PError) {
			fatalError(e.message, min + e.offset, min + e.offset + 1);
		} catch (x) {
			fatalError("TODO", min, min + 1);
		}
	}

	function toPattens( list : Array<RuleSet>, cfg : Config, idmap : Map<String,String> ) {
		var cset = [new lm.Charset.Char(0, cfg.per - 1)];
		var ret = [];
		var index = 0;
		for (g in list) {
			var a = [];
			for (r in g.rules) {
				switch(r.patten.pat) {
				case PNull:
				case PIdent(i):
					var s = idmap.get(i);
					if (s == null)
						fatalError("Undefined: " + i, r.patten.pos, r.patten.pos + i.length);
					a.push( parse(s, cset, r.patten.pos) );
				case PString(s):
					a.push( parse(s, cset, r.patten.pos) );
				}
				r.index = index++;
				cfg.cases.push(r);
			}
			ret.push(a);
		}
		return ret;
	}
	// copy from lm.LexBuilder
	function checking( leg : lm.LexEngine, list : Array<RuleSet> ) {
		var table = leg.table;
		var VALID = 1;
		var INVALID = leg.invalid;
		var exits = haxe.io.Bytes.alloc(leg.nrules);
		for (i in table.length - leg.perExit...table.length) {
			var n = table.get(i);
			if (n == INVALID) continue;
			exits.set(n, VALID);
		}
		function indexPattern( i ) : PatternEx {
			for (g in list) {
				var len = g.rules.length;
				if (i >= len) {
					i -= len;
				} else {
					return g.rules[i].patten;
				}
			}
			throw "NotFound";
		}
		// reachable
		for (n in 0...leg.nrules)
			if (exits.get(n) != VALID) {
				var pex = indexPattern(n);
				var msg = s_patten(pex.pat);
				fatalError("UnReachable pattern: " + msg, pex.pos, pex.pos + msg.length);
			}
		// epsilon
		for (e in leg.entrys) {
			var n = table.exits(e.begin);
			if (n != INVALID) {
				var pex = indexPattern(n);
				var msg = s_patten(pex.pat);
				fatalError("epsilon is not allowed: " + msg, pex.pos, pex.pos + msg.length);
			}
		}
	}
	function hackNullActions(leg: lm.LexEngine, cfg : Config, list: Array<RuleSet>) {
		var table = leg.table;
		var start = leg.nrules;
		if (cfg.cases.length != leg.nrules)
			throw "assert";
		for (i in 1...list.length) { // the null-case of the first rule-set will be "switch-default"
			var g = list[i];
			if (g.epslon != null) {
				var e = leg.entrys[i];
				table.set(table.exitpos(e.begin), start);
				g.epslon.index = start++;
				cfg.cases.push( g.epslon );
			}
		}
		if (list[0].epslon != null) {
			cfg.epsilon = list[0].epslon.action;
		}
	}

	function s_patten( p : Pattern ) {
		return switch(p) {
		case PNull: "_";
		case PIdent(s):  s;
		case PString(s): '"' + s + '"'; // TODO: maybe '"'
		}
	}
	function s_expr(e) {
		return switch(e) {
		case EEof(id):
			"%EOF" + "(" + id + ")";
		case ESrc(id):
			"%SRC" + "(" + id + ")";
		case EMax(i):
			"%SRC" + "(" + i + ")";
		case EAssign(name, value):
			'let $name = $value';
		case ERuleSet(r):
			var s = 'let ${r.name} = function\n';
			for (c in r.rules) {
				var pat = s_patten(c.patten.pat);
				var arrow = c.faildown ? "" : "->";
				s += '| $pat $arrow ${c.action}';
			}
			if (r.epslon != null) {
				s += '| _ -> ${r.epslon.action}';
			}
			s;
		}
	}

	public static var lastLexer : Lexer;
	public static function fatalError( msg : String, min : Int, max : Int ) : Dynamic {
		var lmin = lastLexer.lines.get(min);
		var spos = '${lmin.line}: characters ${lmin.column}-${max - lmin.base + 1}';
		Sys.println(lastLexer.lines.owner + ":" + spos + " : " + msg);
		Sys.exit( -1);
		return null;
	}
}
