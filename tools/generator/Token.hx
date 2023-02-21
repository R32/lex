package tools.generator;

import lm.LineColumn;
import haxe.macro.Expr;

enum abstract Token(Int) to Int {
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
	// extra tokens for SLR
	var OpMul;     // *
	var Comma;     // ,
	var DblDot;    // :
	var LBracket;  // [
	var RBracket;  // ]
	var LBrace;    // {
	var RBrace;    // }
	var DName;     // %NAME
	var DToken;    // %TOKEN
	var DStart;    // %START
	var DLeft;     // %LEFT
	var DRight;    // %RIGHT
	var DNonAssoc; // %NONASSOC
	var DFunc;     // %FUNC
	var DPrec;     // %prec()
	var DDef;      // %DEF()
}

typedef TokenJson = {
	file : String,  //
	stype : String, // e.g: token from enum token {...}
	eof : String,
	utf8 : Bool,    // input source format, not table
	terms : haxe.DynamicAccess<{value : Int, pmin : Int, pmax : Int}>,
	reflect : haxe.DynamicAccess<String>,
}

#if macro
private class Lexer {
	public function new( file : String ){}
}
#else
@:rule(Eof, 127) class Lexer implements lm.Lexer<Token> {

	public var header : String;

	public var footer : String;

	public var lines : LineCounter;    // LF counter

	public var parray : lm.PosArray;   // Map<pos,string>

	public var ccodes : Array<Expr>;   // c language codes, only treated as a string

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

	public function copyAction( i : Int ) : Expr {
		this.pmax = i;
		this.ccodes = [];
		this.sbuf = new StringBuf();
		this.trimbeginning();
		var t = this.clblock();
		if (t == ActRollback) {
			this.pmax = this.pmin;
		}
		this.pmin = i;
		return {expr : EBlock(ccodes), pos : mkpos(i, this.pmax)};
	}

	public function mkpos(min, max) {
		return {min : min, max : max, file : lines.owner};
	}

	static var r_max      = "%[mM][aA][xX]";
	static var r_src      = "%[sS][rR][cC]";
	static var r_eof      = "%[eE][oO][fF]";
	static var r_def      = "%[dD][eE][fF]";
	static var r_token    = "%[tT][oO][kK][eE][nN]";
	static var r_left     = "%[lL][eE][fF][tT]";
	static var r_right    = "%[rR][iI][gG][hH][tT]";
	static var r_start    = "%[sS][tT][aA][rR][tT]";
	static var r_nonassoc = "%[nN][oO][nN][aA][sS][sS][oO][cC]";
	static var r_func     = "%[fF][uU][nN][cC]";
	static var r_prec     = "%[pP][rR][eE][cC]";
	static var r_name     = "%[nN][aA][mM][eE]";

	static var ident = "[a-zA-Z_][a-zA-Z0-9_]*";
	static var integer = "0|[1-9][0-9]*";
	static var CRLF = "\r?\n";

	var token = [
		"%%" => {
			lex.footer = lex.getString(pmax, input.length - pmax);
			Eof;
		},

		CRLF          => { lines.add(pmax); lex.token(); },
		"[ \t]+"      => lex.token(),
		"//[^\n]*"    => lex.token(), // skip CommentLine
		r_eof         => DEoF,
		r_src         => DSrc,
		r_max         => DMax,
		"let"         => KLet,
		"function"    => KFun,
		"="           => OpAssign,
		"->"          => OpArrow,
		"|"           => OpOr,
		"+"           => OpAdd,
		"("           => LParen,
		")"           => RParen,
		// extra start for SLR parser
		"*"           => OpMul,
		","           => Comma,
		":"           => DblDot,
		"["           => LBracket,
		"]"           => RBracket,
		"{"           => LBrace,
		"}"           => RBrace,
		r_name        => DName,
		r_left        => DLeft,
		r_right       => DRight,
		r_start       => DStart,
		r_nonassoc    => DNonAssoc,
		r_token       => DToken,
		r_func        => DFunc,
		r_prec        => DPrec,
		r_def         => DDef,
		// extra end
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

	var str = [
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
	var qstr = [
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

	var commentblock = [
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
	var clblock = [
		"let|%%" => {
			ActRollback; // Reserve for next
		},
		CRLF => {
			if (sbuf.length > 0) {
				ccodes.push({expr : EConst(CString(sbuf.toString())), pos : mkpos(pmin - 1, pmin)}); // dummy pos
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

	var trimbeginning = [
		"[ \t]*" => Exit
	];
}
#end
