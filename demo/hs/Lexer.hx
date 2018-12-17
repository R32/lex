package hs;

enum abstract Token(Int) to Int {
	var Eof = 0;
	// kwd
	var Kwd_Var;
	var Kwd_Function;
	var Kwd_If;
	var Kwd_Else;
	var Kwd_Do;
	var Kwd_While;
	var Kwd_For;
	var Kwd_In;
	var Kwd_Continue;
	var Kwd_Break;
	var Kwd_Return;
	var Kwd_Switch;
	var Kwd_Case;
	var Kwd_Default;
	var Kwd_True;
	var Kwd_False;
	var Kwd_Null;
	var Kwd_New;
	var Kwd_Try;
	var Kwd_Catch;
	var Kwd_Throw;
	// Prepro
	var PrIf;        // #if
	var PrElse;
	var PrElseIf;
	var PrEnd;
	// Const
	var CIdent;
	var CInt;
	var CFloat;
	var CString;
	//
	var Question;     // ?
	var Dot;          // .
	var DblDot;       // :
	var Comma;        // ,
	var Semicolon;    // ;
	var LParen;       // (
	var RParen;       // )
	var LBrace;       // {
	var RBrace;       // }
	var LBracket;     // [
	var RBracket;     // ]
	var Meta;         // "@string" or "@:string"
	var Sharp;        // "#string"
	// op. 0
	var OpMod;        // %
	var OpMul;        // *
	var OpDiv;        // /
	var OpAdd;        // +
	var OpSub;        // -

	var OpShl;        // <<
	//var OpShr;        // >>
	var OpUShr;       // >>>

	var OpOr;         // |
	var OpAnd;        // &
	var OpXor;        // ^

	var OpEq;         // ==
	var OpNotEq;      // !=
	var OpGt;         // >
	var OpLt;         // <
	//var OpGte;        // >=
	var OpLte;        // <=

	var OpInterval;   // ...
	var OpBoolAnd;    // &&
	var OpBoolOr;     // ||

	var OpAssign;     // =
	var OpAssignAdd;  // +=
	var OpAssignSub;  // -=
	var OpAssignMul;  // *=
	var OpAssignDiv;  // /=
	var OpAssignMod;  // %=
	var OpAssignShl;  // <<=
	//var OpAssignShr;  // >>=
	//var OpAssignUShr; // >>>=
	var OpAssignOr;   // |=
	var OpAssignAnd;  // &=
	var OpAssignXor;  // ^=
	var OpArrow;      // =>

	var Arrow;        // ->
	var OpNot;        // !
	var OpBits;       // ~
	var OpIncrement;  // ++
	var OpDecrement;  // --

	// comment
	var Comment;
	var CommentLine;
}

#if !macro
@:rule(Eof, 127) class Lexer implements lm.Lexer<Token> {

	static var ident = "[a-zA-Z_][a-zA-Z0-9_]*";
	static var integer = "0|-?[1-9][0-9]*";

	static var token = [
		"[ \t\r\n]+" => lex.token(),
		// kwd
		"var" =>        Kwd_Var,
		"function" =>   Kwd_Function,
		"if" =>         Kwd_If,
		"else" =>       Kwd_Else,
		"do" =>         Kwd_Do,
		"while" =>      Kwd_While,
		"for" =>        Kwd_For,
		"in" =>         Kwd_In,
		"continue" =>   Kwd_Continue,
		"break" =>      Kwd_Break,
		"return" =>     Kwd_Return,
		"switch" =>     Kwd_Switch,
		"case" =>       Kwd_Case,
		"default" =>    Kwd_Default,
		"true" =>       Kwd_True,
		"false" =>      Kwd_False,
		"null" =>       Kwd_Null,
		"new" =>        Kwd_New,
		"try" =>        Kwd_Try,
		"catch" =>      Kwd_Catch,
		"throw" =>      Kwd_Throw,
		// PrePro
		"#if"     =>    lex.prep.process(PrIf, lex),
		"#else"   =>    lex.prep.process(PrElse, lex),
		"#elseif" =>    lex.prep.process(PrElseIf, lex),
		"#end"    =>    lex.prep.process(PrEnd, lex),
		// Const
		ident =>            CIdent,
		"0x[A-Fa-f0-9]+" => CInt,
		integer =>          CInt,
		integer + "..." => {         // to prevent "0." being parsed as float
			var max = lex.pmax - 3;
			lex.pmax = lex.pmin;     // reset
			lex._token(BEGIN, max);  // see LexBuilder.hx#L229 about BEGIN
		},
		"-?.[0-9]+|-?[0-9]+.[0-9]*" => CFloat,
		//
		"?" => Question,
		"." => Dot,
		":" => DblDot,
		"," => Comma,
		";" => Semicolon,
		"(" => LParen,
		")" => RParen,
		"{" => LBrace,
		"}" => RBrace,
		"[" => LBracket,
		"]" => RBracket,
		"@:?" + ident => Meta,
		// Op
		"%" =>    OpMod,
		"*" =>    OpMul,
		"/" =>    OpDiv,
		"+" =>    OpAdd,
		"-" =>    OpSub,
		"<<" =>   OpShl,
	//	">>" =>   OpShr,
	//	">>>" =>  OpUShr,
		"|" =>    OpOr,
		"&" =>    OpAnd,
		"^" =>    OpXor,
		"==" =>   OpEq,
		"!=" =>   OpNotEq,
		">" =>    OpGt,
		"<" =>    OpLt,
	//	">=" =>   OpGte,             // conflicts.
		"<=" =>   OpLte,
		"..." =>  OpInterval,
		"&&" =>   OpBoolAnd,
		"||" =>   OpBoolOr,          // All "|" at the beginning do not need to be escaped
		"=" =>    OpAssign,
		"+=" =>   OpAssignAdd,       // if "+", "*", "?", "|" is the first char then they do not need to be escaped
		"-=" =>   OpAssignSub,
		"*=" =>   OpAssignMul,
		"/=" =>   OpAssignDiv,
		"%=" =>   OpAssignMod,
		"<<=" =>  OpAssignShl,
	//	">>=" =>  OpAssignShr,
	//	">>>=" => OpAssignUShr,
		"|=" =>   OpAssignOr,
		"&=" =>   OpAssignAnd,
		"^=" =>   OpAssignXor,
		"=>" =>   OpArrow,
		"->" =>   Arrow,

		"!" =>    OpNot,
		"~" =>    OpBits,
		"+\\+" => OpIncrement,       // escaped("++")
		"--" =>   OpDecrement,
		// comment
		"//[^\n]*\n?" => lex.token(),// skip CommentLine
		"/\\*" => {
			var pmin = lex.pmin;
			var t = lex.comment();
			if (t == Eof)
				throw lm.Utils.error("Unclosed " + "comment" + lex.strpos(pmin));
			lex.token();             // skip Comment
		},
		// string
		'"' => {
			var pmin = lex.pmin;
			buff = new StringBuf();
			var t = lex.str();
			if (t == Eof)
				throw lm.Utils.error("Unclosed " + "string" + lex.strpos(pmin));
			lex.pmin = pmin; // punion
			smap.set(pmin, buff.toString());
			t;
		},
		"'" => {
			var pmin = lex.pmin;
			buff = new StringBuf();
			var t = lex.qstr();
			if (t == Eof)
				throw lm.Utils.error("Unclosed " + "string" + lex.strpos(pmin));
			lex.pmin = pmin;
			smap.set(pmin, buff.toString());
			t;
		}
	];

	static var buff: StringBuf;
	static var str = [
		'\\\\"' => {
			buff.addChar('"'.code);
			lex.str();
		},
		"\\\\n" => {
			buff.addChar("\n".code);
			lex.str();
		},
		"\\\\t" => {
			buff.addChar("\t".code);
			lex.str();
		},
		"\\\\r" => {
			buff.addChar("\r".code);
			lex.str();
		},
		'[^\\\\"]+' => {
			buff.add(lex.current);
			lex.str();
		},
		'"' => CString,  // exit
	];
	static var qstr = [
		"\\\\'" => {
			buff.addChar("'".code);
			lex.qstr();
		},
		"\\\\n" => {
			buff.addChar("\n".code);
			lex.qstr();
		},
		"\\\\t" => {
			buff.addChar("\t".code);
			lex.qstr();
		},
		"\\\\r" => {
			buff.addChar("\r".code);
			lex.qstr();
		},
		"[^\\\\']+" => {
			buff.add(lex.current);
			lex.qstr();
		},
		"'" => CString,
	];
	static var comment = [
		"*/" => Comment,
		"[^*]+|*" => lex.comment(),
	];
#else
class Lexer {
#end
	static var smap: Map<Int, String>;      // pmin => String

	var prep: hs.PreProcess;

	public function new(s: lms.ByteData) {  // if "new" already exists, then the macro will not build it again.
		reset(s);
	}

	public function reset(s: lms.ByteData) {
		this.input = s;
		pmin = 0;
		pmax = 0;
		smap = new Map();
		prep = new hs.PreProcess(this);
	}

	function strpos(p:Int):String {
		var line = 1;
		var char = 0;
		var i = 0;
		while (i < p) {
			var c = input.readByte(i++);
			if (c == "\n".code) {
				char = 0;
				++ line;
			} else {
				++ char;
			}
		}
		return " at line: " + line + ", char: " + char;
	}

	static public function s_token(t: Token, ext = "const") {
		return switch(t){
		case Eof: "$";
		// kwd
		case Kwd_Var: "var";
		case Kwd_Function: "function";
		case Kwd_If: "if";
		case Kwd_Else: "else";
		case Kwd_Do: "do";
		case Kwd_While: "while";
		case Kwd_For: "for";
		case Kwd_In: "in";
		case Kwd_Continue: "continue";
		case Kwd_Break: "break";
		case Kwd_Return: "return";
		case Kwd_Switch: "switch";
		case Kwd_Case: "case";
		case Kwd_Default: "default";
		case Kwd_True: "true";
		case Kwd_False: "false";
		case Kwd_Null: "null";
		case Kwd_New: "new";
		case Kwd_Try: "try";
		case Kwd_Catch: "catch";
		case Kwd_Throw: "throw";
		//
		case PrIf: "#if";
		case PrElse: "#else";
		case PrElseIf: "#elseif";
		case PrEnd: "#end";
		// Const
		case CIdent: ext;
		case CInt: ext;
		case CFloat: ext;
		case CString: '"' + ext + '"';
		//
		case Question: "?";
		case Dot: ".";
		case DblDot: ":";
		case Comma: ",";
		case Semicolon: ";";
		case LParen: "(";
		case RParen: ")";
		case LBrace: "{";
		case RBrace: "}";
		case LBracket: "[";
		case RBracket: "]";
		case Meta: "@" + ext;
		case Sharp: "#" + ext;

		// op
		case OpMod: "%";
		case OpMul: "*";
		case OpDiv: "/";
		case OpAdd: "+";
		case OpSub: "-";

		case OpShl: "<<";
	//	case OpShr: ">>";
		case OpUShr: ">>>";

		case OpOr:  "|";
		case OpAnd: "&";
		case OpXor: "^";

		case OpEq: "==";
		case OpNotEq: "!=";
		case OpGt: ">";
		case OpLt: "<";
	//	case OpGte: ">=";
		case OpLte: "<=";

		case OpInterval: "...";
		case OpBoolAnd: "&&";
		case OpBoolOr: "||";

		case OpAssign: "=";
		case OpAssignAdd: "+=";
		case OpAssignSub: "-=";
		case OpAssignMul: "*=";
		case OpAssignDiv: "/=";
		case OpAssignMod: "%=";
		case OpAssignShl: "<<=";
	//	case OpAssignShr: ">>=";
	//	case OpAssignUShr: ">>>=";
		case OpAssignOr: "|=";
		case OpAssignAnd: "&=";
		case OpAssignXor: "^=";
		case OpArrow: "=>";

		case Arrow: "->";
		case OpNot: "!";
		case OpBits: "~";
		case OpIncrement: "++";
		case OpDecrement: "--";

		// comment
		case Comment: "/* ... */";
		case CommentLine: "//...\n";
		case _: "null";
		}
	}
}