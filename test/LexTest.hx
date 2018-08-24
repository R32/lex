package test;

import lm.Charset;
import lm.LexEngine;

class LexTest {
	static function run() {
		cset_test();
		lex_test();
	}
	static function main()  {
		haxe.Timer.measure(run);
	}
	static function cset_test() {
		// charset testing
		inline function c(a, b) return new Char(a, b);
		function eq(c1: Charset, c2: Charset, ?pos: haxe.PosInfos) {
			var b = c1.length == c2.length;
			if (b) {
				for (i in 0...c1.length) {
					if (c1[i].toInt() != c2[i].toInt()) {
						b = false;
						break;
					}
				}
			}
			if (!b) {
				trace("\n c1: " + CSet.string(c1) + "\n c2: " + CSet.string(c2));
				throw lm.Utils.error("LineNumber: " + pos.lineNumber);
			}
		}
		eq( CSet.union([c(32, 32), c(51, 59), c(65, 65), c(97, 102)], [c(10, 10), c(51, 59), c(65, 70)]),
			[c(10, 10), c(32, 32), c(51, 59), c(65, 70), c(97, 102)]
		);
		eq( CSet.inter([c(32, 32), c(51, 59), c(65, 65), c(97, 102)],[c(10, 10), c(51, 59), c(65, 70)]),
			[c(51, 59), c(65, 65)]
		);
		eq( CSet.complement( [c(48, 59), c(65, 65)] ),
			[c(0,47), c(60,64), c(66,255)]
		);
		eq( CSet.complement( [c(48, 59), c(65, 70), c(97, 102)] ),
			[c(0,47), c(60,64), c(71,96), c(103, 255)]
		);
		eq( CSet.diff([c(32, 32), c(34, 51), c(65, 70)],[c(33, 33), c(37, 37), c(41, 59), c(61, 63), c(65, 67)]),
			[c(32, 32), c(34, 36), c(38, 40), c(68,70)]
		);
	}

	static public function lex_test(): Void @:privateAccess {

		var lex = new Lexer(lm.ByteData.ofString(' 123 	+ 	456 	 * 23 +  "hello world" 	 + 	 1'));
		var t = lex.token();
		var a = [];
		while (t != Eof) {
			a.push(Lexer.s_token(t));
			t = lex.token();
		}
		if (a.join("") != "123+456*23+hello world+1") throw lm.Utils.error("TODO");
	}
	static public function s_partern(p: Pattern): String {
		return switch (p) {
		case Empty:
			"";
		case Match(c):
			s_charset(c);
		case Star(a):
			s_partern(a) + "*";
		case Plus(a):
			s_partern(a) + "+";
		case Choice(a):
			s_partern(a) + "?";
		case Next(a, b):
			s_partern(a) + s_partern(b);
		}
	}
	static public function s_charset(cs: Charset): String {
		function unescape(c) {
			return switch(c) {
			case "\\".code, "+".code, "*".code, "?".code, "[".code, "]".code, "-".code:
				"\\" + String.fromCharCode(c);
			default:
				String.fromCharCode(c);
			}
		}
		var s = "";
		for (c in cs) {
			if (c.min == c.max) {
				s += unescape(c.min);
			} else {
				s += '${String.fromCharCode(c.min)}-${String.fromCharCode(c.max)}';
			}
		}
		return if (cs.length > 1) {
			"[" + s + "]";
		} else {
			s;
		}
	}
}

class Lexer implements lm.Lexer<Token> {
	public static var tok = @:rule(127) [  // token() is built automatically by the the first @:rule.
		"[ \t]+" => lexbuf.token(),        // skip. and the "lexbuf" is an instance of this class.
		"+"   => Op(Plus),
		"-"     => Op(Minus),
		"*"   => Op(Times),
		"/" => Op(Div),
		"(" => LParen,
		")" => RParen,
		"0" => CInt(0),
		"-?[1-9][0-9]*" => CInt(Std.parseInt(lexbuf.current)),
		'"' => {
			var s = lexbuf.str();          // str() is created by macro. See the second @:rule set
			lexbuf.pmax++;
			s;
		}
	];
	static var str = @:rule [
		'[^"]*' => CStr(lexbuf.current),
	];

	static function s_op(o) {
		return switch (o) {
		case Plus: "+";
		case Minus: "-";
		case Times: "*";
		case Div: "/";
		}
	}
	static public function s_token(t) {
		return switch (t) {
		case Eof: "<end of file>";
		case CInt(i): "" + i;
		case Op(op): s_op(op);
		case LParen: "(";
		case RParen: ")";
		case CStr(s): s;
		}
	}
}

enum Op {
	Plus;
	Minus;
	Times;
	Div;
}

enum Token {
	Eof;
	CInt(i: Int);
	Op(op: Op);
	LParen;
	RParen;
	CStr(s: String);
}
