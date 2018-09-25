package;

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
		var lex = new Lexer(lms.ByteData.ofString(' 123 	+ 	456 	 * 23 +  "hello world" 	 + 	 1'));
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
		case Choice(a, Empty):
			s_partern(a) + "?";
		case Choice(a, b):
			s_partern(a) + "|" + s_partern(b);
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

/**
 @:rule(127, Eof) that is 127 => charset(0, 127). the Eof is a custom terminator.
 and the "static var..." will be treated as rules if no `@:skip`
*/
@:rule(127, Eof) class Lexer implements lm.Lexer<Token> {

	static var r_zero = "0";      // a pattern can be used in rule sets if there is no @:skip
	static var r_int = "-?[1-9][0-9]*";

	static var tok =  [           // a rule set definition
		"[ \t]+" => lex.token(),  // .token() is automatically built by macro based on the first @:rule
		"+"   => Op(Plus),        // and the "lex" is an instance of this class.
		"-"     => Op(Minus),
		"*"   => Op(Times),
		"/" => Op(Div),
		"(" => LParen,
		")" => RParen,
		r_zero + "|" + r_int => CInt(Std.parseInt(lex.current)),
		'"' => lex.str(),         // str() is created by macro. See the second rule set
	];
	static var str = [
		'[^"]*' => {
			var s = CStr(lex.current);
			lex.pmax++;
			s;
		}
	];

	// custom functions
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
