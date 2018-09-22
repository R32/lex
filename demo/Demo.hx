package;

import lm.Stream;

class Demo {
	static function main() {
		var str = '1 + 1123 + -1 -; ';
		var lex = new Lexer(lms.ByteData.ofString(str));
		var t = lex.token();
		var a = [];
		while (t != Eof) {
			a.push( switch (t) {
				case Eof: "$";
				case CInt: lex.current;
				case OpPlus: "+";
				case OpMinus: "-";
				case OpTimes: "*";
				case OpDiv:   "/";
				case LParen:  "(";
				case RParen:  ")";
				case Semicolon: ";";
			});
			t = lex.token();
		}
		trace(a.join(",") == "1,+,1123,+,-1,-,;");
	}
}

enum abstract Token(Int) to Int {
	var Eof = 0;
	var CInt;
	var OpPlus;
	var OpMinus;
	var OpTimes;
	var OpDiv;
	var LParen;
	var RParen;
	var Semicolon;
}

@:rule(127, Eof) class Lexer implements lm.Lexer<Token> {
	static var r_zero = "0";
	static var r_int = "-?[1-9][0-9]*";
	static var tok =  [
		"[ \t]+" => lex.token(),
		r_zero + "|" + r_int => CInt,
		"+" => OpPlus,
		"-" => OpMinus,
		"*" => OpTimes,
		"/" => OpDiv,
		"(" => LParen,
		")" => RParen,
		";" => Semicolon,
	];
}

class Parser implements lm.LR0<Lexer> {

	static var main = switch(s) {
		case [e = expr, Eof]: e;
	}

	static var expr = switch(s) {
		case [e1 = expr, OpPlus, e2 = expr]: e1 + e2;
		case [CInt]: n;
	}

	@:ofStr(CInt) static inline function int_of_string(s: String):Int return Std.parseInt(s);
}
