package;

class Demo {
	static function main() {
		var str = '1 + 1';
		var lex = new Lexer(lm.ByteData.ofString(str));
		var t = lex.token();
		var a = [];
		while (t != Eof) {
			switch (t) {
			case Eof:
			//case CInt: Std.parseInt(lex.current);
			case _:
				a.push(lex.current);
			}
			t = lex.token();
		}
		trace(a.join(""));
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
	];
}

class Parser implements lm.LR0<Lexer> {
	static var main = switch(s) {
		case [e = expr, Eof]: e;
	}
	static var expr = switch(s) {
		case [e1 = expr, OpPlus, e2 = expr]: e1 + e2;
	//	case [LParen, e = expr, RParen]: e;
		case [CInt(n)]: n;
	//	default: 0;
	}
}
