package;

class Demo {
	static function main() {
		var str = '1 	 + 2  	 *  	 3  "hello world!"	 - 5 ';
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
		trace(a.join("") == "1+2*3hello world!-5");
	}
}
@:enum abstract Token(Int) to Int {
	var Eof = 0;
	var CInt;
	var OpPlus;
	var OpMinus;
	var OpTimes;
	var OpDiv;
	var LParen;
	var RParen;
	var CStr;
}
class Lexer implements lm.Lexer<Token> {
	static var tok = @:rule(127, Eof) [
		"[ \t]+" => lex.token(),
		"+" => OpPlus,
		"-" => OpMinus,
		"*" => OpTimes,
		"/" => OpDiv,
		"(" => LParen,
		")" => RParen,
		"0" => CInt,
		"-?[1-9][0-9]*" => CInt,
		'"' => lex.str(),
	];
	static var str = @:rule [
		'[^"]*' => {
			lex.pmax++; // skip next '"'
			CStr;
		}
	];
}

class Parser implements lm.LR0<Token> {

	static var main = switch(s) {
		case [e = expr, Eof]:
			trace(e);
	}

	static var expr = switch(s) {
		case [e1 = expr, OpPlus,  e2 = expr]: // A+int -> A+A
			e1 + e2;
		case [e1 = expr, OpMinus, e2 = expr]:
			e1 - e2;
		case [e1 = expr, OpTimes, e2 = expr]:
			// e1 = cached[0]()
			// e2 = cached[3]()
			// junk(3)
			e1 * e2;
		case [CInt(n)]:
			// n = Std.parseInt(peek(0).value)
			// s.junk(1)
			n;
		//default: 0;// when matching error then default.
	}

	// LL1
	// A  -> Aa  | B
	// =>
	// A  -> BA'
	// A' -> aA' | epsilon
/*

	static var expr = switch(s) {
		case [CInt(n), e1 = expr1()]:

	}
	static var expr1 = switch(s) {
		case [OpPlus, e2 = expr, e1 = expr1]:
		default: // epsilon
	}
*/



/*
	static var side = switch(s) {
	case [LParen, Op, RParen]:
		6;
	case [LParen, _ = side, RParen]: // 假设包含有"自身归递"的话, 那么 side 必须至少要有一个全是 终结符的 case
		7;
	case [ (n) ]:     // Terminal Universal Set.
		8;
	default:          // epsilon
		9;
	}
*/
}
