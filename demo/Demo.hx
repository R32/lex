package;

class Demo {
	static function main() {
		var str = '1 + 2 + 3 + -1';
		var lex = new Lexer(lms.ByteData.ofString(str));
		var par = new Parser(lex);
		trace(par.main());
	}
}

// NOTICE: the lm.LR0 only works with "enum abstract (Int)"
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

/**
* @:rule(EOF, cmax = 255)
*   Eof is a custom terminator. (required)
*   127 is the char max value.  (optional, default is 255)
*
* and all the `static var X = "string"` will be treated as rules if no `@:skip`
*/
@:rule(Eof, 127) class Lexer implements lm.Lexer<Token> {
	static var r_zero = "0";             // a pattern can be used in rule sets if there is no @:skip
	static var r_int = "-?[1-9][0-9]*";
	static var tok =  [                  // a rule set definition
		"[ \t]+" => lex.token(),         // and the "lex" is an instance of this class.
		r_zero + "|" + r_int => CInt,    //
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
	// TODO: "Operator Priority" hasn't been implemented yet.
	static var expr = switch(s) {
		case [e1 = expr, OpPlus, e2 = expr]: e1 + e2;
		case [e1 = expr, OpMinus, e2 = expr]: e1 - e2;
		case [e1 = expr, OpTimes, e2 = expr]: e1 * e2;
		case [e1 = expr, OpDiv, e2 = expr]: e1 / e2;
		case [LParen, e = expr, RParen]: e;
		case [OpMinus, e = expr]: -e;
		case [CInt(n)]: n;
	}

	// for extract n from CInt(n)
	@:ofStr(CInt) static inline function int_of_string(s: String):Int return Std.parseInt(s);
}
