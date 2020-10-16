package subs;

@:analyzer(no_optimize)
class Demo {
	static function main() {
		var str = '1 - 2 * (3 + 4) + 5 * 6';
		var lex = new Lexer(lms.ByteData.ofString(str));
		var par = new Parser(lex);
		eq(par.main() == (1 - 2 * (3 + 4) + 5 * 6));
	}
	static function eq(b, ?pos: haxe.PosInfos) if (!b) throw("ERROR in " + pos);
}

// The lm.LR0 Parser only works with "enum abstract (Int) to Int"
private enum abstract Token(Int) to Int {
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
* @:rule(EOF, cmax = 255) See the example below:
*	Eof is a custom terminator which is defined in "<Token>" (required)
*	127 is the custom maximum char value. (optional, default is 255)
*/
@:rule(Eof, 127) private class Lexer implements lm.Lexer<Token> {
	static var r_zero = "0";             // static variable will be treated as rules if there is no `@:skip`
	static var r_int = "[1-9][0-9]*";
	static var tok =  [                  // a rule set definition, the first definition will become .token()
		"[ \t]+" => lex.token(),         // "lex" is an instance of this class.
		r_zero + "|" + r_int => CInt,    //
		"+" => OpPlus,
		"-" => OpMinus,
		"*" => OpTimes,
		"/" => OpDiv,
		"(" => LParen,
		")" => RParen,
	];
}

@:rule({
	start: [main],            // Specify start, like the "%start" in ocamlyacc, If not specified, the first "switch" will be selected
	left: ["+", "-"],         // The parser could auto reflect(str) => Token
	left: [OpTimes, OpDiv],   // The lower have higher priority.
	nonassoc: [UMINUS],       // The placeholder must be uppercase
}) class Parser implements lm.LR0<Lexer, Int> {

	static var main = switch(s) {  // the "s" is instance of lm.Stream
		case [e = expr, Eof]: e;
	}

	static var expr = switch(s) {
		case [e1 = expr, op = [OpPlus,OpMinus], e2 = expr]: op == OpPlus ? e1 + e2 : e1 - e2;
		case [e1 = expr, OpTimes, e2 = expr]: e1 * e2;
		case [e1 = expr, OpDiv, e2 = expr]: Std.int(e1 / e2);
		case [LParen, e = expr, RParen]: e;
		case [@:prec(UMINUS) OpMinus, e = expr]: -e;   // %prec UMINUS
		case [CInt(n)]: n;
	}

	// define custom extract function for CInt(n)
	@:rule(CInt) static inline function int_of_string(s: String):Int return Std.parseInt(s);
	// if the custom function has 2 params then the type of the second argument is :lm.Stream.Tok<AUTO>.
	// @:rule(CInt) static inline function int_of_string(input:lms.ByteData, t):Int {
	//	  return Std.parseInt( input.readString(t.pmin, t.pmax - t.pmin) );
	//}
}
