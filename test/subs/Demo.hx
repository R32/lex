package subs;

@:analyzer(no_optimize)
class Demo {
	static function main() {
		var str = '1 - 2 * (3 + 4) + 5 * Unexpected 6';
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
	var CIdent;
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
		r_zero | r_int => CInt,    //
		"+" => OpPlus,
		"-" => OpMinus,
		"*" => OpTimes,
		"/" => OpDiv,
		"(" => LParen,
		")" => RParen,
		"[a-zA-Z_]+" => CIdent,
	];
}

@:rule({
	start: [main],            // Specify start, like the "%start" in ocamlyacc, If not specified, the first "switch" will be selected
	left: ["+", "-"],         // The parser could auto reflect(str) => Token
	left: [OpTimes, OpDiv],   // The lower have higher priority.
	nonassoc: [UMINUS],       // The placeholder must be uppercase
}) class Parser implements lm.SLR<Lexer> {

	var main = switch(s) {
		case [e = expr, Eof]:
			e;
		default:              // place handling error code here
			var t = stream.peek(0);
			switch(t.term) {
			case Eof:
				return 0;
			case CIdent:                // Show recovery from errors, Note: this ability is very weak
				stream.junk(1);         // Discard current token
				slrloop( -1, MAIN_EXP); // main => MAIN_EXP, NOTE: Only the entry switch-case (Specified in "%start") has an EXP value
			default:
				throw "Unexpected: " + stream.str(t);
			}
	}

	var expr : Int = switch(s) {        // Specify Type explicitly
		case [e1 = expr, op = [OpPlus,OpMinus], e2 = expr]: op == OpPlus ? e1 + e2 : e1 - e2;
		case [e1 = expr, OpTimes, e2 = expr]: e1 * e2;
		case [e1 = expr, OpDiv, e2 = expr]: Std.int(e1 / e2);
		case [LParen, e = expr, RParen]: e;
		case [@:prec(UMINUS) OpMinus, e = expr]: -e;   // %prec UMINUS
		case [CInt(n)]: n;
	}

	// define custom extract function for CInt(n)
	@:rule(CInt) inline function int_of_string(s: String):Int return Std.parseInt(s);
	// OR @:rule(CInt) inline function int_of_string( input : lms.ByteData, t : lm.Stream.Tok ) : Int {
	//	  return Std.parseInt( input.readString(t.pmin, t.pmax - t.pmin) );
	//}
}
