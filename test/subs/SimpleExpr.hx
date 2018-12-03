package subs;

class SimpleExpr {
	static public function main() {
		var str = '1 - 2 * (3 + 4) + 5 * 6';
		var lex = new Lexer(lms.ByteData.ofString(str));
		var par = new Parser(lex);
		eq(par.main() == 1 - 2 * (3 + 4) + 5 * 6);
	}
	static function eq(b, ?pos: haxe.PosInfos) if (!b) throw lm.Utils.error("ERROR in " + pos);
}

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

@:rule(Eof, 127) private class Lexer implements lm.Lexer<Token> {
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

@:rule({
	left: [OpPlus, OpMinus],
	left: [OpTimes, OpDiv],
	nonassoc: [UMINUS],
}) private class Parser implements lm.LR0<Lexer, Int> {

	static var main = switch(s) {
		case [e = expr, Eof]: e;
	}
	static var expr = switch(s) {
		case [e1 = expr, op = [OpPlus, OpMinus], e2 = expr]: op == OpPlus ? e1 + e2 : e1 - e2;
		case [e1 = expr, OpTimes, e2 = expr]: e1 * e2;
		case [e1 = expr, OpDiv, e2 = expr]: Std.int(e1 / e2);
		case [LParen, e = expr, RParen]: e;
		case [@:prec(UMINUS) OpMinus, e = expr]: -e;
		case [CInt(n)]: n;
	}

	@:rule(CInt) static inline function int_of_string(s: String):Int return Std.parseInt(s);
}