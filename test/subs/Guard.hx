package subs;

class Guard {
	static public function main() {
		var str = 'ab';
		var lex = new Lexer(lms.ByteData.ofString(str));
		var par = new Parser(lex);
		eq(par.main() == 303);
	}
	static function eq(b, ?pos: haxe.PosInfos) if (!b) throw lm.Utils.error("ERROR in " + pos);
}
private enum abstract Token(Int) to Int {
	var Eof = 0;
	var A;
	var B;
}
@:rule(Eof, 127) private class Lexer implements lm.Lexer<Token> {
	static var tok =  [
		"a" => A,
		"b" => B,
	];
}
private class Parser implements lm.LR0<Lexer, Int> {
	static var main = switch(s) {
		case [e1 = expr, Eof]: e1;
		case [e1 = expr, e2 = expr, Eof]: e1 + e2;
	}
	static var expr = switch(s) {
		case [A, B] if (Math.random() > 0.5): 303;
		case [A]: 101;
		case [B]: 202;
	}
}
