package subs;

class GuardEpsilon {
	static public function main() {
		var str = 'abc(xy)';
		var lex = new Lexer(lms.ByteData.ofString(str));
		var par = new Parser(lex);
		eq(par.main() == 101);
	}
	static function eq(b, ?pos: haxe.PosInfos) if (!b) throw lm.Utils.error("ERROR in " + pos);
}
private enum abstract Token(Int) to Int {
	var Eof = 0;
	var CIdent;
	var LParen;
	var RParen;
}
@:rule(Eof, 127) private class Lexer implements lm.Lexer<Token> {
	static var tok =  [
		"[ \t]+" => lex.token(),
		"[a-z]+" => CIdent,
		"(" => LParen,
		")" => RParen,
	];
}
private class Parser implements lm.LR0<Lexer, Int> {
	static var main = switch(s) {
		case [e = expr, Eof]: e;
		case [e1 = expr, e2 = expr, Eof]: e1 + e2;
	}
	static var expr = switch(s) {
		case [LParen, CIdent(id), RParen]: 1;
		case [CIdent(id), LParen, a = args, RParen]: 1 + a;
		case [CIdent(id)]: 100;
	}
	static var args = switch(s) {
		case [] if(Math.random() > 0.5):
			expr(s);
	}
	@:rule(CIdent) static inline function cident(s: String):String return s;
}
