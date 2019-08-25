package subs;

class LexVoid {
	static function main() {
		var data = lms.ByteData.ofString("abc");
		var lex = new Lexer(data);
		lex.token();
	}
}

private class Lexer implements lm.Lexer<Void> {
	static var tok = [
		"[a-zA-Z_]+" => trace("ident: " + lex.current),
		"[0-9]+"     => trace("int: "   + lex.current),
		null         => trace("unMatched"),
	];
}
