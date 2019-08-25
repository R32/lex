package subs;

class LexVoid {
	static function main() {
		var data = lms.ByteData.ofString("abc123~");
		var max = data.length;
		var lex = new Lexer(data);
		try {
			while (lex.pmax < max) {
				lex.token();
			}
		} catch (e: Dynamic) {
			trace(e);
		}
	}
}

private class Lexer implements lm.Lexer<Void> {
	static var tok = [
		"[a-zA-Z_]+" => trace("ident: " + lex.current),
		"[0-9]+"     => trace("int: "   + lex.current),
		null => {
			trace("UnMatched: \"" + lex.getString(lex.pmax, lex.pmin - lex.pmax) + "\"");
			throw "Exit";
		}
	];
}
