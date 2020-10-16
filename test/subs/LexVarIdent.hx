package subs;

class LexVarIdent {
	public static function main() {
		var str = 'var varibles';
		var lex = new Lexer(lms.ByteData.ofString(str));
		var a = [];
		while (true) {
			var t = lex.token();
			if (t == Eof)
				break;
			a.push(t);
		}
		eq(a[0] == KwdVar && a[1] == CIdent);
	}
	static function eq(b, ?pos: haxe.PosInfos) if (!b) throw("ERROR in " + pos);
}

private enum abstract Token(Int) to Int {
	var Eof;
	var KwdVar;
	var CIdent;
}

@:rule(Eof, 127) private class Lexer implements lm.Lexer<Token> {
	static var tok =  [
		"[ \t\n]+" => lex.token(),
		"var" => KwdVar,
		"[a-zA-Z_][-a-zA-Z0-9_]*" => CIdent,
	];
}