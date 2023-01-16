package subs;

class LexVarIdent {
	public static function main() {
		var str = 'var varibles .1235e-5 0e+7 9e10 3.1446 0 101 .0 %% && ##';
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
	var CFloat;
	var Others;
}

@:rule(Eof, 127) private class Lexer implements lm.Lexer<Token> {

	static var exp = "[eE][+-]?[0-9]+";
	static var integer = "0|[1-9][0-9]*";
	static var floatpoint = ".[0-9]+|[0-9]+.[0-9]*";

	static var tok = [
		integer + Opt(exp) | floatpoint + Opt(exp) => CFloat,
		"[ \t\n]+" => lex.token(),
		"var" => KwdVar,
		"[a-zA-Z_][-a-zA-Z0-9_]*" => CIdent,
		//	"##|%%|&&" => Others,
		"%%|&&|##" => {
			var t = Others;
			t;
		}
	];
}