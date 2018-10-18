package subs;

class LexRollback {
	public static function main() {
		var str = 'casetiny casa';
		var lex = new Lexer(lms.ByteData.ofString(str));
		var a = [];
		while (true) {
			var t = lex.token();
			if (t == Eof)
				break;
			var s = switch (t) {
			case Eof: "$";
			case Ca: "ca";
			case Sa: "sa";
			case Case: "case";
			case Tiny: "tiny";
			case CaseTinx: "casetinx";
			}
			a.push(s);
		}
		eq(a.join("--") == "case--tiny--ca--sa");
	}
	static function eq(b, ?pos: haxe.PosInfos) if (!b) throw lm.Utils.error("ERROR in " + pos);
}

private enum abstract Token(Int) to Int {
	var Eof = 0;
	var Ca;
	var Sa;
	var Case;
	var Tiny;
	var CaseTinx;
}

@:rule(Eof, 127) private class Lexer implements lm.Lexer<Token> {
	static var tok =  [
		"[ \t\n]+" => lex.token(),
		"ca" => Ca,
		"sa" => Sa,
		"case" => Case,
		"tiny" => Tiny,
		"casetinx" => CaseTinx
	];
}
