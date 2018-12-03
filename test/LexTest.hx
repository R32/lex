package;

class LexTest {
	static function main()  {
		// Sets
		subs.Sets.main();
		// lexer
		lexTest();
		subs.LexVarIdent.main();
		subs.LexRollback.main();
		// parser
		subs.Guard.main();
		subs.GuardEpsilon.main();
		subs.SimpleExpr.main();
	}

	static function lexTest() {
		var lex = new Lexer(lms.ByteData.ofString(' 123 	+ 	456 	 * 23 +  "hello world" 	 + 	 1'));
		var t = lex.token();
		var a = [];
		while (t != Eof) {
			a.push(Lexer.s_token(t));
			t = lex.token();
		}
		if (a.join("") != "123+456*23+hello world+1") throw lm.Utils.error("TODO");
	}
}

//////////////

@:rule(127, Eof) private class Lexer implements lm.Lexer<Token> {
	static var r_zero = "0";
	static var r_int = "-?[1-9][0-9]*";
	static var tok =  [
		"[ \t]+" => lex.token(),
		"+" => Op(Plus),
		"-" => Op(Minus),
		"*" => Op(Times),
		"/" => Op(Div),
		"(" => LParen,
		")" => RParen,
		r_zero + "|" + r_int => CInt(Std.parseInt(lex.current)),
		'"' => lex.str(),
	];
	static var str = [
		'[^"]*' => {
			var s = CStr(lex.current);
			lex.pmax++;
			s;
		}
	];
	static function s_op(o) {
		return switch (o) {
		case Plus: "+";
		case Minus: "-";
		case Times: "*";
		case Div: "/";
		}
	}
	static public function s_token(t) {
		return switch (t) {
		case Eof: "<end of file>";
		case CInt(i): "" + i;
		case Op(op): s_op(op);
		case LParen: "(";
		case RParen: ")";
		case CStr(s): s;
		}
	}
}

private enum Op {
	Plus;
	Minus;
	Times;
	Div;
}

private enum Token {
	Eof;
	CInt(i: Int);
	Op(op: Op);
	LParen;
	RParen;
	CStr(s: String);
}
