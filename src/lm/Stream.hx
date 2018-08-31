package lm;

class STok {
	public var char: Int;    // value of enum abstract (Int)
	public var str: String;
	public var pos: lm.Position;
	public function new(c, v, p) {
		char = i;
		str = v;
		pos = p;
	}
}

class Stream {
	var cached: Array<STok>;
	var lex: lm.Lexer<Int>; // force Int type for Parser
	public var len(default, null): Int;
	public function new(l: lm.Lexer<Int>) {
		len = 0;
		lex = l;
		cached = [];
		cached.resize(32);
	}
	public function peek(i: Int):STok {
		while (len <= i) {
			var t = lex.token();
			cached[len++] = new STok(t, lex.current, lex.curpos()); // Eof delecting
		}
		return cached[i];
	}
	public function junk(x: Int) {
		if (x <= 0) return;
		if (len >= x) {
			for (i in 0...len - x)
				cached[i] = cached[x + i];
		} else {
			x -= len;
			while (x-- > 0)
				lex.token();
		}
		len = 0;
	}
}
