package lm;

class Tok {
	public var state: Int;
	public var term: Int;  // terminal & non-terminal value
	public var pmin: Int;
	public var pmax: Int;
	public var val: Dynamic;
	public function new(t, min, max) {
		term = t;
		pmin = min;
		pmax = max;
		// state = lm.LexEngine.INVALID;
	}
	public function getPosition(): lm.Position {
		return new lm.Position(pmin, pmax);
	}
}

class Stream {

	static inline var FULL = 128;

	var cached: haxe.ds.Vector<Tok>;
	var lex: lm.Lexer<Int>;

	public var pos(default, null): Int;
	public var right(default, null): Int;
	public var rest(get, never): Int;
	inline function get_rest():Int return right - pos;

	public function new(l: lm.Lexer<Int>, s: Int) {
		lex = l;
		cached = new haxe.ds.Vector<Tok>(FULL);
		cached[0] = new Tok(0, 0, 0);
		cached[0].state = s;
		right = 1;
		pos = 1;
	}

	public function destroy() {
		lex = null;
		cached = null;
	}

	public function peek(i: Int):Tok {
		while (rest <= i) {
			var t = lex.token();
			cached[right++] = new Tok(t, lex.pmin, lex.pmax);
		}
		return cached[pos + i];
	}

	public function junk(n: Int) {
		if (n <= 0) {
			right = pos;
		} else if (rest >= n) {
			right -= n;
			for (i in pos...right)
				cached[i] = cached[i + n];
		} else {
			n -= rest;
			while (n-- > 0)
				lex.token();
			right = pos;
		}
	}

	inline function str(t: Tok):String return lex.getString(t.pmin, t.pmax - t.pmin);
	inline function stri(dx):String return str( offset(dx) );
	inline function offset(i: Int) return cached[pos + i];  // unsafe

	function next() {
		if (right == pos) {
			var t = lex.token();
			cached[right++] = new Tok(t, lex.pmin, lex.pmax);
		}
		return cached[pos++];
	}

	function rollback(dx: Int) {
		pos -= dx;
		right = pos;
		@:privateAccess lex.pmax = cached[pos].pmin; // hack
	}

	function reduce(lv, w) {
		var pmax = offset(-1).pmax;
		pos -= w;
		var t = cached[pos];
		t.term = lv;
		t.pmax = pmax;
		++ pos;
		// fast junk(w - 1)
		w -= 1;
		right -= w;
		for (i in pos...right)
			cached[i] = cached[i + w];
	}
}
