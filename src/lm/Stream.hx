package lm;

class Tok {
	public var state: Int;
	public var char: Int;
	public var pmin: Int;
	public var pmax: Int;
	public var val: Dynamic;
	public function new(c, min, max) {
		char = c;
		pmin = min;
		pmax = max;
		// state = lm.LexEngine.INVALID;
	}
	public inline function setState(s) state = s;
}

class Stream {

	static inline var FULL = 128;

	var cached: haxe.ds.Vector<Tok>;
	var lex: lm.Lexer<Int>;

	var base: Int;
	public var right(default, null): Int;
	public var length(get, never): Int;
	inline function get_length():Int return right - base;

	public function new(l: lm.Lexer<Int>, s: Int) {
		lex = l;
		cached = new haxe.ds.Vector<Tok>(FULL);
		right = 1;
		base = 1;
		cached[0] = new Tok(0, 0, 0);
		cached[0].state = s;
	}

	public function destroy() {
		lex = null;
		cached = null;
	}

	public function peek(i: Int):Tok {
		while (length <= i) {
			var t = lex.token();
			cached[right++] = new Tok(t, lex.pmin, lex.pmax);
		}
		return cached[right - 1];
	}

	public function junk(n: Int) {
		if (n <= 0) {
			right = base;
		} else if (right >= n) {
			for (i in 0...length - n)
				cached[i] = cached[n + 1];
			right -= n;
		} else {
			n -= length;
			while (n-- > 0)
				lex.token();
			right = base;
		}
	}

	function get(r) {
		if (right == r) {
			var t = lex.token();
			cached[right++] = new Tok(t, lex.pmin, lex.pmax);
		}
		return cached[r];
	}

	inline function str(t: Tok):String return lex.getString(t.pmin, t.pmax - t.pmin);
	inline function limit() base = right;                  // before the user calls s.peek(N)
	inline function unlimited() {right = base;  base = 1;} // restore.
	inline function last() return cached[right - 1];

	function rollback(left) {
		right = left;
		@:privateAccess lex.pmax = cached[left].pmin;
		return cached[left - 1].state;
	}

	function reduce(r, char, value) {
		var t = cached[r];
		t.char = char;
		t.val = value;
		t.pmax = cached[r - 1].pmax; // punion
		right = r + 1;
		return t;
	}
}

/*
class Stream {

	static inline var FULL = 256;

	var cached: haxe.ds.Vector<Tok>;
	var lex: lm.Lexer<Int>;

	var lvl: Int;    // 0 or FULL
	var base: Int;
	public var right(default, null): Int;
	inline function length() return right + lvl - base;

	public function new(l: lm.Lexer<Int>) {
		lex = l;
		cached = new haxe.ds.Vector<Tok>(FULL);
		reset();
	}

	public function reset() {
		lvl = 0;
		base = 0;
		right = 0;
	}

	public function destroy() {
		lex = null;
		cached = null;
	}

	function pull() {
		if (right == FULL) {
			if (base == FULL && lvl > 0) base = 0;
			right = 0;
			lvl = FULL;
		}
		if (right == base && lvl > 0) ++base;
		var t = lex.token();
		cached[right] = new Tok(t, lex.curpos());
		return cached[right++];
	}

	public function peek(i: Int):Tok {
		var len = this.length();
		while (len <= i) {
			pull();
			++ len;
		}
		i += base;
		if (i >= FULL) i -= FULL;
		return cached[i];
	}

	public function junk(x: Int) {
		var len = this.length();
		if ( x <= 0 ) { // then junk(len)
			if (base >= right) lvl = 0;
			base = right;
		} else if ( len >= x ) {
			shift(x);
		} else {
			x -= len;
			while (x-- > 0)
				pull();
			if (base >= right) lvl = 0;
			base = right;
		}
	}

	//////// unsafe method set

	inline function shift(n: Int) {
		base += n;
		if (base >= FULL) {
			base -= FULL;
			lvl = 0;
		}
	}

	function rollback(left: Int) {
		if (left < 0) {
			left += FULL;
			lvl = 0;
		}
		right = left;
		lex.setPosition(cached[left].pos.pmin);
	}

	function unsafeGet(r: Int) {
		if (r == right)
			return pull();
		return cached[r];
	}

	function reduce(r: Int, n: Int, char: Int, value: Dynamic): Tok {
		var last = r - 1;
		if (last < 0) last += FULL;
		r -= n;
		if (r < 0) {
			r += FULL;
			lvl = 0;
		}
		cached[r].char = char;
		cached[r].val = value;
		cached[r].pos = new lm.Position(cached[r].pos.pmin, cached[last].pos.pmax);
		right = r + 1;
		return cached[r];
	}

	function str(r) {
		if (r < 0) r += FULL;
		var pos = cached[r].pos;
		return lex.getString(pos.pmin, pos.plen);
	}

	function pos(r) {
		if (r < 0) r += FULL;
		return cached[r].pos;
	}

	//public static function punion(p1:lm.Position, p2:lm.Position):lm.Position {
	//	var pmin = lm.Utils.imin(p1.pmin, p2.pmin);
	//	var pmax = lm.Utils.imax(p1.pmax, p2.pmax);
	//	return new lm.Position(pmin, pmax);
	//}
}
*/