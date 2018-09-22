package lm;

class Tok {
	public var char: Int;
	public var pos: lm.Position;
	public var val: Dynamic; // TODO
	public function new(c, p) {
		char = c;
		pos = p;
	}
	public inline function puinon(p2: lm.Position):Void this.pos = Stream.punion(this.pos, p2);
}

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
		if (len == i) return pull();
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

	function unsafeGet(r: Int) {
		if (r == right)
			return pull();
		return cached[r];
	}

	function rollback(r: Int, n: Int) {
		r -= n;
		if (r < 0) {
			r += FULL;
			lvl = 0;
		}
		right = r;
		lex.setPosition(cached[r].pos.pmin);
	}

	function reduce(r: Int, n: Int, value: Dynamic): Tok {


		throw "TODO";
	}

	inline function strpos(dx) {
		var pos = cached[right + dx].pos;
		return lex.getString(pos.pmin, pos.plen);
	}

	public static function punion(p1:lm.Position, p2:lm.Position):lm.Position {
		var pmin = lm.Utils.imin(p1.pmin, p2.pmin);
		var pmax = lm.Utils.imax(p1.pmax, p2.pmax);
		return new lm.Position(pmin, pmax);
	}
}
