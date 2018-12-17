package lm;

#if static
@:generic
#end
@:allow(lm.Stream)
class Tok<LHS> {
	public var state(default, null): Int;
	public var term(default, null): Int;  // terminal & non-terminal value
	public var pmin(default, null): Int;
	public var pmax(default, null): Int;
	public var val(default, null): LHS;
	public function new(t, min, max) {
		term = t;
		pmin = min;
		pmax = max;
		// state = lm.LexEngine.INVALID;
	}
	public inline function pstr():String return '$pmin-$pmax';
}

#if static
@:generic
#end
class Stream<LHS> {

	var cached: haxe.ds.Vector<Tok<LHS>>;
	var lex: lm.Lexer<Int>;

	var pos: Int;
	var right: Int;
	var rest(get, never): Int;
	inline function get_rest():Int return right - pos;

	public function new(l: lm.Lexer<Int>, s: Int) {
		lex = l;
		cached = new haxe.ds.Vector<Tok<LHS>>(128);
		cached[0] = new Tok<LHS>(0, 0, 0);
		cached[0].state = s;
		right = 1;
		pos = 1;
	}

	public function peek(i: Int):Tok<LHS> {
		while (rest <= i) {
			var t = lex.token();
			cached[right++] = new Tok<LHS>(t, lex.pmin, lex.pmax);
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

	function stri(dx):String return str( offset(dx) );

	public function error(msg:String, t: Tok<LHS>) return lm.Utils.error(msg + lm.Utils.posString(t.pmin, lex.input));

	public inline function UnExpected(t: Tok<LHS>) return error('Unexpected "' + str(t) + '"', t);

	public inline function str(t: Tok<LHS>):String return lex.getString(t.pmin, t.pmax - t.pmin);

	inline function offset(i: Int) return cached[pos + i];  // unsafe

	function next() {
		if (right == pos) {
			var t = lex.token();
			cached[right++] = new Tok<LHS>(t, lex.pmin, lex.pmax);
		}
		return cached[pos++];
	}

	function rollback(dx: Int, maxv: Int) {// if you want set lex.pmax, set maxv = 0, otherwise use MAXVALUE
		pos -= dx;
		dx = pos;
		while (dx < right) {
			if (cached[dx].term >= maxv) { // non-terminal if true then need hack the lex.pmax
				right = dx;
				@:privateAccess lex.pmax = cached[dx].pmin;
				break;
			}
			++ dx;
		}
	}
	function reduce(lvw): Tok<LHS> { // lvw == lv << 8 | w;
		var w = lvw & 0xFF;
		if (w == 0)
			return reduceEP(lvw >>> 8);
		var pmax = offset(-1).pmax;
		pos -= w;
		var t = cached[pos];
		t.term = lvw >>> 8;
		t.pmax = pmax;
		++ pos;
		// fast junk(w - 1)
		-- w;
		right -= w;
		var i = pos;
		while (i < right) {
			cached[i] = cached[i + w];
			++ i;
		}
		return t;
	}
	function reduceEP(lv): Tok<LHS> {
		var prev = cached[pos - 1];
		var t = new Tok<LHS>(lv, prev.pmax, prev.pmax);
		shift(t);
		return t;
	}
	inline function shift(t: Tok<LHS>) {
		var i = right;
		while (--i >= pos)
			cached[i + 1] = cached[i];
		cached[pos] = t;
		++pos;
		++right;
	}
}
