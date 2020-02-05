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
	function new(t, min, max) {
		term = t;
		pmin = min;
		pmax = max;
		// state = lm.LexEngine.INVALID;
	}

	var nxt: Tok<LHS>;

	public inline function pstr():String return '$pmin-$pmax';
}

#if static
@:generic
#end
class Stream<LHS> {

	var h : Tok<LHS>;

	function reclaim(tok: Tok<LHS>) {
		tok.nxt = h;
		h = tok;
	}

	function newTok(term, min, max): Tok<LHS> {
		return if ( h == null ) {
			new Tok(term, min, max);
		} else {
			var t = h;
			h = h.nxt;
			//
			t.term = term;
			t.pmin = min;
			t.pmax = max;
			t;
		}
	}

	var cached: haxe.ds.Vector<Tok<LHS>>;
	var lex: lm.Lexer<Int>;

	var pos: Int;
	var right: Int;
	public var rest(get, never): Int;
	inline function get_rest():Int return right - pos;

	function new(l: lm.Lexer<Int>) {
		lex = l;
		cached = new haxe.ds.Vector<Tok<LHS>>(128);
		right = 0;
		pos = 0;
	}

	/**
	 for example: `.peek(0)` will get the next token from stream`
	*/
	public function peek(i: Int):Tok<LHS> {
		while (rest <= i) {
			var t = lex.token();
			cached[right++] = newTok(t, lex.pmin, lex.pmax);
		}
		return cached[pos + i];
	}

	/**
	 `if n > 0` then discard N token from stream
	*/
	public function junk(n: Int) {
		if (n <= 0) return;
		if (rest >= n) {
			var i = n;
			while (i-- > 0)
				reclaim(cached[pos + i]);

			i = pos;
			right -= n;
			while (i < right) {
				cached[i] = cached[i + n];
				++ i;
			}
		} else { // let right = pos
			n -= rest;
			while (n-- > 0)
				lex.token();
			while (right > pos)
				reclaim(cached[--right]);
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
			cached[right++] = newTok(t, lex.pmin, lex.pmax);
		}
		return cached[pos++];
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
		-- w;
		var i = w;
		while (i-- > 0)
			reclaim(cached[pos + i]);
		// fast junk(w - 1)
		right -= w;
		i = pos;
		while (i < right) {
			cached[i] = cached[i + w];
			++ i;
		}
		return t;
	}
	function reduceEP(lv): Tok<LHS> {
		var prev = cached[pos - 1];
		var t = newTok(lv, prev.pmax, prev.pmax);
		unshift(t);
		return t;
	}
	inline function unshift(t: Tok<LHS>) {
		var i = right;
		while (--i >= pos)
			cached[i + 1] = cached[i];
		cached[pos] = t;
		++pos;
		++right;
	}
}
