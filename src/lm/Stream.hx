package lm;

@:allow(lm.Stream)
class Tok implements lm.Lexer.Position {
	public var state(default, null) : Int;
	public var term(default, null) : Int;  // terminal & non-terminal value
	public var pmin(default, null) : Int;
	public var pmax(default, null) : Int;
	public var val(default, null) : Dynamic;
	function new( t, min , max ) {
		term = t;
		pmin = min;
		pmax = max;
		// state = lm.LexEngine.INVALID;
	}
	var nxt : Tok;
}

class Stream {

	var h : Tok;

	function reclaim( tok : Tok ) {
		tok.nxt = h;
		h = tok;
	}

	function newTok( term, min, max ) : Tok {
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

	var cached : haxe.ds.Vector<Tok>;
	var lex : lm.Lexer<Int>;

	var pos : Int;
	var right : Int;
	public var rest(get, never) : Int;
	inline function get_rest() : Int return right - pos;

	function new( l : lm.Lexer<Int> ) {
		lex = l;
		cached = new haxe.ds.Vector<Tok>(64);
		right = 0;
		pos = 0;
	}

	/**
	 for example: `.peek(0)` will get the next token from stream`
	*/
	public function peek( i : Int ) : Tok {
		while (rest <= i) {
			var t = lex.token();
			cached[right++] = newTok(t, lex.pmin, lex.pmax);
		}
		return cached[pos + i];
	}

	/**
	 `if n > 0` then discard N token from stream
	*/
	public function junk( n : Int ) {
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

	function stri(dx) : String return str( offset(dx) );

	public function error( msg : String, t : Tok ) return msg + lm.Utils.posString(t.pmin, lex.input);

	public inline function UnExpected( t : Tok ) return error('Unexpected "' + str(t) + '"', t);

	public inline function str( t : Tok ):String return lex.getString(t.pmin, t.pmax - t.pmin);

	inline function offset( i : Int ) return cached[pos + i];  // unsafe

	function next() {
		if (right == pos) {
			var t = lex.token();
			cached[right++] = newTok(t, lex.pmin, lex.pmax);
		}
		return cached[pos++];
	}

	function reduce(lvw) : Tok {// lvw == lv << 8 | w;
		var w = lvw & 0xFF;
		if (w == 0)
			return reduceEP(lvw >>> 8);
		var pmax = offset( -1).pmax; // save pmax before update stream->pos
		w--;                         // reserve 1 block
		pos -= w;                    // update
		right -= w;
		var t = offset( -1);         // related to the reserved block
		t.term = lvw >>> 8;
		t.pmax = pmax;
		if (w == 0)
			return t;
		var i = w;
		while (i-- > 0)
			reclaim(cached[pos + i]);
		i = pos;
		while (i < right) {
			cached[i] = cached[i + w];
			++ i;
		}
		return t;
	}

	function reduceEP(lv) : Tok {
		var prev = cached[pos - 1];
		var t = newTok(lv, prev.pmax, prev.pmax);
		unshift(t);
		return t;
	}
	inline function unshift( t : Tok ) {
		var i = right;
		while (--i >= pos)
			cached[i + 1] = cached[i];
		cached[pos] = t;
		++pos;
		++right;
	}
}
