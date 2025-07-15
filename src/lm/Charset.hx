package lm;

extern abstract Char(Int) {

	var min(get, never) : Int;

	private inline function get_min() : Int return this & 0xFFFF;

	var max(get, never) : Int;
	private inline function get_max() : Int return (this >>> 16);

	inline function new( i : Int ) this = i;

	inline function copy() : Char return new Char(this);

	inline function toInt() : Int return this;

	inline function toString() : String return '[$min, $max]';

	static inline function RANGE( min : Int, max : Int ) : Char return new Char(min | max << 16);
}

// @:using(lm.Charset.CSet)
typedef Charset = Array<Char>;

/*
 * Helps for Charset
 */
class CSet {

	static public var C_EMPTY : Charset = [];

	static public var C_255 : Charset = [CHAR(0, 255)]; // [0-255]

	static public inline function single( c : Int ) : Charset return [CHAR(c, c)];

	static public inline function isSingle( cs : Charset ) : Bool return cs.length == 1 && cs[0].min == cs[0].max;

	static public inline function CHAR( min : Int, max : Int) return Char.RANGE(min, max);

	static public function complement( cs : Charset, ?all : Charset ) : Charset {
		if (all == null) all = C_255;
		if (cs.length == 0) return all.copy();
		var res = [];
		var left = all[0].min - 1; // -1
		for (c in cs) {
			if (c.min > left + 1)
				res.push(CHAR(left + 1, c.min - 1));
			if (left < c.max)
				left = c.max;
		}
		var right = all[all.length - 1].max;
		if (left < right)
			res.push(CHAR(left + 1, right));
		return res;
	}
	static public function union( c1 : Charset, c2 : Charset ) : Charset {
		if (c1.length == 0)
			return c2.copy();
		if (c2.length == 0)
			return c1.copy();
		var res = [];
		var i = 0, j = 0;
		var curr = if (c1[0].min < c2[0].min) {
			i = 1;
			c1[0].copy();
		} else {
			j = 1;
			c2[0].copy();
		}
		while (i < c1.length || j < c2.length) {
			var next = if (i < c1.length && (j >= c2.length || c1[i].min < c2[j].min)) {
				c1[i++];
			} else {
				c2[j++];
			}
			if (next.min <= 1 + curr.max) { // merge (1, 5) (6, 8) => (1, 8)
				if (curr.max < next.max)
					curr = CHAR(curr.min, next.max);
			} else {
				res.push(curr);
				curr = next.copy();
			}
		}
		res.push(curr);
		return res;
	}
	static public function inter( c1 : Charset, c2 : Charset ) : Charset {
		if (c1.length == 0 || c2.length == 0)
			return [];
		var res = [];
		var i = 0, j = 0;
		while (i < c1.length && j < c2.length) {
			var a = c1[i];
			var b = c2[j];
			var start = Utils.imax(a.min, b.min);
			var end = Utils.imin(a.max, b.max);
			if (start <= end) {
				res.push(CHAR(start, end));
			}
			if (a.max < b.max) {
				i++;
			} else {
				j++;
			}
		}
		return res;
	}
	static public function diff( cset : Charset, sub : Charset ) : Charset {
		if (cset.length == 0) return [];
		if (sub.length == 0) return cset.copy();
		var res = [];
		var i = 0, j = 0;
		var curr = cset[0].copy();
		while (i < cset.length && j < sub.length) {
			var s = sub[j];
			//        |--C--|
			//               |--S--|
			if (curr.max < s.min) {
				res.push(curr);
				curr = ++i < cset.length ? cset[i].copy() : null;
				continue;
			}
			//        |--C--|
			// |--S--|
			if (curr.min > s.max) {
				j++;
				continue;
			}
			//        |--C..
			//           |--S..
			if (curr.min < s.min) {
				res.push(CHAR(curr.min, s.min - 1));
			}
			//         ..C--|
			//      ..S--|
			if (curr.max > s.max) {
				curr = CHAR(s.max + 1, curr.max);
				j++;
			} else {
			//         ..C--|
			//           ..S--|
				curr = ++i < cset.length ? cset[i].copy() : null;
			}
		}
		if (curr != null)
			res.push(curr);
		while (++i < cset.length) {
			res.push(cset[i].copy());
		}
		return res;
	}
	/*
	 * return a new sorted charset.
	 */
	static public function sorting( cs : Charset ) : Charset {
		var r = [];
		for (c in cs)
			r = union(r, [c]);
		return r;
	}
	static public function string( cs : Charset ) : String {
		var res = [];
		for (i in 0...cs.length) {
			res[i] = cs[i].toString();
		}
		return "{" + res.join(",") + "}";
	}
}
