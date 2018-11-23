package lm;

typedef Charset = Array<Char>;

extern abstract Char(Int) {
	var min(get, never):Int; // 0~7
	private inline function get_min():Int return this & MAX;

	var max(get, never):Int; // 16~23
	private inline function get_max():Int return (this >>> 16);

	var ext(get, never):Int; // 8~15
	private inline function get_ext():Int return (this >> 8) & MAX;

	inline function new(min: Int, max: Int) this = min | max << 16;

	inline function duplicate():Char return cast this;
	inline function toInt():Int return this;
	inline function toString():String return '[$min, $max]';

	static inline function ofInt(i: Int):Char return cast i;
	static inline function c3(min: Int, max: Int, ext:Int):Char return cast min | ext << 8 | max << 16;
	static inline var MAX = 255;
}

/**
 Helps for Charset
*/
class CSet {

	static public var C_EMPTY: Charset = [];

	static public var C_255: Charset = [new Char(0, Char.MAX)]; // [0-255]

	static public inline function single(c: Int):Charset return [new Char(c, c)];

	static public inline function isSingle(cs: Charset):Bool return cs.length == 1 && cs[0].min == cs[0].max;

	static public function complement(c: Charset, ?all: Charset): Charset {
		var i = 0, len = c.length;
		if (all == null)
			all = C_255;
		if (len == 0) return all;
		var start = all[0].min;
		var cmax = all[all.length - 1].max;
		if (c[0].min == 0) {
			start = c[0].max + 1;
			++ i;
		}
		var ret = [];
		while (i < len) {
			ret.push(new Char(start, c[i].min - 1));
			start = c[i].max + 1;
			++ i;
		}
		if (start <= cmax)
			ret.push(new Char(start, cmax));
		return ret;
	}

	static public function union(c1: Charset, c2: Charset): Charset {
		if (c1.length == 0) return c2;
		if (c2.length == 0) return c1;
		var a_min = c1[0].min;
		var a_max = c1[0].max;
		var b_min = c2[0].min;
		var b_max = c2[0].max;
		var i = 1, j = 1;
		var ret = [];
		while (true) {
			if (a_min <= b_min) {
				if (a_max < b_max) {
					if (a_max + 1 < b_min) {
						ret.push(new Char(a_min, a_max));
					} else {
						b_min = a_min;
					}
					if (i < c1.length) {
						a_min = c1[i].min;
						a_max = c1[i++].max;
					} else {
						ret.push(new Char(b_min, b_max));
						while (j < c2.length) {
							ret.push(c2[j++].duplicate());
						}
						break;
					}
				} else {
					if (j < c2.length) {
						b_min = c2[j].min;
						b_max = c2[j++].max;
					} else {
						ret.push(new Char(a_min, a_max));
						while (i < c1.length) {
							ret.push(c1[i++].duplicate());
						}
						break;
					}
				}
			} else {
				var t = c1;
				c1 = c2;
				c2 = t;

				var ti = i;
				i = j;
				j = ti;

				ti = a_min;
				a_min = b_min;
				b_min = ti;

				ti = a_max;
				a_max = b_max;
				b_max = ti;
			}
		}
		return ret;
	}

	static public function inter(c1: Charset, c2: Charset): Charset {
		//return ccomplement(cunion(ccomplement(c1), ccomplement(c2)));
		if (c1.length == 0 || c2.length == 0) return C_EMPTY;
		var a_min = c1[0].min;
		var a_max = c1[0].max;
		var b_min = c2[0].min;
		var b_max = c2[0].max;
		var i = 1, j = 1;
		var ret = [];
		while (true) {
			if (a_max <= b_max) {
				var left = Utils.imax(a_min, b_min);
				if (a_max >= left) {
					var right = Utils.imin(a_max, b_max);
					ret.push(new Char(left, right));
					if (right < b_max) {
						b_min = right + 1;
					} else {
						if (j < c2.length) {
							b_min = c2[j].min;
							b_max = c2[j++].max;
						} else {
							break;
						}
					}
				}
				if (i < c1.length) {
					a_min = c1[i].min;
					a_max = c1[i++].max;
				} else {
					break;
				}
			} else {
				var t = c1;
				c1 = c2;
				c2 = t;

				var ti = i;
				i = j;
				j = ti;

				ti = a_min;
				a_min = b_min;
				b_min = ti;

				ti = a_max;
				a_max = b_max;
				b_max = ti;
			}
		}
		return ret;
	}

	static public function diff(par: Charset, sub: Charset): Charset {
		//return ccomplement(cunion(ccomplement(par), sub));
		if (par.length == 0) return C_EMPTY;
		if (sub.length == 0) return par;
		var p_min = par[0].min;
		var p_max = par[0].max;
		var s_min = sub[0].min;
		var s_max = sub[0].max;
		var i = 1, j = 1;
		var ret = [];
		while (true) {
			if (p_max >= s_max) {
				if (p_min < s_min)
					ret.push(new Char(p_min, s_min - 1));
				if (p_min <= s_max)
					p_min = s_max + 1;
				if (j < sub.length) {
					s_min = sub[j].min;
					s_max = sub[j++].max;
				} else {
					if (p_min <= p_max)
						ret.push(new Char(p_min, p_max));
					while (i < par.length) {
						ret.push(par[i++].duplicate());
					}
					break;
				}
			} else {
				if (p_max >= s_min)
					p_max = s_min - 1;
				if (p_min <= p_max)
					ret.push(new Char(p_min, p_max));
				if (i < par.length) {
					p_min = par[i].min;
					p_max = par[i++].max;
				} else {
					break;
				}
			}
		}
		return ret;
	}

	/**
	 return a new sorted charset. the param will not be changed.
	*/
	static public function sorting(cs: Charset): Charset {
		var r = C_EMPTY;
		for (c in cs)
			r = union(r, [c]);
		return r;
	}

	static public function string(cs: Charset): String {
		var ret = [];
		for (i in 0...cs.length) {
			ret[i] = cs[i].toString();
		}
		return "{" + ret.join(",") + "}";
	}
}
