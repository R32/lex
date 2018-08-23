package lm;

#if large_pos

class Position {
	public var pmin(default, never):Int;
	public var pmax(default, never):Int;
	public var plen(get, never)
	inline function get_plen():Int; return pmax - pmin;
	public function new(min, max) {
		pmin = min;
		pmax = max;
	}
	inline public function toString() return '$pmin-$pmax';
}

#else

extern abstract Position(Int) {
	var pmin(get, never):Int; // 20BIT, 0xFFFFF -> 1048575
	var plen(get, never):Int; // 12BIT, 0xFFF   -> 4095
	var pmax(get, never):Int;
	private inline function get_pmin():Int return this & 0xFFFFF;
	private inline function get_plen():Int return this >>> 20;
	private inline function get_pmax():Int return pmin+plen;
	inline function new(min:Int, max:Int) this = min | (max-min) << 20;
	inline function toString():String return '$pmin-$pmax';
}

#end