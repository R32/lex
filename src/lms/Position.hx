package lms;

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
	var pmin(get, never):Int; // 16BIT, 0xFFFF -> 65535
	var plen(get, never):Int; // 16BIT, 0xFFFF -> 65535
	var pmax(get, never):Int;
	private inline function get_pmin():Int return this & 0xFFFF;
	private inline function get_plen():Int return pmax - pmin;
	private inline function get_pmax():Int return this >>> 16;
	inline function new(min:Int, max:Int) this = min | max << 16;
	inline function toString():String return '$pmin-$pmax';
}

#end