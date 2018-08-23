package lm;

class Utils {
	static public inline function imin(a: Int, b: Int) return a < b ? a: b;
	static public inline function imax(a: Int, b: Int) return a > b ? a: b;
	static public inline function error(s: String) return #if js new js.Error(s); #else return s; #end
}