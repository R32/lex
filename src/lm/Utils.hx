package lm;

class Utils {
	static public inline function imin(a: Int, b: Int) return a < b ? a: b;
	static public inline function imax(a: Int, b: Int) return a > b ? a: b;

	macro static public function swapi(a, b) return macro @:mergeBlock {
		$b += $a;
		$a = $b - $a;
		$b -= $a;
	}
}