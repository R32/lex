package lm;

class Utils {
	static public inline function imin(a: Int, b: Int) return a < b ? a: b;
	static public inline function imax(a: Int, b: Int) return a > b ? a: b;
	static public inline function error(s: String) return #if js new js.Error(s); #else return s; #end

	static public inline function line(input: lms.ByteData, pmin: Int) {
		var line = 1;
		var char = 0;
		var i = 0;
		while (i < pmin) {
			var c = input.readByte(i++);
			if (c == "\n".code) {
				char = 0;
				++ line;
			} else {
				++ char;
			}
		}
		return new lms.Position(line, char);
	}
}