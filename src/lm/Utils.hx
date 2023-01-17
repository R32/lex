package lm;

class Utils {
	static public inline function imin(a: Int, b: Int) return a < b ? a: b;
	static public inline function imax(a: Int, b: Int) return a > b ? a: b;

	#if static @:generic #end
	static public function bsearch<T>( array : Array<T>, key : T, compar : (T, T)->Int ) : Int {
		var sign : Int;
		var pivot : Int;
		var i = 0;
		var j = array.length - 1;
		while (i <= j) {
			pivot = (i + j) >> 1;
			sign = compar(key, array[pivot]);
			if (sign == 0)
				return pivot;
			if (sign < 0) {
				j = pivot - 1;
			} else {
				i = pivot + 1;
			}
		}
		return -1;
	}

	static public function onIntCompar( a : Int, b : Int ) return a - b;

	static public function onStringCompar( a : String, b : String ) : Int {
		var asize = a.length;
		var bsize = b.length;
		var len = asize < bsize ? asize : bsize;
		var i = 0;
		var sign : Int;
		while (i < len) {
			sign = StringTools.fastCodeAt(a, i) - StringTools.fastCodeAt(b, i);
			if (sign != 0)
				return sign;
			i++;
		}
		return asize - bsize;
	}

	static public function posString(pmin: Int, input: lms.ByteData): String {
		var line = 1;
		var char = 1;
		var i = 0;
		while (i < pmin) {
			var c = input.readByte(i++);
			if (c == "\n".code) {
				char = 1;
				++ line;
			} else {
				++ char;
			}
		}
		return " at line: " + line + ", column: " + char;
	}

	// Inverse lexEngine.parse
	static public function unescape( s : String ) {
		var i = 0;
		var p = 0;
		var len = s.length;
		var buf = new StringBuf();
		while (i < len) {
			var c = StringTools.fastCodeAt(s, i++);
			if (c == "\\".code) {
				var w = i - p - 1;
				if (w > 0)
					buf.addSub(s, p, w);
				c = StringTools.fastCodeAt(s, i++);
				switch(c) {
				case "\\".code, "+".code, "*".code, "?".code, "[".code, "]".code, "-".code, "|".code:
					buf.addChar(c);
				case "x".code, "X".code:
					c = Std.parseInt("0x" + s.substr(i, 2));
					buf.addChar(c);
					i += 2;
				case _:
					throw "Invalid hexadecimal escape sequence";
				}
				p = i;
			}
		}
		if (p == 0) {
			return s;
		} else {
			if (p < i)
				buf.addSub(s, p, i - p);
			return buf.toString();
		}
	}
}
