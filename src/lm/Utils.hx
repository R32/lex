package lm;

class Utils {
	static public inline function imin(a: Int, b: Int) return a < b ? a: b;
	static public inline function imax(a: Int, b: Int) return a > b ? a: b;

	@:deprecated static public inline function error( s : String ) {
		return s;
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
#if macro
	static public function getClassFullName(cls: haxe.macro.Type.ClassType) {
		if (StringTools.endsWith(cls.module, cls.name))
			return cls.module;
		return cls.module + "." + cls.name;
	}
#end
}