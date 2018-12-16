package lm;

class Utils {
	static public inline function imin(a: Int, b: Int) return a < b ? a: b;
	static public inline function imax(a: Int, b: Int) return a > b ? a: b;
	static public inline function error(s: String) #if js return new js.Error(s); #else return s; #end

#if macro
	static public function getClsFullName(cls: haxe.macro.Type.ClassType) {
		if (StringTools.endsWith(cls.module, cls.name))
			return cls.module;
		return cls.module + "." + cls.name;
	}
#end
}