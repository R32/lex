package lm;

/**
the lexBuilder will auto generate all the fields.
*/
#if !macro
@:autoBuild(lm.LexBuilder.build())
#end
#if !flash
@:remove
#end
interface Lexer<T> {
	var input(default, null): lms.ByteData;
	var pmin(default, null): Int;
	var pmax(default, null): Int;
	var current(get, never): String;
	var extra : Dynamic; // a extra temporary variable.
	function token(): T;
	function getString(p:Int, len:Int):String;
}
