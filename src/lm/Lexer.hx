package lm;

/**
the lexBuilder will auto generate all the fields.
*/
#if !macro
@:autoBuild(lm.LexBuilder.build())
#end
@:remove interface Lexer<T> {
	var input(default, null): lms.ByteData;
	var pmin(default, null): Int;  // make lm.Stream works better.
	var pmax(default, null): Int;  // Because the lm.position has size limit.
	var current(get, never): String;
	function curpos(): lm.Position;
	function token(): T;
	function getString(p:Int, len:Int):String;
}
