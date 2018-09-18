package lm;

#if !macro
@:autoBuild(lm.LexBuilder.build())
#end
@:remove interface Lexer<T> {
	var current(get, never): String;
	function curpos(): lm.Position;
	function token(): T;
}