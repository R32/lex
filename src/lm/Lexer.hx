package lm;

#if !macro
@:autoBuild(lm.LexBuilder.build())
#end
@:remove interface Lexer<T> {
	var current(default, null): String;
	function curpos(): lm.Position;
	function token(): T;
}