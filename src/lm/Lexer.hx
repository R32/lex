package lm;

import haxe.Constraints.NotVoid;

/**
the lexBuilder will auto generate all the fields.
*/
#if !macro
@:autoBuild(lm.LexBuilder.build())
#end
#if !(flash || cpp)
@:remove
#end
interface Lexer<T:NotVoid> {
	var input(default, null) : lms.ByteData;
	var pmin(default, null) : Int;
	var pmax(default, null) : Int;
	var current(get, never) : String;
	function token() : T;
	function getString( p : Int, len : Int ) : String;
}
