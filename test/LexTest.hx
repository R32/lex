package;

class LexTest {
	@:access(subs) static function main()  {
		// Sets
		 subs.Sets.main();
		// lexer
		subs.LexBase.main();
		#if !cpp
		subs.LexVoid.main();
		#end
		subs.LexVarIdent.main();
		// parser
		subs.Demo.main();
	}
}
