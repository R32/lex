package;

class LexTest {
	@:access(subs) static function main()  {
		// Sets
		subs.Sets.main();
		// lexer
		subs.LexBase.main();
		subs.LexVarIdent.main();
		// subs.LexRollback.main();
		// parser
		subs.Demo.main();
	}
}
