package test;

import lm.LexEngine;

class LexTest {
	static function main() @:privateAccess {
		haxe.Timer.measure(lm.LexEngine.Test.run);
	}
}