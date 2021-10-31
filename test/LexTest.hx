package;

import lm.LineColumn;

class LexTest {
	@:access(subs) static function main()  {
		// Sets
		subs.Sets.main();
		// lexer
		subs.LexBase.main();

		subs.LexVarIdent.main();
		// parser
		subs.Demo.main();

		var conter = new LConter("test");
		conter.add(15); // line 2
		conter.add(20); // line 3
		conter.add(30); // line 4
		conter.add(35); // line 5
		if ((conter.get(0).line == 1 && conter.get(0).column == 1)
		&& (conter.get(15).line == 2 && conter.get(15).column == 1)
		&& (conter.get(20).line == 3 && conter.get(20).column == 1)
		&& (conter.get(30).line == 4 && conter.get(30).column == 1)
		&& (conter.get(31).line == 4 && conter.get(31).column == 2)
		) {
			trace("LineColumn is done");
		}

		var parray = new lm.PosArray();
		parray.add(10, "a");
		parray.add(20, "b");
		parray.add(30, "c");
		parray.add(40, "d");
		parray.add(50, "e");
		if ((parray.get(30) == "c")
		&& (parray.get(20) == "b")
		&& (parray.get(10) == "a")
		&& (parray.get(40) == "d")
		&& (parray.get(50) == "e")
		&& (parray.get(22) == null)
		) {
			trace("PosMap is done");
		}
	}
}
