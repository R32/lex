package subs;

import lm.Charset;

class Sets {
	static public function main() {
		// charset testing
		inline function c(a, b) return new Char(a, b);
		function eq(c1: Charset, c2: Charset, ?pos: haxe.PosInfos) {
			var b = c1.length == c2.length;
			if (b) {
				for (i in 0...c1.length) {
					if (c1[i].toInt() != c2[i].toInt()) {
						b = false;
						break;
					}
				}
			}
			if (!b) {
				trace("\n c1: " + CSet.string(c1) + "\n c2: " + CSet.string(c2));
				throw lm.Utils.error("LineNumber: " + pos.lineNumber);
			}
		}
		eq( CSet.union([c(32, 32), c(51, 59), c(65, 65), c(97, 102)], [c(10, 10), c(51, 59), c(65, 70)]),
			[c(10, 10), c(32, 32), c(51, 59), c(65, 70), c(97, 102)]
		);
		eq( CSet.inter([c(32, 32), c(51, 59), c(65, 65), c(97, 102)],[c(10, 10), c(51, 59), c(65, 70)]),
			[c(51, 59), c(65, 65)]
		);
		eq( CSet.complement( [c(48, 59), c(65, 65)] ),
			[c(0,47), c(60,64), c(66,255)]
		);
		eq( CSet.complement( [c(48, 59), c(65, 70), c(97, 102)] ),
			[c(0,47), c(60,64), c(71,96), c(103, 255)]
		);
		eq( CSet.diff([c(32, 32), c(34, 51), c(65, 70)],[c(33, 33), c(37, 37), c(41, 59), c(61, 63), c(65, 67)]),
			[c(32, 32), c(34, 36), c(38, 40), c(68,70)]
		);
	}
}