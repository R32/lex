package subs;

import lm.Charset;

class Sets {
	static public function main() {
		// charset testing
		inline function c(a, b) return Char.RANGE(a, b);
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
				throw("LineNumber: " + pos.lineNumber);
			}
		}
		eq(CSet.sorting([c(6, 8), c(1, 5)]), [c(1, 8)]);
		eq(CSet.union([c(1, 5)], [c(6, 8)]), [c(1, 8)]);
		eq(CSet.union([c(1, 5)], [c(5, 8)]), [c(1, 8)]);
		eq(CSet.union([c(1, 5)], [c(4, 8)]), [c(1, 8)]);
		eq(CSet.union([c(32, 32), c(51, 59), c(65, 65), c(97, 102)], [c(10, 10), c(51, 59), c(65, 70)]),
			[c(10, 10), c(32, 32), c(51, 59), c(65, 70), c(97, 102)]
		);
		eq(CSet.union([], [c(1, 5)]), [c(1, 5)]);
		eq(CSet.union([c(1, 5)], []), [c(1, 5)]);
		eq(CSet.union([], []), []);
		eq(CSet.inter([c(10, 20)], [c(15, 25)]), [c(15, 20)]);
		eq(CSet.inter([c(10, 20)], [c(20, 30)]), [c(20, 20)]);
		eq(CSet.inter([c(32, 32), c(51, 59), c(65, 65), c(97, 102)], [c(10, 10), c(51, 59), c(65, 70)]),
			[c(51, 59), c(65, 65)]
		);
		eq(CSet.inter([c(1, 5)], [c(6, 8)]), []);
		eq(CSet.inter([], [c(1, 5)]), []);
		eq(CSet.inter([c(1, 5)], []), []);

		eq(CSet.complement([]), CSet.C_255);
		eq(CSet.complement(CSet.C_255), []);
		eq(CSet.complement([c(10, 20)]), [c(0, 9), c(21, 255)]);
		eq(CSet.complement([c(10, 20), c(15, 25)]), [c(0, 9), c(26, 255)]);
		eq(CSet.complement([c(48, 59), c(65, 65)]), [c(0, 47), c(60, 64), c(66, 255)]);
		eq(CSet.complement([c(0, 255)]), []);

		eq(CSet.diff([c(10, 20)], [c(30, 40)]), [c(10, 20)]);
		eq(CSet.diff([c(10, 20)], [c(5, 25)]), []);
		eq(CSet.diff([c(10, 100)], [c(15, 20), c(30, 40), c(90, 200)]),
			[c(10, 14), c(21, 29), c(41, 89)]
		);
		eq(CSet.diff([c(10, 20), c(30, 40)], [c(20, 30)]), [c(10, 19), c(31, 40)]);
		eq(CSet.diff([c(10, 20), c(30, 40)], [c(15, 35)]), [c(10, 14), c(36, 40)]);
		eq(CSet.diff([c(32, 32), c(34, 51), c(65, 70)], [c(33, 33), c(37, 37), c(41, 59), c(61, 63), c(65, 67)]),
			[c(32, 32), c(34, 36), c(38, 40), c(68, 70)]
		);
		eq(CSet.diff([], [c(1, 5)]), []);
		eq(CSet.diff([c(1, 5)], []), [c(1, 5)]);
		eq(CSet.diff([], []), []);

		eq(CSet.union([c(1, 1)], [c(1, 1)]), [c(1, 1)]);
		eq(CSet.inter([c(1, 1)], [c(1, 1)]), [c(1, 1)]);
		eq(CSet.diff([c(1, 1)], [c(1, 1)]), []);
	}
}
