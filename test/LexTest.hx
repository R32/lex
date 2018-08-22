package test;

import lm.Charset;
import lm.LexEngine;
import lm.Lexer;
import lm.Position;
import lm.Stream;

/**
LexEngine table format: [trans-seg0|trans-seg1|......|trans-segN|check-seg]
*/
extern abstract Table(haxe.io.Bytes) {
	inline function new(b: haxe.io.Bytes) this = b;

	inline function tbl(p: Int, i: Int):Int return this.get((p * (MAX + 1)) + i);

	inline function exits(i: Int):Int return this.get(this.length - i - 1);

	static inline var MAX = 255;
}

class LexTest {
	static function run() {
		cset_test();
		lex_test();
	}

	static function main()  {
		haxe.Timer.measure(run);
	}

	static function cset_test() {
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
				throw "LineNumber: " + pos.lineNumber;
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
	static public function lex_test(): Void {
		var r = ["ab*c", "abc", "ah?g", "\\+", "\\-", "\\*", "/", "[ \t]", "(", ")", "0", "-?[1-9][0-9]*"];
		var pa = r.map( s -> LexEngine.parse( lm.ByteData.ofString(s), [new Char(0, Table.MAX)] ));
		var lex = new LexEngine(pa, Table.MAX);
		#if debug
		trace("table size: " + (lex.table.length / 1024) + "Kb");
		#end
		var table = new Table(lex.table);
		var sa = ["abc", "ag", "abc", "abbbbbc", "ahg"];
		for (si in 0...sa.length) {
			var s = sa[si];
			var state = LexEngine.BEGIN; // BEGIN;
			for (i in 0...s.length) {
				var c = StringTools.fastCodeAt(s, i);
				state = table.tbl(state, c);
				if (state >= lex.seg) {  // Trans Table
					break;
				}
			}
			var p = table.exits(state);  // Check Table
			if (p == LexEngine.INVALID)
				throw "exits Unmatched: " + s;
			if (!(switch (si) {
			case 0: p == 0; // match r[p]
			case 1: p == 2;
			case 2: p == 0;
			case 3: p == 0;
			case 4: p == 2;
			default: true;
			})) {
				throw "Wrong Matching: \"" + s + "\" with /" + r[p] + "/";
			}
			#if debug
			trace("Matched: \"" + s + "\" with /" + r[p] + "/");
			#end
		}
	}
	static public function s_partern(p: Pattern): String {
		return switch (p) {
		case Empty:
			"";
		case Match(c):
			s_charset(c);
		case Star(a):
			s_partern(a) + "*";
		case Plus(a):
			s_partern(a) + "+";
		case Choice(a):
			s_partern(a) + "?";
		case Next(a, b):
			s_partern(a) + s_partern(b);
		}
	}
	static public function s_charset(cs: Charset): String {
		function unescape(c) {
			return switch(c) {
			case "\\".code, "+".code, "*".code, "?".code, "[".code, "]".code, "-".code:
				"\\" + String.fromCharCode(c);
			default:
				String.fromCharCode(c);
			}
		}
		var s = "";
		for (c in cs) {
			if (c.min == c.max) {
				s += unescape(c.min);
			} else {
				s += '${String.fromCharCode(c.min)}-${String.fromCharCode(c.max)}';
			}
		}
		return if (cs.length > 1) {
			"[" + s + "]";
		} else {
			s;
		}
	}
}
