package debug;

import StringTools.rpad;
import StringTools.lpad;
import lm.Charset;
import lm.LexEngine;
import lm.Parser;

@:access(lm)
class Print {

	static var mapp = [
		"Eof"     => "$",
		"CInt"    => "int",
		"CStr"    => "str",
		"CIdent"    => "id",
		"CFloat"    => "float",
		"CHex"    => "hex",
		"OpPlus"  => "+",
		"OpAdd"  => "+",
		"OpMinus" => "-",
		"OpSub" => "-",
		"OpTimes" => "*",
		"OpMul" => "*",
		"OpDiv"   => "/",
		"OpMod"   => "%",
		"LParen"  => "(",
		"RParen"  => ")",
		"Semicolon" => ";",
		"Percent"   => "%",

	//	"main" => "S",
	//	"expr" => "E",
	];

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
		case Choice(a, Empty):
			s_partern(a) + "?";
		case Choice(a, b):
			s_partern(a) + "|" + s_partern(b);
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
				if (c > 0x20 && c < 0x7F)
					String.fromCharCode(c);
				else
					"\\x" + StringTools.hex(c, 2);
			}
		}
		var s = "";
		for (c in cs) {
			if (c.min == c.max) {
				s += unescape(c.min);
			} else {
				s += '${unescape(c.min)}-${unescape(c.max)}';
			}
		}
		return if (cs.length > 1) {
			"[" + s + "]";
		} else {
			s;
		}
	}

	static public function production(par: lm.Parser) {
		var used = getUsed(par);
		var buf = new StringBuf();
		var R = 0;
		inline function LABEL() buf.add("  (R"+ (R++) +")");
		inline function ARROW() buf.add(" -->");
		inline function newLine() buf.add("\n");
		var smax = Lambda.fold(par.lhsA, ( (l, n)-> l.name.length > n ? l.name.length : n ), 0);
		smax += 2;
		inline function LHS(n) buf.add(lpad(n, " ", smax));

		for (lhs in par.lhsA) {
			var name = mapp.get(lhs.name);
			if (name == null)
				name = lhs.name.toUpperCase();
			LABEL(); LHS(name); ARROW();
			for (li in lhs.cases) {
				if (li != lhs.cases[0]) { // first
					newLine(); LABEL(); LHS(""); ARROW();
				}
				for (s in li.syms) {
					buf.add(" ");
					if (s.t) {
						var r = "";
						var ct = 0;
						for (c in s.cset) {
							for (i in c.min...c.max + 1) {
								r += used.get(i);
								++ ct;
							}
						}
						if (ct > 1)
							r = "[" + r +"]";
						buf.add(r);
					} else {
						buf.add( used.get(s.cset[0].min) );
					}
				}
			}
			if (lhs.epsilon) buf.add(" epsilon");
			newLine();
		}
		return buf.toString();
	}

	static public function parTable(par: lm.Parser, lex: LexEngine) {
		// all used terminal
		var used = getUsed(par);
		var col:Array<{name:String, value:Int}> = [];
		var smax = 0;
		for (i in used.keys()) {
			var name = used.get(i);
			if (smax < name.length) smax = name.length;
			col.push({name: name, value: i});
		}
		smax += 2; // | 10 |
		if (smax < 9) smax = 9;
		col.sort( (a, b) -> a.value - b.value);

		var buf = new StringBuf();
		inline function add(s) buf.add( sPad(s, smax) );
		inline function sp() buf.add("|");
		inline function nxtLine() buf.add("\n");
		var lineWidth = 1 + (smax + 1) * (col.length + 3);
		inline function horLine() buf.add(sRepeat(lineWidth, "-"));

		var raw = lex.table;
		var rollpos = lex.posRB();
		var rlenpos = lex.posRBL();
		var INVALID = lex.invalid;
		function s_epsilon(fid: Int) {
			var s = lex.table.get(lex.table.length - 1 - fid);
			add(s == INVALID ? "NULL" : "R" + s);
		}
		function s_rollback(i: Int) {
			var s = lex.table.get(rollpos + i);
			add(s == INVALID ? "NULL" : "R" + s + "+L" + lex.table.get(rlenpos + i));
		}
		function s_row(i: Int, begin: Int, name: String) {
			horLine(); ( if (i == begin) buf.add(" " + name) ); nxtLine();
			sp(); add(i + ""); sp(); s_rollback(i); sp(); s_epsilon(i); sp();
			var base = i * lex.per;
			for (v in col) {
				var shift = raw.get(base + v.value);
				if (shift != INVALID) {
					if (shift < lex.segs) {
						add("" + shift);
					} else {
						add("R" + raw.get(raw.length - 1 - shift) + ",S" + shift);
					}
				} else {
					add("");
				}
				sp();
			}
			nxtLine();
		}
		// header
		horLine(); nxtLine();
		sp(); add("(S)"); sp(); add("(RB)"); sp(); add("(EP)"); sp();
		for (v in col) {
			add(v.name); sp();
		}
		nxtLine();
		// body
		for (j in 0...lex.entrys.length) {
			var l = par.lhsA[j];
			var name = mapp.get(l.name);
			if (name == null)
				name = l.name.toUpperCase();
			var e = lex.entrys[j];
			for (i in e.begin...e.begin + e.segs)
				s_row(i, e.begin, name);
			horLine(); nxtLine();
		}
		if (lex.segsEx > lex.segs) {
			horLine(); nxtLine();
		}
		for (i in lex.segs...lex.segsEx) {
			s_row(i, lex.segs, "Operator Precedence");
		}
		// end line
		horLine(); nxtLine();
		// final states
		buf.add(sRepeat( 1 + (smax + 1) * 2, "-") + "\n");
		for (i in lex.segsEx...lex.nstates) {
			sp(); add(i + ""); sp(); s_rollback(i); sp();
			buf.add("\n" + sRepeat( 1 + (smax + 1) * 2, "-") + "\n");
		}
		return buf.toString();
	}

	static function getUsed(par: lm.Parser) {
		var used = new Map<Int, String>();
		function s_token(i) {
			var t = Lambda.find(par.termls, t -> t.value == i);
			var name = t != null ? t.name : "null";
			var r = mapp.get(name);
			return r == null ? name.toLowerCase() : r;
		}
		function set(syms: Symbol, used) {
			if (syms.t) {
				for (c in syms.cset)
					for (i in c.min...c.max + 1)
						used.set(i, s_token(i));
			} else {
				var name = syms.name;
				var r = mapp.get(name);
				used.set(syms.cset[0].min, r == null ? name.toUpperCase() : r);
			}
		}
		for (lhs in par.lhsA) {
			for (cc in lhs.cases) {
				for (s in cc.syms) {
					set(s, used);
				}
			}
		}
		return used;
	}

	static function sRepeat(i, c = " ") {
			var s = "";
			while (i-- > 0) s += c;
			return s;
	}

	static function sPad(s: String, smax) {
		var left = smax - s.length >> 1;
		if (left < 0) return s;
		return sRepeat(left) + s + sRepeat(smax - s.length - left);
	}
}