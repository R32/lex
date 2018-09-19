package debug;

import StringTools.rpad;
import StringTools.lpad;
import lm.LexEngine;
import lm.LR0;

@:access(lm)
class Print {

	static var mapp = [
		"Eof"     => "$",
		"CInt"    => "int",
		"OpPlus"  => "+",
		"OpMinus" => "-",
		"OpTimes" => "*",
		"OpDiv"   => "/",
		"LParen"  => "(",
		"RParen"  => ")",
	];

	static public function lr0Production(lrb: LR0Builder) {
		var used = getUsed(lrb);
		var buf = new StringBuf();
		var R = 0;
		inline function LABEL() buf.add("  (R"+ (R++) +")");
		inline function ARROW() buf.add(" -->");
		inline function newLine() buf.add("\n");
		var cmax = Lambda.fold(lrb.lhsA, ( (l, n)-> l.name.length > n ? l.name.length : n ), 0);
		cmax += 2;
		inline function LHS(n) buf.add(lpad(n, " ", cmax));

		for (lhs in lrb.lhsA) {
			LABEL(); LHS(lhs.name.toUpperCase()); ARROW();
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
			if (lhs.epsilon) buf.add("epsilon");
			newLine();
		}
		return buf.toString();
	}

	static public function lr0Table(lrb: LR0Builder, lex: LexEngine) {
		// all used terminal
		var used = getUsed(lrb);
		var col:Array<{name:String, value:Int}> = [];
		var smax = 0;
		for (i in used.keys()) {
			var name = used.get(i);
			if (smax < name.length) smax = name.length;
			col.push({name: name, value: i});
		}
		smax += 4; // | 10 |
		if (smax < 5) smax = 5;
		col.sort( (a, b) -> a.value - b.value);

		var buf = new StringBuf();
		inline function add(s) buf.add( sPad(s, smax) );
		inline function sp() buf.add("|");

		// header
		var lineWidth = 1 + (smax + 1) * (col.length + 3);
		var lineSp = sRepeat(lineWidth, "-") + "\n";
		buf.add(lineSp);
		lineSp = "\n" + lineSp;
		sp(); add("(S)"); sp(); add("(RB)"); sp(); add("(EP)"); sp();
		for (v in col) {
			add(v.name); sp();
		}
		buf.add(lineSp);

		var raw = lex.table;
		var invalid = lex.per - 1;
		var rollpos = lex.per * lex.segs;
		var rlenpos = rollpos + lex.per;
		function s_epsilon(i) {
			var s = lex.table.get(lex.table.length - 1 - i);
			add(s == invalid ? "NULL" : "R" + s);
		}
		function s_rollback(i) {
			var s = lex.table.get(rollpos + i);
			add(s == invalid ? "NULL" : "R" + s + "+L" + lex.table.get(rlenpos + i));
		}
		// body
		for (i in 0...lex.segs) {
			sp(); add(i + ""); sp(); s_rollback(i); sp(); s_epsilon(i); sp();
			var base = i * lex.per;
			for (v in col) {
				var shift = raw.get(base + v.value);
				if (shift != invalid) {
					if (shift < lex.segs) {
						add("" + shift);
					} else {
						add("R" + raw.get(raw.length - 1 - shift));
					}
				} else {
					add("");
				}
				sp();
			}
			buf.add(lineSp);
		}
		// footer
		//sp(); add("size: "); sp();
		//buf.add(rpad(" (segs+1)*per=(" + lex.segs + "+1)*" + lex.per + "=" + raw.length + "b" ," ",(smax + 1) * (col.length+2) - 1)); sp();
		//buf.add(lineSp);
		return buf.toString();
	}

	static function getUsed(lrb: LR0Builder) {
		var used = new Map<Int, String>();
		function s_token(i) {
			var t = Lambda.find(lrb.termls, t -> t.value == i);
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
				used.set(syms.cset[0].min, syms.name.toUpperCase());
			}
		}
		for (lhs in lrb.lhsA) {
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