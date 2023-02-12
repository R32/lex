package debug;

import lm.Charset;
import lm.LexEngine;
import lm.ParserBase;
import StringTools.rpad;
import StringTools.lpad;

@:access(debug.Print)
class SLRPrint {

	static public function production( parser : ParserBase ) {
		var buffer = new StringBuf();
		var tokens = getAllToken(parser);
		var lhsides = parser.lhsides;
		var RPAD = parser.nrules <= 10 ? 2 : (parser.nrules <= 100 ? 3 : 4);
		var R = 0;
		inline function LABEL() buffer.add("  (" + lpad("R" + R++, " ", RPAD) +")");
		inline function ARROW() buffer.add(" -->");
		inline function newLine() buffer.add("\n");
		var smax = Lambda.fold(lhsides, ( (l, n)-> l.name.length > n ? l.name.length : n ), 0);
		smax += 2;
		inline function LHS(n) buffer.add(lpad(n, " ", smax));
		for (lhs in lhsides) {
			var name = lhs.name.toUpperCase();
			LABEL(); LHS(name); ARROW();
			var epsilon = false;
			for (stsets in lhs.cases) {
				if (stsets != lhs.cases[0]) { // skip first
					newLine(); LABEL(); LHS(""); ARROW();
				}
				for (tk in stsets.sets) {
					buffer.add(" ");
					if (!tk.t) {
						buffer.add(tk.name.toUpperCase());
						continue;
					}
					var a = [];
					for (c in tk.cset) {
						for (i in c.min...c.max + 1) {
							a.push(tokens.get(i));
						}
					}

					if (a.length == 1)
						buffer.add(a[0]);
					else
						buffer.add("[" + a.join(" ") + "]");
				}
				if (stsets.sets.length == 0)
					epsilon = true;
			}
			if (lhs.edef != null || epsilon)
				buffer.add(" epsilon");
			newLine();
		}
		return buffer.toString();
	}

	static public function table( parser : ParserBase, lex : LexEngine, process : Array<{begin : Int, index : Int}> ) {
		var tokens = getAllToken(parser);
		var smax = 0;
		var col : Array<{name : String, value : Int}> = [];
		for (i => s in tokens) {
			if (smax < s.length)
				smax = s.length;
			col.push({name : s, value : i});
		}
		smax += 2;
		if (smax < 7)
			smax = 7;
		col.sort( (a, b) -> a.value - b.value);

		var buffer = new StringBuf();
		inline function add(s) buffer.add(Print.sPad(s, smax));
		inline function sp() buffer.add("|");
		inline function nxtLine() buffer.add("\n");
		var lineWidth = 1 + (smax + 1) * (col.length + 2);
		inline function horLine() buffer.add(Print.sRepeat(lineWidth, "-"));
		//
		var raw = lex.table;
		var stride = lex.per;
		var INVALID = lex.invalid;
		function s_epsilon( fid : Int ) {
			var s = raw.get(raw.length - 1 - fid);
			add(s == INVALID ? "NULL" : "R" + s);
		}
		function s_row( i : Int, begin : Int, name : String) {
			horLine(); ( if (i == begin) buffer.add(" " + name) ); nxtLine();
			sp(); add(i + ""); sp(); s_epsilon(i); sp();
			var base = i * stride;
			for (v in col) {
				var shift = raw.get(base + v.value);
				if (shift != INVALID) {
					add("" + shift);
				} else {
					add("");
				}
				sp();
			}
			nxtLine();
		}
		// header
		horLine(); nxtLine();
		sp(); add("(S)"); sp(); add("(EP)"); sp();
		for (v in col) {
			add(v.name); sp();
		}
		nxtLine();
		// body
		var len = process.length;
		if (len == 0)
			return buffer.toString();

		for (i in 0...len) {
			var en = process[i];
			var nmax = i + 1 < len ? process[i + 1].begin : lex.segs;
			var lhs = parser.lhsides[en.index];
			var name = lhs.name.toUpperCase();
			for (j in en.begin...nmax)
				s_row(j, en.begin, name);
			horLine();
			nxtLine();
		}
		// final states
		var nstates = @:privateAccess lex.lstates.length;
		buffer.add(Print.sRepeat( 1 + (smax + 1) * 2, "-") + "\n");
		for (i in lex.segs...nstates) {
			sp(); add(i + ""); sp(); s_epsilon(i); sp();
			buffer.add("\n" + Print.sRepeat( 1 + (smax + 1) * 2, "-") + "\n");
		}
		return buffer.toString();
	}

	static function getAllToken( parser : ParserBase ) {
		var result = new Map<Int, String>();
		function stoken( i : Int ) {
			var tk = Lambda.find(parser.terms_map, t -> t.value == i);
			if (tk == null)
				return "unknown";
			var s = parser.reflect.get(tk.name);
			if (s != null)
				return s;
			var s = Print.mapp.get(tk.name);
			if (s != null)
				return s;
			return tk.name.toLowerCase();
		}
		function set( tk : StreamToken ) {
			if (!tk.t) {
				result.set(tk.cset[0].min, tk.name.toUpperCase());
				return;
			}
			for (c in tk.cset) {
				for (i in c.min...c.max + 1) {
					result.set(i, stoken(i));
				}
			}
		}
		for (lhs in parser.lhsides) {
			for (stsets in lhs.cases) {
				for (tk in stsets.sets) {
					set(tk);
				}
			}
		}
		return result;
	}
}