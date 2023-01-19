package lm;

#if (macro || eval || display)
import lm.LexEngine;
import haxe.macro.Expr;
import haxe.macro.Type;
 using haxe.macro.ExprTools;

typedef RuleCase = {
	pattern : Expr,
	action : Expr,
}
typedef RuleCaseGroup = {
	name : String,
	rules : Array<RuleCase>,
	unmatch : RuleCase,
}

/*
 *
 * Some public functions are used for macro or eval (interp)
 *
 */
class ExprHelps {

	/*
	 * parse expr patterns
	 */
	static public function parsePaterns( e : Expr, varmap : Map<String, Expr>, charset : lm.Charset ) : lm.LexEngine.Pattern {
		inline function parseInner(e) return parsePaterns(e, varmap, charset);
		return switch (e.expr) {
		case EConst(CString(s)):
			LexEngine.parse(s, charset);
		case EConst(CIdent(i)):
			var e2 = varmap.get(i);
			if (e2 == null)
				throw new Error("Undefined identifier: " + i, e.pos);
			parseInner(e2);
		case EBinop(OpAdd, e1, e2):
			Next(parseInner(e1), parseInner(e2));
		case EBinop(OpOr, e1, e2):
			Choice(parseInner(e1), parseInner(e2));
		case EParenthesis(e):
			parseInner(e);
		case ECall(fn, [arg]):
			switch (fn.expr) {
			case EConst(CIdent(i)):
				var pattern = parseInner(arg);
				switch (i) {
				case "Star":
					Star(pattern);
				case "Plus":
					Plus(pattern);
				case "Opt":
					Choice(pattern, Empty);
				default:
					throw new Error("Only Star(), Plus(), and Opt() are supported.", e.pos);
				}
			default:
				throw new Error("Invalid rule", e.pos);
			}
		default:
			throw new Error("Invalid rule", e.pos);
		}
	}

	static public function classFullName( clst : ClassType ) {
		if (StringTools.endsWith(clst.module, clst.name))
			return clst.module;
		return clst.module + "." + clst.name;
	}

	static public function objFieldPos( objfield : ObjectField )  {
		var pos : Position = (objfield : Dynamic).name_pos;
		return pos != null ? pos : objfield.expr.pos;
	}

	/*
	 *
	 */
	static public function lexChecking( lex : lm.LexEngine, groups : Array<RuleCaseGroup> ) {
		var table = lex.table;
		var VALID = 1;
		var INVALID = lex.invalid;
		// init
		var exits = haxe.io.Bytes.alloc(lex.nrules);
		for (i in table.length - lex.perExit...table.length) {
			var n = table.get(i);
			if (n == INVALID) continue;
			exits.set(n, VALID);
		}
		// reachable
		for (n in 0...lex.nrules) {
			if (exits.get(n) != VALID) {
				var pattern = indexRuleCase(groups, n).pattern;
				throw new Error("UnReachable pattern: " + pattern.toString(), pattern.pos);
			}
		}
		// epsilon
		for (i in 0...lex.entrys.length) {
			var e = lex.entrys[i];
			var n = table.exits(e.begin);
			if (n == INVALID)
				continue;
			var pattern = indexRuleCase(groups, n).pattern;
			if (i == 0) {
				throw new Error("Empty match is not allowed: " + pattern.toString(), pattern.pos);
			} else if (groups[i].unmatch != null) {
				var unmatch = groups[i].unmatch.pattern;
				throw new Error(pattern.toString() +" conflicts with \"case " + unmatch.toString() + '"', pattern.pos);
			}
		}
	}
	static function indexRuleCase( groups : Array<RuleCaseGroup>, i : Int ) : RuleCase {
		for (g in groups) {
			var len = g.rules.length;
			if (i >= len) {
				i -= len;
			} else {
				return g.rules[i];
			}
		}
		return null;
	}
	/*
	 * for "_ => Actoin", Should be called after "lexChecking"
	 */
	static public function lexUnMatchedActions( lex : lm.LexEngine, groups : Array<RuleCaseGroup> ) : Array<Case> {
		var extra = [];
		var table = lex.table;
		var start = lex.nrules;
		for (i in 1...groups.length) { // the first one[0] will be "switch-default"
			var g = groups[i];
			if (g.unmatch != null) {
				var e = lex.entrys[i];
				table.set(table.exitpos(e.begin), start);
				extra.push({
					values : [{
						expr : EConst(CInt("" + start)),
						pos : g.unmatch.pattern.pos
					}],
					expr: g.unmatch.action
				});
				++start;
			}
		}
		return extra;
	}
}
#else
class ExprHelps {}
#end
