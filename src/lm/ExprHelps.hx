package lm;

#if (macro || eval || display)
import haxe.macro.Expr;
import haxe.macro.Type;

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
				var patern = parseInner(arg);
				switch (i) {
				case "Star":
					Star(patern);
				case "Plus":
					Plus(patern);
				case "Opt":
					Choice(patern, Empty);
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
}
#else
class ExprHelps {}
#end
