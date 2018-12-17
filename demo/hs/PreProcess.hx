package hs;

import hs.Lexer;
import hscript.Expr;

@:rule({
	left: ["||"],
	left: ["&&"],
	nonassoc: ["!"],
}) class PreProcess implements lm.LR0<hs.Lexer, Expr> {

	public var defines : Map<String, Dynamic> = new Map();

	public var stack: Array<{r: Bool}> = [];

	public function eval(e: Expr) {
		return switch( e ) {
		case EIdent(id):              defines.get(id) != null;
		case EUnop("!", _, e):        !eval(e);
		case EParent(e):              eval(e);
		case EBinop("&&", e1, e2):    eval(e1) && eval(e2);
		case EBinop("||", e1, e2):    eval(e1) || eval(e2);
		default:                      throw lm.Utils.error("TODO");
		}
	}

	public function process(t: Token, lex: hs.Lexer):Token {
		return switch(t) {
		case PrIf:
			if ( eval(parse()) ) {
				stack.push( {r: true} );
				return lex.token();
			}
			stack.push( {r: false} );
			skipTokens(lex);
		case PrElse, PrElseIf if (stack.length > 0):
			if (stack[stack.length - 1].r) {
				stack[stack.length - 1].r = false;
				skipTokens(lex);
			} else if (t == PrElse) {
				stack.pop();
				stack.push({ r : true });
				lex.token();
			} else {
				stack.pop();
				process(PrIf, lex);
			}
		case PrEnd if (stack.length > 0):
			stack.pop();
			lex.token();
		default: t;
		}
	}

	function skipTokens(lex: hs.Lexer):Token {
		var spos = stack.length - 1;
		var obj = stack[spos];
		var t = lex.token();
		while (t != Eof) {
			if (obj != stack[spos])
				return t;
			t = lex.token();
		}
		throw lm.Utils.error( "Unclosed: " + "#if/else" + lm.Utils.posString(lex.pmin, lex.input) );
	}

	@:rule(CIdent) static inline function id_ofstring(s:String):String return s;

	// %start parse
	static var parse = switch(s) {
		case [e = cond]:                      @:privateAccess s.rollback(0, 0); e; // since the next token was parsered in stream.
		case [Eof]:                           throw s.error("Unclosed: " + "#if/else", _t1);
	}
	static var cond = switch(s) {
		case [c1 = cond, "&&", c2 = cond]:    EBinop("&&", c1, c2);
		case [c1 = cond, "||", c2 = cond]:    EBinop("||", c1, c2);
		case ["(", c = cond, ")"]:            EParent(c);
		case ["!", c = cond]:                 EUnop("!", true, c);
		case [CIdent(i)]:                     EIdent(i);
	}
}