package hs;

import hs.Lexer;
import hscript.Expr;

@:rule({
	left: ["||"],
	left: ["&&"],
	nonassoc: ["!"],
}) class PreProcess implements lm.LR0<hs.Lexer, Expr> {

	public var defines : Map<String, Dynamic> = ["lex" => "1", "hscript" => 1];

	public var stack: Array<{r: Bool}> = [];

	public var inParsing: Bool = false;

	public function eval(e: Expr) {
		return switch( e ) {
		case EIdent(id):              defines.get(id) != null;
		case EUnop("!", _, e):        !eval(e);
		case EParent(e):              eval(e);
		case EBinop("&&", e1, e2):    eval(e1) && eval(e2);
		case EBinop("||", e1, e2):    eval(e1) || eval(e2);
		default:                      throw("TODO");
		}
	}

	inline function parentIsFalse() {
		return stack.length > 1 && false == stack[stack.length - 2].r;
	}

	public function process(t: Token, lex: hs.Lexer):Token {
		return if (inParsing) {
			t;
		} else if ( parentIsFalse() && t != PrEnd ) {
			skipTokens(lex);
		} else {
			switch(t) {
			case PrIf:
				var stub = {r: false};
				stack.push(stub);
				inParsing = true;
				var v = parse();
				inParsing = false;
				if ( eval(v) ) {
					stub.r = true;
					lex.token();
				} else {
					skipTokens(lex);
				}
			case PrEnd if (stack.length > 0):
				stack.pop();
				if (stack.length == 0 || stack[stack.length - 1].r) {
					lex.token();
				} else {
					skipTokens(lex);
				}
			case PrElse, PrElseIf if (stack.length > 0):
				if (stack[stack.length - 1].r) {
					skipTokens(lex);
				} else if (t == PrElse) {
					stack.pop();
					stack.push({ r : true });
					lex.token();
				} else {
					stack.pop();
					process(PrIf, lex);
				}
			case _:
				t;
			}
		}
	}

	function skipTokens(lex: hs.Lexer):Token {
		var spos = stack.length - 1;
		var obj = stack[spos];
		var t = lex.token();
		while (obj == stack[spos]) {
			if (t == Eof)
				throw( "Unclosed: " + "#if/else" + lm.Utils.posString(lex.pmin, lex.input) );
			t = lex.token();
		}
		return t;
	}

	@:rule(CIdent) static inline function id_ofstring(s:String):String return s;

	// %start parse
	static var parse = switch(s) {
		case [e = cond]:
			var s = stream;
			if (s.rest > 0) @:privateAccess {
				s.lex.pmax = s.peek(0).pmin; // FORCE rollback, since the next token was parsered in stream.
				s.junk(s.rest);              // then clear stream cache
			}
			e;
		case [Eof]:
			throw stream.error("Unclosed: " + "#if/else", _t1);
	}
	static var cond = switch(s) {
		case [c1 = cond, "&&", c2 = cond]:    EBinop("&&", c1, c2);
		case [c1 = cond, "||", c2 = cond]:    EBinop("||", c1, c2);
		case ["(", c = cond, ")"]:            EParent(c);
		case ["!", c = cond]:                 EUnop("!", true, c);
		case [CIdent(i)]:                     EIdent(i);
	}
}