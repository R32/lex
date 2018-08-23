package lm;

#if macro
import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
using haxe.macro.Tools;

class LexBuilder {
	static public function build():Array<Field> {
		var cmax = 255;
		var cls = Context.getLocalClass().get();
		var ct_lex = TPath({pack: cls.pack, name: cls.name});
		for (it in cls.interfaces)
			if (it.t.toString() == "lm.Lexer") {
				switch (it.params[0]) {
				case TInst(_.get() => {kind: KExpr(macro $v{(i:Int)})}, _):
					cmax = (Std.parseInt(i) & 0xFF) | 15; // limit in 15-255
				default:
				}
			}
		var ret = [];
		var fields = Context.getBuildFields();
		var rules = [];
		var cases = [];
		var all_fields = new haxe.ds.StringMap<Bool>();
		for (f in fields) {
			if (f.access.indexOf(AStatic) > -1) {
				switch (f.kind) {
				case FVar(t, e) if (e != null):
					switch(e.expr){
					case EMeta({name: ":rule"}, e):
						transform(rules, cases, e);
						continue;
					default:
					}
				default:
				}
			}
			all_fields.set(f.name, true);
			ret.push(f);
		}
		if (rules.length > 0) {
			var cset = [new lm.Charset.Char(0, cmax)];
			var rules = rules.map( s -> LexEngine.parse(s, cset) );
			var lex = new LexEngine(rules, cmax);
			var name = "_" + StringTools.hex(haxe.crypto.Crc32.make(lex.table)).toLowerCase();
			var getTrans= macro $i{name}.get(($v{lex.per} * s) + c);
			var getExits = macro $i{name}.get(($v{lex.table.length - 1} - s));
			var bytes = macro haxe.Resource.getBytes($v{name});
			if (!Context.defined("js") || Context.defined("force_bytes")) {
				Context.addResource(name, lex.table);
			} else {
				var out = haxe.macro.Compiler.getOutput() + ".lex-table";
				var f = sys.io.File.write(out);
				lex.write(f, "");
				f.close();
				bytes = macro ($e{haxe.macro.Compiler.includeFile(out, Inline)});
				getTrans = macro $i{name}.charCodeAt(($v{lex.per} * s) + c);
				getExits = macro $i{name}.charCodeAt($v{lex.table.length - 1} - s);
			}

			var gotos = [];
			for (i in 0...rules.length)
				gotos[i] = macro (lexbuf: $ct_lex) -> $e{cases[i]};
			var def = macro class {
				static var _goto = [$a{gotos}];
				static var $name = $bytes;
				static public inline function trans(s: Int, c: Int):Int return $e{getTrans};
				static public inline function exits(s: Int):Int return $e{getExits};
				static public inline var SEGS = $v{lex.segs};
				static public inline var SIZES = $v{rules.length};
				var input: lm.ByteData;
				var source: String;
				var pmin: Int;
				var pmax: Int;
				public var current(default, null): String;
				public function new(input, source = "<null>") {
					current = "";
					this.input = input;
					this.source = source;
					pmin = 0;
					pmax = 0;
				}
				public function curpos() return new lm.Position(pmin, pmax);
				public function token() {
					var i = pmax, len = input.length;
					if (i >= len) return Eof;
					pmin = i;
					var state = 0;
					var prev = 0;
					while (i < len) {
						var c = input.readByte(i++);
						state = trans(state, c);
						if (state >= SEGS)
							break;
						prev = state;
					}
					state = exits(state);
					return if (state < SIZES) {
						pmax = i;
						current = input.readString(pmin, pmax - pmin);
						_goto[state](this);
					} else {
						state = exits(prev);
						if (state < SIZES) {
							pmax = i - 1;
							current = input.readString(pmin, pmax - pmin);
							_goto[state](this);
						} else {
							throw lm.Utils.error("UnMatached: " + pmin + "-" + pmax + ': "' + input.readString(pmin, i - pmin) + '"');
						}
					}
				}
			} // class end
			for (f in def.fields)
				if (!all_fields.exists(f.name)) // Customizable
					ret.push(f);
		}
		return ret;
	}

	static function transform(rules, cases, e: Expr) {
		var ret = [];
		switch(e.expr) {
		case EArrayDecl(el):
			for (e in el)
				switch (e.expr) {
				case EBinop(OpArrow, rule, e):
					rules.push(eString(rule));
					cases.push(e);
				default:
					Context.error("Expected pattern => function", e.pos);
				}
		default:
			Context.error("Expected pattern => function map declaration", e.pos);
		}
		return ret;
	}
	static function eString(e: Expr): String {
		return switch (e.expr) {
		case EConst(CString(s)): s;
		default:
			Context.error("Invalid rule", e.pos);
		}
	}
}
#else
class RuleBuilder {}
#end