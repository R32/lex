package lm;

#if macro
import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
using haxe.macro.Tools;

typedef Group = {
	name: String,
	rules: Array<String>,
	cases: Array<Expr>,
}

class LexBuilder {
	static public function build():Array<Field> {
		var cls = Context.getLocalClass().get();
		var cmax = -1;
		var ct_lex = TPath({pack: cls.pack, name: cls.name});
		var ret = [];
		var fields = Context.getBuildFields();
		var groups = [];
		var all_fields = new haxe.ds.StringMap<Bool>();
		for (f in fields) {
			if (f.access.indexOf(AStatic) > -1) {
				switch (f.kind) {
				case FVar(t, e) if (e != null):
					switch(e.expr){
					case EMeta({name: ":rule", params: n}, e):
						var g:Group = {name: f.name, rules: [], cases: []};
						if (cmax == -1) {
							cmax = if (n.length > 0)
								switch (n[0].expr) {
								case EConst(CInt(i)): Std.parseInt(i);
								default: 255;
								}
							else
								255;
						}
						transform(g, e);
						if (g.rules.length > 0)
							groups.push(g);
						continue;
					default:
					}
				default:
				}
			}
			all_fields.set(f.name, true);
			ret.push(f);
		}
		if (groups.length == 0) return null;
		cmax = cmax & 255 | 15;
		var force_bytes = !Context.defined("js") || Context.defined("force_bytes");
		var base = macro class {
			var input: lm.ByteData;
			public var pmin(default, null): Int;
			public var pmax(default, null): Int;
			public var current(default, null): String;
			public function curpos() return new lm.Position(pmin, pmax);
			public function new(s: lm.ByteData) {
				this.input = s;
				current = "";
				pmin = 0;
				pmax = 0;
			}
		}
		for (f in base.fields)
			if (!all_fields.exists(f.name))
				ret.push(f);
		for (i in 0...groups.length) {
			var g = groups[i];
			var cset = [new lm.Charset.Char(0, cmax)];
			var rules = g.rules.map( s -> LexEngine.parse(s, cset) );
			var lex = new LexEngine(rules, cmax);
			var resname = "_" + StringTools.hex(haxe.crypto.Crc32.make(lex.table)).toLowerCase();
			var bytes = macro haxe.Resource.getBytes($v{resname});
			if (force_bytes) {
				Context.addResource(resname, lex.table);
			} else {
				var out = haxe.macro.Compiler.getOutput() + ".lex-table" + i;
				var f = sys.io.File.write(out);
				lex.write(f);
				f.close();
				bytes = macro ($e{haxe.macro.Compiler.includeFile(out, Inline)});
			}
			//////
			inline function suff(s:String) return s + i;
			var s_table = suff("_t");
			var get_trans = macro $i{s_table}.get(($v{lex.per} * s) + c);
			var get_exits = macro $i{s_table}.get(($v{lex.table.length - 1} - s));
			if (!force_bytes) {
				get_trans = macro StringTools.fastCodeAt($i{s_table}, ($v{lex.per} * s) + c);
				get_exits = macro StringTools.fastCodeAt($i{s_table}, $v{lex.table.length - 1} - s);
			}
			var gotos = [];
			for (i in 0...rules.length) {
				gotos.push(macro (lexbuf: $ct_lex) -> $e{g.cases[i]});
			}
			var s_goto = suff("_g");
			var s_trans = suff("trans");
			var s_exits = suff("exits");
			var s_segs = suff("segs");
			var s_sizes = suff("sizes");
			var s_token = i == 0 ? "token" : g.name;
			var ext = macro class {
				static var $s_goto = [$a{gotos}];
				static var $s_table = $bytes;
				static inline function $s_trans(s: Int, c: Int):Int return $e{get_trans};
				static inline function $s_exits(s: Int):Int return $e{get_exits};
				static inline var $s_segs = $v{lex.segs};
				static inline var $s_sizes = $v{rules.length};
				public function $s_token() {
					var i = pmax, len = input.length;
					if (i >= len) return Eof;
					pmin = i;
					var state = 0;
					var prev = 0;
					while (i < len) {
						var c = input.readByte(i++);
						state = $i{s_trans}(state, c);
						if (state >= $i{s_segs})
							break;
						prev = state;
					}
					state = $i{s_exits}(state);
					return if (state < $i{s_sizes}) {
						pmax = i;
						current = input.readString(pmin, pmax - pmin);
						$i{s_goto}[state](this);
					} else {
						state = $i{s_exits}(prev);
						if (state < $i{s_sizes}) {
							pmax = i - 1;
							current = input.readString(pmin, pmax - pmin);
							$i{s_goto}[state](this);
						} else {
							throw lm.Utils.error("UnMatached: " + pmin + "-" + pmax + ': "' + input.readString(pmin, i - pmin) + '"');
						}
					}
				}
			}
			for (f in ext.fields)
				if (!all_fields.exists(f.name))
					ret.push(f);
		}
		return ret;
	}
	static function transform(g: Group, e: Expr) {
		var ret = [];
		switch(e.expr) {
		case EArrayDecl(el):
			for (e in el)
				switch (e.expr) {
				case EBinop(OpArrow, rule, e):
					g.rules.push(eString(rule));
					g.cases.push(e);
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