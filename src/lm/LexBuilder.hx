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
	static function getMeta(metas: Array<Expr>) {
		var ret = {cmax: -1, eof: null};
		for (meta in metas) {
			switch (meta.expr) {
			case EConst(CInt(i)):
				ret.cmax = Std.parseInt(i);
			case EConst(CIdent(i)):
				ret.eof = meta;
			default:
			}
		}
		return ret;
	}
	static public function build():Array<Field> {
		var cls = Context.getLocalClass().get();
		var cmax = -1;
		var EEOF:Expr = macro null;
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
					case EMeta({name: ":rule", params: metas}, e):
						var g:Group = {name: f.name, rules: [], cases: []};
						if (cmax == -1) { // the first @:rule is valid.
							var x = getMeta(metas);
							cmax = x.cmax == -1 ? 255 : x.cmax;
							if (x.eof != null)
								EEOF = x.eof;
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
		if (Context.defined("str_lex")) force_bytes = false; // force string as table format
		// Table Build
		var cset = [new lm.Charset.Char(0, cmax)];
		var rules = [];
		var cases = [];
		for (g in groups) {
			rules.push( g.rules.map( s -> LexEngine.parse(s, cset) ));
			for (i in 0...g.rules.length) {
				cases.push(macro (lex: $ct_lex) -> $e{g.cases[i]});
			}
		}
		var lex = new LexEngine(rules, cmax);
		var resname = "_" + StringTools.hex(haxe.crypto.Crc32.make(lex.table)).toLowerCase();
		var raw = macro haxe.Resource.getBytes($v{resname});
		if (force_bytes) {
			Context.addResource(resname, lex.table);
		} else {
			var out = haxe.macro.Compiler.getOutput() + ".lex-table";
			var f = sys.io.File.write(out);
			lex.write(f);
			f.close();
			raw = macro ($e{haxe.macro.Compiler.includeFile(out, Inline)});
		}
		var get_trans = macro raw.get(($v{lex.per} * s) + c);
		var get_exits = macro raw.get(($v{lex.table.length - 1} - s));
		if (!force_bytes) {
			get_trans = macro StringTools.fastCodeAt(raw, ($v{lex.per} * s) + c);
			get_exits = macro StringTools.fastCodeAt(raw, $v{lex.table.length - 1} - s);
		}
		var defs = macro class {
			static var raw = $raw;
			static var cases = [$a{cases}];
			static inline function trans(s: Int, c: Int):Int return $e{get_trans};
			static inline function exits(s: Int):Int return $e{get_exits};
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
			function _token(state: Int) {
				var i = pmax, len = input.length;
				if (i >= len) return $EEOF;
				pmin = i;
				var prev = $v{lex.per - 1}; // wrong state
				while (i < len) {
					var c = input.readByte(i++);
					state = trans(state, c);
					if (state >= $v{lex.segs})
						break;
					prev = state;
				}
				state = exits(state);
				return if (state < $v{lex.nrules}) {
					pmax = i;
					current = input.readString(pmin, pmax - pmin);
					cases[state](this);
				} else {
					state = exits(prev);
					if (state < $v{lex.nrules}) {
						pmax = i - 1; // one char one state
						current = input.readString(pmin, pmax - pmin);
						cases[state](this);
					} else {
						throw lm.Utils.error("UnMatached: " + pmin + "-" + pmax + ': "' + input.readString(pmin, i - pmin) + '"');
					}
				}
			}
		}// class end
		for (i in 0...groups.length) {
			var g = groups[i];
			var meta = lex.metas[i];
			defs.fields.push({
				name: i == 0 ? "token" : g.name,
				access: [AInline, APublic],
				kind: FFun({
					args: [],
					ret: null,
					expr: macro return _token($v{meta.begin})
				}),
				pos: TPositionTools.here(),
			});
		}
		for (f in defs.fields)
				if (!all_fields.exists(f.name))
					ret.push(f);
		return ret;
	}
	static function transform(g: Group, e: Expr) {
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