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
	static function getMeta(metas: Array<MetadataEntry>) {
		var ret = {cmax: 255, eof: null};
		if (metas.length > 0)
			for (meta in metas[0].params) {
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
		var ct_lex = TPath({pack: cls.pack, name: cls.name});
		var meta = getMeta(cls.meta.extract(":rule"));
		var ret = [];
		var groups: Array<Group> = [];
		var idmap = new Map<String,String>();
		var all_fields = new haxe.ds.StringMap<Bool>();
		// transform
		for (f in Context.getBuildFields()) {
			if (f.access.indexOf(AStatic) > -1 && f.access.indexOf(AInline) == -1
			&& Lambda.exists(f.meta, m->m.name == ":skip") == false) {
				switch (f.kind) {
				case FVar(_, {expr: EArrayDecl(el)}):
					var g:Group = {name: f.name, rules: [], cases: []};
					for (e in el)
						switch (e.expr) {
						case EBinop(OpArrow, rule, e):
							g.rules.push(exprString(rule, idmap));
							g.cases.push(e);
						default:
							Context.error("Expected pattern => function", e.pos);
						}
					if (g.rules.length > 0) {
						if (Lambda.exists(groups, x->x.name == g.name))
							Context.error("Duplicate: " + g.name, f.pos);
						groups.push(g);
					}
					continue;
				case FVar(_, {expr: EConst(CString(s))}):
					idmap.set(f.name, s);
					continue;
				default:
				}
			}
			all_fields.set(f.name, true);
			ret.push(f);
		}
		if (groups.length == 0) return null;
		meta.cmax = meta.cmax & 255 | 15;
		var force_bytes = !Context.defined("js") || Context.defined("force_bytes");
		if (Context.defined("lex_str")) force_bytes = false; // force string as table format
		// lexEngine
		var c_all = [new lm.Charset.Char(0, meta.cmax)];
		var rules = [];
		var cases = [];
		for (g in groups) {
			rules.push( g.rules.map( s -> LexEngine.parse(s, c_all) ));
			for (i in 0...g.rules.length) {
				cases.push(macro (lex: $ct_lex) -> $e{g.cases[i]});
			}
		}
		var lex = new LexEngine(rules, meta.cmax);
		// generate
		var resname = "_" + StringTools.hex(haxe.crypto.Crc32.make(lex.table)).toLowerCase();
		var raw = macro haxe.Resource.getBytes($v{resname});
		if (force_bytes) {
			Context.addResource(resname, lex.table);
		} else {
			var out = haxe.macro.Compiler.getOutput() + ".lex-table";
			var dir = haxe.io.Path.directory(out);
			if (!sys.FileSystem.exists(dir))
				sys.FileSystem.createDirectory(dir);
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
		var useSwitch = lex.nrules < 6 || Context.defined("lex_switch");
		var gotos = useSwitch ? (macro cases(s, lex)) : (macro cases[s](lex));
		var defs = macro class {
			static var raw = $raw;
			static inline var NRULES = $v{lex.nrules};
			static inline var NSEGS = $v{lex.segs};
			static inline function trans(s: Int, c: Int):Int return $get_trans;
			static inline function exits(s: Int):Int return $get_exits;
			static inline function gotos(s: Int, lex: $ct_lex) return $gotos;
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
				if (i >= len) return $e{meta.eof};
				pmin = i;
				var prev = state;
				while (i < len) {
					var c = input.readByte(i++);
					state = trans(state, c);
					if (state >= NSEGS)
						break;
					prev = state;
				}
				state = exits(state);
				return if (state < NRULES) {
					pmax = i;
					current = input.readString(pmin, pmax - pmin);
					gotos(state, this);
				} else {
					state = exits(prev);
					if (state < NRULES) {
						pmax = i - 1; // one char one state
						current = input.readString(pmin, pmax - pmin);
						gotos(state, this);
					} else {
						throw lm.Utils.error("UnMatached: " + pmin + "-" + pmax + ': "' + input.readString(pmin, i - pmin) + '"');
					}
				}
			}
		}// class end
		var pos = TPositionTools.here();
		for (i in 0...groups.length) {
			var g = groups[i];
			var seg = lex.metas[i];
			var sname = i == 0 ? "BEGIN" : g.name.toUpperCase() + "_BEGIN";
			defs.fields.push( addInlineFVar(sname, seg.begin, pos) );
			defs.fields.push({
				name: i == 0 ? "token" : g.name,
				access: [AInline, APublic],
				kind: FFun({
					args: [],
					ret: null,
					expr: macro return _token($i{sname})
				}),
				pos: pos,
			});
		}
		// switch or functions array jump table.
		if (useSwitch) {
			cases = []; // rewrite cases
			for (g in groups)
				for (c in g.cases)
					cases.push(c);
			var ca = [];
			for (i in 0...cases.length - 1) {
				ca.push({values: [macro $v{i}], expr: cases[i]});
			}
			var eSwitch = {expr: ESwitch(macro (s), ca, cases[cases.length - 1]), pos: pos};
			defs.fields.push({
				name: "cases",
				access: [AStatic],
				kind: FFun({
					args: [{name: "s", type: macro: Int}, {name: "lex", type: ct_lex}],
					ret: null,
					expr: macro return $eSwitch,
				}),
				pos: pos,
			});
		} else {
			defs.fields.push({
				name: "cases",
				access: [AStatic],
				kind: FVar(null, macro [$a{cases}]),
				pos: pos,
			});
		}

		for (f in defs.fields)
				if (!all_fields.exists(f.name))
					ret.push(f);
		return ret;
	}
	static function exprString(e: Expr, map:Map<String,String>): String {
		return switch (e.expr) {
		case EConst(CString(s)): s;
		case EConst(CIdent(i)):
			var s = map.get(i);
			if (s == null)
				Context.error("Undefined identifier: " + i, e.pos);
			s;
		case EBinop(OpAdd, e1, e2):
			exprString(e1, map) + exprString(e2, map);
		case EParenthesis(e):
			exprString(e, map);
		default:
			Context.error("Invalid rule", e.pos);
		}
	}
	static function addInlineFVar(name, value, pos):Field {
		return {
			name: name,
			access: [AStatic, AInline],
			kind: FVar(null, macro $v{value}),
			pos: pos,
		}
	}
}
#else
class RuleBuilder {}
#end