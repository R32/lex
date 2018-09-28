package lm;

#if macro
import lm.LexEngine.INVALID;
import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
using haxe.macro.Tools;

private typedef Group = {
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
		if (meta.eof == null)
			Context.error("Need an identifier as the Token terminator by \"@:rule\"", cls.pos);
		for (it in cls.interfaces) {
			if (it.t.toString() == "lm.Lexer") {
				var t = it.params[0];
				if (Context.unify(t, Context.typeof(meta.eof)) == false)
					Context.error('Unable to unify "' + t.toString() + '" with "' + meta.eof.toString() + '"', cls.pos);
				break;
			}
		}
		var ret = [];
		var groups: Array<Group> = [];
		var idmap = new Map<String,String>();
		var all_fields = new haxe.ds.StringMap<Bool>();
		// transform
		for (f in Context.getBuildFields()) {
			if (f.access.indexOf(AStatic) > -1 && f.access.indexOf(AInline) == -1
			&& Lambda.exists(f.meta, m->m.name == ":skip") == false) { // static, no inline, no @:skip
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
		var force_bytes = !Context.defined("js") || Context.defined("lex_rawtable");
		if (Context.defined("lex_strtable")) force_bytes = false; // force string as table format
		// lexEngine
		var c_all = [new lm.Charset.Char(0, meta.cmax)];
		var rules = groups.map( g -> g.rules.map(s->LexEngine.parse(s, c_all)) );
		var lex = new LexEngine(rules, meta.cmax);
		#if lex_table
		var f = sys.io.File.write("lex-table.txt");
		lex.write(f, true);
		f.close();
		#end
		// generate
		var resname = "_" + StringTools.hex(haxe.crypto.Crc32.make(lex.table)).toLowerCase();
		var raw = macro haxe.Resource.getBytes($v{resname});
		var getU8 = macro raw.get(i);
		if (force_bytes) {
			Context.addResource(resname, lex.table);
			#if hl
			raw = macro haxe.Resource.getBytes($v{resname}).getData().bytes;
			#end
		} else {
			var out = haxe.macro.Compiler.getOutput() + ".lex-table";
			var dir = haxe.io.Path.directory(out);
			if (!sys.FileSystem.exists(dir))
				sys.FileSystem.createDirectory(dir);
			var f = sys.io.File.write(out);
			lex.write(f);
			f.close();
			raw = macro ($e{haxe.macro.Compiler.includeFile(out, Inline)});
			getU8 = macro StringTools.fastCodeAt(raw, i);
		}
		var defs = macro class {
			static var raw = $raw;
			static inline var NRULES = $v{lex.nrules};
			static inline var NSEGS = $v{lex.segs};
			static inline function getU8(i:Int):Int return $getU8;
			static inline function trans(s:Int, c:Int):Int return getU8($v{lex.per} * s + c);
			static inline function exits(s:Int):Int return getU8($v{lex.table.length - 1} - s);
			static inline function rollB(s:Int):Int return getU8(s + $v{lex.posRB()});
			static inline function rollL(s:Int):Int return getU8(s + $v{lex.posRBL()});
			static inline function gotos(s:Int, lex: $ct_lex) return cases(s, lex);
			public var input(default, null): lms.ByteData;
			public var pmin(default, null): Int;
			public var pmax(default, null): Int;
			public var current(get, never): String;
			inline function get_current():String return input.readString(pmin, pmax - pmin);
			public inline function getString(p, len):String return input.readString(p, len);
			public inline function curpos() return new lm.Position(pmin, pmax);
			public function new(s: lms.ByteData) {
				this.input = s;
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
				#if lex_charmax
					if (c > $v{meta.cmax}) c = $v{meta.cmax};
				#end
					state = trans(state, c);
					if (state >= NSEGS)
						break;
					prev = state;
				}
				if (state == $v{INVALID}) {
					state = prev;
					prev = 1; // use prev as dx.
				} else {
					prev = 0;
				}
				var q = exits(state);
				if (q < NRULES) {
					pmax = i - prev;
				} else {
					q = rollB(state);
					if (q < NRULES) {
						pmax = i - prev - rollL(state);
					} else {
						throw lm.Utils.error("UnMatached: " + pmin + "-" + pmax + ': "' + input.readString(pmin, i - pmin) + '"');
					}
				}
				return gotos(q, this);
			}
		}// class end
		var pos = TPositionTools.here();
		for (i in 0...groups.length) {
			var g = groups[i];
			var seg = lex.entrys[i];
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
		// build switch
		var cases = Lambda.flatten( groups.map(g -> g.cases) );
		var defCase = cases.pop();
		var liCase = Lambda.mapi( cases, (i, e)->({values: [macro $v{i}], expr: e}: Case) );
		var eSwitch = {expr: ESwitch(macro (s), liCase, defCase), pos: pos};
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