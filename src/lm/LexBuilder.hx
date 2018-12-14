package lm;

#if macro
import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
using haxe.macro.Tools;

private typedef Group = {
	name: String,
	rules: haxe.ds.Vector<{es: Expr, action: Expr}>,
}

class LexBuilder {

	static public var lmap: Map<String, Map<String, String>>; // LexName => [PatternString => TokenString]

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

	// only for enum abstract XXX(Int)
	static function absTokens(t: Type, map: Map<String, Bool>) {
		return switch (t) {
		case TAbstract(_.get() => ab, _):
			for (f in ab.impl.get().statics.get())
				map.set(f.name, true);
			true;
		case _:
			false;
		}
	}

	static public function build():Array<Field> {
		if (lmap == null)
			lmap = new Map();
		var p2t = new Map(); // patternString => TokenString
		var cls = Context.getLocalClass().get();
		var ct_lex = TPath({pack: cls.pack, name: cls.name});
		var meta = getMeta(cls.meta.extract(":rule"));
		if (meta.eof == null)
			Context.fatalError("Need an identifier as the Token terminator by \"@:rule\"", cls.pos);
		var tmap = new Map();
		var abst = false;
		for (it in cls.interfaces) {
			if (it.t.toString() == "lm.Lexer") {
				var t = it.params[0];
				if (Context.unify(t, Context.typeof(meta.eof)) == false)
					Context.fatalError('Unable to unify "' + t.toString() + '" with "' + meta.eof.toString() + '"', cls.pos);
				abst = absTokens(t, tmap);
				lmap.set(Utils.getClsFullName(cls), p2t); // store
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
				case FVar(_, {expr: EArrayDecl(el)}) if (el.length > 0):
					var i = 0;
					var len = el.length;
					var g:Group = {name: f.name, rules: new haxe.ds.Vector(len)};
					for (e in el) {
						switch (e.expr) {
						case EBinop(OpArrow, s, e):
							g.rules[i++] = {es: s, action: e};
						default:
							Context.fatalError("Expected pattern => function", e.pos);
						}
					}
					for (x in groups)
						if (x.name == g.name)
							Context.fatalError("Duplicated: " + g.name, f.pos);
					groups.push(g);
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

		var c_all = [new lm.Charset.Char(0, meta.cmax)];
		var apats = [];
		for (g in groups) {
			var pats = [];
			for (r in g.rules) {
				if (abst) {
					// if "action" is just a simple Token then store it for "reflect"
					switch(r.action.expr) {
					case EConst(CIdent(v)):
						var k = unescape(r.es, idmap);
						if ( !tmap.exists(v) )
							Context.fatalError("Unknown identifier: " + v, r.action.pos);
						p2t.set(k, v);
					case _:
					}
				}
				// String -> Pattern
				try {
					pats.push( LexEngine.parse(exprString(r.es, idmap), c_all) );
				} catch(err: Dynamic) {
					Context.fatalError(Std.string(err), r.es.pos);
				}
			}
			apats.push(pats);
		}
		// lexEngine
		var lex = new LexEngine(apats, meta.cmax);
		#if lex_table
		var f = sys.io.File.write("lex-table.txt");
		lex.write(f, true);
		f.close();
		#end
		// checking
		reachable(lex, groups);

		// generate
		var force_bytes = !Context.defined("js") || Context.defined("lex_rawtable");
		if (Context.defined("lex_strtable")) force_bytes = false; // force string as table format
		if (lex.isBit16() && force_bytes == false && !Context.defined("utf16")) force_bytes = true; // if platform doesn't support ucs2 then force bytes.
		var getU: Expr = null;
		var raw: Expr = null;
		if (force_bytes) {
			var bytes = lex.table.toByte(lex.isBit16());
			var resname = "_" + StringTools.hex(haxe.crypto.Crc32.make(bytes)).toLowerCase();
			Context.addResource(resname, bytes);
			#if hl
			getU = lex.isBit16() ? macro raw.getUI16(i << 1) : macro raw.get(i);
			raw = macro haxe.Resource.getBytes($v{resname}).getData().bytes;
			#else
			getU = lex.isBit16() ? macro raw.getUInt16(i << 1) : macro raw.get(i);
			raw = macro haxe.Resource.getBytes($v{resname});
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
			getU = macro StringTools.fastCodeAt(raw, i);
		}
		var defs = macro class {
			static var raw = $raw;
			static inline var INVALID = $v{lex.invalid};
			static inline var NRULES = $v{lex.nrules};
			static inline var NSEGS = $v{lex.segs};
			static inline function getU(i:Int):Int return $getU;
			static inline function trans(s:Int, c:Int):Int return getU($v{lex.per} * s + c);
			static inline function exits(s:Int):Int return getU($v{lex.table.length - 1} - s);
			static inline function rollB(s:Int):Int return getU(s + $v{lex.posRB()});
			static inline function rollL(s:Int):Int return getU(s + $v{lex.posRBL()});
			static inline function gotos(s:Int, lex: $ct_lex) return cases(s, lex);
			public var input(default, null): lms.ByteData;
			public var pmin(default, null): Int;
			public var pmax(default, null): Int;
			public var current(get, never): String;
			inline function get_current():String return input.readString(pmin, pmax - pmin);
			public inline function getString(p, len):String return input.readString(p, len);
			public inline function curpos() return new lms.Position(pmin, pmax);
			public function new(s: lms.ByteData, ?len:Int) {
				this.input = s;
				pmin = 0;
				pmax = 0;
			}
			function _token(state:Int, right:Int) {
				var i = pmax;
				pmin = i;
				if (i >= right) return $e{meta.eof};
				var prev = state;
				while (i < right) {
					var c = input.readByte(i++);
				#if lex_charmax
					if (c > $v{meta.cmax}) c = $v{meta.cmax};
				#end
					state = trans(state, c);
					if (state >= NSEGS)
						break;
					prev = state;
				}
				if (state == INVALID) {
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
		var pos = Context.currentPos();
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
					expr: macro return _token($i{sname}, this.input.length)
				}),
				pos: pos,
			});
		}
		// build switch
		var actions = [];
		actions.resize(lex.nrules);
		var i = 0;
		for (g in groups)
			for (r in g.rules)
				actions[i++] = r.action;
		var defCase = actions.pop();
		var liCase: Array<Case> = [];
		liCase.resize(actions.length);
		for (i in 0...actions.length) {
			liCase[i] = {values: [macro $v{i}], expr: actions[i]};
		}
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
				Context.fatalError("Undefined identifier: " + i, e.pos);
			s;
		case EBinop(OpAdd, e1, e2):
			splitMix(exprString(e1, map), exprString(e2, map));
		case EParenthesis(e):
			exprString(e, map);
		default:
			Context.fatalError("Invalid rule", e.pos);
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

	static function reachable(lex: lm.LexEngine, groups: Array<Group>) {
		var table = lex.table;
		var INVALID = lex.invalid;
		var exits = new haxe.ds.Vector<Int>(lex.nrules);
		for (n in 0...lex.nrules)
			exits[n] = INVALID;
		for (i in table.length-lex.perRB...table.length) {
			var n = table.get(i);
			if (n == INVALID) continue;
			exits[n] = table.length - 1 - i;
		}
		function indexPattern(i):Expr {
			for (g in groups) {
				var len = g.rules.length;
				if (i >= len) {
					i -= len;
				} else {
					return g.rules[i].es;
				}
			}
			throw "NotFound";
		}
		for (n in 0...lex.nrules)
			if (exits[n] == INVALID)
				Context.fatalError("UnReachable pattern", indexPattern(n).pos);
	}

	// assoc with lexEngine.parse
	static function unescape(es: Expr, idmap:Map<String,String>) {
		var s = exprString(es, idmap);
		var i = 0;
		var p = 0;
		var len = s.length;
		var buf = new StringBuf();
		while (i < len) {
			var c = StringTools.fastCodeAt(s, i++);
			if (c == "\\".code) {
				buf.addSub(s, p, i - p - 1);
				c = StringTools.fastCodeAt(s, i++);
				switch(c) {
				case "\\".code, "+".code, "*".code, "?".code, "[".code, "]".code, "-".code, "|".code:
					buf.addChar(c);
				case "x".code, "X".code:
					c = Std.parseInt("0x" + s.substr(i, 2));
					buf.addChar(c);
					i += 2;
				case _:
					Context.fatalError("Invalid hexadecimal escape sequence", es.pos);
				}
				p = i;
			}
		}
		if (p < i)
			buf.addSub(s, p, i - p);
		return buf.toString();
	}

	// ("a", "0")  => "a0"
	// ("a|b","0") => "a0|b0"
	// ("a","0|1") => "a0|a1"
	// ("a|b","0|1") => "a0|a1|b0|b1"
	static function splitMix(s1:String, s2:String): String {
		function split(s:String): Array<String> {
			var i = 0;
			var left = 0;
			var len = s.length;
			var ret = [];
			// Ignore all "|" at the beginning
			while (i < len) {
				var c = StringTools.fastCodeAt(s, i);
				if (c == "|".code)
					++ i;
				else
					break;
			}
			while (i < len) {
				var c = StringTools.fastCodeAt(s, i++);
				if (c == "|".code && i < len && StringTools.fastCodeAt(s, i - 2) != "\\".code) {
					ret.push( s.substr(left, i - left - 1) );
					left = i;
				}
			}
			if (left == 0) {
				ret.push(s);
			} else if (i > left) {
				ret.push( s.substr(left, i - left) );
			}
			return ret;
		}
		function mix(a1: Array<String>, a2: Array<String>): Array<String>{
			var ret = [];
			for (x in a1)
				for (y in a2)
					ret.push(x + y);
			return ret;
		}
		return mix(split(s1), split(s2)).join("|");
	}
}
#else
class RuleBuilder {}
#end