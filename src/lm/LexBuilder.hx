package lm;

#if macro
import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
using haxe.macro.Tools;

private typedef Group = {
	name: String,
	rules: Array<{pat: Expr, action: Expr}>,
	unmatch: {pat: Expr, action: Expr},
}

class LexBuilder {

	@:persistent static public var lmap: Map<String, Map<String, String>>; // LexName => [PatternString => TokenString]

	static function getMeta(metas: Array<MetadataEntry>) {
		var ret = {cmax: 255, eof: null, isVoid: true};
		if (metas.length > 0)
			for (meta in metas[0].params) {
				switch (meta.expr) {
				case EConst(CInt(i)):
					ret.cmax = Std.parseInt(i) & 255 | 15;
				case EConst(CIdent(i)):
					ret.eof = meta;
					ret.isVoid = i == "Void";
				default:
				}
			}
		return ret;
	}

	// only for enum abstract XXX(Int)
	static function readIntTokens(t: Type, map: Map<String, Bool>) {
		if ( !Context.unify(t, Context.getType("Int")) ) return;
		return switch (t) {
		case TAbstract(_.get() => ab, _):
			for (f in ab.impl.get().statics.get())
				map.set(f.name, true);
		case _:
		}
	}

	static function fatalError(msg, p) return Context.fatalError("[lex build] " + msg , p);

	static public function build():Array<Field> {
		var cl = Context.getLocalClass().get();
		var ct_lex = TPath({pack: cl.pack, name: cl.name});
		var meta = getMeta(cl.meta.extract(":rule"));
		var tmap = new Map();
		var reflect = new Map(); // patternString => TokenString, Both "lmap" and "reflect" will be used for LR0Parser
		for (it in cl.interfaces) {
			if (it.t.toString() == "lm.Lexer") {
				var t = it.params[0];
				if (meta.isVoid) {
					if (!Context.unify(t, Context.getType("Void"))) {
						if (meta.eof == null) {
							fatalError("Need an identifier as the Token terminator by \"@:rule\"", cl.pos);
						} else {
							fatalError("\"<" + t.toString() + ">\" should be \"Void\"", cl.pos);
						}
					}
					meta.eof = null;  // for "EReturn(meta.eof)"
				} else {
					if (!Context.unify(t, Context.typeof(meta.eof)))
						fatalError('Unable to unify "' + t.toString() + '" with "' + meta.eof.toString() + '"', cl.pos);
					readIntTokens(t, tmap);
				}
				if (lmap == null) lmap = new Map();
				lmap.set(Utils.getClsFullName(cl), reflect); // store
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
					var g:Group = {name: f.name, rules: [], unmatch: null};
					for (e in el) {
						switch (e.expr) {
						case EBinop(OpArrow, s, e):
							switch(s.expr) {
							case EConst(CIdent("null")):
								g.unmatch = {pat: s, action: e};
							default:
								g.rules.push({pat: s, action: e});
							}
						default:
							fatalError("Expected pattern => function", e.pos);
						}
					}
					for (x in groups)
						if (x.name == g.name)
							fatalError("Duplicated: " + g.name, f.pos);
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

		var c_all = [new lm.Charset.Char(0, meta.cmax)];
		var apats = [];
		var have_int_tokens = !Lambda.empty(tmap);
		for (g in groups) {
			var pats = [];
			for (r in g.rules) {
				if (have_int_tokens) {
					// if "action" is just a simple Token then store it for "reflect"
					switch(r.action.expr) {
					case EConst(CIdent(v)):
						var k = unescape(r.pat, idmap);
						if ( !tmap.exists(v) )
							fatalError("Unknown identifier: " + v, r.action.pos);
						reflect.set(k, v);
					case _:
					}
				}
				// String -> Pattern
				try {
					var pat = exprString(r.pat, idmap);
					pats.push( LexEngine.parse(pat, c_all) );
				} catch(err: Dynamic) {
					fatalError(Std.string(err), r.pat.pos);
				}
			}
			apats.push(pats);
		}
		// lexEngine
		var lex = new LexEngine(apats, meta.cmax);
		#if lex_table
		var f = sys.io.File.write("lex-table.txt");
		lex.debugWrite(f);
		f.close();
		#end
		// check & hacking
		var casesExtra = checkAHack(lex, groups);
		// generate
		var force_bytes = !Context.defined("js") || Context.defined("lex_rawtable");
		// force string as table format if `-D lex_strtable` and ucs2
		if (Context.defined("lex_strtable") && Context.defined("utf16")) force_bytes = false;
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
			raw = macro $v{ lex.table.map(i -> String.fromCharCode(i)).join("") };
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
			public var input(default, null): lms.ByteData;
			public var pmin(default, null): Int;
			public var pmax(default, null): Int;
			public var current(get, never): String;
			inline function get_current():String return input.readString(pmin, pmax - pmin);
			public inline function getString(p, len):String return input.readString(p, len);
			public function new(s: lms.ByteData) {
				this.input = s;
				pmin = 0;
				pmax = 0;
			}
			function _token(state:Int, right:Int) {
				var i = pmax;
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
					-- i;
				}
				var q = exits(state);
				if (q < NRULES) {
					pmin = pmax; // update
					pmax = i;
				} else if (i >= right) {
					return $e{meta.eof};
				} else {
					pmin = i;    // used for Error, the position of the UnMatached char
				}
				$e{ meta.isVoid ? macro cases(q, this) : macro return cases(q, this) }
			}
		}// class end
		var pos = Context.currentPos();
		for (i in 0...groups.length) {
			var g = groups[i];
			var seg = lex.entrys[i];
			var sname = i == 0 ? "BEGIN" : g.name.toUpperCase() + "_BEGIN";
			var expr_token = macro _token($i{sname}, this.input.length);
			defs.fields.push( addInlineFVar(sname, seg.begin, pos) );

			defs.fields.push({
				name: i == 0 ? "token" : g.name,
				access: [AInline, APublic],
				kind: FFun({
					args: [],
					ret: null,
					expr: meta.isVoid ? expr_token : (macro return $expr_token)
				}),
				pos: pos,
			});
		}
		// build switch
		var casesA: Array<Case> = [];
		casesA.resize( lex.nrules + casesExtra.length );
		var i = 0;
		for (g in groups) {
			for (rule in g.rules) {
				casesA[i] = {values: [macro @:pos(rule.pat.pos) $v{i}], expr: rule.action}; // $v{i} & [i]
				++ i;
			}
		}
		for (c in casesExtra)
			casesA[i++] = c;
		var caseDef = macro throw lm.Utils.error("UnMatached char: '" + lex.input.readString(lex.pmin, 1) + "'" + lm.Utils.posString(lex.pmin, lex.input));
		var eSwitch = {expr: ESwitch(macro (s), casesA, caseDef), pos: pos};
		defs.fields.push({
			name: "cases",
			access: [AStatic],
			kind: FFun({
				args: [{name: "s", type: macro: Int}, {name: "lex", type: ct_lex}],
				ret: null,
				expr: meta.isVoid ? eSwitch : (macro return $eSwitch)
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
				fatalError("Undefined identifier: " + i, e.pos);
			s;
		case EBinop(OpAdd, e1, e2):
			splitMix(exprString(e1, map), exprString(e2, map));
		case EParenthesis(e):
			exprString(e, map);
		default:
			fatalError("Invalid rule", e.pos);
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

	static function checkAHack(lex: lm.LexEngine, groups: Array<Group>) : Array<Case> {
		var table = lex.table;
		var INVALID = lex.invalid;
		var exits = new haxe.ds.Vector<Int>(lex.nrules);
		// Init vector
		for (n in 0...lex.nrules) exits[n] = INVALID;
		// Scan the "exit" segment,            (state => quit)
		for (i in table.length - lex.perExit...table.length) {
			var n = table.get(i);
			if (n == INVALID) continue;
			exits[n] = table.length - 1 - i; //(quit  => state), This value(state) is not used here, could be any non-invalid value
		}
		function indexPattern(i):Expr {
			for (g in groups) {
				var len = g.rules.length;
				if (i >= len) {
					i -= len;
				} else {
					return g.rules[i].pat;
				}
			}
			throw "NotFound";
		}
		// reachable
		for (n in 0...lex.nrules)
			if (exits[n] == INVALID) {
				var pat = indexPattern(n);
				Context.fatalError("UnReachable pattern: " + pat.toString(), pat.pos);
			}

		// epsilon
		for (e in lex.entrys) {
			var n = table.exits(e.begin);
			if (n != INVALID) {
				var pat = indexPattern(n);
				Context.fatalError("epsilon is not allowed: " + pat.toString(), pat.pos);
			}
		}

		// hacking for "null => Actoin", must be after epsilon checking
		var start = lex.nrules;
		var extra = [];
		for (i in 0...groups.length) {
			var g = groups[i];
			if (g.unmatch != null) {
				var e = lex.entrys[i];
				lex.table.set(lex.table.exitpos(e.begin), start);
				extra.push( {values: [macro @:pos(g.unmatch.pat.pos) $v{start}], expr: g.unmatch.action } );
				++start;
			}
		}
		return extra;
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