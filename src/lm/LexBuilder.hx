package lm;

#if macro
import lm.ExprHelps;
import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
using haxe.macro.Tools;

class LexBuilder {

	/**
	 LexerClassName => [PatternString => TokenString], it will be used by LR0Parser.
	*/
	@:persistent static public var lmap : Map<String, Map<String, String>> = new Map();

	static function getMeta( metas : Array<MetadataEntry> ) : {cmax : Int, eof : Null<Expr>} {
		var ret = {cmax : 255, eof : null};
		if (metas.length > 0)
			for (meta in metas[0].params) {
				switch (meta.expr) {
				case EConst(CInt(i)):
					ret.cmax = Std.parseInt(i) & 255 | 15;
				case EConst(CIdent(i)):
					ret.eof = meta;
				default:
				}
			}
		return ret;
	}

	static function readIntTokens( t : Type, out : Map<String, Bool> ) {
		if ( !Context.unify(t, Context.getType("Int")) )
			return;
		switch (t) {
		case TAbstract(_.get() => ab, _):
			for (f in ab.impl.get().statics.get()) {
				if (Utils.isLowerCaseFirst(f.name))
					throw new Error("The first char should be uppercase : " + f.name, f.pos);
				out.set(f.name, true);
			}
		case _:
		}
	}

	static public function build() : Array<Field> {
		return try {
			buildInner();
		} catch ( e : Error ) {
			Context.fatalError("[lex build] " + e.message , e.pos);
		}
	}

	static public function buildInner() : Array<Field> {
		var cl = Context.getLocalClass().get();
		var ct_lex = TPath({pack: cl.pack, name: cl.name});
		var meta = getMeta(cl.meta.extract(":rule"));
		var tmap = new Map();    // TokenString   => Bool, used for LR0Parser
		var reflect = new Map(); // PatternString => TokenString, used for LR0Parser
		for (it in cl.interfaces) {
			if (it.t.toString() == "lm.Lexer") {
				var t = it.params[0];
				if (!Context.unify(t, Context.typeof(meta.eof)))
					throw new Error('Unable to unify "' + t.toString() + '" with "' + meta.eof.toString() + '"', cl.pos);
				readIntTokens(t, tmap);
				lmap.set(ExprHelps.classFullName(cl), reflect);
				break;
			}
		}
		var ret = [];
		var groups : Array<RuleCaseGroup> = [];
		var varmap = new Map<String, Expr>();         // variable name => patternString
		var reserve = new haxe.ds.StringMap<Bool>();  // reserved fields
		// transform
		for (f in Context.getBuildFields()) {
			if (f.access.indexOf(AInline) == -1
			&& f.access.indexOf(AFinal) == -1
			&& Lambda.exists(f.meta, m->m.name == ":skip") == false) { // no inline, no @:skip
				switch (f.kind) {
				case FVar(_, {expr: EArrayDecl(el)}) if (el.length > 0):
					var g : RuleCaseGroup = {name: f.name, rules: [], unmatch: null};
					for (e in el) {
						switch (e.expr) {
						case EBinop(OpArrow, s, e):
							switch(s.expr) {
							case EConst(CIdent(i)) if (i == "null" || i == "_"):
								if (g.unmatch != null)
									throw new Error("Duplicated: " + i, s.pos);
								g.unmatch = {pattern: s, action: e};
							default:
								g.rules.push({pattern: s, action: e});
							}
						default:
							throw new Error("Expected pattern => function", e.pos);
						}
					}
					for (x in groups)
						if (x.name == g.name)
							throw new Error("Duplicated: " + g.name, f.pos);
					groups.push(g);
					continue;
				case FVar(_, e = {expr : _}):
					switch(e.expr) {
					case EConst(CString(_)), EBinop(OpAdd, _, _), EBinop(OpOr, _, _):
						varmap.set(f.name, e);
						continue;
					default:
					}
				default:
				}
			}
			reserve.set(f.name, true);
			ret.push(f);
		}
		if (groups.length == 0) return null;

		var paterns = [];
		var charall = [new lm.Charset.Char(0, meta.cmax)];
		var empty_tokmap = Lambda.empty(tmap);
		for (g in groups) {
			var pats = [];
			for (r in g.rules) {
				var pattern = ExprHelps.parsePaterns(r.pattern, varmap, charall);
				pats.push(pattern);
				if (empty_tokmap)
					continue;
				// reflect for LR0 Parser, if "action" is a simple Token then store it.
				parseReflect(r.pattern, r.action, tmap, reflect, true);
			}
			paterns.push(pats);
		}
		// lexEngine
		var lex = new LexEngine(paterns, meta.cmax);
		#if lex_table
		var f = sys.io.File.write("lex-table.txt");
		lex.debugWrite(f);
		f.close();
		#end
		// check & hacking
		ExprHelps.lexChecking(lex, groups);
		var nullCases = ExprHelps.lexUnMatchedActions(lex, groups);
		// generate
		var forceBytes = !Context.defined("js") || Context.defined("lex_rawtable");
		// force string as table format if `-D lex_strtable` and ucs2
		if (Context.defined("lex_strtable") && Context.defined("utf16")) forceBytes = false;
		var getU: Expr = null;
		var raw: Expr = null;
		if (forceBytes) {
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
		var charmax_limit = if (lex.per == 128 || Context.defined("lex_charmax") || !forceBytes) {
			macro if (c > $v{meta.cmax}) c = $v{meta.cmax}
		} else {
			macro {}
		}
		var defs = macro class {
			static var raw = $raw;
			static inline var INVALID = $v{lex.invalid};
			static inline var NRULES = $v{lex.nrules};
			static inline var NSEGS = $v{lex.segs};
			static inline function getU(raw, i) return $getU;
			static inline function trans(r, s, c):Int return getU(r, $v{lex.per} * s + c);
			static inline function exits(r, s):Int return getU(r, $v{lex.table.length - 1} - s);
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
			function _token( init : Int, right : Int ) {
				if (pmax >= right) {
					pmin = pmax;
					return $e{meta.eof};
				}
				var raw = raw;
				var i = pmax;
				var state = init;
				var prev = state;
				var c : Int;
				while (i < right) {
					c = input.readByte(i++);
					$charmax_limit;
					state = trans(raw, state, c);
					if (state >= NSEGS)
						break;
					prev = state;
				}
				pmin = i; // if UnMatached
				if (state == INVALID) {
					state = prev;
					-- i;
				}
				var q = exits(raw, state);
				if (i > pmax && q < NRULES) {
					pmin = pmax; // update
					pmax = i;
				} else {
					q = exits(raw, init); // goto "null => Action" or "throw"
				}
				return cases(q);
			}
		}// class end
		var pos = Context.currentPos();
		for (i in 0...groups.length) {
			var g = groups[i];
			var seg = lex.entrys[i];
			var sname = i == 0 ? "BEGIN" : g.name.toUpperCase() + "_BEGIN";
			var expr_token = macro _token($i{sname}, this.input.length);
			defs.fields.push( mkInlineFVar(sname, seg.begin, pos) );

			defs.fields.push({
				name: i == 0 ? "token" : g.name,
				access: [AInline, APublic],
				kind: FFun({
					args: [],
					ret: null,
					expr: macro return $expr_token
				}),
				pos: pos,
			});
		}
		// build switch
		var ecases: Array<Case> = [];
		ecases.resize( lex.nrules + nullCases.length );
		var i = 0;
		for (g in groups) {
			for (rule in g.rules) {
				ecases[i] = {values: [macro @:pos(rule.pattern.pos) $v{i}], expr: rule.action}; // $v{i} & [i]
				++ i;
			}
		}
		for (c in nullCases)
			ecases[i++] = c;

		var edef = if (groups[0].unmatch == null) {
			macro throw("UnMatached: '" + lex.input.readString(lex.pmax, lex.pmin - lex.pmax) + "'" + lm.Utils.posString(lex.pmax, lex.input));
		} else {
			groups[0].unmatch.action;
		}
		var eswitch = {expr: ESwitch(macro (s), ecases, edef), pos: pos};
		defs.fields.push({
			name: "cases",
			access: [],
			kind: FFun({
				args: [{name: "s", type: macro: Int}],
				ret: null,
				expr: macro { final lex = this; return $eswitch; }
			}),
			pos: pos,
		});

		for (f in defs.fields)
				if (!reserve.exists(f.name))
					ret.push(f);
		return ret;
	}

	static function parseReflect( e : Expr, action : Expr, token : Map<String, Bool>, reflect : Map<String, String>, strict : Bool ) {
		switch (e.expr) {
		case EConst(CString(spat)):
			switch(action.expr) {
			case EConst(CIdent(v)):
				if (!token.exists(v))
					if (strict)
						throw new Error("Unknown identifier: " + spat, action.pos);
					else
						return;
				try {
					reflect.set(Utils.unescape(spat), v); // unescape will throw an error
				} catch(x) {
					throw new Error(Std.string(x), e.pos);
				}
			case EBlock(a) if (a.length > 0):
				parseReflect(e, a[a.length - 1], token, reflect, false);
			case _:
			}
		default:
		}
	}

	static function mkInlineFVar(name, value, pos) : Field {
		return {
			name: name,
			access: [AStatic, AInline],
			kind: FVar(null, macro $v{value}),
			pos: pos,
		}
	}




}
#else
class LexBuilder {}
#end