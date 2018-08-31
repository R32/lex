package lm;

#if macro
import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
import lm.Charset;
import lm.LexEngine;

using haxe.macro.Tools;

typedef Udt = {    // from User Defined enum abstract
	t: Bool,       // terminal or not
	name: String,  // identifier
	value: Int,    // similar to char
	cset: Charset, // CSet.single(value)
	pos: Position, // haxe.macro.Position
}

typedef Symbol = { // from switch cases
	t: Bool,
	name: String,  // maybe should be removed. e.g: CStr("const") then name = "const" for terminal | e = expr then name = "expr" for non-terminal
	cset: Charset,
	ex: String,    // extract name. CStr(s)| then ex = "s" for terminal   | e = expr then ex = "e" for non-terminal
	pos: Position,
}

typedef SymbolSet = {
	expr: Expr,
	syms: Array<Symbol>,
}

typedef Rules = {  // all cases of a switch.
	name: String,
	value: Int,
	epsilon: Bool,
	cases: Array<SymbolSet>,
	rec: Bool,     // is self nesting?
	pure: Bool,    // have one case in cases that doesn't contain any non-terminals
	subs: Array<String>,        // used to verify if there is infinite recursion
	leftsubs: Array<String>,    // for combine something..
	pos: Position,
}

typedef RuleSet = Array<Rules>;    // all switches.


class LR0Builder {
	static var termls: Array<Udt> = [];             // all Terminal
	static var termlCSet: Charset = [];             // Terminal Universal Set.
	static var udtmap: Map<String,Udt> = new Map(); // Terminal + Non-Terminal
	static var maxValue: Int = 0;                   // if value >= maxValue then it must be a non-terminal
	//static var ruleset: RuleSet;
	static var rulemap = new Map<String,Rules>();

	static public function build() {
		var cls = Context.getLocalClass().get();
		var tk:Type = null;
		for (it in cls.interfaces)
			if (it.t.toString() == "lm.LR0") {
				tk = it.params[0];
				break;
			}
		if (Context.unify(tk, Context.getType("Int")) == false)
			Context.error("Only \"enum abstract (Int)\" type is supported", cls.pos);

		maxValue = 1 + calcTerminal(tk, termls);
		for (t in termls) {
			udtmap.set(t.name, t);
			termlCSet = CSet.union(termlCSet, t.cset);
		}

		var ret = [];
		var allFields = new haxe.ds.StringMap<Bool>();
		var switches = filter(Context.getBuildFields(), allFields, maxValue);
		for (sw in switches) {
			udtmap.set(sw.name, {t: false, name: sw.name, value: sw.value, cset: CSet.single(sw.value), pos: sw.pos});
		}
		// rule parsing
		var ruleset = switches.map(transformCases);
		// duplicate checking
		for (r in ruleset) {
			if (rulemap.exists(r.name))
				Context.error("Duplicate rule field declaration: " + r.name, r.pos);
			rulemap.set(r.name, r);
		}
		// dead rec checking
		for (rule in ruleset) {
			if (rule.pure) continue;
			var done = false;
			for (name in rule.subs) {
				var sr = rulemap.get(name);
				if (sr.pure) {
					done = true;
					break;
				}
			}
			if (!done) Context.error("Need Terminal case: " + rule.name, rule.pos);
		}
		// combine
		var rules = combine(ruleset);
		var ap = rules.map(toParterns);
		var plex = new LexEngine(ap);
		// recalculation, according to nstates
		plex = new LexEngine(ap, (plex.nstates + 1) | 15);

		//var f = sys.io.File.write("r:/lr0.txt");
		//plex.write(f, true);
		//f.close();
		// generate
		var force_bytes = !Context.defined("js") || Context.defined("force_bytes");
		var resname = "_" + StringTools.hex(haxe.crypto.Crc32.make(plex.table)).toLowerCase();
		var raw = macro haxe.Resource.getBytes($v{resname});
		if (force_bytes) {
			Context.addResource(resname, plex.table);
		} else {
			var out = haxe.macro.Compiler.getOutput() + ".lr0-table";
			var dir = haxe.io.Path.directory(out);
			if (!sys.FileSystem.exists(dir))
				sys.FileSystem.createDirectory(dir);
			var f = sys.io.File.write(out);
			plex.write(f);
			f.close();
			raw = macro ($e{haxe.macro.Compiler.includeFile(out, Inline)});
		}
		var get_trans = macro raw.get($v{plex.per} * s + c);
		var get_exits = macro raw.get(($v{plex.table.length - 1} - s));
		if (!force_bytes) {
			get_trans = macro StringTools.fastCodeAt(raw, ($v{plex.per} * s) + c);
			get_exits = macro StringTools.fastCodeAt(raw, $v{plex.table.length - 1} - s);
		}
		//var lex_switch = false;
		//var gotos = lex_switch ? (macro cases(s, lex)) : (macro cases[s](lex));
		var defs = macro class {
			static var raw = $raw;
			static inline function trans(s: Int, c: Int):Int return $get_trans;
			static inline function exits(s: Int):Int return $get_exits;
			//static inline function gotos(s: Int, lex: $ct_lex) return $gotos;
			var stream: lm.Stream;
			var stack: Array<Int>; // state stack.
			public function new(lex: lm.Lexer<Int>){
				this.stream = new lm.Stream(lex);
				stack = [];
			}
			function _fixme_(state) {
				var i = 0;
				stack[i] = state;
				while (stack[i] < $v{plex.segs}) {
					var t = stream.peek(i++); // start at 0.
					stack[i] = trans(stack[i - 1], t.char);
				}
				while (i > 1) {
					state = exits(stack[i]);
					if (state < $v{plex.nrules}) {
						// 匹配成功, 开始归约.
						// var v = cases(state, stream);

						// gotos's return type is generic.
						// checking for reduction
						// 由于不可能以终结符结束, 因此总是归约, 除非. 遇 ACC 才退出. 即 state == 0
						// prev
					} else {
						// 单独地归约, 问题是选择
						// trans(state, t.char)

					}
					-- i;
				}

				// if have epsilon
				//if ($v{ruleset[0].eplison}) {
				//	return
				//}
			}
		}
		return ret;
	}

	/**
	 A => Ct | Dt
	 B => At
	 C => Et
	 D => t
	 E => t
	 X => Yt
	 Y => t
	 result: [[A, B, C, D, E],  [X, Y]]
	*/
	static function combine(rules: RuleSet) {
		function inter(a:RuleSet, b:RuleSet) {
			for (r in b)
				if (a.indexOf(r) > 0)
					return true;
			return false;
		}
		function union(acc:RuleSet, b:RuleSet) {
			for (r in b)
				if (acc.indexOf(r) == -1)
					acc.push(r);
		}
		function adding(top: String, acc: RuleSet, names: Array<String>) {
			var subs = names.map(n -> rulemap.get(n));
			for (r in subs) {
				if (r.name == top)
					Context.error("Infinite recursion: ", r.pos);
				acc.push(r);
			}
			for (r in subs)
				adding(top, acc, r.leftsubs);
		}
		var col: Array<RuleSet> = [];
		for (r in rules) {
			var acc = [r];
			adding(r.name, acc, r.leftsubs);
			col.push(acc);
		}
		//
		var i = 0, len = col.length;
		while (i < len) {
			var a = col[i];
			if (a != null) {
				var j = i;
				while (j < len) {
					var b = col[j];
					if (b != null && a != b) {
						if (inter(a, b)) {
							union(a, b);
							col[j] = null;
						}
					}
					++ j;
				}
			}
			++ i;
		}
		while (col.remove(null)) {
		}
		for (c in col)
			c.sort( (a, b)->a.value - b.value);
		// TODO: combine cases
		return col;
	}

	static function toParterns(ruleset: RuleSet): PatternSet {
		function charAdd(r, cur, epsilon) {
			var r2 = Match(cur);
			if (epsilon) {
				r2 = Choice(r2);
			}
			return @:privateAccess LexEngine.next(r, r2);
		}
		var ret = [];
		for (r in ruleset) {
			for (c in r.cases) {
				var p = Empty;
				for (s in c.syms) {
					if(s.t) {
						p = charAdd(p, s.cset, false);
					} else {
						p = charAdd(p, s.cset, rulemap.get(s.name).epsilon);
					}
				}
				if (p == Empty)
					Context.error("TODO: toParterns", r.pos);
				ret.push(p);
			}
		}
		/*// print
		function s_charset(cset: Charset, max: Int):String {
			var a = "a".code;
			var A = "A".code;
			var s = "";
			for (c in cset) {
				for (i in c.min...c.max + 1) {
					if (i == 0) // the value of Eof is 0.
						s += "$";
					else if (i >= max)
						s += String.fromCharCode(A + i - max);
					else
						s += String.fromCharCode(a + i - 1);
				}
			}
			return s;
		}
		function s_pattern(p: Pattern, max: Int):String {
			return switch (p) {
			case Choice(p):
				s_pattern(p, max) + "?";
			case Next(r1, r2):
				s_pattern(r1, max) + s_pattern(r2, max);
			case Match(cset):
				s_charset(cset, max);
			default:
				throw "todo";
			}
		}
		var i = 0;
		for (r in ruleset) {
			var udt = udtmap.get(r.name);
			for (j in 0...r.cases.length) {
				trace(r.value + " -- " + r.name + ": " + s_charset(udt.cset, maxValue) + " => " + s_pattern(ret[i], maxValue));
				++ i;
			}
		}*/
		return ret;
	}

	static function filter(fields: Array<Field>, allFields, index) {
		var ret = [];
		for (f in fields) {
			if (f.access.indexOf(AStatic) > -1) {
				switch (f.kind) {
				case FVar(ct, e) if (e != null):
					switch(e.expr) {
					case ESwitch(macro ($i{"s"}), cl, edef):
						if (cl.length == 0) continue;
						if (edef != null)
							cl.push({values: [macro _], expr: edef, guard: null});
						startWithChecking(f.name, LOWER, f.pos);
						ret.push({name: f.name, value: index, ct: ct, cases: cl, pos: f.pos});
						++ index;
					case _:
					}
					continue;
				default:
				}
			}
			allFields.set(f.name, true);
		}
		return ret;
	}

	static function getTermlCSet(name: String) {
		if (name == "_") return termlCSet;
		var t = udtmap.get(name);
		if (t != null) {
			return t.t ? t.cset : null; // Non-Termls if .t == false
		}
		if (name.length >= 2) {  // if name == "Op" then OpPlus, OpMinus, OpXxxx ....
			var cset = [];
			for (t in termls)
				if (StringTools.startsWith(t.name, name))
					cset = CSet.union(cset, t.cset);
			// to prevent being abused
			if (cset.length > 1 || (cset.length > 0 && (cset[0].max > cset[0].min)))
				return cset;
		}
		return null;
	}

	static function getNonTermlCSet(name: String) {
		var t = udtmap.get(name);
		if (t == null || t.t)
			throw "Todo: getNonTermlCSet";
		return t.cset;
	}

	static inline var UPPER = true;
	static inline var LOWER = false;
	static function startWithChecking(s: String, upper: Bool, pos: Position) {
		var c = s.charCodeAt(0);
		if (upper) {
			if ( c >= "a".code && c <= "z".code || c == "_".code )
				Context.error("Should be start with a capital letter: " + s, pos);
		} else {
			if ( c >= "A".code && c <= "Z".code )
				Context.error("Should be start with a lowercase letter: " + s, pos);
		}
	}

	static function transformCases(sw: {name:String, value:Int, ct:ComplexType, cases:Array<Case>, pos:Position}) {
		function csetOrErr(name, pos) {
			var cset = getTermlCSet(name);
			if (cset == null)
				Context.error("Undefined: " + name, pos);
			return cset;
		}
		var rules: Rules = { name: sw.name, value: sw.value, cases: [], epsilon: false, rec: false, pure: false, subs: [], leftsubs:[], pos: sw.pos};
		for (c in sw.cases) {
			if (c.guard != null)
				Context.error("Doesn't support guard", c.guard.pos);
			switch(c.values) {
			case [{expr:EArrayDecl(el), pos: pos}]:
				if (rules.epsilon)
					Context.error('This case is unused', c.values[0].pos);
				var nonTerml = false;
				var g: SymbolSet = {expr: c.expr, syms: []};
				for (e in el) {
					switch (e.expr) {
					case EConst(CIdent(i)):
						startWithChecking(i, UPPER, e.pos);
						g.syms.push( {t: true, name: null, cset: csetOrErr(i, e.pos), ex: null, pos: e.pos} );

					case EParenthesis(macro $i{i}):
						startWithChecking(i, LOWER, e.pos);
						g.syms.push( {t: true, name: null, cset: termlCSet,           ex: i,    pos: e.pos} );

					case ECall(macro $i{i}, [macro $i{v}]):            // e.g: CInt(n)
						startWithChecking(v, LOWER, e.pos);
						g.syms.push( {t: true, name: null, cset: csetOrErr(i, e.pos), ex: v,    pos: e.pos} );

					case EBinop(OpAssign, macro $i{v}, macro $i{nt}):  // e.g: e = expr
						nonTerml = true;
						var udt = udtmap.get(nt);
						if (udt == null || udt.t == true)
							Context.error("Undefined non-terminal: " + nt, e.pos);
						if (nt == sw.name) {
							rules.rec = true;
						} else {
							rules.subs.push(nt);
							if (e == el[0])
								rules.leftsubs.push(nt);
						}
						g.syms.push( {t: false, name: nt, cset: udt.cset ,ex: v ,pos: e.pos} );

					case _:
						Context.error("Unsupported: " + e.toString(), e.pos);
					}
				} // end a line.
				if (!rules.pure && !nonTerml) rules.pure = true;
				if (g.syms.length == 0) {
					if (rules.epsilon)
						Context.error('Duplicate "default:" or "case _:"', pos);
					rules.epsilon = true;
				} else {
					rules.cases.push(g);
				}
			case [{expr:EConst(CIdent("_")), pos: pos}]: // epsilon
				if (rules.epsilon)
					Context.error('Duplicate "default:" or "case _:"', pos);
				rules.epsilon = true;
			case [e]:
				Context.error("Expected [ patterns ]", e.pos);
			case _:
				Context.error("Comma notation is not allowed while matching streams", c.values[0].pos);
			}
		}
		return rules;
	}

	static function calcTerminal(tk: Type, out) {
		var max = 0;
		switch (tk) {
		case TAbstract(_.get() => ab, _):
			for (field in ab.impl.get().statics.get()) {
				for (meta in field.meta.get()) {
					if (meta.name == ":value") {
						switch(meta.params[0].expr) {
						case ECast({expr: EConst(CInt(i))}, _):
							var n = Std.parseInt(i);
							if (n < 0 || n > 126) // TODO:
								Context.error("Value should be [0-126]", field.pos);
							if (n > max) max = n;
							startWithChecking(field.name, UPPER, field.pos);
							out.push({t: true, name: field.name, value: n, cset: CSet.single(n), pos: field.pos});
						case _:
						}
					}
				}
			}
		case _:
		}
		return max;
	}
}

#else
class LR0Builder{}

@:autoBuild(lm.LR0Builder.build())
#end
@:remove interface LR0<T> {
}
