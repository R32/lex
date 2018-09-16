package lm;

#if macro
import haxe.macro.Context;
import haxe.macro.Expr;
//import haxe.macro.Expr.Position;
import haxe.macro.Type;
import lm.Charset;
import lm.LexEngine;

using haxe.macro.Tools;

typedef Udt = {    // user defined token
	t: Bool,       // terminal or not
	name: String,  // ident
	value: Int,    // must be `enum abstract (Int)`
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

typedef SymSwitch = {
	name: String,        // Associated var name
	value: Int,          // Automatic growth
	ct: ComplexType,     // Type of siwtch
	cases: Array<Case>,
	pos: Position,
}

typedef SymbolSet = {
	expr: Expr,
	syms: Array<Symbol>,
}

typedef Lhs = {    // one switch == one Lhs
	name: String,  // associated var name(non-terminals name)
	value: Int,
	epsilon: Bool, //
	cases: Array<SymbolSet>,
	rec: Bool,     // is self nesting?
	pure: Bool,    // if all the productions(rhs) are terminators.
	subs: Array<String>,         // used to verify if there is infinite recursion
	leftsubs: Array<String>,     // for call group()
	pos: Position,
}

typedef LhsArray = Array<Lhs>;   // all switches.

class LR0Builder {

	var termls: Array<Udt>;      // all Terminal
	var termlsC_All: Charset;    // Terminal Universal Set.
	var umap: Map<String,Udt>;   // User Defined Terminal + Non-Terminal
	var maxValue: Int;           // if value >= maxValue then it must be a non-terminal
	var lhsa: LhsArray;
	var lmap: Map<String,Lhs>;

	public function new(tk) {
		maxValue = 0;
		termls = [];
		termlsC_All = [];
		lhsa = [];
		umap = new Map();
		lmap = new Map();
		parseToken(tk);
	}

	function parseToken(tk: Type) {
		switch (tk) {
		case TAbstract(_.get() => ab, _):
			for (field in ab.impl.get().statics.get()) {
				for (meta in field.meta.get()) {
					if (meta.name != ":value")
						continue;
					switch(meta.params[0].expr) {
					case ECast({expr: EConst(CInt(i))}, _):
						var n = Std.parseInt(i);
						if (n < 0 || n > 126) // TODO:
							Context.error("Value should be [0-126]", field.pos);
						if (n > maxValue) maxValue = n;
						firstCharChecking(field.name, UPPER, field.pos);
						var t = {t: true, name: field.name, value: n, cset: CSet.single(n), pos: field.pos};
						termls.push(t);
						umap.set(t.name, t);
						termlsC_All = CSet.union(termlsC_All, t.cset);
					case _:
					}
				}
			}
		case _:
		}
		++ maxValue;
	}

	function getTermlCSet(name: String) {
		if (name == "_") return termlsC_All;
		var t = umap.get(name);
		if (t != null) {
			return t.t ? t.cset : null; // Non-Termls if .t == false
		}
		if (name.length >= 2) {         // if name == "Op" then OpPlus, OpMinus, OpXxxx ....
			var cset = [];
			for (t in termls)
				if (StringTools.startsWith(t.name, name))
					cset = CSet.union(cset, t.cset);
			// to prevent abuse
			if (cset.length > 1 || (cset.length > 0 && (cset[0].max > cset[0].min)))
				return cset;
		}
		return null;
	}

	function getNonTermlCSet(name: String) {
		var t = umap.get(name);
		return t == null || t.t ? null : t.cset;
	}

	function transform(swa: Array<SymSwitch>) {
		inline function setEpsilon(lhs, pos) {
			if (lhs.epsilon) Context.error('Duplicate "default:" or "case _:"', pos);
			lhs.epsilon = true;
		}
		function csetOrError(name, pos) {
			var cset = getTermlCSet(name);
			if (cset == null) Context.error("Undefined: " + name, pos);
			return cset;
		}
		for (sw in swa) { // init first..
			this.umap.set(sw.name, {t: false, name: sw.name, value: sw.value, cset: CSet.single(sw.value), pos: sw.pos});
		}
		for (sw in swa) {
			var lhs: Lhs = { name: sw.name, value: sw.value, cases: [], epsilon: false, rec: false, pure: false, subs: [], leftsubs:[], pos: sw.pos};
			for (c in sw.cases) {
				if (c.guard != null)
					Context.error("Doesn't support guard", c.guard.pos);
				switch(c.values) {
				case [{expr:EArrayDecl(el), pos: pos}]:
					if (lhs.epsilon)
						Context.error('This case is unused', c.values[0].pos);
					var nonTerml = false;
					var g: SymbolSet = {expr: c.expr, syms: []};
					for (e in el) {
						switch (e.expr) {
						case EConst(CIdent(i)):
							firstCharChecking(i, UPPER, e.pos);            // e.g: CInt
							g.syms.push( {t: true, name: null, cset: csetOrError(i, e.pos), ex: null, pos: e.pos} );

						case EParenthesis(macro $i{i}):
							firstCharChecking(i, LOWER, e.pos);            // for all termls
							g.syms.push( {t: true, name: null, cset: termlsC_All,           ex: i,    pos: e.pos} );

						case ECall(macro $i{i}, [macro $i{v}]):            // e.g: CInt(n)
							firstCharChecking(v, LOWER, e.pos);
							g.syms.push( {t: true, name: null, cset: csetOrError(i, e.pos), ex: v,    pos: e.pos} );

						case EBinop(OpAssign, macro $i{v}, macro $i{nt}):  // e.g: e = expr
							nonTerml = true;
							var udt = umap.get(nt);
							if (udt == null || udt.t == true)
								Context.error("Undefined non-terminal: " + nt, e.pos);
							if (nt == sw.name) {
								if (el.length == 1) Context.error("Infinite recursion", e.pos);
								lhs.rec = true;
							} else {
								lhs.subs.push(nt);
								if (e == el[0])
									lhs.leftsubs.push(nt);
							}
							g.syms.push( {t: false, name: nt, cset: udt.cset ,ex: v ,pos: e.pos} );

						case _:
							Context.error("Unsupported: " + e.toString(), e.pos);
						}
					}
					if (!lhs.pure && !nonTerml)
						lhs.pure = true;
					if (g.syms.length == 0)
						setEpsilon(lhs, pos);
					lhs.cases.push(g);
				case [{expr:EConst(CIdent("_")), pos: pos}]: // case _: || defualt:
					setEpsilon(lhs, pos);
					lhs.cases.push({expr: c.expr, syms: []});
				case [e]:
					Context.error("Expected [ patterns ]", e.pos);
				case _:
					Context.error("Comma notation is not allowed while matching streams", c.values[0].pos);
				}
			}
			this.lhsa.push(lhs);
		}
	}

	function checking() {
		// duplicate checking & add to lmap.
		for (lhs in lhsa) {
			if (lmap.exists(lhs.name))
				Context.error("Duplicate rule field declaration: " + lhs.name, lhs.pos);
			lmap.set(lhs.name, lhs);
		}

		// dead rec checking
		for (lhs in lhsa) {
			if (lhs.pure) continue;
			var done = false;
			for (name in lhs.subs) {
				var sub = lmap.get(name);
				if (sub.pure) {
					done = true;
					break;
				}
			}
			if (!done) Context.error("Need Terminal case: " + lhs.name, lhs.pos);
		}
	}

	function modify(lex: LexEngine) {
		var b = lex.table;
		var per = lex.per;
		var invalid = per - 1;
		for (i in 0...lex.segs) {
			var base = i * per;
			for (l in lhsa) {
				if (b.get(base + l.value) == invalid)
					continue;
				var clo = lex.clos[l.value - maxValue];
				//
				@:privateAccess lm.LexEngine.makeTrans(b, base, clo.trans, clo.state.targets); // TODO: Conflict detection?
				if (l.epsilon) {
					for (p in base...base + per)
						if (b.get(p) == invalid)
							b.set(p, clo.fid);
				}
			}
		}
	}

	static public function build() {
		var cls = Context.getLocalClass().get();
		var ct_lr0 = TPath({pack: cls.pack, name: cls.name});
		var tk:Type = null;
		for (it in cls.interfaces) {
			if (it.t.toString() == "lm.LR0") {
				tk = it.params[0];
				break;
			}
		}
		if (tk == null && Context.unify(tk, Context.getType("Int")) == false)
			Context.error("Only \"enum abstract (Int)\" type is supported", cls.pos);
		var lrb = new LR0Builder(tk);
		var allFields = new haxe.ds.StringMap<Bool>();
		var switches = filter(Context.getBuildFields(), allFields, lrb.maxValue);
		lrb.transform(switches);
		lrb.checking();

		var pats = lrb.lhsa.map(toPartern);
		var lex = new LexEngine(pats);
		// recalculation to reduce the table size.
		var lex = new LexEngine(pats, (lex.nstates + 1) | 15);

		// modify lex.table as LR0
		lrb.modify(lex);

		#if lex_dump
		var f = sys.io.File.write("lr0.txt");
		lex.write(f, true);
		f.close();
		#end

		var ret = [];
		generate(ct_lr0, lrb, lex);
		return ret;
	}

	static function generate(ct: ComplexType, lrb: LR0Builder, lex: lm.LexEngine) {
		var force_bytes = !Context.defined("js") || Context.defined("force_bytes");
		var resname = "_" + StringTools.hex(haxe.crypto.Crc32.make(lex.table)).toLowerCase();
		var raw = macro haxe.Resource.getBytes($v{resname});
		if (force_bytes) {
			Context.addResource(resname, lex.table);
		} else {
			var out = haxe.macro.Compiler.getOutput() + ".lr0-table";
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
		var useSwitch = false;
		var gotos = useSwitch ? (macro cases(s, lr0)) : (macro cases[s](lr0));

		var defs = macro class {
			static var raw = $raw;
			static inline var NRULES = $v{lex.nrules};
			static inline var NSEGS = $v{lex.segs};
			static inline function trans(s: Int, c: Int):Int return $get_trans;
			static inline function exits(s: Int):Int return $get_exits;
			static inline function gotos(s: Int, lr0: $ct) return $gotos;
			var stream: lm.Stream; var stack: Array<Int>; // state stack.
			var stack: Array<Int>; // state stack.
			public function new(lex: lm.Lexer<Int>) {
				this.stream = new lm.Stream(lex);
			}
			function entry(state) {
				var i = 0;
				stack[i] = state;
				while (stack[i] < NSEGS) {
					var t = stream.peek(i++); // start at 0.
					stack[i] = trans(stack[i - 1], t.char);
				}
				while (i > 0) {
					state = exits(stack[i]);
					if (state < NRULES) {
						var v = gotos(state, this);
						if (state == 0) return v; // ACC

					}
					-- i;
				}
				return null;
			}
		}
	}

	static function toPartern(lhs: Lhs): PatternSet {
		inline function csetAdd(r, cur) return @:privateAccess LexEngine.next(r, Match(cur));
		var ret = [];
		for (c in lhs.cases) {
			var p = Empty;
			for (s in c.syms) {
				p = csetAdd(p, s.cset);
			}
			ret.push(p);
		}
		return ret;
	}

	static function filter(fields: Array<Field>, allFields, value) {
		var ret = [];
		for (f in fields) {
			if (f.access.indexOf(AStatic) > -1) {
				switch (f.kind) {
				case FVar(ct, e) if (e != null):
					switch(e.expr) {
					case ESwitch(macro ($i{"s"}), cl, edef):
						if (cl.length == 0) continue; // TODO: if (cl.length == 0 && edef == null)
						if (edef != null)
							cl.push({values: [macro _], expr: edef, guard: null});
						firstCharChecking(f.name, LOWER, f.pos);
						ret.push({name: f.name, value: value++, ct: ct, cases: cl, pos: f.pos});
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

	static inline var UPPER = true;
	static inline var LOWER = false;
	static function firstCharChecking(s: String, upper: Bool, pos: Position) {
		var c = s.charCodeAt(0);
		if (upper) {
			if ( !(c >= "A".code && c <= "Z".code) )
				Context.error("Should be start with a capital letter: " + s, pos);
		} else {
			if ( !(c >= "a".code && c <= "z".code || c == "_".code) )
				Context.error("Should be start with a lowercase letter: " + s, pos);
		}
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

	static function group(lhsa: LhsArray, lmap: Map<String, Lhs>) {
		function inter(a:LhsArray, b:LhsArray) {
			for (r in b)
				if (a.indexOf(r) > 0)
					return true;
			return false;
		}
		function union(acc:LhsArray, b:LhsArray) {
			for (r in b)
				if (acc.indexOf(r) == -1)
					acc.push(r);
		}
		function adding(top: String, acc: LhsArray, names: Array<String>) {
			var subs = names.map(n -> lmap.get(n));
			for (r in subs) {
				if (r.name == top)
					Context.error("Infinite recursion: ", r.pos);
				acc.push(r);
			}
			for (r in subs)
				adding(top, acc, r.leftsubs);
		}
		var col: Array<LhsArray> = [];
		for (r in lhsa) {
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
		return col;
	}
	*/
}

#else
class LR0Builder{}

@:autoBuild(lm.LR0Builder.build())
#end
@:remove interface LR0<T> {
}
