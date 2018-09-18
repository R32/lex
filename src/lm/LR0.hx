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
	name: String,  // ident name
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
	pure: Bool,    // if all the productions(rhs) are terminators.
	subs: Array<String>,         // used to verify if there is infinite recursion
	leftsubs: Array<String>,     // for call group(), it's useless that should be removed..
	pos: Position,
}

typedef LhsArray = Array<Lhs>;   // all switches.

class LR0Builder {

	var termls: Array<Udt>;      // all Terminal
	var termlsC_All: Charset;    // Terminal Universal Set.
	var udtMap: Map<String,Udt>; // User Defined Terminal + Non-Terminal
	var maxValue: Int;           // if value >= maxValue then it must be a non-terminal
	var lhsA: LhsArray;
	var lhsMap: Map<String,Lhs>;
	var reduces: Array<Char>;    // min=junk(N), max=lhs.value;
	var sEof: String;            // by @:rule from Lexer
	public function new(tk, es) {
		maxValue = 0;
		termls = [];
		termlsC_All = [];
		lhsA = [];
		reduces = [];
		udtMap = new Map();
		lhsMap = new Map();
		sEof = es;
		parseToken(tk);
	}

	function parseToken(tk: Type) {
		switch (tk) {
		case TAbstract(_.get() => ab, _):
			for (field in ab.impl.get().statics.get()) {
				for (meta in field.meta.get()) {
					if (meta.name != ":value") continue;
					switch(meta.params[0].expr) {
					case ECast({expr: EConst(CInt(i))}, _):
						var n = Std.parseInt(i);
						if (n < 0 || n > 126) // TODO:
							Context.error("Value should be [0-126]", field.pos);
						if (n > maxValue) maxValue = n;
						firstCharChecking(field.name, UPPER, field.pos);
						var t = {t: true, name: field.name, value: n, cset: CSet.single(n), pos: field.pos};
						termls.push(t);
						udtMap.set(t.name, t);
						if (t.name != this.sEof)
							termlsC_All = CSet.union(termlsC_All, t.cset);
					case _:
					}
				}
			}
		case _:
		}
		if (udtMap.get(this.sEof) == null) throw "Invalid EOF value: " + this.sEof; //
		++ maxValue;
	}

	function getTermlCSet(name: String) {
		if (name == "_") return termlsC_All;
		var t = udtMap.get(name);
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
		var t = udtMap.get(name);
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
			this.udtMap.set(sw.name, {t: false, name: sw.name, value: sw.value, cset: CSet.single(sw.value), pos: sw.pos});
		}
		for (sw in swa) {
			var lhs: Lhs = { name: sw.name, value: sw.value, cases: [], epsilon: false, pure: false, subs: [], leftsubs:[], pos: sw.pos};
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
							g.syms.push( {t: true, name: i, cset: csetOrError(i, e.pos), ex: null, pos: e.pos} );

						case EParenthesis(macro $i{i}):
							firstCharChecking(i, LOWER, e.pos);            // for all termls
							g.syms.push( {t: true, name: null, cset: termlsC_All,           ex: i,    pos: e.pos} );

						case ECall(macro $i{i}, [macro $i{v}]):            // e.g: CInt(n)
							firstCharChecking(i, UPPER, e.pos);
							firstCharChecking(v, LOWER, e.pos);
							g.syms.push( {t: true, name: i, cset: csetOrError(i, e.pos), ex: v,    pos: e.pos} );

						case EBinop(OpAssign, macro $i{v}, macro $i{nt}):  // e.g: e = expr
							nonTerml = true;
							var udt = udtMap.get(nt);
							if (udt == null || udt.t == true)
								Context.error("Undefined non-terminal: " + nt, e.pos);
							if (nt == sw.name) {
								if (el.length == 1)
									Context.error("Infinite recursion", e.pos);
							} else {
								lhs.subs.push(nt);
								if (e == el[0])
									lhs.leftsubs.push(nt);
							}
							g.syms.push( {t: false, name: nt, cset: udt.cset , ex: v , pos: e.pos} );

						case _:
							Context.error("Unsupported: " + e.toString(), e.pos);
						}
					}
					if (!nonTerml) lhs.pure = true;
					if (g.syms.length == 0) setEpsilon(lhs, pos);
					lhs.cases.push(g);
					this.reduces.push(new Char(g.syms.length, lhs.value));
				case [{expr:EConst(CIdent("_")), pos: pos}]: // case _: || defualt:
					setEpsilon(lhs, pos);
					lhs.cases.push({expr: c.expr, syms: []});
					this.reduces.push(new Char(0, lhs.value));
				case [e]:
					Context.error("Expected [ patterns ]", e.pos);
				case _:
					Context.error("Comma notation is not allowed while matching streams", c.values[0].pos);
				}
			}
			this.lhsA.push(lhs);
		}
	}

	function checking() {
		// add to lhsMap.
		for (lhs in lhsA) {
			if (lhsMap.exists(lhs.name))
				Context.error("Duplicate rule field declaration: " + lhs.name, lhs.pos);
			lhsMap.set(lhs.name, lhs);
		}

		// the "entry" must place  "EOF " at the end
		var entry = lhsA[0];
		for (li in entry.cases) {
			var last = li.syms[li.syms.length - 1];
			if (last.name == null || last.name != this.sEof)
				Context.error("Must place *"+ this.sEof +"* at the end", last.pos);
		}

		// duplicate var checking.
		for (lhs in lhsA) {
			for (li in lhs.cases) {
				var row = new Map<String,Bool>();
				for (s in li.syms) {
					if (s.t == false && s.name == entry.name)
						Context.error("the entry non-terminal(\"" + s.name +"\") is not allowed on the right", s.pos);
					if (s.ex == null)
						continue;
					if (row.exists(s.ex))
						Context.error("duplicate var: " + s.ex, s.pos);
					row.set(s.ex, true);
				}
			}
		}
		// dead rec checking
		for (lhs in lhsA) {
			if (lhs.pure) continue;
			var done = false;
			for (name in lhs.subs) {
				var sub = lhsMap.get(name);
				if (sub.pure) {
					done = true;
					break;
				}
			}
			if (!done) Context.error("Need Terminal case: " + lhs.name, lhs.pos);
		}
	}

	function toPartern(): Array<PatternSet> {
		inline function csetAdd(r, cur) return @:privateAccess LexEngine.next(r, Match(cur));
		var ret = [];
		for (lhs in this.lhsA) {
			var a = [];
			for (c in lhs.cases) {
				var p = Empty;
				for (s in c.syms)
					p = csetAdd(p, s.cset);
				a.push(p);
			}
			ret.push(a);
		}
		return ret;
	}

	function rawReWrite(lex: LexEngine, src: Int, dst: Int, lvalue: Int, lpos) {
		// inline function getState(id) { return lex.clos.states[ id < lex.segs ? id : (id + lex.nstates - 1 - lex.clos.states[lex.nstates - 1].id) ];
		var state = lex.clos.states[src];
		var targets = state.targets;
		var dstStart = dst * lex.per;
		var invalid = lex.per - 1;
		for (c in lex.clos.trans[state.part]) {
			var i = c.min;
			var max = c.max;
			var s = targets[c.ext];
			while ( i <= max ) {
				var follow = lex.table.get(dstStart + i);
				if (i == lvalue) {
					var next = lex.table.get(src * lex.per + lvalue);
					if (follow < lex.segs && next < lex.segs) {
						rawReWrite(lex, next, follow, -1, lpos); //
					}
				} else {
					if (follow != invalid)
						Context.error("rewrite conflict", lpos);
					lex.table.set(dstStart + i, s);
				}
				++ i;
			}
		}
	}

	function modify(lex: LexEngine) {
		var b = lex.table;
		var per = lex.per;
		var invalid = per - 1;
		for (i in 0...lex.segs) {
			var base = i * per;
			for (l in lhsA) {
				if (b.get(base + l.value) == invalid)
					continue;
				var assoc = lex.metas[l.value - this.maxValue];
				if (assoc.begin == i) continue; // skip self.
				rawReWrite(lex, assoc.begin, i, l.value, l.pos);
				if (l.epsilon) {
					var dst = b.length - 1 - i;
					if (b.get(dst) != invalid)
						Context.error("epsilon conflict with " + l.name, l.pos);
					var src = b.length - 1 - assoc.begin;
					b.set(dst, b.get(src));
				}
			}
		}
	}

	static public function build() {
		var cls = Context.getLocalClass().get();
		var ct_lr0 = TPath({pack: cls.pack, name: cls.name});
		var tk:Type = null;
		var eof: Expr = null;
		for (it in cls.interfaces) {
			if (it.t.toString() == "lm.LR0") {
				switch(it.params[0]) {
				case TInst(_.get() => lex, _):
					for (it in lex.interfaces) {
						if (it.t.toString() == "lm.Lexer") {
							tk = it.params[0];
							eof = @:privateAccess LexBuilder.getMeta(lex.meta.extract(":rule")).eof;
							if (eof == null || eof.toString() == "null") // "null" is not allowed as an EOF in parser
								Context.error("Invalid EOF value " + eof.toString(), lex.pos);
							break;
						}
					}
				default:
				}
				break;
			}
		}
		if (tk == null || !Context.unify(tk, Context.getType("Int")))
			Context.error("Wrong generic Type for lm.LR0<?>", cls.pos);
		var lrb = new LR0Builder(tk, eof.toString());
		var allFields = new haxe.ds.StringMap<Bool>();
		var switches = filter(Context.getBuildFields(), allFields, lrb.maxValue);
		if (switches.length == 0)
			return null;
		lrb.transform(switches);
		lrb.checking();

		var pats = lrb.toPartern();
		var lex = new LexEngine(pats);
		// recalculation to reduce the table size.
		var lex = new LexEngine(pats, (1 + lex.nstates) | 15, true);

	#if lex_lr0table
		var f = sys.io.File.write("lr0-table.txt");
		f.writeString("Auto generated by \"-D lex_lr0table\"\n-----------------\n\nProduction:\n");
		f.writeString(debug.Print.lr0Production(lrb));
		f.writeString("\n\nstep 1: Init by lexEngine:\n");
		f.writeString(debug.Print.lr0Table(lrb, lex));
	#end
		// modify lex.table as LR0
		lrb.modify(lex);
	#if lex_lr0table
		f.writeString("\nstep 2: Modified to LR0:\n");
		f.writeString(debug.Print.lr0Table(lrb, lex));
		f.writeString("\n\nFinal RAW:\n");
		lex.write(f, true);
		f.close();
	#end
		var ret = [];
		//generate(ct_lr0, lrb, lex);
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
		//var allCases: Array<Expr> = Lambda.flatten( lrb.lhsA.map(l -> l.cases) ).map( s -> s.expr );
		var reduces: Array<ExprOf<Int>> = lrb.reduces.map(i -> macro $v{i});
		var defs = macro class {
			static var raw = $raw;
			static inline var NRULES = $v{lex.nrules};
			static inline var NSEGS = $v{lex.segs};
			static inline function trans(s: Int, c: Int):Int return $get_trans;
			static inline function exits(s: Int):Int return $get_exits;
			static inline function gotos(s: Int, lr0: $ct) return $gotos;
			var stream: lm.Stream; var stack: Array<Int>; // state stack.
			var stack: Array<Int>; // state stack.
			var reduces: Array<Int> = [$a{reduces}];
			public function new(lex: lm.Lexer<Int>) {
				this.stream = new lm.Stream(lex);
			}
			function entry(state) {
				var i = 0;
				var t: lm.Stream.STok = null;
				stack[i] = state;
				while (true) {
					while (stack[i] < NSEGS) {
						t = stream.peek(i++);     // start at 0.
						stack[i] = trans(stack[i - 1], t.char);
					}
					while (i > 0) {
						state = exits(stack[i]);
						if (state < NRULES) {
							var v = gotos(state, this);
							if (state == 0) return v; // ACC, maybe should clean something???
							// reduce(n) & push(s) =>

							break;
						}
						-- i;
					}
					if (t.char == 0) break; // chagne to Eof
				}
				return null;
			}
		}
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
							cl.push({values: [macro @:pos(edef.pos) _], expr: edef, guard: null});
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

	static function group(lhsA: LhsArray, lhsMap: Map<String, Lhs>) {
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
			var subs = names.map(n -> lhsMap.get(n));
			for (r in subs) {
				if (r.name == top)
					Context.error("Infinite recursion: ", r.pos);
				acc.push(r);
			}
			for (r in subs)
				adding(top, acc, r.leftsubs);
		}
		var col: Array<LhsArray> = [];
		for (r in lhsA) {
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
