package lm;

#if macro
import lm.Charset;
import lm.LexEngine;
import lm.ParserBase;
import lm.ParserEngine;
import haxe.macro.Expr;
import haxe.macro.Type;
import haxe.macro.Context;
 using haxe.macro.Tools;

class SLRBuilder {

	var parser : ParserBase;

	var engine : ParserEngine;

	var reserved : Array<Field>;

	public function new() {
		this.parser = new ParserBase();
		getTerms();
		getReserved();
		this.parser.transform();
		this.engine = new ParserEngine(this.parser);
	}

	function buildErrorIfElse() : Expr {
		if (engine.entrys.length == 1)
			return macro {};
		var ifelse : Expr = null;
		var exit = engine.nrules;
		var pos = Context.currentPos();
		for (i in 1...engine.entrys.length) {
			var en = engine.entrys[i];
			var cond = macro state >= $v{en.begin};
			var expr = macro q = $v{exit};
			ifelse = {expr : EIf(cond, expr, ifelse), pos : pos};
			exit++;
		}
		return ifelse;
	}

	function generate() : Array<Field> {
		if (engine.entrys.length == 0)
			return [];
		var forceBytes = !Context.defined("js") || Context.defined("lex_rawtable");
		if (Context.defined("lex_strtable") && Context.defined("utf16"))
			forceBytes = false;

		var get : Expr = null;
		var raw : Expr = null;
		if (forceBytes) {
			var bytes = engine.table.toByte(engine.isBit16());
			var resname = "_" + StringTools.hex(haxe.crypto.Crc32.make(bytes)).toLowerCase();
			Context.addResource(resname, bytes);
		#if hl
			get = engine.isBit16() ? macro raw.getUI16(i << 1) : macro raw.get(i);
			raw = macro haxe.Resource.getBytes($v{resname}).getData().bytes;
		#else
			get = engine.isBit16() ? macro raw.getUInt16(i << 1) : macro raw.get(i);
			raw = macro haxe.Resource.getBytes($v{resname});
		#end
		} else {
			raw = macro $v{ engine.table.map(i -> String.fromCharCode(i)).join("") };
			get = macro StringTools.fastCodeAt(raw, i);
		}
		var errorIfElse = buildErrorIfElse();
		var recudeData = this.parser.reduce_data.map(n -> macro $v{n});
		var fields = (macro class {
			var s(get, never) : lm.Stream;
			@:dce inline function get_s() return this.stream;
			static var raw = $raw;
			static var rdatas : Array<Int> = [$a{recudeData}];
			static inline var INVALID = $v{engine.invalid};
			static inline var NRULES  = $v{engine.nrules};
			static inline var NSEGS   = $v{engine.segs};
			static inline function get(raw, i) return $get;
			static inline function trans(r, s, t) return get(r, $v{engine.per} * s + t.term);
			static inline function exits(r, s) return get(r, $v{engine.table.length - 1} - s);
			inline function gotos( fid : Int ) : Dynamic return cases(fid);
			final stream : lm.Stream;
			public function new( lex : lm.Lexer<Int> ) {
				this.stream = @:privateAccess new lm.Stream(lex);
			}
			@:access(lm.Stream, lm.Tok)
			function slrloop( state : Int, exp : Int) : Dynamic {
				var t : lm.Stream.Tok;
				if (state >= 0) {
					t = stream.newTok($i{this.parser.eof}, 0, 0);
					t.state = state;
					stream.unshift(t);       // should be removed when returning
				} else {
					t = stream.offset( -1);  // try to restore from error
					state = t.state;
				}
				var raw = raw;
				var q = INVALID;
				while (true) {
					while (true) {
						t = stream.next();   // next token
						state = trans(raw, state, t);
						if (state >= NSEGS)
							break;
						t.state = state;     // update state to stream-token
					}
					if (state == INVALID) {
						stream.pos -= 1;     // reverts if INVALID
						state = stream.offset( -1).state;
					}
					while (true) {
						q = exits(raw, state);
						if (q >= NRULES) {
							$e{errorIfElse};
							return gotos(q);
						}
						var value : Dynamic = gotos(q);
						t = stream.reduce( rdatas[q] );
						if (t.term == exp) {
							stream.pos -= 2; // ready to discard
							stream.junk(2);  // commit
							return value;
						}
						t.val = value;
						state = trans(raw, stream.offset( -2).state, t);
						t.state = state;
						if (state < NSEGS)
							break;
					}
				}
				return gotos(q);
			}
		}).fields;
		// switch default
		var lhsides = this.parser.lhsides;
		var fstindex = engine.entrys[0].index;
		var unmatch : Expr = lhsides[fstindex].edef;
		if (unmatch == null) {
			unmatch = macro {
				var t = stream.peek(0);
				throw stream.error('Unexpected "' + (t.term != $i{this.parser.eof} ? stream.str(t): $v{this.parser.eof}) + '"', t);
			}
		}
		// switch cases
		var epsilons = ExprHelps.lexUnMatchedActions(engine, this.parser.rule_groups, fstindex + 1);
		var ecases : Array<Case> = [];
		ecases.resize(engine.nrules + epsilons.length);
		var index = 0;
		for (lhs in lhsides) {
			for (stsets in lhs.cases) {
				ecases[index] = {
					values : [{expr : EConst(CInt("" + index)), pos : stsets.action.pos}],
					expr : stsets.action
				};
				index++;
			}
		}
		for (c in epsilons)
			ecases[index++] = c;

		var here = Context.currentPos();
		var eswitch = {expr: ESwitch(macro (q), ecases, unmatch), pos: here}
		fields.push({
			name: "cases",
			access: [],
			kind: FFun({
				args: [{name: "q", type: macro : Int}],
				ret: macro :Dynamic,
				expr: macro { return $eswitch; },
			}),
			pos: here,
		});
		// entrys
		for (en in engine.entrys) {
			var lhs = lhsides[en.index];
			fields.push({
				name: lhs.name,
				access: [APublic, AInline],
				kind: FFun({
					args: [],
					ret: lhs.ctype,
					expr: macro return slrloop($v{en.begin}, $v{lhs.value})
				}),
				pos: lhs.pos,
			});
			fields.push({
				name: lhs.name.toUpperCase() + "_EXP",
				access: [AInline, AStatic],
				kind: FVar(null, macro $v{lhs.value}),
				pos: lhs.pos,
			});
		}
		return fields;
	}

	function getTermsType( t : Type ) {
		switch(t) {
		case TInst(_.get() => lex, _):
			for (it in lex.interfaces) {
				if (it.t.toString() != "lm.Lexer")
					continue;
				var seof = "null";
				var eof = @:privateAccess LexBuilder.getMeta(lex.meta.extract(":rule")).eof;
				if (eof != null)
					seof = eof.toString();
				if (seof == "null")
					throw new Error("Invalid EOF value : " + seof, lex.pos);
				this.parser.eof = seof;
				this.parser.reflect = LexBuilder.lmap.get(ExprHelps.classFullName(lex));
				return it.params[0];
			}
		default:
		}
		return null;
	}

	function getTerms() {
		var cl = Context.getLocalClass().get();
		var t : Type;
		for (it in cl.interfaces) {
			var ts = it.t.toString();
			if (!(ts == "lm.SLR" || ts == "lm.LR0"))
				continue;
			t = getTermsType(it.params[0]); // + eof, reflect
			//t_lhs = it.params[1];
			break;
		}
		if (t == null || !Context.unify(t, Context.getType("Int")))
			throw new Error("Wrong generic Type for " + lm.SLR + "<???>", cl.pos);
		var fields = [];
		switch (t.follow()) {
		case TAbstract(_.get() => ab, _):
			fields = ab.impl.get().statics.get();
		default:
		}
		for (field in fields) {
			for (meta in field.meta.get()) {
				if (meta.name != ":value")
					continue;
				var e = meta.params[0];
				switch(e.expr) {
				case EConst(CInt(i)), ECast({expr : EConst(CInt(i))}, _):
					var n = Std.parseInt(i);
					if (n == null)
						throw new Error("Expect int type :" + i ,e.pos);
					this.parser.addTerm(field.name, n, e.pos);
				case _:
				}
			}
		}
		this.parser.terms_ct = t.toComplexType();
		this.getPrecedence(cl);
	}

	function getPrecedence( cl : ClassType ) {
		var rules = cl.meta.extract(":rule");
		function readrec( out : Array<Expr>, e : Expr ) {
			switch(e.expr) {
			case EConst(CIdent(_)), EConst(CString(_)), EMeta(_, _):
				out.push(e);
			case EArrayDecl(a):
				for (e in a)
					readrec(out, e);
			default:
				throw new Error("UnSupperted : " + e.toString(), e.pos);
			}
		}
		var estarts = [];
		var precs = [];
		var obj = rules.length == 1 ? rules[0].params[0] : {expr : EConst(CIdent("null")), pos : cl.pos};
		switch(obj.expr) {
		case EObjectDecl(a):
			for (item in a) {
				var name = item.field;
				var type = PrecType.Left;
				switch (name) {
				case "left":
				case "right":
					type = PrecType.Right;
				case "nonassoc":
					type = PrecType.Nonassoc;
				case "start":
					readrec(estarts, item.expr);
					continue;
				default:
					throw new Error("UnSupported : " + name, ExprHelps.objFieldPos(item));
				}
				var prec = {type : type, sets : []} ;
				readrec(prec.sets, item.expr);
				precs.push(prec);
			}
		default:
		}
		this.parser.starts = estarts.map(e -> e.toString());
		this.parser.initPrecMap(precs);
	}

	function getReserved() {
		var funcs : Array<{f : Field, fun : Function, pos : Position}> = [];
		// add non-term
		var acc = this.parser.max_term_value;
		this.reserved = [];
		for (f in Context.getBuildFields()) {
			switch(f.kind) {
			case FVar(ct, e) if (e != null):
				switch(e.expr) {
				case ESwitch(_, cases, edef):
					if (cases.length > 0 || edef != null)
						this.parser.addNonTerm(f.name, acc++, ct, cases, edef, e.pos);
					continue;
				case _:
				}
			case FFun(fun):
				funcs.push({f : f, fun : fun, pos : f.pos});
			default:
			}
			reserved.push(f);
		}
		// funmap(name => function)
		// e.g: @:rule(CInt) function int_of_string( str : String ) return Std.parseInt(str);
		var ct_source = macro :lms.ByteData;
		var ct_stream = macro :lm.Stream;
		var ct_lexer_position = macro :lm.Lexer.Position;
		for (item in funcs) {
			var field = item.f;
			var fun = item.fun;
			var extract = Lambda.find(field.meta, m->m.name == ":rule");
			if (extract == null || extract.params.length == 0)
				continue;
			for (t in extract.params) {
				switch (t.expr) {
				case EConst(CIdent(s)) | EConst(CString(s)):
					this.parser.funmap.set(s, {name : field.name, ct : fun.ret, args : fun.args.length, pos : item.pos});
				default:
					throw new Error("UnSupperted value for @:rule: " + t.toString(), t.pos);
				}
			}
			// auto add type to argument if null
			switch (fun.args.length) {
			case 1:
				if (fun.args[0].type == null)
					fun.args[0].type = macro :String;
			case 2:
				if (fun.args[0].type == null)
					fun.args[0].type = ct_source;
				if (fun.args[1].type == null)
					fun.args[1].type = ct_lexer_position;
			}
		}
	}

	public static function build() {
		var ret = [];
		try {
			var slr = new SLRBuilder();
			var fields = slr.generate();
			for (f in fields) {
				if (Lambda.exists(slr.reserved, rf -> rf.name == f.name))
					continue;
				ret.push(f);
			}
			for (f in slr.reserved)
				ret.push(f);
		} catch ( e : Error ) {
			Context.fatalError("[SLR build] " + e.message , e.pos);
		}
		return ret;
	}
}
#else
extern class SLRBuilder {}
@:autoBuild(lm.SLRBuilder.build())
#end
@:remove interface SLR<LEX> {
}
