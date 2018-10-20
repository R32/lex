package lm;

#if macro
import lm.Parser;
import lm.Charset;
import lm.LexEngine;
import haxe.macro.Type;
import haxe.macro.Expr;
import haxe.macro.Context;

private typedef OpAssocExt = {
	var lvl: Int;    // which state is op value found
	var nxt: Int;    // in lvl state, when hit (a op value) then goto nxt.
	var hit: Int;    // in nxt state, a hit value
	var fid: Int;    // in nxt state, when hit (a hit value) then goto final state.
	var prio: Int;   // op precedence. The higher the value, the higher the priority
	var value: Int;  // op value
	var left: Bool;  // is left assoc?
}

class LR0Builder extends lm.Parser {

	var s2Lhs: haxe.ds.Vector<Lhs>;  // State belone which Lhs. available after lexData()

	inline function byState(s):Lhs return s2Lhs[s];

	// called by LexEngine, NOTICE: the lex.table is invalid atm.
	@:allow(lm.LexEngine)
	function doPrecedence(lex: LexEngine) {
		if (assoc.length == 0) return;
		inline function segStart(i) return i * lex.per;
		var INVALID = lex.invalid;
		var lvlMap = new Map<Int, Array<OpAssocExt>>(); // lvl => []
		var fidMap = new Map<Int, Array<OpAssocExt>>(); // fid => []
		function parse(table: Table, begin, max) {
			for (i in begin...max) {
				var start = segStart(i);
				for (op in assoc) {
					var nxt = table.get(start + op.value);
					if (nxt >= lex.segs) continue;
					var fi = segStart(nxt);
					for (p in (fi + maxValue)...(fi + maxValue + lhsA.length)) { // only non-terminal
						var fid = table.get(p);
						if (fid == INVALID || fid < lex.segs) continue;
						// now check which stage can jump to "i"
						var hit = p - fi;
						for (j in begin...max) {
							if (table.get(segStart(j) + hit) == i) {
								var col = lvlMap.get(i);
								if (col == null) {
									col = [];
									lvlMap.set(i, col);
								}
								var ex = fidMap.get(fid);
								if (ex == null) {
									ex = [];
									fidMap.set(fid, ex);
								}
								var x = {lvl: i, hit: hit, nxt: nxt, fid: fid, prio: op.prio, value: op.value, left: op.left};
								col.push(x);
								ex.push(x);
								break;
							}
						}
					}
				}
			}
		}
		function swap(dst, src) {
			if (dst == src) return;
			var x = lex.states[dst]; // x.id == dst
			var y = lex.states[src]; // y.id == src;
			// swap targets
			for (s in lex.states) {
				for (i in 0...s.targets.length) {
					var t = s.targets[i];
					if (t == dst) {
						s.targets[i] = src;
					} else if (t == src) {
						s.targets[i] = dst;
					}
				}
			}
			// swap states
			x.id = src;
			y.id = dst;
			lex.states[dst] = y;
			lex.states[src] = x;
			// swap opAssocEx.
			var a = fidMap.get(dst);
			var b = fidMap.get(src);
			if (a != null) {
				for (op in a)
					op.fid = src;
				fidMap.set(src, a);
			} else {
				fidMap.remove(src);
			}
			for (op in b)
				op.fid = dst;
			fidMap.set(dst, b);
		}

		// need a tmp table for analysis
		var tmp = new Table( segStart(lex.segs) );
		for (i in 0...tmp.length)
			tmp.set(i, INVALID);
		for (s in lex.states) {
			if (s.id >= lex.segs) continue;
			@:privateAccess LexEngine.makeTrans(tmp, segStart(s.id), s.trans, s.targets);
		}
		// parse
		for (e in lex.entrys)
			parse(tmp, e.begin, e.begin + e.segs);

		// If the same ".fid" has an inconsistent ".prio" then raise a conflict error.
		for (a in fidMap) {
			var prio = a[0].prio;
			for (i in 1...a.length)
				if (a[i].prio != prio)
					throw "Operator Precedence Conflict";
		}
		// If all of the left assoc op has same ".prio" in same ".lvl" then remove it
		for (lvl in lvlMap.keys()) {
			var a = lvlMap.get(lvl);
			var prio = a[0].prio;
			var find = false;
			for (i in 1...a.length)
				if (prio != a[i].prio || a[i].left == false) {
					find = true;
					break;
				}
			if (!find) lvlMap.remove(lvl);
		}
		// highest(maximum) precedence in lvl
		var larMap = new haxe.ds.Vector<{left:Int, right: Int}>(lex.perRB);
		for (a in lvlMap) {
			var lar = {left: -1, right: -1};
			larMap.set(a[0].lvl, lar);
			for (op in a) {
				if (op.left) {
					if (op.prio > lar.left) lar.left = op.prio;
				} else {
					if (op.prio > lar.right) lar.right = op.prio;
				}
			}
		}
		// (for reduce the size of the table)
		var dst = lex.segs;
		var skiped = 0;
		var count = 0;
		var dupMap = new haxe.ds.Vector<Bool>(lex.perRB);
		for (a in lvlMap) {
			var lar = larMap.get(a[0].lvl);
			for (op in a) {
				if (dupMap.get(op.fid)) continue;
				count ++;
				if (op.left && lar.left > lar.right && op.prio == lar.left) {
					++ skiped;
				} else {
					swap(dst++, op.fid);
				}
				dupMap.set(op.fid, true);
			}
		}
		@:privateAccess lex.segsEx = lex.segs + count - skiped;
		// write to s.targets and this.trans
		for (fid in lex.segs...lex.segsEx) {
			var s = lex.states[fid];
			var own = fidMap.get(fid)[0];
			var tar = [];  // targets
			var cset = []; // trans
			var i = 0;
			var a = lvlMap.get(own.lvl);
			for (op in a) {
				if (own.left) {
					if (op.prio > own.prio) {
						tar[i] = op.nxt;
						cset[i] = Char.c3(op.value, op.value, i);
						++ i;
					}
				} else {
					if (op.prio >= own.prio) {
						tar[i] = op.nxt;
						cset[i] = Char.c3(op.value, op.value, i);
						++ i;
					}
				}
			}
			s.trans = cset;
			s.targets = tar;
		}
	}

	function lexData(lex: LexEngine) {
		this.s2Lhs = new haxe.ds.Vector<Lhs>(lex.nstates);
		for (i in 0...lex.entrys.length) {
			var e = lex.entrys[i];
			var l = this.lhsA[i];
			for (i in e.begin...e.begin + e.segs)
				this.s2Lhs[i] = l;
		}
		// final states
		for (i in lex.segs...lex.nstates) {
			var n = lex.table.exits(i);
			this.s2Lhs[i] = byRule(n);
		}
	}

	function checking(lex: LexEngine) {
		var table = lex.table;
		var INVALID = lex.invalid;
		// init exitN/NRule => final_state rules
		var rules = new haxe.ds.Vector<Int>(lex.nrules);
		for (n in 0...lex.nrules)
			rules[n] = INVALID;
		for (i in table.length-lex.perRB...table.length) {
			var n = table.get(i);
			if (n == INVALID) continue;
			// since: i = table.length - 1 - state; // table.exitpos
			// so   : state = table.length - 1 - i;
			rules[n] = table.length - 1 - i;
		}
		// 1. Is switch case unreachable?
		for (n in 0...lex.nrules)
			if (rules[n] == INVALID)
				Context.fatalError("Unreachable switch case", this.ruleToCase(n).pos);

		// 2. A non-terminator(lhs) must be able to derive at least one terminator directly or indirectly or be epsilon.
		for (index in 0...lex.entrys.length) {
			var entry = lex.entrys[index];
			var base = entry.begin * lex.per;
			var find = false;
			if (table.exits(entry.begin) != INVALID) // epsilon
				continue;
			for (i in base ... base + this.maxValue) {
				var c = table.get(i);
				if (c != INVALID) {
					find = true;
					break;
				}
			}
			if (!find) Context.fatalError("There must be at least one terminator.", lhsA[index].pos);
		}

		// 3. switch guard
		var n = 0;
		for (lhs in this.lhsA) {
			for (li in lhs.cases) {
				if (li.guard != null) {
					var final_state = rules[n];
					if (lex.table.get(lex.posRB() + final_state) == INVALID)
						Context.fatalError("No switch case that can be rollback from here", li.guard.pos);
				}
				++ n;
			}
		}
		// more?
	}

	function mixing(lex: LexEngine, src: Int, dst: Int, lvalue: Int) {
		var state = lex.states[src];
		var targets = state.targets;
		var dstStart = dst * lex.per;
		for (c in state.trans) {
			var i = c.min;
			var max = c.max;
			var s = targets[c.ext];
			while ( i <= max ) {
				var dst_nxt = lex.table.get(dstStart + i);
				if (i == lvalue) {
					var src_nxt = lex.table.get(src * lex.per + lvalue);
					if (dst_nxt < lex.segs && src_nxt < lex.segs) {
						mixing(lex, src_nxt, dst_nxt, -1);
					}
				} else {
					if (dst_nxt != lex.invalid) {
						if (s != dst_nxt)
							Context.fatalError("rewrite conflict: " + Lambda.find(udtMap, u -> u.value == i).name, byState(src).pos);
					} else {
						lex.table.set(dstStart + i, s);
					}
				}
				++ i;
			}
		}
	}

	function modify(lex: LexEngine) {
		var b = lex.table;
		var INVALID = lex.invalid;
		var per = lex.per;
		// sort
		var h = lhsA.copy();
		h.sort(function(L1, L2) {
			var I1 = L1.value - this.maxValue;
			var I2 = L2.value - this.maxValue;
			var S1 = lex.entrys[I1].begin;
			var S2 = lex.entrys[I2].begin;
			var E1 = b.get(S2 * per + L1.value);
			var E2 = b.get(S1 * per + L2.value);
			if (E1 != INVALID) {
				if (E2 != INVALID)
					Context.fatalError("conflict: " + I1 + " <-> " + I2, L1.pos);
				return 1;
			}
			if (E2 != INVALID)
				return -1;
			return L1.value - L2.value;
		});
		for (seg in 0...lex.segs) {
			var base = seg * per;
			for (l in h) {
				if (b.get(base + l.value) == INVALID)
					continue;
				var entry = lex.entrys[l.value - this.maxValue]; // the entry Associated with lhs
				if (entry.begin == seg) continue; // skip self.
				mixing(lex, entry.begin, seg, l.value);
				if (l.epsilon) {
					var dst = b.exitpos(seg);
					if (b.get(dst) != INVALID)
						Context.fatalError("epsilon conflict with " + l.name, l.pos);
					b.set(dst, b.exits(entry.begin));
				}
			}
		}
		rollback(lex);
	}

	function rollback(lex: LexEngine) {
		var table = lex.table;
		inline function epsilon(seg) return table.exits(seg);
		var alt = new haxe.ds.Vector<Bool>(lex.perRB);
		for (i in 0...alt.length) alt[i] = false;
		var INVALID = lex.invalid;
		var rollpos = lex.posRB();
		var rlenpos = lex.posRBL();
		var que = new List<{exit: Int, nxt: Int, len: Int}>(); // FIFO
		function loop(exit, seg, length) {
			alt[seg] = true;
			var base = seg * lex.per;
			for (lv in 0...lex.per) {
				var nxt = table.get(lv + base);
				if (nxt == INVALID || alt[nxt] || lv >= maxValue) continue;
				table.set(rollpos + nxt, exit);
				table.set(rlenpos + nxt, length);
				if (epsilon(nxt) == INVALID)
					que.add({exit: exit, nxt: nxt, len: length + 1});
			}
		}
		for (seg in 0...lex.segs) {
			var exit = epsilon(seg);
			if (exit == INVALID) continue;
			loop(exit, seg, 1);
		}
		while (true) {
			var x = que.pop();
			if (x == null) break;
			loop(x.exit, x.nxt, x.len);
		}
	}

	function generate(lex: lm.LexEngine) {
		var force_bytes = !Context.defined("js") || Context.defined("lex_rawtable");
		if (Context.defined("lex_strtable")) force_bytes = false; // force string as table format
		if (false == force_bytes && !Context.defined("utf16")) force_bytes = true;
		var getU:Expr = null;
		var raw:Expr = null;
		if (force_bytes) {
			var bytes = lex.bytesTable();
			var resname = "_" + StringTools.hex(haxe.crypto.Crc32.make(bytes)).toLowerCase();
			Context.addResource(resname, bytes);
			#if hl
			getU = lex.invalid == LexEngine.U16MAX ? macro raw.getUI16(i << 1) : macro raw.get(i);
			raw = macro haxe.Resource.getBytes($v{resname}).getData().bytes;
			#else
			getU = lex.invalid == LexEngine.U16MAX ? macro raw.getUInt16(i << 1) : macro raw.get(i);
			raw = macro haxe.Resource.getBytes($v{resname});
			#end
		} else {
			var out = haxe.macro.Compiler.getOutput() + ".lr0-table";
			var dir = haxe.io.Path.directory(out);
			if (!sys.FileSystem.exists(dir))
				sys.FileSystem.createDirectory(dir);
			var f = sys.io.File.write(out);
			lex.write(f);
			f.close();
			getU = macro StringTools.fastCodeAt(raw, i);
			raw = macro ($e{haxe.macro.Compiler.includeFile(out, Inline)});
		}
		var lva = n2Lhs.map(n -> macro $v{n}).toArray();
		var defs = macro class {
			static var raw = $raw;
			static var lva:Array<Int> = [$a{lva}];
			static inline var INVALID = $v{lex.invalid};
			static inline var NRULES  = $v{lex.nrules};
			static inline var NSEGS   = $v{lex.segs};
			static inline var NSEGSEX = $v{lex.segsEx};
			static inline function getU(i:Int):Int return $getU;
			static inline function trans(s:Int, c:Int):Int return getU($v{lex.per} * s + c);
			static inline function exits(s:Int):Int return getU($v{lex.table.length - 1} - s);
			static inline function rollB(s:Int):Int return getU(s + $v{lex.posRB()});
			static inline function rollL(s:Int):Int return getU(s + $v{lex.posRBL()});
			static inline function gotos(fid:Int, s:$ct_stream) return cases(fid, s);
			var stream: $ct_stream;
			public function new(lex: lm.Lexer<Int>) {
				this.stream = new lm.Stream<$ct_lhs>(lex, $v{lex.entrys[0].begin});
			}
			@:access(lm.Stream, lm.Tok)
			static function _entry(stream: $ct_stream, state:Int, exp:Int):$ct_lhs {
				var prev = state;
				var t: lm.Stream.Tok<$ct_lhs> = null;
				var dx = 0;
				var keep = stream.pos; // used for _side.
				while (true) {
					while (true) {
						t = stream.next();
						state = trans(prev, t.term);
						t.state = state;
						if (state >= NSEGS)
							break;
						prev = state;
					}
					if (state == INVALID) {
						state = prev;
						dx = 1;
					}
					var q = exits(state);
					if (q < NRULES) {
						stream.pos -= dx;
					} else {
						q = rollB(state);
						if (q < NRULES) {
							var dy = dx + rollL(state);
							t = stream.offset(-1 - dy);
							if ( trans(t.state, lva[q] >> 8) == INVALID ) {
								stream.pos -= dx;
								exp = 0;
								break;
							}
							stream.rollback(dy, $v{maxValue});
						} else {
							break;  // throw error.
						}
					}
					dx = 0;         // reset dx after rollback
					while (true) {
						var value:$ct_lhs = gotos(q, stream);
						t = stream.offset( -1); // last token
						if (t.term == exp) {
							-- stream.pos;      // discard the last token
							stream.junk(1);
							return value;
						}
						t.val = value;
						t.state = trans(stream.offset( -2).state, t.term);
						prev = t.state;
						if (prev < NSEGSEX) break;
						if (prev == INVALID) {
							if (exp == -1)
								return stream.cached[keep].val;
							throw lm.Utils.error('Unexpected "' + stream.str(t) + '"' + stream.errpos(t.pmin));
						}
						q = exits(prev);
					}
				}
				if (exp == -1 && (stream.pos - dx) > keep)
					return stream.cached[keep].val;
				t = stream.offset( -1);
				throw lm.Utils.error('Unexpected "' + (t.term != $i{sEof} ? stream.str(t): $v{sEof}) + '"' + stream.errpos(t.pmin));
			}
			@:access(lm.Stream, lm.Tok)
			static function _side(stream: $ct_stream, state:Int, lv: Int):$ct_lhs {
				var keep = stream.pos;
				var prev = stream.offset( -1);
				var t = new lm.Stream.Tok<$ct_lhs>(lv, prev.pmax, prev.pmax);
				t.state = state;
				stream.shift(t);
				var value = _entry(stream, state, -1); // -1 then until to match failed
				stream.pos = keep;
				stream.junk(2);
				return value;
			}
		}
		// build switch
		var exprs = Lambda.flatten( lhsA.map(l -> l.cases) ).map( s -> s.expr );
		var here = Context.currentPos();
		var defCase = exprs.pop();
		var liCase = Lambda.mapi( exprs, (i, e)->({values: [macro $v{i}], expr: e}: Case) );
		var eSwitch = {expr: ESwitch(macro (_q), liCase, defCase), pos: here};
		defs.fields.push({
			name: "cases",
			access: [AStatic],
			kind: FFun({
				args: [{name: "_q", type: macro: Int}, {name: "s", type: ct_stream}],
				ret: ct_lhs,
				expr: macro {
					@:mergeBlock $b{preDefs};
					var _v = $eSwitch;
					@:privateAccess s.reduce(lva[_q]);
					return _v;
				}
			}),
			pos: here,
		});

		// main entry => member inline
		var entry = lex.entrys[0];
		var lhs = lhsA[0];
		defs.fields.push({
			name: lhs.name,
			access: [APublic, AInline],
			kind: FFun({
				args: [],
				ret: ct_lhs,
				expr: macro return _entry(stream, $v{entry.begin}, $v{lhs.value})
			}),
			pos: lhs.pos,
		});
		// other entrys => static inline
		for (i in 1...lex.entrys.length) {
			var entry = lex.entrys[i];
			var lhs = lhsA[i];
			defs.fields.push({
				name: lhs.name,
				access: [AStatic, AInline],
				kind: FFun({
					args: [{name: "s", type: ct_stream}],
					ret: ct_lhs,
					expr: macro return _side(s, $v{entry.begin}, $v{lhs.value})
				}),
				pos: lhs.pos,
			});
		}
		return defs.fields;
	}

	static public function build() {
		// begin
		var allFields = new haxe.ds.StringMap<Field>();
		var lrb = new LR0Builder("lm.LR0", allFields);
		if (lrb.lhsA.length == 0)
			return null;
		var lex = new LexEngine(lrb.toPartern(), lrb.maxValue + lrb.lhsA.length | 15, lrb);
		lrb.lexData(lex);

		// modify lex.table
		lrb.modify(lex);

	#if lex_lr0table
		var f = sys.io.File.write("lr0-table.txt");
		f.writeString("\nProduction:\n");
		f.writeString(debug.Print.production(lrb));
		f.writeString(debug.Print.parTable(lrb, lex));
		f.writeString("\n\nRAW:\n");
		lex.write(f, true);
		f.close();
	#end

		// checking
		lrb.checking(lex);

		// generate
		var defs = lrb.generate(lex);

		// combine
		var ret = [];
		for (f in defs)
			if (!allFields.exists(f.name))
				ret.push(f);
		for (f in allFields) ret.push(f);
		return ret;
	}
}

#else
class LR0Builder{}

@:autoBuild(lm.LR0Builder.build())
#end
@:remove interface LR0<LEX, LHS> {
}
