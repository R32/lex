package lm;

#if macro
import lm.Parser;
import lm.Charset;
import lm.LexEngine;
import haxe.macro.Type;
import haxe.macro.Expr;
import haxe.macro.Context;

@:access(lm.LexEngine)
class LR0Builder extends lm.Parser {

	public static inline var U8MAX = 0xFF;
	public static inline var U16MAX = 0xFFFF;

	var uid(default, null): Int;
	var h: Map<String, Int>;
	var final_counter: Int;
	var lstates: List<State>;
	var per: Int;
	var perRB: Int;
	var table: Table;
	var segs: Int;
	var segsEx: Int;
	var nrules: Int;
	var nstates: Int;
	var invalid: Int;
	var entrys: Array<{index:Int, begin:Int, width:Int}>;
	var nfas: haxe.ds.Vector<Array<Node>>; // assoc with lhsA
	var used: haxe.ds.Vector<Bool>;        // Does not perform reachable detection for unused LHS

	public inline function write(out, split = false) this.table.write(posRB(), per, perRB, isBit16(), out, split);
	public inline function posRB() return this.segsEx * this.per;
	public inline function posRBL() return posRB() + this.perRB;
	inline function isBit16() return this.invalid == U16MAX;
	inline function isFinal(n: Node) return n.id < this.nrules;

	function new(s_it, rest) {
		super(s_it, rest);
		this.h = new Map();
		this.lstates = new List();
		this.segs = 0;      // state_counter
		this.invalid = U16MAX;
		this.entrys = [];
		this.nfas = new haxe.ds.Vector(lhsA.length);
		this.used = new haxe.ds.Vector(lhsA.length);
		this.final_counter = U16MAX - 1; // will compress it later.
		this.per = (this.width() - 1 | 15 ) + 1;
		this.nrules = Lambda.fold(this.lhsA, (l, n) -> l.cases.length + n, 0);
		this.uid = nrules;  // so all the finalsID must be less then nrules.
	}

	function dump(file:String) {
		var f = sys.io.File.write(file);
		f.writeString("\nProduction:\n");
		f.writeString(debug.Print.production(this));
		f.writeString(debug.Print.lr0Table(this));
		f.writeString("\n\nRAW:\n");
		this.write(f, true);
		f.close();
	}

	function make() {
		if ( isEmpty() ) return [];
		// Pattern -> NFA(nodes)
		var i = 0;
		var a = this.toPartern();
		for (p in 0...a.length) {
			var pats = a[p];
			var len = pats.length;
			var nodes = [];
			for (j in 0...len) {
				var f = new Node(i++);
				var n = initNode(pats[j], f);
				nodes[j] = n;
			}
			nfas[p] = nodes;
		}
		var begin = 0;
		function addEntry(i) {
			this.entrys.push({index: i, begin: begin, width: this.segs - begin});
			if (begin == this.segs)
				Context.fatalError("Empty: " + lhsA[i].name, lhsA[i].pos);
			begin = this.segs;
			this.used[i] = true;
		}
		// NFA -> DFA
		// main entry
		compile( LexEngine.addNodes([], this.nfas[0]) );
		addEntry(0);
		// side entrys
		for (i in 1...this.lhsA.length) {
			if (this.lhsA[i].side == false) continue;
			this.h = new Map(); // reset
			compile( LexEngine.addNodes([], this.nfas[i]) );
			addEntry(i);
		}

		// properties
		this.segsEx = this.segs;
		this.nstates = lstates.length;
		this.invalid = nstates < U8MAX ? U8MAX : U16MAX;
		this.perRB = 1 + ((nstates - 1) | 15);
		// compress finalState
		var diff = final_counter + 1 - segs;
		for (s in lstates) {
			for (i in 0...s.targets.length)
				if (s.targets[i] > segs)
					s.targets[i] -= diff;
			if (s.id > segs) s.id -= diff;
		}

		this.doPrecedence();

		// DFA -> Tables
		this.makeTable();
		this.rollback();
		#if lex_lr0table
		this.dump("lr0-table.txt");
		#end
		this.checking();
		return this.generate();
	}

	function makeTable() {
		var INVALID = this.invalid;
		var bytes = (segsEx * per) + (3 * perRB);
		var tbls = new Table(bytes);
		for (i in 0...bytes) tbls.set(i, INVALID);

		for (s in this.lstates) {
			tbls.set(tbls.exitpos(s.id), s.finalID == -1 ? INVALID: s.finalID);
			if (s.id < segsEx)
				LexEngine.makeTrans(tbls, s.id * per, s.trans, s.targets);
		}
		this.table = tbls;
	}

	function closure(nodes: Array<Node>) {
		function noSelf(dst: Array<Node>, src: Array<Node>, self: Int) {
			function isSelf (n: Node) {
				for (nc in n.trans)
					for (c in nc.chars)
						return c.min == self;
				return false;
			}
			for (n in src)
				if (!isSelf(n))
					LexEngine.addNode(dst, n);
		}
		var alt = new haxe.ds.Vector<Bool>(lhsA.length);
		for (n in nodes) {
			for (nc in n.trans) {
				for (c in nc.chars) {
					var atLast = isFinal(nc.n);
					var self = c.min;
					if ( isNonTerm(self) ) {
						var i = self - maxValue;
						if (alt[i]) continue;
						var ex = null;
						if (!atLast) {
							LexEngine.addNodes(nodes, this.nfas[i]);
						} else {
							noSelf(nodes, this.nfas[i], self);
						}
						alt[i] = true;
						used[i] = true;
						// left subs.
						for (s in lhsA[i].lsubs) {
							var j = s - maxValue;
							if (alt[j]) continue;
							if (!atLast) {
								LexEngine.addNodes(nodes, this.nfas[j]);
							} else {
								noSelf(nodes, this.nfas[j], self);
							}
							alt[j] = true;
							used[j] = true;
						}
					}
				}
			}
		}
		return nodes;
	}

	function compile(nodes: Array<Node>): Int {
		var sid = nodes.map( n -> n.id ).join("+");
		var id = h.get(sid);
		if (id != null)
			return id;
		var ta: Array<NAChars> = LexEngine.getTransitions( closure(nodes) );
		var len = ta.length;
		id = if (len == 0) {
			final_counter--; // final state.
		} else {
			segs++;
		}
		h.set(sid, id);

		var trans: Array<Char> = [];
		for (i in 0...len)
			for (c in ta[i].chars)
				trans.push(Char.c3(c.min, c.max, i));

		var targets = []; targets.resize(len);
		for (i in 0...len)
			targets[i] = compile(ta[i].ns);

		var exits = [];
		for (n in nodes)
			if ( isFinal(n) )
				exits.push(n.id);
		var f = switch (exits.length) {
		case 0: -1;
		case 1: exits[0];
		default: // since LR(0) so the conflicts can not be resolved.
			Context.fatalError("conflicts: " + exits.join(","), ruleToCase(exits[0]).pos);
		}
		lstates.push(new State(id, trans, targets, f));
		return id;
	}

	inline function node() return new Node(uid++);

	function initNode(p: Pattern, f: Node) {
		return switch (p) {
		case Empty:
			f;
		case Match(c):
			var n = node();
			n.trans.push(new NChars(c, f));
			n;
		case Star(p):
			var n = node();
			var an = initNode(p, n);
			n.epsilon.push(an);
			n.epsilon.push(f);
			n;
		case Plus(p):
			var n = node();
			var an = initNode(p, n);
			n.epsilon.push(an);
			n.epsilon.push(f);
			an;
		case Choice(a, b):
			var n = node();
			n.epsilon.push(initNode(a, f));
			n.epsilon.push(initNode(b, f));
			n;
		case Next(a, b):
			initNode( a, initNode(b, f) );
		}
	}

	function rollback() {
		inline function epsilon(seg) return table.exits(seg);
		var alt = new haxe.ds.Vector<Bool>(this.perRB);
		var INVALID = this.invalid;
		var rollpos = this.posRB();
		var rlenpos = this.posRBL();
		var que = new List<{exit: Int, nxt: Int, len: Int}>(); // FIFO
		function loop(exit, seg, length) {
			alt[seg] = true;
			var base = seg * this.per;
			for (lv in 0...this.per) {
				var nxt = table.get(lv + base);
				if (nxt == INVALID || alt[nxt] || isNonTerm(lv)) continue;
				table.set(rollpos + nxt, exit);
				table.set(rlenpos + nxt, length);
				if (epsilon(nxt) == INVALID)
					que.add({exit: exit, nxt: nxt, len: length + 1});
			}
		}
		function noNxt(seg) {
			if (table.get(rollpos + seg) != INVALID) return;
			var base = seg * this.per;
			for (lv in 0...this.per) {
				var nxt = table.get(lv + base);
				if (nxt != INVALID)
					alt[nxt] = true;
			}
		}
		for (e in entrys) {
			for (i in 0...alt.length) alt[i] = false; // reset

			for (seg in e.begin...e.begin + e.width) {
				var exit = epsilon(seg);
				if (exit == INVALID)
					noNxt(seg);
				else
					loop(exit, seg, 1);
			}

			while (true) {
				var q = que.pop();
				if (q == null) break;
				loop(q.exit, q.nxt, q.len);
			}
		}
	}

	function doPrecedence() {
		if (Lambda.empty( this.opSMap ))
			return;
		var states = Lambda.array(lstates);
		states.sort(State.onSort);

		inline function indexByLval(lv) return lv - this.maxValue;
		function swap(dst, src, begin, max) {
			if (dst == src) return;
			var x = states[dst]; // x.id == dst
			var y = states[src]; // y.id == src;
			// swap targets
			for (si in begin...max) {
				var s = states[si];
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
			states[dst] = y;
			states[src] = x;
		}

		var moves:Array<{fid:Int, begin:Int, smax:Int}> = [];
		for (e in this.entrys) {
			var fvsets = new haxe.ds.Vector(this.lhsA.length);        // follow_sets of all LHS, (tval => tar)
			for (i in 0...fvsets.length) {
				var vec = new haxe.ds.Vector<Int>(this.maxValue);     // no "non-termls" here
				for (j in 0...this.maxValue)
					vec[j] = -1;
				fvsets[i] = vec;
			}
			var vrule = new haxe.ds.Vector<Int>(this.lstates.length); // State => "which Rule"
			for (i in 0...vrule.length) vrule[i] = -1;

			for (i in e.begin...e.begin + e.width) {
				var s = states[i];
				if (s.finalID != -1)
					vrule[i] = s.finalID;
				for (c in s.trans) {
					var nxt = s.targets[c.ext];
					if (nxt >= this.segs)
						vrule[nxt] = states[nxt].finalID;
					if ( this.isTerm(c.min) || nxt >= this.segs)
						continue;
					var ns = states[nxt];
					for (nc in ns.trans) {
						for (n in nc.min...nc.max + 1) {
							if (this.isTerm(n) && this.opIMap.get(n) != null) {
								var tar = ns.targets[nc.ext];
								var vec = fvsets[ indexByLval(c.min) ]; // c is a "non-terml"
								if (vec[n] != -1 && vec[n] != tar)
									throw "Operator precedence confict";
								vec[n] = tar;
							}
						}
					}
				}
			}
			// Vector => List
			var flsets = new haxe.ds.Vector(this.lhsA.length);
			for (i in 0...flsets.length) {
				var li = new List<OpAssoc>();
				var vec = fvsets[i];
				for (j in 0...vec.length) {
					if (vec[j] == -1) continue;
					li.add( this.opIMap.get(j) );
				}
				flsets[i] = li;
			}
			//

			for (fid in 0...vrule.length) {
				if (vrule[fid] == -1) continue;
				var line = this.ruleToCase( vrule[fid] );
				var prec = line.prec;
				if (prec == null || prec.type == NonAssoc) continue;
				var rights = flsets.get( indexByLval(prec.lval) );
				// left vs right
				var col = new List<{tar:Int, tval: Int}>();
				for (r in rights) {
					if (prec.type == Left && prec.prio < r.prio
					|| prec.type == Right && prec.prio <= r.prio) {
						col.add({tval: r.tval, tar: fvsets[indexByLval(prec.lval)][r.tval]});
					}
				}
				if (col.length == 0) continue;
				if (fid >= this.segs)
					moves.push({fid: fid, begin: e.begin, smax: e.begin + e.width});
				// write. NOTE: There is no merge for the same target
				var tar = states[fid].targets;
				var cset = states[fid].trans;
				for (x in col) {
					var c = Char.c3(x.tval, x.tval, tar.length);
					if (fid < this.segs && CSet.inter(cset, [c]).length > 0)
						Context.fatalError("Operator precedence confict", line.pos);
					cset.push(c);
					tar.push(x.tar);
				}
			}
		}
		moves.sort((a, b) -> a.fid - b.fid);
		// move to the front
		var li = new List<{dst:Int, src:Int}>();
		for (m in moves) {
			var fnew = this.segsEx++;
			li.add({dst: fnew, src: m.fid});
			swap(fnew, m.fid, m.begin, m.smax);
		}
		// swap the rest
		for (x in li) {
			if (x.dst == x.src) continue;
			for (si in this.segs...this.segsEx) {
				var s = states[si];
				for (i in 0...s.targets.length) {
					var t = s.targets[i];
					if (t == x.dst) {
						s.targets[i] = x.src;
					} else if (t == x.src) {
						s.targets[i] = x.dst;
					}
				}
			}
		}
	}

	function checking() {
		var INVALID = this.invalid;
		// init exitN/NRule => final_state rules
		var rules = new haxe.ds.Vector<Int>(this.nrules);
		for (n in 0...this.nrules)
			rules[n] = INVALID;
		for (i in table.length-this.perRB...table.length) {
			var n = table.get(i);
			if (n == INVALID) continue;
			// since: i = table.length - 1 - state; // table.exitpos
			// so   : state = table.length - 1 - i;
			rules[n] = table.length - 1 - i;
		}
		// 1. Is switch case unreachable?
		for (n in 0...this.nrules)
			if (rules[n] == INVALID) {
				var lhs = byRule(n);
				if ( !this.used.get( index(lhs) ) ) continue;
				var msg = "Unreachable switch case";
				if (!lhs.side)
					msg += ' OR missing @:side on "' + lhs.name + '"';
				Context.fatalError(msg, this.ruleToCase(n).pos);
			}
		// 2. each state
		for (i in 0...this.segs) {
			if (table.exits(i) != INVALID) continue; // epsilon
			var base = i * this.per;
			var find = false;
			for (p in base ... base + this.maxValue) {
				var c = table.get(p);
				if (c != INVALID) {
					find = true;
					break;
				}
			}
			if (!find) throw("Missing terminator in STATE: " + i);
		}
		// 3. switch guard
		var n = 0;
		for (lhs in this.lhsA) {
			for (li in lhs.cases) {
				if (li.guard != null) {
					var final_state = rules[n];
					if (this.table.get(this.posRB() + final_state) == INVALID)
						Context.fatalError("No switch case that can be rollback from here", li.guard.pos);
				}
				++ n;
			}
		}
		// more?
	}

	function generate(): Array<Field> {
		var force_bytes = !Context.defined("js") || Context.defined("lex_rawtable");
		if (Context.defined("lex_strtable")) force_bytes = false; // force string as table format
		if (isBit16() && force_bytes == false && !Context.defined("utf16")) force_bytes = true;

		var getU:Expr = null;
		var raw:Expr = null;
		if (force_bytes) {
			var bytes = this.table.toByte(isBit16());
			var resname = "_" + StringTools.hex(haxe.crypto.Crc32.make(bytes)).toLowerCase();
			Context.addResource(resname, bytes);
		#if hl
			getU = isBit16() ? macro raw.getUI16(i << 1) : macro raw.get(i);
			raw = macro haxe.Resource.getBytes($v{resname}).getData().bytes;
		#else
			getU = isBit16() ? macro raw.getUInt16(i << 1) : macro raw.get(i);
			raw = macro haxe.Resource.getBytes($v{resname});
		#end
		} else {
			var out = haxe.macro.Compiler.getOutput() + ".lr0-table";
			var dir = haxe.io.Path.directory(out);
			if (!sys.FileSystem.exists(dir))
				sys.FileSystem.createDirectory(dir);
			var f = sys.io.File.write(out);
			this.write(f);
			f.close();
			getU = macro StringTools.fastCodeAt(raw, i);
			raw = macro ($e{haxe.macro.Compiler.includeFile(out, Inline)});
		}
		var lva = this.n2Lhs.map(n -> macro $v{n}).toArray();
		var defs = macro class {
			static var raw = $raw;
			static var lva:Array<Int> = [$a{lva}];
			static inline var INVALID = $v{this.invalid};
			static inline var NRULES  = $v{this.nrules};
			static inline var NSEGSEX = $v{this.segsEx};
			static inline function getU(i:Int):Int return $getU;
			static inline function trans(s:Int, c:Int):Int return getU($v{this.per} * s + c);
			static inline function exits(s:Int):Int return getU($v{this.table.length - 1} - s);
			static inline function rollB(s:Int):Int return getU(s + $v{this.posRB()});
			static inline function rollL(s:Int):Int return getU(s + $v{this.posRBL()});
			static inline function gotos(fid:Int, s:$ct_stream) return cases(fid, s);
			var stream: $ct_stream;
			public function new(lex: lm.Lexer<Int>) {
				this.stream = new lm.Stream<$ct_lval>(lex, 0);
			}
			@:access(lm.Stream, lm.Tok)
			static function _entry(stream: $ct_stream, state:Int, exp:Int, until:Bool):$ct_lval {
				var prev = state;
				var t: lm.Stream.Tok<$ct_lval> = null;
				var dx = 0;
				var keep = stream.pos; // used for _side.
				while (true) {
					while (true) {
						t = stream.next();
						state = trans(prev, t.term);
						t.state = state;
						if (state >= NSEGSEX)
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
							t = stream.offset( -1 - dy);
							if ( trans(t.state, lva[q] >> 8) == INVALID ) {
								until = false; // force error
								break;
							}
							stream.rollback(dy, $v{maxValue});
						} else {
							break;  // throw error.
						}
					}
					dx = 0;         // reset dx after rollback
					while (true) {
						var value:$ct_lval = gotos(q, stream);
						t = stream.offset( -1); // reduced token
						if (t.term == exp && !until) {
							-- stream.pos;      // discard the last token
							stream.junk(1);
							return value;
						}
						t.val = value;
						t.state = trans(stream.offset( -2).state, t.term);
						prev = t.state;
						if (prev < NSEGSEX) break;
						if (prev == INVALID) {
							if (until && exp == t.term)
								return value;
							throw stream.UnExpected(t);
						}
						q = exits(prev);
					}
				}
				if ( until && (stream.pos - dx == keep + 1) && (exp == stream.cached[keep].term) )
					return stream.cached[keep].val;
				t = stream.offset( -1);
				throw stream.error('Unexpected "' + (t.term != $i{sEof} ? stream.str(t): $v{sEof}) + '"', t);
			}
			@:access(lm.Stream, lm.Tok)
			static function _side(stream: $ct_stream, state:Int, lv: Int):$ct_lval {
				var keep = stream.pos;
				var prev = stream.offset( -1);
				var t = new lm.Stream.Tok<$ct_lval>(lv, prev.pmax, prev.pmax);
				t.state = state;
				stream.shift(t);
				var value = _entry(stream, state, lv, true);
				stream.pos = keep;
				stream.junk(2);
				return value;
			}
		}
		// build switch
		var actions = Lambda.flatten( lhsA.map(l -> l.cases) ).map( s -> s.action );
		var here = Context.currentPos();
		var defCase = actions.pop();
		var liCase = Lambda.mapi( actions, (i, e)->({values: [macro $v{i}], expr: e}: Case) );
		var eSwitch = {expr: ESwitch(macro (_q), liCase, defCase), pos: here};
		defs.fields.push({
			name: "cases",
			access: [AStatic],
			kind: FFun({
				args: [{name: "_q", type: macro: Int}, {name: "s", type: ct_stream}],
				ret: ct_lval,
				expr: macro {
					@:mergeBlock $b{preDefs};
					var _v:$ct_lval = $eSwitch;
					@:privateAccess s.reduce(lva[_q]);
					return _v;
				}
			}),
			pos: here,
		});

		// main entry
		var en = this.entrys[0];
		var lhs = lhsA[en.index];
		defs.fields.push({
			name: lhs.name,
			access: [APublic, AInline],
			kind: FFun({
				args: [],
				ret: lhs.ctype,
				expr: macro return _entry(stream, $v{en.begin}, $v{lhs.value}, false)
			}),
			pos: lhs.pos,
		});
		// other entrys =>
		for (i in 1...this.entrys.length) {
			var en = this.entrys[i];
			var lhs = lhsA[en.index];
			defs.fields.push({
				name: lhs.name,
				access: [AStatic, AInline],
				kind: FFun({
					args: [{name: "s", type: ct_stream}],
					ret: lhs.ctype,
					expr: macro return _side(s, $v{en.begin}, $v{lhs.value})
				}),
				pos: lhs.pos,
			});
		}
		return defs.fields;
	}

	public static function build() {
		var allFields = new Map<String, Field>();
		var lrb = new LR0Builder("lm.LR0", allFields);
		var defs = lrb.make();
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
extern class LR0Builder{}

@:autoBuild(lm.LR0Builder.build())
#end
@:remove interface LR0<LEX, LHS> {
}