package lm;

#if (macro || eval || display)
import lm.Charset;
import lm.LexEngine;
import lm.ParserBase;

class ParserEngine extends lm.LexEngine {

	var parser : ParserBase;

	var nfas : Array<Array<Node>>;

	public var nstates : Int;

	public function new( parser : ParserBase ) {
		this.parser = parser;
		var cmax = parser.max_term_value + parser.lhsides.length;
		var patterns = parser.toPatternSets();
		super(patterns, (cmax - 1) | 15);
	}

	override function make( patterns : Array<PatternSet> ) : Void {
		if (patterns.length == 0)
			return;
		this.nfas = [];
		this.entrys = [];
		this.nrules = this.parser.nrules;
		this.generator = new NodeGenerator(this.nrules);
		// pattern -> NFA(nodes)
		for (pset in patterns) {
			var nodes = [];
			for (p in pset) {
				var f = generator.newFinal();
				var n = generator.normalize(p, f);
				nodes.push(n);
			}
			this.nfas.push(nodes);
		}
		// NFA -> DFA
		for (i in 0...this.nfas.length) {
			if (!this.parser.lhsides[i].entry)
				continue;
			this.entrys.push({begin : this.segs, index : i});
			var nodes = LexEngine.addNodes([], this.nfas[i]);
			this.compile(nodes);
			this.h = new Map(); // reset for next
		}
		// properties
		this.nstates = lstates.length;
		this.invalid = nstates < U8MAX ? U8MAX : U16MAX;
		this.perExit = 1 + ((nstates - 1) | 15);
		// compress finalState
		var diff = final_counter + 1 - segs;
		for (s in lstates) {
			for (i in 0...s.targets.length)
				if (s.targets[i] > segs)
					s.targets[i] -= diff;
			if (s.id > segs)
				s.id -= diff;
		}
		// DFA -> Tables
		this.makeTables();

		#if lex_slrtable
		var f = sys.io.File.write("slr-table.txt");
		f.writeString("\nProduction:\n");
		f.writeString(debug.SLRPrint.production(this.parser));
		f.writeString("\n");
		f.writeString(debug.SLRPrint.table(this.parser, this, this.entrys));
		f.writeString("\n\nRAW:\n");
		this.table.debugWrite(this.per, this.perExit, this.isBit16(), f);
		f.close();
		#end
		lm.ExprHelps.lexChecking(this, this.parser.rule_groups);
	}

	override function closure( nodes : Array<Node> ) : Array<Node> {
		inline function addAll(nodes, nfa) LexEngine.addNodes(nodes, nfa);
		var alt = new haxe.ds.Vector<Bool>(this.nfas.length);
		var p = 0;
		var len = nodes.length;
		while (p < len) { // nodes will grows in loop
			for (arrow in nodes[p++].arrows) {
				for (c in arrow.chars) {
					var tval = c.min;
					if (this.parser.is_term(tval))
						break;
					var i = tval - this.parser.max_term_value;
					if (alt[i])
						continue;
					var atLast = generator.isFinal(arrow.n);
					var exit = arrow.n.id;
					if (atLast) {
						mixing(nodes, i, tval, exit);
					} else {
						addAll(nodes, this.nfas[i]);
					}
					alt[i] = true;
					// lsubs
					for (s in this.parser.lhsides[i].lsubs) {
						var j = s - this.parser.max_term_value;
						if (alt[j])
							continue;
						if (atLast) {
							mixing(nodes, j, tval, exit);
						} else {
							addAll(nodes, this.nfas[j]);
						}
						alt[j] = true;
					}
				}
				break; // non-term has only one value
			}
		}
		return nodes;
	}

	function mixing( dst : Array<Node>, idx : Int, tval : Int, rule : Int) {
		var nfa = this.nfas[idx];
		var lhs = this.parser.lhsides[idx];
		var kaze = this.parser.caseFromRule(rule);
		var same = this.parser.lhsFromRule(rule) == lhs;
		var left = kaze.prec?.left;
		if (left == null) {
			for (i in 0...nfa.length) {
				if (!equalToFirst(tval, kaze, lhs.cases[i], same))
					LexEngine.addNode(dst, nfa[i]);
			}
			return;
		}
		for (i in 0...nfa.length) {
			var right = lhs.cases[i].prec?.right;
			if (left.type != Right && (right == null) && equalToFirst(tval, kaze, lhs.cases[i], same))
				continue;
			if (right == null || right.value != tval
			|| (right.prio >= 0 && (left.prio < right.prio || left.type == Right && left.prio <= right.prio))
			) LexEngine.addNode(dst, nfa[i]);
		}
	}

	function equalToFirst( value : Int, source : StreamTokenSets, target : StreamTokenSets, same : Bool ) {
		if (source == target)
			return true;
		if (target.sets.length == 0)
			return false; // allows empty
		if (!same && target.sets.length > 1)
			return false; // not single [E]
		return value == target.sets[0].cset[0].min;
	}
	public static inline var U8MAX = 0xFF;
	public static inline var U16MAX = 0xFFFF;
}
#end
