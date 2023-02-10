package tools.generator;

typedef TplEntrys = {
	name : String,
	uname : String, // name.toUpperCase()
	begin : Int,    // start state
	?value : Int,    // (Only SLR) lhs.value
	?stype : String, // (Only SLR)
}

/*
 * for haxe.Template
 */
class TplVars {

	/*
	 * prefix name, Specified by %NAME()
	 *
	 * default is "path.name"
	 */
	public var prefix : String; // prefix name

	/*
	 * stride * nsegs + sizeof(exit) = tabsize;
	 */
	public var stride : Int;

	/*
	 * raw datas
	 */
	public var table : String;

	/*
	 * The character size of the "table"
	 */
	public var tabsize : Int;

	/*
	 * the available states
	 */
	public var nsegs : Int;

	/*
	 * number of available rules
	 */
	public var nrules : Int;

	/*
	 * if (available states + empty states) >= 255
	 */
	public var states_over_255 : Bool;

	/*
	 * (Lex Only), Specified by %EOF()
	 */
	public var eof : String;

	/*
	 * (Lex Only) the source format, Specified by %SRC()
	 *
	 * default is true
	 */
	public var utf8 : Bool;

	/*
	 * invalid character, 0xFF or 0xFFFF
	 */
	public var invalid : Int;

	/*
	 * cases of switch
	 */
	public var cases   : Array<{ index : Int, action : String }>;

	/*
	 * default of switch
	 */
	public var epsilon : String; // case _: actions

	public var entrybegin : Int;

	public var entrys : Array<TplEntrys>;

	public function new ( name : String ) {
		prefix = name;
		stride = 128;
		utf8 = true;
		eof = null;
		table = "";
		epsilon = "";
		cases = [];
		entrys = [];
	}

	public function update( lexe : lm.LexEngine ) {
		this.stride  = lexe.per;
		this.nsegs   = lexe.segs;
		this.nrules  = lexe.nrules;
		this.invalid = lexe.invalid;
		this.tabsize = lexe.table.length;
		this.table   = ExprHelps.lexstable(lexe);
		this.states_over_255 = lexe.isBit16();
		this.entrybegin = lexe.entrys[0].begin;
	}
}
