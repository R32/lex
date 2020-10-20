package lm;

class LineColumn {
	public var line(default, null) : Int;
	public var base(default, null) : Int;   // the position of current line
	public var column(default, null) : Int;
	public function new(a, b, c) {
		line = a;
		base = b;
		column = c;
	}
}

/**
* line conter for single file in Lexer
*/
@:access(lm.LineColumn)
class LConter { //
	public var owner : String;
	public var cache : Array<Int>;
	public function new( source : String ) {
		owner = source;
		cache = [0]; // (line 1, column 1) at 0;
	}
	public function add( pos : Int ) {
		var len = cache.length;
		if (pos > cache[len - 1]) {
			cache.push(pos);
		}
	}
	public function get( pos : Int ) : LineColumn {
		var ret = new LineColumn(1, 0, pos + 1);
		var a = cache;
		var i = 0;
		var j = a.length - 1;
		while(i <= j) {
			var k = i + (j - i >> 1);
			var p = a[k];
			if (pos < p) {
				j = k - 1;
			} else {
				if (k < j && pos >= a[k + 1]) {
					i = k + 1;
					continue;
				}
				ret.column = pos - p + 1;
				ret.line = k + 1;
				ret.base = p;
				break;
			}
		}
		return ret;
	}
}

class Multiple {
	public var current : LConter;
	public var all : Array<LConter>;

	public function new() {
		current = new LConter("");
		all = [];
	}
	function choise( source : String ) {
		if (current.owner == source)
			return true;
		for (c in all) {
			if (c.owner == source) {
				current = c;
				return true;
			}
		}
		return false;
	}

	public function add( source : String, pos : Int ) {
		if (!choise(source)) {
			var c = new LConter(source);
			all.push(c);
			current = c;
		}
		current.add(pos);
	}

	public function get( source : String, pos : Int ) : LineColumn {
		if (!choise(source)) {
			return new LineColumn(1, 0, pos + 1);
		}
		return current.get(pos);
	}
}
