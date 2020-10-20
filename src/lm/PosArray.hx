package lm;

class PosString {
	public var pos : Int;
	public var str : String;
	public function new(s, p) {
		str = s;
		pos = p;
	}
}
// it's simpler then Map<Int,String> to do `map.set(pmin, string)`
class PosArray {
	var data : Array<PosString>;
	public function new() {
		data = [];
	}
	public function get( pos : Int ) : String {
		var i = 0;
		var j = data.length - 1;
		while (i <= j) {
			var k = i + ((j - i) >> 1);
			var p = data[k].pos;
			if (pos == p) {
				return data[k].str;
			} else if (pos < p) {
				j = k - 1;
			} else {
				i = k + 1;
			}
		}
		return null;
	}
	public function add( pos : Int, str : String ) {
		var i = data.length - 1;
		if (i == -1 || i >= 0 && pos > data[i].pos) {
			data.push(new PosString(str, pos));
		}
	}
}
