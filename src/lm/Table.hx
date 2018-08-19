package lm;

/**
table => [tbl->........|.<-exits]
*/
extern abstract Table(haxe.io.Bytes) {
	public inline function new(b: haxe.io.Bytes) this = b;

	public inline function exits(i: Int):Int return this.get(this.length - i - 1);

	public inline function tbl(p: Int, i: Int):Int return this.get((p * 256) + i);
}