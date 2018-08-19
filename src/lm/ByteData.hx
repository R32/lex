package lm;

extern abstract ByteData(haxe.io.Bytes) {

	public var length(get,never):Int;
	private inline function get_length():Int return this.length;

	private inline function new(data:haxe.io.Bytes) this = data;

	inline function readByte(i:Int):Int return this.get(i);

	inline function readString(pos:Int, len:Int):String return this.getString(pos, len);

	static inline function ofString(s:String):ByteData return new ByteData(haxe.io.Bytes.ofString(s));

	static inline function ofBytes(b:haxe.io.Bytes):ByteData return new ByteData(b);
}