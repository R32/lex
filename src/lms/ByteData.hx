package lms;

/**
* then you can use "--remap <lms:mypkg>" to override this class.
*/
#if (!js || force_bytes)
extern abstract ByteData(haxe.io.Bytes) {
	public var length(get,never):Int;
	private inline function get_length():Int return this.length;

	inline function readByte(i:Int):Int return this.get(i);

	inline function readString(pos:Int, len:Int):String return this.getString(pos, len, UTF8);

	static inline function ofString(s:String):ByteData return cast haxe.io.Bytes.ofString(s, UTF8);

	static inline function ofBytes(b:haxe.io.Bytes):ByteData return cast b;
}
#else
extern abstract ByteData(String) {
	public var length(get,never):Int;
	private inline function get_length():Int return this.length;

	inline function readByte(i:Int):Int return StringTools.fastCodeAt(this, i);

	inline function readString(pos:Int, len:Int):String return (this:Dynamic).substr(pos, len);

	static inline function ofString(s:String):ByteData return cast s;

	static inline function ofBytes(b:haxe.io.Bytes):ByteData return cast b.toString();
}
#end
