package tools.generator;

class DArgs {

	/*
	 * The target file that will be processed
	 */
	public var lexeme : String;

	/*
	 * The target file that will be processed
	 */
	public var simpleLR : String;

	public var outdir : String;

	/*
	 * the path of haxelib lex
	 */
	public var libpath : String;

	var rest : Map<String, String>;

	public function new( args : Array<String> ) {
		this.rest = new Map();
		this.outdir = "";
	#if interp
		if (Sys.getEnv("HAXELIB_RUN") == "1") {
			this.libpath = Sys.getCwd();
			Sys.setCwd(args.pop());
		} else {
			this.libpath = getLibPath();
		}
	#else
		this.libpath = new haxe.io.Path(Sys.programPath()).dir;
	#end
		// parse args
		var i = 0;
		var max = args.length;
		while (i < max) {
			var v = args[i++];
			if (v.charCodeAt(0) != "-".code) {
				if (!FILE_EXISTS(v))
					fatal("File not found: " + v);
				this.lexeme = v;
			}
			else if (v == "-o" || v == "--out") {
				v = args[i++];
				this.outdir = v;
			}
			else if (v == "-h" || v == "--help") {
				this.lexeme = null;
				this.simpleLR = null;
				return;
			}
			else if (v == "--slr") {
				v = args[i++];
				if (!FILE_EXISTS(v))
					fatal("File not found: " + v);
				this.simpleLR = v;
			}
			else if (v.substr(0, 2) == "--") {
				v = v.substring(2);
				this.set(v, "true");
			}
		}
	}

	public inline function get( key : String ) : String {
		return rest.get(key);
	}

	public inline function set( key : String, value : String ) {
		rest.set(key, value);
	}

	static public function string_has( s : String ) : Bool {
		return s != null && s != "";
	}

	static function getLibPath() : String {
		var proc = new sys.io.Process("haxelib", ["libpath", "lex"]);
		if (proc.exitCode() != 0) {
			var msg = proc.stderr.readAll().toString();
			proc.close();
			throw msg;
		}
		var out = proc.stdout.readUntil("\n".code);
		proc.close();
		if (out.charCodeAt(out.length - 1) == "\r".code)
			return out.substr(0, out.length - 1)
		else
			return out;
	}

	public function normalizePath( path : haxe.io.Path ) {
		path.file += "_" + path.ext;
		path.ext = "c";
		if (string_has(outdir))
			path.dir = haxe.io.Path.removeTrailingSlashes(outdir);
		return path.toString();
	}

	public static inline function FILE_EXISTS(f) {
		return sys.FileSystem.exists(f) && !sys.FileSystem.isDirectory(f);
	}

	public static function fatal( msg : String, code = -1 ) : Dynamic {
		Sys.stderr().writeString("[haxelib run lex]: " + msg);
		Sys.exit(code);
		return null;
	}
}
