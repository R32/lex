package tools;

import sys.FileSystem;
import sys.io.File;
import haxe.Template;
import tools.CLexer;

/**
 build lex for c language
*/
class Run {

	static inline var version = "0.3";

	var files : Array<String>;
	var libPath : String;
	var outdir : String;

	function usage() {
		Sys.print('Lexer Tool v$version
Usage : haxelib run lex [options] <files>
 Options :
  -o, --out <dir> : specify output directory
  -h, --help      : print help infomation
');
	}

	function doLexer() {
		var smt = "";
		if ( EXISTS( LEXER_TEMPLATE ) ) {
			smt = File.getContent(LEXER_TEMPLATE);
		} else if (libPath != null) {
			smt = File.getContent(libPath + "tools/" + LEXER_TEMPLATE);
		}
		if (smt == "")
			throw "Needs " + LEXER_TEMPLATE;
		var mt = new Template(smt);
		for (f in files) {
			if (EXISTS(f)) {
				new CLexer(f, mt, {outdir : outdir});
			}
		}
	}

	function doParser() {
		Sys.print("UnImplements\n");
	}

	public function new( args : Array<String> ) {

		files = [];
		outdir = "";

		if (Sys.getEnv("HAXELIB_RUN") == "1") {
			libPath = Sys.getCwd();
			Sys.setCwd(args.pop());
		} else {
			libPath = getLibPath();
		}

		// parse args
		var i = 0;
		var max = args.length;
		var isParser = false;
		while (i < max) {
			var v = args[i++];
			if (v.charCodeAt(0) != "-".code) {
				files.push(v);
				continue;
			}
			if (v == "-o" || v == "--out") {
				outdir = args[i++];
				continue;
			}
			if (v == "-h" || v == "--help") {
				return usage();
			}
			if (v == "-p" || v == "--parser") {
				isParser = true;
			}
		}
		if (files.length == 0)
			return usage();
		// run
		if (isParser) {
			doParser();
		} else {
			doLexer();
		}
	}

	static inline var LEXER_TEMPLATE = "clex.template";

	static inline function EXISTS(f) return FileSystem.exists( f ) && !FileSystem.isDirectory(f);

	static function main() {
		new Run(Sys.args());
	}

	static function getLibPath() : String {
		var proc = new sys.io.Process("haxelib", ["path", "lex"]);
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
}
