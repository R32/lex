package tools;

import sys.io.File;
import haxe.Template;
import tools.generator.DArgs;
import tools.generator.DArgs.*;

/*
 * build lex and simple lr for c language
 */
class Run {

	static inline var version = "0.5";

	public var dargs : DArgs;

	function usage() {
		Sys.print('Lexer Tool v$version
Usage : haxelib run lex [options] <lex file>
 Options :
  -o, --out <dir> : specify output directory
  -h, --help      : print help infomation
  --slr <file>    : build Simple LR
');
	}

	function getTemplate( name : String ) {
		var smt = "";
		if (FILE_EXISTS(name)) {
			smt = File.getContent(name);
		} if (string_has(dargs.libpath)) {
			smt = File.getContent(dargs.libpath + "tools/" + name);
		}
		if (smt == "") {
			fatal('Requires: "$name"\n');
		}
		return smt;
	}

	function doLexer() {
		var smt = getTemplate(LEX_TEMPLATE);
		var mt = new Template(smt);
		new tools.generator.CLexer(mt, dargs);
	}

	function doSLR() {
		if (!string_has(dargs.lexeme))
			fatal("Requires a lexeme file.");
		var smt = getTemplate(SLR_TEMPLATE);
		var mt = new Template(smt);
		new tools.generator.CSLR(mt, dargs);
	}

	public function new( args : Array<String> ) {
		dargs = new DArgs(args);
		// lexer
		if (string_has(dargs.lexeme))
			doLexer();
		// simple lr
		if (string_has(dargs.simpleLR))
			doSLR();
		// help infomation
		if (!string_has(dargs.lexeme) && !string_has(dargs.simpleLR))
			usage();
	}

	static inline var LEX_TEMPLATE = "clex.template";

	static inline var SLR_TEMPLATE = "cslr.template";

	static function main() {
		new Run(Sys.args());
	}
}
