#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "rlex.h"
#include "rstream.h"

enum token {
	Eof = 0,
	CInt,
	OpMul,
	OpDiv,
	OpAdd,
	OpSub,
	UnMathed,
};

%% // lexer starts,

/*
 * The tool will generate the following types and macros in the beginning of the file:
 *
 * #define LEXCHAR unsigned char  // if %SRC is UTF8, or "#define LEXCHAR unsigned short" for UCS2
 *
 * #define LEXCHAR_UCS2           // if %SRC is UCS2, you will see this
 *
 * #define rlex_char(lex, i)      (((LEXCHAR *)(lex)->src)[i])
 * #define rlex_current(lex)      (((LEXCHAR *)(lex)->src) + (lex)->pos.min)
 */

%NAME(test)    // (Optional) default is current file name.
               // Then the generated function names could use it as a prefix

%TOKEN(token)  // (Optional if no SLR), It is provided to Simple LR.
               // %TOKEN(enum, ?filename),
               // - @enum : the name of enum.
               // - @filename : the file where enum is, default is current file.

%EOF(Eof)      // (Required) Specify EOF name.

%SRC(UTF8)     // (Optional) Specify the encoding format of the input source. (UTF8|UCS2)
               // default is UTF8.

%MAX(127)      // (Optional) Specify the maximum character.(127|255)
               // It will affect the size of the jump table, The byte size for each state will be (MAX + 1)
               // default is 127.


let integer = "0" | "[1-9][0-9]*"


let token = function
| "[ \t\n\r]+" -> token()  // recursion call token
| integer      -> CInt
| "+"          -> OpAdd
| "-"          -> OpSub
| "*"          -> OpMul
| "/"          -> OpDiv
| _ ->                     // optional error handing
	printf("UnMatched : '%c' at %d-%d\n", rlex_char(lex, lex->pos.max), lex->pos.max, lex->pos.min);
	exit(-1);
	0 // dummy line for template processing, 
	  // Because the last statement will be "_ret = (last stsm);"

%% // lexer end

// normal c code here
