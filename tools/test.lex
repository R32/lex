#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "rlex.h"

enum token {
	Eof = 0,
	CInt,
	CIdent,
	CString,  // "strinig"
	CQString, // 'string'
	OpMul,
	OpDiv,
	OpAdd,
	OpSub,
};

%%         // lexer starts,
           // NOTE: only line comments are accepted in this area

%EOF(Eof)  // Specify the return value when EOF.
           // If not provided then `void` means no value will be returned

%SRC(UTF8) // Specify the encoding format of the input source. (UTF8|UCS2)
           // If not provided the default is UTF8. (WARINNING: only acill chars even UCS2)

%MAX(127)  // Specify the width of table-state-chunk. (127|255)
           // If not provided the default is 127

let ident = "[a-zA-Z_][a-zA-Z0-9_]*"

let integer = "0|[1-9][0-9]*"

let token = function
| "[ \t\n]+"     -> TOKEN()
| ident          -> CIdent
| "0"
| "[1-9][0-9]*"  -> CInt
| "+"            -> OpAdd
| "-"            -> OpSub
| "*"            -> OpMul
| "/"            -> OpDiv
| '"'            ->
	int min = lex->pmin;
	enum token t = STR();
	if (t == Eof) {
		printf("UnClosed String: %d-%d",min, lex->pmax);
		exit(-1);
	}
	lex->pmin = min; // position union
	t
| "'" ->
	int min = lex->pmin;
	enum token t = QSTR();
	if (t == Eof) {
		printf("UnClosed String: %d-%d",min, lex->pmax);
		exit(-1);
	}
	lex->pmin = min;
	t
| _ ->
	printf("UnMathed : %c\n", LEX_CHAR(lex->pmax)); // if error then pmin >= pmax
	exit(-1);
	Eof

let str = function
| '"'            -> CString
| '\\"'          -> STR()
| '[^"]+'        -> STR()

let qstr = function
| "'"            -> CQString
| "\\'"          -> QSTR()
| "[^']+"        -> QSTR()

%%

int main(int argc, char** argv) {
	char buff[256];
	char* text = "1 + 2 - \"string\" * _ident_ / 101";
	struct rlex lex;

	test_init(&lex, text, strlen(text)); // filename_init

	while(1) {
		int tok = rlex_token(&lex);
		switch(tok) {
		case Eof:
			goto Endloop;
		case CInt:
		{
			long n = atol(rlex_current(&lex));
			printf("%ld\n", n);
		}
			break;
		case CIdent:
		{
			int size = rlex_cursize(&lex);
			memcpy(buff, rlex_current(&lex), size);
			buff[size] = 0;
			printf("%s\n", buff);
		}
			break;
		case CString:
		case CQString:
		{
			int size = rlex_cursize(&lex);
			memcpy(buff, rlex_current(&lex), size);
			buff[size] = 0;
			printf("%s\n", buff);
		}
			break;
		case OpMul:
			printf("*\n");
			break;
		case OpDiv:
			printf("/\n");
			break;
		case OpAdd:
			printf("+\n");
			break;
		case OpSub:
			printf("-\n");
			break;
		default:
			printf("something is wroing, tok: %d\n", tok);
		}
	}
	Endloop:
	return 0;
}
