#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "rlex.h"
#include "rstream.h"

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
	UnMathed,
};

%%         // lexer starts,
           // NOTE: only line comments are accepted in this area

%EOF(Eof)  // (Required) Specify the EOF Token.

%SRC(UTF8) // Specify the encoding format of the input source. (UTF8|UCS2)
           // If not provided the default is UTF8. (WARINNING: only acill chars even UCS2)

%MAX(127)  // Specify the width of table-state-chunk. (127|255)
           // If not provided the default is 127

let ident = "[a-zA-Z_][a-zA-Z0-9_]*"

let integer = "0|[1-9][0-9]*"

//  "|" must be on the leftmost side of Line
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
	int min = lex->pos.min;
	enum token t = STR();
	if (t == Eof) {
		printf("UnClosed String: %d-%d",min, lex->pos.max);
		exit(-1);
	}
	lex->pos.min = min; // position union
	t
| "'" ->
	int min = lex->pos.min;
	enum token t = QSTR();
	if (t == Eof) {
		printf("UnClosed String: %d-%d",min, lex->pos.max);
		exit(-1);
	}
	lex->pos.min = min;
	t
| _ ->
	UnMathed

let str = function
| '"'            -> CString
| '\\"'          -> STR()
| '[^"]+'        -> STR()

let qstr = function
| "'"            -> CQString
| "\\'"          -> QSTR()
| "[^']+"        -> QSTR()

%%


void test_lexer() {
	char buff[256];
	char* text = "1 + 2 - \"string\" * ident / 101 [";
	struct rlex lex;
	test_lexinit(&lex, text, strlen(text)); // filename + "lexinit"
	#undef TOKEN
	#define TOKEN() rlex_token(&lex)
	assert(TOKEN() == CInt);     // 1
	assert(TOKEN() == OpAdd);    // +
	assert(TOKEN() == CInt);     // 2
	assert(TOKEN() == OpSub);    // -
	assert(TOKEN() == CString);  // ""
	assert(TOKEN() == OpMul);    // *
	assert(TOKEN() == CIdent);   // ident
	assert(TOKEN() == OpDiv);    // /
	assert(TOKEN() == CInt);     // 101
	assert(TOKEN() == UnMathed); // [, means error
	assert(lex.pos.min > lex.pos.max); // if error you will get pmin > pmax

	struct rlex_position p1 = {20, 80}, p2 = {10, 60};
	struct rlex_position p3 = rlex_position_union(p1, p2);
	assert(p3.min == p2.min && p3.max == p1.max);
/*
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
		case UnMathed:
			printf("UnMathed : %c\n", rlex_char(&lex, lex.pos.max)); // if error then pmin >= pmax
			goto Endloop;
			break;
		default:
			break;
		}
	}
	Endloop:
*/
}

void test_stream() {
	struct rlex lex;
	struct rstream_tok *pt = NULL;
	struct rstream stream = { // same as rstram_init(&stream)
		.head = 0,
		.tail = 0,
		.lex = &lex,
	};
	char* text = "id 1 + 2 * 3 / 4 - id";
	test_lexinit(&lex, text, strlen(text));
	#undef TOKEN
	#define TOKEN()     (rstream_next(&stream))
	#define TERM(t)     ((t)->term)
	#define PEEK(i)     (rstream_peek(&stream, i))
	#define JUNK(n)     (rstream_junk(&stream, n))
	#define REDUCE(w)   (rstream_reduce(&stream, w))
	#define UNSHIFT(t)  (rstream_unshift(&stream, t))

	assert(TERM(TOKEN()) == CIdent); // next <= id
	assert(TERM(TOKEN()) == CInt);   // next <= 1
	assert(TERM(TOKEN()) == OpAdd);  // next <= +
	assert(TERM(TOKEN()) == CInt);   // next <= 2

	assert(stream.tail - stream.head == 0 && stream.head == 4);
	pt = REDUCE(4);
	assert(pt->pos.min == 0 && pt->pos.max == lex.pos.max);
	assert(stream.tail - stream.head == 0 && stream.head == 1);

	assert(TERM(PEEK(0)) == OpMul);  // peek <= *
	assert(TERM(PEEK(1)) == CInt);   // peek <= 3
	assert(TERM(PEEK(2)) == OpDiv);  // peek <= /
	assert(stream.tail - stream.head == 3 && stream.head == 1);
	JUNK(2);
	assert(stream.tail - stream.head == 1 && stream.head == 1);
	assert(TERM(PEEK(0)) == OpDiv);

	UNSHIFT(PEEK(0));
	assert(stream.tail - stream.head == 1 && stream.head == 2);
	assert(TERM(PEEK(-1)) == OpDiv);  // unsafe
	assert(TERM(PEEK( 0)) == OpDiv);
	JUNK(1);
	assert(stream.tail - stream.head == 0 && stream.head == 2);
	// TODO: more
}

int main(int argc, char** argv) {
	test_lexer();
	test_stream();
	return 0;
}
