// Generated by haxelib lex
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

// For template variables, please check CLexer.Config


typedef unsigned char rlexsrc;
#define rlex_char(lex, i)     ((lex)->src[i])
#define rlex_current(lex)     ((lex)->src + (lex)->pos.min)




static unsigned  char  _lextable[] = {
// STATE 0
0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0x01,0x01,0xFF,0xFF,0xFF,0xFF,0xFF,
0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
0x01,0xFF,0x14,0xFF,0xFF,0xFF,0xFF,0x13,0xFF,0xFF,0x12,0x11,0xFF,0x10,0xFF,0x0F,
0x0E,0x02,0x02,0x02,0x02,0x02,0x02,0x02,0x02,0x02,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
0xFF,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,
0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0xFF,0xFF,0xFF,0xFF,0x03,
0xFF,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,
0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0xFF,0xFF,0xFF,0xFF,0xFF,
// STATE 1
0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0x01,0x01,0xFF,0xFF,0xFF,0xFF,0xFF,
0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
0x01,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
// STATE 2
0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
0x02,0x02,0x02,0x02,0x02,0x02,0x02,0x02,0x02,0x02,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
// STATE 3
0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
0xFF,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,
0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0xFF,0xFF,0xFF,0xFF,0x03,
0xFF,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,
0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03,0xFF,0xFF,0xFF,0xFF,0xFF,
// STATE 4
0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,
0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,
0x05,0x05,0x0D,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,
0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,
0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,
0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x06,0x05,0x05,0x05,
0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,
0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,
// STATE 5
0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,
0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,
0x05,0x05,0xFF,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,
0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,
0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,
0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,
0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,
0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,
// STATE 6
0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,
0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,
0x05,0x05,0x0C,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,
0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,
0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,
0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,
0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,
0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,0x05,
// STATE 7
0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x0B,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x09,0x08,0x08,0x08,
0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
// STATE 8
0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
0x08,0x08,0x08,0x08,0x08,0x08,0x08,0xFF,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
// STATE 9
0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x0A,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
// EXIT 
0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0x08,0x09,0x06,0x04,0x05,
0x07,0x02,0x0A,0x0B,0x0D,0x0E,0x0F,0x0F,0xFF,0x0C,0x0C,0xFF,0x01,0x03,0x00,0xFF
};

#define LEX_TABSIZE      1312
#define LEX_TABSPAN      128
#define LEX_EXIT(s)      (_lextable[(LEX_TABSIZE - 1) - (s)])
#define LEX_TRANS(s, c)  (_lextable[((s) * LEX_TABSPAN) + (c)])

static int _entry(struct rlex*, int);


#define TOKEN_BEGIN    0
#define TOKEN()        (_entry(lex, 0))

#define STR_BEGIN    4
#define STR()        (_entry(lex, 4))

#define QSTR_BEGIN    7
#define QSTR()        (_entry(lex, 7))

static int _cases(struct rlex* lex, int _q) {
	int _ret = Eof;
	switch(_q) {

	case 0:
	{
		_ret = TOKEN();
	}
	break;

	case 1:
	{
		_ret = CIdent;
	}
	break;

	case 2:

	case 3:
	{
		_ret = CInt;
	}
	break;

	case 4:
	{
		_ret = OpAdd;
	}
	break;

	case 5:
	{
		_ret = OpSub;
	}
	break;

	case 6:
	{
		_ret = OpMul;
	}
	break;

	case 7:
	{
		_ret = OpDiv;
	}
	break;

	case 8:
	{
		int min = lex->pos.min;
		enum token t = STR();
		if (t == Eof) {
		printf("UnClosed String: %d-%d",min, lex->pos.max);
		exit(-1);
		}
		lex->pos.min = min;
		_ret = t;
	}
	break;

	case 9:
	{
		int min = lex->pos.min;
		enum token t = QSTR();
		if (t == Eof) {
		printf("UnClosed String: %d-%d",min, lex->pos.max);
		exit(-1);
		}
		lex->pos.min = min;
		_ret = t;
	}
	break;

	case 10:
	{
		_ret = CString;
	}
	break;

	case 11:
	{
		_ret = STR();
	}
	break;

	case 12:
	{
		_ret = STR();
	}
	break;

	case 13:
	{
		_ret = CQString;
	}
	break;

	case 14:
	{
		_ret = QSTR();
	}
	break;

	case 15:
	{
		_ret = QSTR();
	}
	break;

	default:
	{
		_ret = UnMathed;
	}
	break;
	}
	return _ret;
}

#undef TOKEN

#undef STR

#undef QSTR


static int _entry(struct rlex* lex, int begin) {
	if (rlex_end(lex))
		return Eof;
	int c;
	int i = lex->pos.max;
	int state = begin;
	int prev = begin;
	while(i < lex->size) {
		c = rlex_char(lex, i++);

		if (c > 127)
			c = 127;

		state = LEX_TRANS(state, c);
		if (state >= 10)
			break;
		prev = state;
	}
	lex->pos.min = i; // if UnMatached then pmin >= pmax
	if (state == 255) {
		state = prev;
		i--;
	}
	int q = LEX_EXIT(state);
	if (i > lex->pos.max && q < 16) {
		lex->pos.min = lex->pos.max;
		lex->pos.max = i;
	} else {
		q = LEX_EXIT(begin);
	}
	return _cases(lex, q);
}
static int __token(struct rlex* lex) {
	return _entry(lex, 0 );
}

// public function
void test_lexinit(struct rlex* lex, rlexsrc *src, int size) {
	lex->pos = (struct rlex_position){0, 0};
	lex->size = size;
	lex->src = (unsigned char*)src;
	lex->token = __token;
}



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
	char *text = "id 1 + 2 * 3 / 4 - id";
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
