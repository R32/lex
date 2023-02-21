// Generated by haxelib lex
#define LEXCHAR unsigned char
#define rlex_char(lex, i)     (((LEXCHAR *)(lex)->src)[i])
#define rlex_current(lex)     (((LEXCHAR *)(lex)->src) + (lex)->pos.min)

enum token {
	Eof = 0,
	CInt = 1,
	OpMul = 2,
	OpDiv = 3,
	OpAdd = 4,
	OpSub = 5,
	UnMathed = 6,
};

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "rlex.h"
#include "rstream.h"

static int int_of_string(const LEXCHAR *source, const struct rstream_tok *t)
{
	unsigned char buff[32];
	int i = 0;
	int j = t->pmin;
	int len = t->pmax - t->pmin;
	while (i < len) {
		buff[i] = (unsigned char)source[j + i];
		i++;
	}
	buff[i] = 0;
	return atoi(buff);
}


const static int SLR_RDATA[] = {
1794, 2051, 2051, 2051, 2051, 2049
};

const static unsigned  char  _slrtable[] = {
// STATE 0
0xFF,0x0B,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0x01,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
// STATE 1
0x0A,0xFF,0x02,0x03,0x04,0x06,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
// STATE 2
0xFF,0x0B,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0x09,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
// STATE 3
0xFF,0x0B,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0x08,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
// STATE 4
0xFF,0x0B,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0x05,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
// STATE 5
0xFF,0xFF,0x02,0x03,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
// STATE 6
0xFF,0x0B,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0x07,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
// STATE 7
0xFF,0xFF,0x02,0x03,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
// EXIT 
0xFF,0xFF,0xFF,0xFF,0x05,0x00,0x03,0x04,0x02,0xFF,0x01,0xFF,0xFF,0xFF,0xFF,0xFF
};

#define SLR_EXIT(s)      (_slrtable[(144 - 1) - (s)])
#define SLR_TRANS(s, c)  (_slrtable[((s) * 16) + (c)])

void *test_slrloop(struct rstream *stream, int begin, int exp);
#define test_slrloop            slrloop

#define MAIN_BEGIN               0
#define MAIN_EXP                 7
#define test_main(stream)   (int)(size_t)(slrloop(stream, 0, 7))


#define stream_offset(i)              (stream->head + (i))
#define stream_peek(i)                (rstream_peek(stream, i))
#define stream_junk(n)                (rstream_junk(stream, n))
#define stream_current(t)             (((LEXCHAR *)(stream->lex)->src) + t->pmin)

static void *slrcases(struct rstream *stream, int _q) {
	void *_ret = NULL;
	switch(_q) {

	case 0:
	{
		const struct rstream_tok *T1 = stream_offset(-2);
		const int e = (int)(size_t) T1->value;
		const struct rstream_tok *T2 = stream_offset(-1);
		_ret = (void *)(size_t)(e);
	}
	break;

	case 1:
	{
		const struct rstream_tok *T1 = stream_offset(-3);
		const int e1 = (int)(size_t) T1->value;
		const struct rstream_tok *T2 = stream_offset(-2);
		const struct rstream_tok *T3 = stream_offset(-1);
		const int e2 = (int)(size_t) T3->value;
		_ret = (void *)(size_t)(e1 + e2);
	}
	break;

	case 2:
	{
		const struct rstream_tok *T1 = stream_offset(-3);
		const int e1 = (int)(size_t) T1->value;
		const struct rstream_tok *T2 = stream_offset(-2);
		const struct rstream_tok *T3 = stream_offset(-1);
		const int e2 = (int)(size_t) T3->value;
		_ret = (void *)(size_t)(e1 - e2);
	}
	break;

	case 3:
	{
		const struct rstream_tok *T1 = stream_offset(-3);
		const int e1 = (int)(size_t) T1->value;
		const struct rstream_tok *T2 = stream_offset(-2);
		const struct rstream_tok *T3 = stream_offset(-1);
		const int e2 = (int)(size_t) T3->value;
		_ret = (void *)(size_t)(e1 * e2);
	}
	break;

	case 4:
	{
		const struct rstream_tok *T1 = stream_offset(-3);
		const int e1 = (int)(size_t) T1->value;
		const struct rstream_tok *T2 = stream_offset(-2);
		const struct rstream_tok *T3 = stream_offset(-1);
		const int e2 = (int)(size_t) T3->value;
		_ret = (void *)(size_t)(e1 / e2);
	}
	break;

	case 5:
	{
		const struct rstream_tok *T1 = stream_offset(-1);
		const int n = int_of_string(stream->lex->src, T1);
		_ret = (void *)(size_t)(n);
	}
	break;

	default:
	{
		struct rstream_tok *t = stream_peek(0);
		fprintf(stderr, "UnExpepted: \"");
		for (int i = t->pos.min; i < t->pos.max; i++ )
			fprintf(stderr, "%c", rlex_char(stream->lex, i));
		fprintf(stderr, "\" at %d-%d\n", t->pos.min, t->pos.max);
		exit(-1);

	}
	break;

	}
	return _ret;
}

// public function
void *test_slrloop(struct rstream *stream, int state, int exp)
{
#define NRULES          6
#define NSEGS           8
#define INVALID         255

	struct rstream_tok *t;
	if (state >= 0) {
		t = rstream_reserve(stream);
		t->state = state;
	} else {
		t = stream_offset(-1);
		state = t->state;
	}
	void *value;
	int reduce;
	int q = INVALID;
	while (1) { FirstLoop:
		t = rstream_next(stream);
		state = SLR_TRANS(state, t->term);
		if (state >= NSEGS)
			break;
		t->state = state;
	}
	if (state == INVALID) {
		stream->head -= 1;
		state = stream_offset(-1)->state;
	}
	while (1) {
		q = SLR_EXIT(state);
		if (q >= NRULES) {
			
			return slrcases(stream, q);
		}
		value = slrcases(stream, q);
		reduce = SLR_RDATA[q];
		t = rstream_reduce(stream, reduce & 0xFF);
		t->term = reduce >> 8;
		if (t->term == exp) {
			stream->head -= 2;
			rstream_junk(stream, 2);
			return value;
		}
		t->value = value;
		state = SLR_TRANS(stream_offset(-2)->state, t->term);
		t->state = state;
		if (state < NSEGS)
			goto FirstLoop;
	}
	return slrcases(stream, q);

#undef NRULES
#undef NSEGS
#undef INVALID
#undef goto_cases
}
// undef
#undef stream_offset
#undef stream_peek
#undef stream_junk
#undef stream_current
#undef SLR_EXIT
#undef SLR_TRANS


// auto generated by lex "NAME_init_lexeme"
void test_init_lexeme(struct rlex* lex, LEXCHAR *src, int size);

int main(int argc, char** argv) {
	struct rlex lex;
	struct rstream stream;
	char *text = "3 + 9 * 3 - 1 / 1 + 1";

	// init lexer
	test_init_lexeme(&lex, text, strlen(text));

	// init simple LR parser
	rstream_init(&stream, &lex);

	// PREFIX_main
	const int result = test_main(&stream);
	printf("result : %d === %d\n", result, 3 + 9 * 3 - 1 / 1 + 1);
	return 0;
}
