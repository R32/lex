#include "rstream.h"

#define OFFSET(n)   (&stream->cached[stream->head + (n)])
#define LENGTH()    (stream->tail - stream->head)
#define TOKEN()     (stream->lex->token(stream->lex))

/**
 NOTE: It can only be used manually, because there is no corresponding Parser Tool

 And I haven't tested it, Good Luck :)
*/

// peek(0) means peek current token
struct rstream_tok* rstream_peek(struct rstream *stream, int i) {
	while(LENGTH() <= i) {
		int term = TOKEN();
		struct rstream_tok* tok = &stream->cached[stream->tail++];
		tok->term = term;
		tok->pos = stream->lex->pos;
	}
	return OFFSET(i);
}

void rstream_junk(struct rstream *stream, int n) {
	if (n <= 0)
		return;
	int len = LENGTH();
	if (len > n) {
		stream->tail -= n;
		len -= n;
		int i = 0;
		struct rstream_tok* const cur = OFFSET(0);
		while (i < len) {
			cur[i] = cur[i + n]; // struct copy from LEFT to RIGHT
			++i;
		}
	} else {
		stream->tail = stream->head;
	/*
		n -= len;
		while(n--) {
			TOKEN(); // discad
		}
	*/
	}
}

void rstream_init(struct rstream *stream, struct rlex *lex) {
	stream->lex = lex;
	stream->head = 0;
	stream->tail = 0;
}

struct rstream_tok* rstream_next(struct rstream *stream) {
	struct rstream_tok* tok = OFFSET(0);
	if(stream->head == stream->tail) { // LENGTH() == 0
		stream->tail++;
		tok->term = TOKEN();
		tok->pos = stream->lex->pos;
	}
	stream->head++;
	return tok;
}

// move (tail~head) blocks to right(n)
static void move(struct rstream *stream, int n) {
	int len = LENGTH();
	if (!len || n <= 0)
		return;
	struct rstream_tok *curr = OFFSET(0);
	while(len--) {
		curr[len + n] = curr[len]; // struct copy from RIGHT to LEFT
	}
	stream->head += n;
	stream->tail += n;
}

static struct rstream_tok* rstream_reduceEP(struct rstream *stream) {
	struct rstream_tok *curr = OFFSET(0);
	int pmax = (curr - 1)->pos.max;
	move(stream, 1);
	curr->pos = (struct rlex_position){pmax, pmax};
	return curr;
}

void rstream_unshift(struct rstream *stream, struct rstream_tok* src) {
	struct rstream_tok *dst = OFFSET(0);
	move(stream, 1);
	*dst = *src; // struct copy
}

// you have to update the values of (.term, .state, .pvalue), after returnning.
struct rstream_tok* rstream_reduce(struct rstream *stream, int width) {
	if (width == 0)
		return rstream_reduceEP(stream);
	int pmax = OFFSET(-1)->pos.max; // save "pmax" before update stream->head
	width--;                        // reserve 1 block
	stream->head -= width;          // update stream->head
	stream->tail -= width;
	struct rstream_tok* const tok = OFFSET(-1); // related to the reserved block
	tok->pos.max = pmax;
	if (width) {
		// fast junk(width)
		int len = LENGTH();
		int i = 0;
		while(i++ < len) {       // i start at "1" because tok is -1
			tok[i] = tok[i + width];
		}
	}
	return tok;
}
