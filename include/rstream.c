/*
 * SPDX-License-Identifier: GPL-2.0
 */

#include "rstream.h"

#define stream_offset(rs, i)   ((rs)->head + (i))
#define stream_length(rs)      ((int)((rs)->tail - (rs)->head))
#define lexeme_token(rs)       ((rs)->lex->token((rs)->lex))

// peek(0) means peek current token
struct rstream_tok *rstream_peek(struct rstream *stream, int i)
{
	while (stream_length(stream) <= i) {
		stream->tail->term = lexeme_token(stream);
		stream->tail->pos = stream->lex->pos;
		stream->tail++;
	}
	return stream_offset(stream, i);
}

void rstream_junk(struct rstream *stream, int n)
{
	if (n <= 0)
		return;
	int len = stream_length(stream);
	if (len > n) {
		stream->tail -= n;
		len -= n;
		int i = 0;
		struct rstream_tok *const cur = stream_offset(stream, 0);
		while (i < len) {
			cur[i] = cur[i + n]; // struct copy from LEFT to RIGHT
			++i;
		}
	} else {
		stream->tail = stream->head;
	/*
		n -= len;
		while (n--) {
			lexeme_token(stream); // discad
		}
	*/
	}
}

void rstream_init(struct rstream *stream, struct rlex *lex)
{
	stream->lex = lex;
	stream->head = stream->cached;
	stream->tail = stream->cached;
}

// For internal use only
struct rstream_tok *rstream_next(struct rstream *stream)
{
	if (stream->head == stream->tail) { // stream_length(stream) == 0
		stream->tail->term = lexeme_token(stream);
		stream->tail->pos = stream->lex->pos;
		stream->tail++;
	}
	return stream->head++;
}

// Reserve n blocks
static void move(struct rstream *stream, int n)
{
	if (n <= 0)
		return;
	struct rstream_tok *curr = stream_offset(stream, 0);
	int len = stream_length(stream);
	while (len--) {
		curr[len + n] = curr[len]; // struct copy from RIGHT to LEFT
	}
	stream->head += n;
	stream->tail += n;
}

// For internal use only
struct rstream_tok *rstream_reserve(struct rstream *stream)
{
	struct rstream_tok *curr = stream_offset(stream, 0);
	move(stream, 1);
	curr->pos = (struct rlex_position){0, 0};
	return curr;
}

static struct rstream_tok *rstream_reduce_epsilon(struct rstream *stream)
{
	struct rstream_tok *curr = stream_offset(stream, 0);
	int pmax = (curr - 1)->pos.max;
	move(stream, 1);
	curr->pos = (struct rlex_position){pmax, pmax};
	return curr;
}

// For internal use only
struct rstream_tok *rstream_reduce(struct rstream *stream, int width)
{
	if (width == 0)
		return rstream_reduce_epsilon(stream);
	int pmax = stream_offset(stream, -1)->pos.max; // save "pmax" before update stream->head
	width--;                        // reserve 1 block
	stream->head -= width;          // update stream->head
	stream->tail -= width;
	struct rstream_tok *const tok = stream_offset(stream, -1); // related to the reserved block
	tok->pos.max = pmax;
	if (width) {
		// fast junk(width)
		int len = stream_length(stream);
		int i = 0;
		while (i++ < len) {         // i start at "1" because tok is -1
			tok[i] = tok[i + width];
		}
	}
	return tok;
}
