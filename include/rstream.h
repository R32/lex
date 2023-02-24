/*
 * SPDX-License-Identifier: GPL-2.0
 */

#ifndef R_STREAM_H
#define R_STREAM_H
#include "rlex.h"

struct rstream_tok {
	struct rlex_position pos;
	int state;
	int term;
	union {
		void      *value;
		struct {
			int   i32;
			int   high;
		};
		long long i64;
		float     f32;
		double    f64;
	};
};

struct rstream {
	struct rstream_tok *head;
	struct rstream_tok *tail;
	struct rlex *lex;
	struct rstream_tok cached[64];
};

#ifdef __cplusplus
extern "C" {
#endif

/*
 * The returned (rstream_tok *) will point to an element from rstream.cached[X],
 */
struct rstream_tok *rstream_peek(struct rstream *stream, int i);

void rstream_junk(struct rstream *stream, int n);

void rstream_init(struct rstream *stream, struct rlex *lex);

struct rstream_tok *rstream_reserve(struct rstream *stream);           // For internal

struct rstream_tok *rstream_next(struct rstream *stream);              // For internal

struct rstream_tok *rstream_reduce(struct rstream *stream, int width); // For internal

#ifdef __cplusplus
}
#endif

#endif
