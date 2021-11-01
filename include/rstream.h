#ifndef r32_stream
#define r32_stream
#include <string.h>
#include "rlex.h"

struct rstream_tok {
	int state;         // for internal of parser tool,
	int term;
	int pmin;
	int pmax;
	union {
		struct {
			int   i4;  // low
			int   i4h; // high
		};
		void      *pval;
		long long i8;
		float     f4;
		double    f8;
	};
};

struct rstream {
	int head;
	int tail;
	struct rlex *lex;
	struct rstream_tok cached[32]; // BEWARE: Here will not detect whether it overflows.
};

#ifdef __cplusplus
extern "C" {
#endif


void rstream_junk(struct rstream *stream, int n);

void rstream_init(struct rstream *stream, struct rlex *lex);

void rstream_unshift(struct rstream *stream, struct rstream_tok* src);

/*
 IMPORTANT:

   The (rstream_tok*) returned by all the following functions is point to an item from the array(stream.cached[N]), 

   So you DONT'T have to do `.free(tok)` for it.

   And its internal value will be changed at any time.
*/

struct rstream_tok* rstream_peek(struct rstream *stream, int i);

struct rstream_tok* rstream_next(struct rstream *stream);

struct rstream_tok* rstream_reduce(struct rstream *stream, int width);


#ifdef __cplusplus
}
#endif

#endif
