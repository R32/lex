/*
 * SPDX-License-Identifier: GPL-2.0
 */

#ifndef R_LEX_H
#define R_LEX_H

struct rlex_position {
	int min, max;
};

struct rlex {
	struct rlex_position pos;
	int size;  // src size in characters
	int ___x;  // align pad
	void *src; // LEXCHAR
	int (*token)(struct rlex *lex);
};

#define rlex_token(lex)     ((lex)->token(lex))

#define rlex_end(lex)       ((lex)->pos.max >= (lex)->size)

// if error
#define rlex_error(lex)     ((lex)->pos.min >= (lex)->pos.max)

// if no error
#define rlex_cursize(lex)   ((lex)->pos.max - (lex)->pos.min)

#define rlex_position_union(p1, p2) \
	((struct rlex_position) { \
		(p1).min < (p2).min ? (p1).min : (p2).min, \
		(p1).max > (p2).max ? (p1).max : (p2).max  \
	})

#define rlex_contiguous(p1, p2) ((p1).max == (p2).min)

#endif
