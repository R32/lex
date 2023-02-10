/*
 * SPDX-License-Identifier: GPL-2.0
 */

#ifndef R_LEX_H
#define R_LEX_H
/*
Syntax:

enum token {
	Eof,
	CInt,
	CString,
};

%%         // lexer starts,
           // NOTE: only line comments are accepted in this area

%EOF(Eof)  // Specify the return value when EOF.
           // If not provided then `void` means no value will be returned

%SRC(UTF8) // Specify the encoding format of the input source. (UTF8|UCS2)
           // If not provided the default is UTF8. (WARINNING: only acill chars even UCS2)

%MAX(127)  // Specify the width of table-state-chunk. (127|255)
           // If not provided the default is 127


let spaces = "[ \t\n]+"

let token = function
| spaces     -> token()
| "[0-9]+"
| "0x[a-fA-F0-9]+" ->
	CInt
| "'"        ->
	int min = rlex->pos.min;
	enum token tok = qstr();
	if (tok == Eof) {
		printf("UnClosed String: %d-%d",min, lex->pos.max);
		exit(-1);
	}
	rlex->pos.min = min; // union
	tok
| _ ->
	printf("UnMathed : %c\n", lex->src[lex->pos.max]); // if error then pmin >= pmax
	exit(-1);
	Eof

let qstr = function
| "\\'"   -> qstr()
| "[^']"  -> qstr()
| "'"     -> CString

let str = function
| '\\"'   -> str()
| '[^"]'  -> str()
| '"'     -> CString

%%  // lex Ends
*/

struct rlex_position {
	int min, max;
};

struct rlex {
	union {
		struct rlex_position pos;
		struct {
			int pmin, pmax;
		};
	};
	int size;  // src size in characters
	int ___x;  // align pad
	void *src; // LEXCHAR
	int (*token)(struct rlex *lex);
};

#define rlex_token(lex)     ((lex)->token(lex))
//
#define rlex_end(lex)       ((lex)->pos.max >= (lex)->size)

// Call it after executing .token()
#define rlex_error(lex)     ((lex)->pos.min >= (lex)->pos.max)

// WARINNING: Make sure rlex_error() is false before calling.
#define rlex_cursize(lex)   ((lex)->pos.max - (lex)->pos.min)

// rlexsrc, rlex_char(lex, i), rlex_current(lex) are now defined in "lex.template"

#define rlex_position_union(p1, p2) \
	((struct rlex_position) { \
		(p1).min < (p2).min ? (p1).min : (p2).min, \
		(p1).max > (p2).max ? (p1).max : (p2).max  \
	})

#define rlex_contiguous(p1, p2) ((p1).max == (p2).min)

#endif
