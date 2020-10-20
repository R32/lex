#ifndef r32_lex
#define r32_lex
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
	int min = rlex->pmin;
	enum token tok = qstr();
	if (tok == Eof) {
		printf("UnClosed String: %d-%d",min, lex->pmax);
		exit(-1);
	}
	rlex->pmin = min; // union
	tok
| _ ->
	printf("UnMathed : %c\n", lex->src[lex->pmax]); // if error then pmin >= pmax
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

struct rlex {
	int pmin;
	int pmax;
	int size;  // src size in characters
	int _pad;  // x64 align
	union {
		unsigned char* src;
		unsigned short* wsrc;
	};
	union {
		int (*token)(struct rlex* lex);
		void (*vtoken)(struct rlex* lex);
	};
};

//
#define rlex_end(rlex)       ((rlex)->pmax >= (rlex)->size)

// Call it after executing .token()
#define rlex_error(rlex)     ((rlex)->pmin >= (rlex)->pmax)

// Only works if there is no error
#define rlex_cursize(rlex)   (((rlex)->pmax) - (rlex)->pmin)
#define rlex_current(rlex)   ((rlex)->src + (rlex)->pmin)
#define rlex_wcurrent(rlex)  ((rlex)->wsrc + (rlex)->pmin)


#endif