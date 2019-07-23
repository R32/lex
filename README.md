Lex
------

Build lexer and simple parser(LR0) in macro.

## example

* [hscript parser(*As an example only*)](/demo/)

* [css selector](https://github.com/R32/css-selector/blob/master/csss/LRParser.hx)

## status

NOTE: you can't use it in macro since the limit of [`macro-in-macro`](https://github.com/HaxeFoundation/haxe/pull/7496)

* Lexer: *the most of this code is taken from [LexEngine.nml](https://github.com/HaxeFoundation/neko/blob/master/src/core/LexEngine.nml). But the difference is:*

  All *finalStates* have been moved out for save memory/bytes,

* Parser: Only LR(0) is available. (WIP)

  Unlike normal LR parser, there is no *action table*(just *jump table*), So how did it *shift/reduce*?

    1. if you got a valid *state* and if `state < SEGS` then *shift* else *reduce*

    2. if you got a invalid *state* on valid *prevState*, if can be *exit(prevState)* then *reduce(prevState)* else throw an error.

  Since there is no *action table*, so some conflicts that can be resolved in normal *LALR/LR1* but here an error will be thrown directly.

  - Operator Precedence:

    ```haxe
    // the operator precedence definitions:
    @:rule({
        left: ["+", "-"],         // The parser could auto reflect(str) => Token
        left: [OpTimes, OpDiv],   // The lower have higher Priority.
        nonassoc: [UMINUS],       // All characters of the placeholder must be uppercase
    }) class MyParser implements lm.LR0<MyLexer,...

    // Different from the normal LR parser, the behavior of "nonassoc" is same as "left". Since
    // this parser is not very necessary to refer the operator precedence definitions.

    // Refer to the following stream matching cases: UPPERCASE is "non-terml" and LOWERCASE is "terml"
    [..., op, E]: if defined(op)     then case.left.lval = E.value // the left type is OpLeft
    [..., op, E]: if not defined(op) then case.left = null
    [..., T, E] | [E] | [..., t]:    then case.left = null
    [E, op, ...]: if defined(op)     then case.right.own = E.value // the right type is OpRight
    [E, op, ...]: if not defined(op) then case.right.prio = -1
    [E, T, ...]                      then case.right.prio = -1
    [E]:                             then case.right.prio = -2
    [t, ...]                         then case.right = null

    // when calculating closure(Array<NFA>):
    [..., E]: if E at the and of case, then will according the case.left and .rights to do some mix.

    // you can use @:prec(FOLLOW) to specify both the value of OpLeft and OpRight(If there are not null). e.g:
    [@:prec(UMINUS) "-", e = expr]:   // only case.left.prio & .type = UMINUS, since case.right is null

    // for a string: "var a:Array<Int>=[]", the close token ">" will be parsed as ">=" by Lexer. so:
    [@:prec(">=")   e1 = expr, ">", "=", e2 = expr]: if (_t2.pmax == _t3.pmin) ...
    ```

  - **Position**: Inside the actions, you could use `_t1~_tN` to access the position, which is the instance of `lm.Stream.Tok`

    ```hx
    _t1.pmax - _t1.pmin;
    ```

    And also inside the actions, the varialbe `s` is the instance of `lm.Stream`, so you can't use `s` as variable in `case [....]`

    ```hx
    var tok = s.peek(0);
    if (tok.term == SomeToken) s.junk(1);
    ```

  - **Combine Tokens**: Since the Parser can only be used with `enum abstract(Int)`, So there are two ways to combine Tokens

    ```haxe
    // 1. same prefix(At least 2 characters).
    switch(s) {
    case [e1=expr, Op(t), e2=expr]: switch(t) { case OpPlus: .... }
    }

    // 2. []
    switch(s) {
    case [e1=expr, t=[OpPlus, OpMinus], e2=expr]: t == OpPlus ? e1 + e2 : e1 - e2;
    }
    ```

    NOTE: if you put tokens together with **different priorities**, you will get a conflict error.

  - **Terml Reflect**: You can use string literals instead of simple terminators in stream match.

    ```haxe
    switch(s) {
    case [e1=expr, t=["+", "-"], e2=expr]: t == OpPlus ? e1 + e2 : e1 - e2;
    case ["(", e = expr, ")"]: e;
    }
    ```

  - **different LHS types**: When the LHS type cannot be unified then the `Dynamic` is used as the type of `Stream.Tok`

    ```haxe
    class Parser implements lm.LR0<Lexer, Int> {  // "Int" indicates that all LHS types default to "Int"
        static var main = switch(s) {
            case [e = expr, Eof]: Std.int(e);
        }
        static var expr:Float = switch(s) {       // Explicit declaration "expr" type is "Float"
            case [e1 = expr, "+", e2 = expr]: e1 + e2;
            case [CFloat(f)]: f;
        }

        // extract CFloat(f) => float
        @:rule(CFloat) static inline function float_of_string(s: String):Float return Std.parseFloat(s);
    }
    ```

### CHANGES

* `x.x.x`: Simplify
  - [parser] use "%start" instead of ~~`@:side`~~
  - [lexer] Added `null => Action` when there is no match
* `0.8.0`: Improvements
* `0.7.0`: Removed unstable & useless code.
* `0.6.0`:
  - Added `Terml Reflect`
  - Allow different LHS types
  - Reimplemented Operator Precedence
  - Removed useless Guard.
* `0.5.0`: ~~Added `@:side`(ReImplement LR0 Parser)~~
* `0.4.0`: ~~Independent LHS~~
* `0.3.0`: Automatically grows to 16 bits when *number of States* exceeds 8bit.
* `0.2.0`: Operator Precedence
* `0.1.x`: init

### Defines

* `-D lex_charmax`: to simply handle for utf16 char, Because the State Transition Table is 8-bit

  ```hx
  // source code from lm.LexBuilder
  var c = input.readByte(i++);
  #if lex_charmax
  if (c > CMAX) c = CMAX;
  #end
  state = trans(state, c);
  ```
* `-D lex_rawtable or -D lex_strtable`: use `Bytes`(*No encoding specified*) or `String` as table format.

  By default, `String` format is used for **JS**, other platforms use `Bytes` format.

* `-D lex_rawinput`: then force use `Bytes` as the input format, default is `String`. see `lms.ByteData`

  actually you can use `--remap <package:target>` to override `lms.*`.

* `-D lex_lr0table`: for debug. it will generate a LR0 table save as `lr0-table.txt`. for example:

  > You may need to modify the `mmap` field in `debug.Print`

  ```
  Production:
    (R0)  MAIN --> EXPR $
    (R1)  EXPR --> EXPR [+ -] EXPR
    (R2)       --> EXPR * EXPR
    (R3)       --> EXPR / EXPR
    (R4)       --> ( EXPR )
    (R5)       --> - EXPR
    (R6)       --> int
  -----------------------------------------------------------------------------------------
  |  (S)  | (EP)  |   $   |  int  |   +   |   -   |   *   |   /   |   (   |   )   | EXPR  |
  ----------------------------------------------------------------------------------------- MAIN
  |   0   | NULL  |       |  14   |       |   1   |       |       |   2   |       |   8   |
  -----------------------------------------------------------------------------------------
  |   1   | NULL  |       |  14   |       |   1   |       |       |   2   |       |  10   |
  -----------------------------------------------------------------------------------------
  |   2   | NULL  |       |  14   |       |   1   |       |       |   2   |       |   3   |
  -----------------------------------------------------------------------------------------
  |   3   | NULL  |       |       |   4   |   4   |   6   |   7   |       |  11   |       |
  -----------------------------------------------------------------------------------------
  |   4   | NULL  |       |  14   |       |   1   |       |       |   2   |       |   5   |
  -----------------------------------------------------------------------------------------
  |   5   |  R1   |       |       |       |       |   6   |   7   |       |       |       |
  -----------------------------------------------------------------------------------------
  |   6   | NULL  |       |  14   |       |   1   |       |       |   2   |       |  13   |
  -----------------------------------------------------------------------------------------
  |   7   | NULL  |       |  14   |       |   1   |       |       |   2   |       |  12   |
  -----------------------------------------------------------------------------------------
  |   8   | NULL  |   9   |       |   4   |   4   |   6   |   7   |       |       |       |
  -----------------------------------------------------------------------------------------
  -----------------
  |   9   |  R0   |
  -----------------
  |  10   |  R5   |
  -----------------
  |  11   |  R4   |
  -----------------
  |  12   |  R3   |
  -----------------
  |  13   |  R2   |
  -----------------
  |  14   |  R6   |
  -----------------
  ```

## Usage

copy from [test/subs/Demo.hx](test/subs//Demo.hx)

```hx
package;

class Demo {
    static function main() {
        var str = '1 - 2 * (3 + 4) + 5 * 6';
        var lex = new Lexer(lms.ByteData.ofString(str));
        var par = new Parser(lex);
        trace(par.main() == (1 - 2 * (3 + 4) + 5 * 6));
    }
}

// NOTICE: the lm.LR0 only works with "enum abstract (Int) to Int"
enum abstract Token(Int) to Int {
    var Eof = 0;
    var CInt;
    var OpPlus;
    var OpMinus;
    var OpTimes;
    var OpDiv;
    var LParen;
    var RParen;
    var CString;
}

/**
* @:rule(EOF, cmax = 255)
*   Eof is a custom terminator. (required)
*   127 is the char max value.  (optional, default is 255)
*
* and all the `static var X = "string"` will be treated as rules if no `@:skip`
*/
@:rule(Eof, 127) class Lexer implements lm.Lexer<Token> {
    static var r_zero = "0";             // a pattern can be used in rule sets if there is no @:skip
    static var r_int = "[1-9][0-9]*";
    static var tok =  [                  // a rule set definition
        "[ \t]+" => lex.token(),         // and the "lex" is an instance of this class.
        r_zero + "|" + r_int => CInt,    //
        "+" => OpPlus,
        "-" => OpMinus,
        "*" => OpTimes,
        "/" => OpDiv,
        "(" => LParen,
        ")" => RParen,
        '"' => {
            var pmin = lex.pmin;
            var t = lex.str(); // maybe Eof.
            lex.pmin = pmin;   // punion
            t;
        }
    ];

    static var str = [
        '\\\\"' => lex.str(),
        '[^\\\\"]+' => lex.str(),
        '"' => CString,       // do escape in Parser @:rule(CString)
    ];
}

@:rule({
    start: [main],            // Specify start, like the "%start" in ocamlyacc, If not specified, the first "switch" will be selected
    left: ["+", "-"],         // The parser could auto reflect(str) => Token
    left: [OpTimes, OpDiv],   // The lower have higher Priority.
    nonassoc: [UMINUS],       // All characters of the placeholder must be uppercase
}) class Parser implements lm.LR0<Lexer, Int> {

    static var main = switch(s) {  // the "s" is instance of lm.Stream
        case [e = expr, Eof]: e;
    }

    static var expr = switch(s) {
        case [e1 = expr, op = [OpPlus,OpMinus], e2 = expr]: op == OpPlus ? e1 + e2 : e1 - e2;
        case [e1 = expr, OpTimes, e2 = expr]: e1 * e2;
        case [e1 = expr, OpDiv, e2 = expr]: Std.int(e1 / e2);
        case [LParen, e = expr, RParen]: e;
        case [@:prec(UMINUS) OpMinus, e = expr]: -e;   // %prec UMINUS
        case [CInt(n)]: n;
    }

    // for extract n from CInt(n)
    @:rule(CInt) static inline function int_of_string(s: String):Int return Std.parseInt(s);

    // if the @:rule function has 2 params then the type of the second argument is :lm.Stream.Tok<AUTO>.
    // Note: This function does not handle escape
    @:rule(CString) static function unescape(input: lms.ByteData, t):String {
        return input.readString(t.pmin + 1, t.pmax - t.pmin - 2);
    }
}
```

compile:

```bash
# NOTE: "-D nodejs" is used to remove js.compat.TypedArray
haxe -dce full -D analyzer-optimize -D nodejs -lib lex -main Demo -js demo.js
```

<br />

#### js output

```js
// Generated by Haxe 4.0.0-rc.2+8d5baab66
(function ($global) { "use strict";
var Demo = function() { };
Demo.main = function() {
    console.log("Demo.hx:8:",_$Demo_Parser._entry(new _$Demo_Parser(new _$Demo_Lexer("1 - 2 * (3 + 4) + 5 * 6")).stream,0,9) == 17);
};
var _$Demo_Lexer = function(s) {
    this.input = s;
    this.pmin = 0;
    this.pmax = 0;
};
_$Demo_Lexer.cases = function(s,lex) {
    switch(s) {
    case 0:
        return lex._token(0,lex.input.length);
    case 1:
        return 1;
    case 2:
        return 2;
    case 3:
        return 3;
    case 4:
        return 4;
    case 5:
        return 5;
    case 6:
        return 6;
    case 7:
        return 7;
    case 8:
        var pmin = lex.pmin;
        var t = lex._token(3,lex.input.length);
        lex.pmin = pmin;
        return t;
    case 9:
        return lex._token(3,lex.input.length);
    case 10:
        return lex._token(3,lex.input.length);
    default:
        return 8;
    }
};
_$Demo_Lexer.prototype = {
    getString: function(p,len) {
        return this.input.substr(p,len);
    }
    ,_token: function(state,right) {
        var i = this.pmax;
        this.pmin = i;
        if(i >= right) {
            return 0;
        }
        var prev = state;
        while(i < right) {
            var c = this.input.charCodeAt(i++);
            state = _$Demo_Lexer.raw.charCodeAt(128 * state + c);
            if(state >= 6) {
                break;
            }
            prev = state;
        }
        if(state == 255) {
            state = prev;
            prev = 1;
        } else {
            prev = 0;
        }
        var q = _$Demo_Lexer.raw.charCodeAt(783 - state);
        if(q < 12) {
            this.pmax = i - prev;
        } else {
            throw new Error("UnMatached: " + this.input.substr(this.pmin,i - this.pmin - prev) + lm_Utils.posString(this.pmin,this.input));
        }
        return _$Demo_Lexer.cases(q,this);
    }
    ,token: function() {
        return this._token(0,this.input.length);
    }
};
var _$Demo_Parser = function(lex) {
    this.stream = new lm_Stream(lex,0);
};
_$Demo_Parser._entry = function(stream,state,exp) {
    var prev = state;
    var t = null;
    var dx = 0;
    while(true) {
        while(true) {
            t = stream.next();
            state = _$Demo_Parser.raw.charCodeAt(16 * prev + t.term);
            t.state = state;
            if(state >= 9) {
                break;
            }
            prev = state;
        }
        if(state == 255) {
            state = prev;
            dx = 1;
        }
        var q = _$Demo_Parser.raw.charCodeAt(159 - state);
        if(q < 7) {
            stream.pos -= dx;
        } else {
            break;
        }
        dx = 0;
        while(true) {
            var value = _$Demo_Parser.cases(q,stream);
            t = stream.reduce(_$Demo_Parser.lvs[q]);
            if(t.term == exp) {
                --stream.pos;
                stream.junk(1);
                return value;
            }
            t.val = value;
            t.state = _$Demo_Parser.raw.charCodeAt(16 * stream.cached[stream.pos + -2].state + t.term);
            prev = t.state;
            if(prev < 9) {
                break;
            }
            q = _$Demo_Parser.raw.charCodeAt(159 - prev);
        }
    }
    t = stream.cached[stream.pos + -1];
    throw stream.error("Unexpected \"" + (t.term != 0 ? stream.lex.getString(t.pmin,t.pmax - t.pmin) : "Eof") + "\"",t);
};
_$Demo_Parser.cases = function(q,s) {
    switch(q) {
    case 0:
        return s.cached[s.pos + -2].val;
    case 1:
        var e1 = s.cached[s.pos + -3].val;
        var e2 = s.cached[s.pos + -1].val;
        if(s.cached[s.pos + -2].term == 2) {
            return e1 + e2;
        } else {
            return e1 - e2;
        }
        break;
    case 2:
        return s.cached[s.pos + -3].val * s.cached[s.pos + -1].val;
    case 3:
        return s.cached[s.pos + -3].val / s.cached[s.pos + -1].val | 0;
    case 4:
        return s.cached[s.pos + -2].val;
    case 5:
        return -s.cached[s.pos + -1].val;
    default:
        return Std.parseInt(s.stri(-1));
    }
};
var Std = function() { };
Std.parseInt = function(x) {
    var v = parseInt(x, x && x[0]=="0" && (x[1]=="x" || x[1]=="X") ? 16 : 10);
    if(isNaN(v)) {
        return null;
    }
    return v;
};
var lm_Tok = function(t,min,max) {
    this.term = t;
    this.pmin = min;
    this.pmax = max;
};
var lm_Stream = function(l,s) {
    this.lex = l;
    this.cached = new Array(128);
    this.cached[0] = new lm_Tok(0,0,0);
    this.cached[0].state = s;
    this.right = 1;
    this.pos = 1;
};
lm_Stream.prototype = {
    recycle: function(tok) {
        tok.nxt = this.cached[0].nxt;
        this.cached[0].nxt = tok;
    }
    ,reqTok: function(term,min,max) {
        if(this.cached[0].nxt == null) {
            return new lm_Tok(term,min,max);
        } else {
            var t = this.cached[0].nxt;
            this.cached[0].nxt = t.nxt;
            t.nxt = null;
            t.term = term;
            t.pmin = min;
            t.pmax = max;
            return t;
        }
    }
    ,junk: function(n) {
        if(n > 0 && this.right - this.pos >= n) {
            var i = n;
            while(i-- > 0) this.recycle(this.cached[this.pos + i]);
            i = this.pos;
            this.right -= n;
            while(i < this.right) {
                this.cached[i] = this.cached[i + n];
                ++i;
            }
        } else {
            if(n > 0) {
                n -= this.right - this.pos;
                while(n-- > 0) this.lex.token();
            }
            while(this.right > this.pos) this.recycle(this.cached[--this.right]);
        }
    }
    ,stri: function(dx) {
        var t = this.cached[this.pos + dx];
        return this.lex.getString(t.pmin,t.pmax - t.pmin);
    }
    ,error: function(msg,t) {
        return new Error(msg + lm_Utils.posString(t.pmin,this.lex.input));
    }
    ,next: function() {
        if(this.right == this.pos) {
            var t = this.lex.token();
            this.cached[this.right++] = this.reqTok(t,this.lex.pmin,this.lex.pmax);
        }
        return this.cached[this.pos++];
    }
    ,reduce: function(lvw) {
        var w = lvw & 255;
        if(w == 0) {
            return this.reduceEP(lvw >>> 8);
        }
        var pmax = this.cached[this.pos + -1].pmax;
        this.pos -= w;
        var t = this.cached[this.pos];
        t.term = lvw >>> 8;
        t.pmax = pmax;
        ++this.pos;
        var i = --w;
        while(i-- > 0) this.recycle(this.cached[this.pos + i]);
        this.right -= w;
        i = this.pos;
        while(i < this.right) {
            this.cached[i] = this.cached[i + w];
            ++i;
        }
        return t;
    }
    ,reduceEP: function(lv) {
        var prev = this.cached[this.pos - 1];
        var t = this.reqTok(lv,prev.pmax,prev.pmax);
        var i = this.right;
        while(--i >= this.pos) this.cached[i + 1] = this.cached[i];
        this.cached[this.pos] = t;
        ++this.pos;
        ++this.right;
        return t;
    }
};
var lm_Utils = function() { };
lm_Utils.posString = function(pmin,input) {
    var line = 1;
    var char = 0;
    var i = 0;
    while(i < pmin) if(input.charCodeAt(i++) == 10) {
        char = 0;
        ++line;
    } else {
        ++char;
    }
    return " at line: " + line + ", char: " + char;
};
_$Demo_Lexer.raw = "ÿÿÿÿÿÿÿÿÿ\x01ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ\x01ÿ\x0Fÿÿÿÿÿ\x0E\r\x0C\x0Bÿ\nÿ\t\x08\x02\x02\x02\x02\x02\x02\x02\x02\x02ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ\x01ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ\x01ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x07\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x05\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04ÿ\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04ÿ\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04\x04ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ\x06ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ\x08\x06\x07\x04\x02\x03\x05\x01\x0B\tÿ\nÿ\x01\x00ÿ";
_$Demo_Parser.raw = "ÿ\x0Eÿ\x01ÿÿ\x02ÿÿÿ\x08ÿÿÿÿÿÿ\x0Eÿ\x01ÿÿ\x02ÿÿÿ\nÿÿÿÿÿÿ\x0Eÿ\x01ÿÿ\x02ÿÿÿ\x03ÿÿÿÿÿÿÿ\x04\x04\x06\x07ÿ\x0Bÿÿÿÿÿÿÿÿÿ\x0Eÿ\x01ÿÿ\x02ÿÿÿ\x05ÿÿÿÿÿÿÿÿÿ\x06\x07ÿÿÿÿÿÿÿÿÿÿÿ\x0Eÿ\x01ÿÿ\x02ÿÿÿ\rÿÿÿÿÿÿ\x0Eÿ\x01ÿÿ\x02ÿÿÿ\x0Cÿÿÿÿÿ\tÿ\x04\x04\x06\x07ÿÿÿÿÿÿÿÿÿÿÿ\x06\x02\x03\x04\x05\x00ÿÿÿ\x01ÿÿÿÿÿ";
_$Demo_Parser.lvs = [2306,2563,2563,2563,2563,2562,2561];
Demo.main();
})({});
```
