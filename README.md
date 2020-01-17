Lex
------

Build lexer and simple parser(LR0) in macro.

## Samples

* [hello world](#Usage)

* [hscript parser](/demo/)

* [css selector](https://github.com/R32/css-selector/blob/master/csss/LRParser.hx)

## Status

LIMIT: you can't use it in [`macro-in-macro`](https://github.com/HaxeFoundation/haxe/pull/7496)

* Lexer: *the most of this code is taken from [LexEngine.nml](https://github.com/HaxeFoundation/neko/blob/master/src/core/LexEngine.nml)*

  - All *finalStates* have been moved out, it not only saves memory and also makes state detection more faster.

* Parser: Only LR(0) is available. (WIP)

  Unlike normal LR parser, there is no *action table*(just *jump table*), So how did it *shift/reduce*?

    1. if you got a valid *state* and if `state < SEGS` then *shift* else *reduce*

    2. if you got a invalid *state* on valid *prevState*, if can be *exit(prevState)* then *reduce(prevState)* else throw an error.

  Some conflicts may be resolved in normal *LALR/LR1*, but since there is no *action table* here, so it will throw an error directly

  - **Position**: Inside the actions, you could use `_t1~_tN` to access the position, which is the instance of `lm.Stream.Tok`

    ```hx
    _t1.pmax - _t1.pmin;
    ```

    And inside the actions, the varialbe `s` is the instance of `lm.Stream`, so you can't use `s` as variable in `case [....]`

    ```hx
    var tok = s.peek(0);
    if (tok.term == SomeToken) s.junk(1);
    ```

  - **Combine Tokens**: Since the Parser can only be used with `enum abstract(Int)`, So there are two ways to combine Tokens

    ```haxe
    // 1. the same prefix(At least 2 characters).
    switch(s) {
    case [e1=expr, Op(t), e2=expr]: switch(t) { case OpPlus: .... }
    }

    // 2. uses "[]"
    switch(s) {
    case [e1=expr, t=[OpPlus, OpMinus], e2=expr]: t == OpPlus ? e1 + e2 : e1 - e2;
    }
    ```

    **NOTE**: if you put tokens together with **different priorities**, you will get a conflict error.

  - You can use string literals instead of simple terminators in stream match.

    ```haxe
    switch(s) {
    case [e1=expr, t=["+", "-"], e2=expr]: t == OpPlus ? e1 + e2 : e1 - e2;
    case ["(", e = expr, ")"]: e;
    }
    ```

  - different types:

    ```haxe
    class Parser implements lm.LR0<MyLexer, Int> { // "Int" indicates that all LHS types default to "Int"
        static var main = switch(s) {
            case [e = expr, Eof]: Std.int(e);
        }
        static var expr:Float = switch(s) {       // Explicit declaration "expr" type is "Float"
            case [e1 = expr, "+", e2 = expr]: e1 + e2;
            case [CFloat(f)]: f;
        }
    }
    ```

  - **Operator Precedence**:

    ```haxe
    // the operator precedence definitions:
    @:rule({
        left: ["+", "-"],         // The parser could auto reflect(str) => Token
        left: [OpTimes, OpDiv],   // The lower have higher Priority.
        nonassoc: [UMINUS],       // All characters of the placeholder must be uppercase
    }) class MyParser implements lm.LR0<MyLexer,...
    // Different from the normal LR parser, the behavior of "nonassoc" is same as "left". Since
    // this parser is not very necessary to refer the operator precedence definitions.
    ```

<pre><details>
    <summary>details...</summary><p>
    ```
    // Refer to the following stream matching cases: UPPERCASE is "non-terml" and LOWERCASE is "terml"</span>
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
</p></details></pre>


### CHANGES

* `0.9.2`: Some Improvements.
* `0.9.1`:
  - [lexer] Fixed `null => Action`
  - [lexer] Allow `lm.Lexer<Void>`. [More...](test/subs/LexVoid.hx)
* `0.9.0`: Simplify
  - [parser] use "%start" instead of ~~`@:side`~~
  - [lexer] Added `null => Action` when there is no match

### Defines

<pre><details><summary>minor...</summary><p>

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

</p></details></pre>

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

copy from [test/subs/Demo.hx](test/subs/Demo.hx)

```hx
@:analyzer(no_optimize)
class Demo {
    static function main() {
        var str = '1 - 2 * (3 + 4) + 5 * 6';
        var lex = new Lexer(lms.ByteData.ofString(str));
        var par = new Parser(lex);
        trace(par.main() == (1 - 2 * (3 + 4) + 5 * 6));
    }
}

// The lm.LR0 Parser only works with "enum abstract (Int) to Int"
enum abstract Token(Int) to Int {
    var Eof = 0;
    var CInt;
    var OpPlus;
    var OpMinus;
    var OpTimes;
    var OpDiv;
    var LParen;
    var RParen;
}

/**
* @:rule(EOF, cmax = 255) See the example below:
*   Eof is a custom terminator which is defined in "<Token>" (required)
*   127 is the custom maximum char value. (optional, default is 255)
*/
@:rule(Eof, 127) class Lexer implements lm.Lexer<Token> {
    static var r_zero = "0";             // static variable will be treated as rules if there is no `@:skip`
    static var r_int = "[1-9][0-9]*";
    static var tok =  [                  // a rule set definition, the first definition will become .token()
        "[ \t]+" => lex.token(),         // "lex" is an instance of this class.
        r_zero + "|" + r_int => CInt,    //
        "+" => OpPlus,
        "-" => OpMinus,
        "*" => OpTimes,
        "/" => OpDiv,
        "(" => LParen,
        ")" => RParen,
    ];
}

@:rule({
    start: [main],            // Specify start, like the "%start" in ocamlyacc, If not specified, the first "switch" will be selected
    left: ["+", "-"],         // The parser could auto reflect(str) => Token
    left: [OpTimes, OpDiv],   // The lower have higher priority.
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

    // Define custom extract function for CInt(n)
    @:rule(CInt) static inline function int_of_string(s: String):Int return Std.parseInt(s);
    // if the custom function has 2 params then the type of the second argument is :lm.Stream.Tok<AUTO>.
    // @:rule(CInt) static inline function int_of_string(input:lms.ByteData, t):Int {
    //    return Std.parseInt( input.readString(t.pmin, t.pmax - t.pmin) );
    //}
}

```

compile:

```bash
haxe -dce full -D analyzer-optimize -lib lex -main Demo -js demo.js
```

<br />

Generated JS:

```js
// Generated by Haxe 4.1.0-rc.1
(function ($global) { "use strict";
var Demo = function() { };
Demo.main = function() {
    var str = "1 - 2 * (3 + 4) + 5 * 6";
    var lex = new Lexer(str);
    var par = new Parser(lex);
    console.log("src/Demo.hx:9:",Parser._entry(par.stream,0,8) == 17);
};
var Lexer = function(s) {
    this.input = s;
    this.pmin = 0;
    this.pmax = 0;
};
Lexer.cases = function(s,lex) {
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
    default:
        throw new Error("UnMatached: '" + lex.input.substr(lex.pmax,lex.pmin - lex.pmax) + "'" + lm_Utils.posString(lex.pmax,lex.input));
    }
};
Lexer.prototype = {
    getString: function(p,len) {
        return this.input.substr(p,len);
    }
    ,_token: function(init,right) {
        if(this.pmax >= right) {
            return 0;
        }
        var i = this.pmax;
        var state = init;
        var prev = init;
        while(i < right) {
            var c = this.input.charCodeAt(i++);
            state = Lexer.raw.charCodeAt(128 * state + c);
            if(state >= 3) {
                break;
            }
            prev = state;
        }
        this.pmin = i;
        if(state == 255) {
            state = prev;
            --i;
        }
        var q = Lexer.raw.charCodeAt(399 - state);
        if(i > this.pmax && q < 8) {
            this.pmin = this.pmax;
            this.pmax = i;
        } else {
            q = Lexer.raw.charCodeAt(399 - init);
        }
        return Lexer.cases(q,this);
    }
    ,token: function() {
        return this._token(0,this.input.length);
    }
};
var Parser = function(lex) {
    this.stream = new lm_Stream(lex);
};
Parser._entry = function(stream,state,exp) {
    var t = stream.newTok(0,0,0);
    t.state = state;
    var i = stream.right;
    while(--i >= stream.pos) stream.cached[i + 1] = stream.cached[i];
    stream.cached[stream.pos] = t;
    ++stream.pos;
    ++stream.right;
    var prev = state;
    var dx = 0;
    while(true) {
        while(true) {
            t = stream.next();
            state = Parser.raw.charCodeAt(16 * prev + t.term);
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
        var q = Parser.raw.charCodeAt(159 - state);
        if(q < 7) {
            stream.pos -= dx;
        } else {
            break;
        }
        dx = 0;
        while(true) {
            var value = Parser.cases(q,stream);
            t = stream.reduce(Parser.lvs[q]);
            if(t.term == exp) {
                stream.pos -= 2;
                stream.junk(2);
                return value;
            }
            t.val = value;
            t.state = Parser.raw.charCodeAt(16 * stream.cached[stream.pos + (-2)].state + t.term);
            prev = t.state;
            if(prev < 9) {
                break;
            }
            q = Parser.raw.charCodeAt(159 - prev);
        }
    }
    return Parser.cases(7,stream);
};
Parser.cases = function(q,s) {
    switch(q) {
    case 0:
        return s.cached[s.pos + (-2)].val;
    case 1:
        var e1 = s.cached[s.pos + (-3)].val;
        var e2 = s.cached[s.pos + (-1)].val;
        if(s.cached[s.pos + (-2)].term == 2) {
            return e1 + e2;
        } else {
            return e1 - e2;
        }
        break;
    case 2:
        return s.cached[s.pos + (-3)].val * s.cached[s.pos + (-1)].val;
    case 3:
        return s.cached[s.pos + (-3)].val / s.cached[s.pos + (-1)].val | 0;
    case 4:
        return s.cached[s.pos + (-2)].val;
    case 5:
        return -s.cached[s.pos + (-1)].val;
    case 6:
        return Std.parseInt(s.stri(-1));
    default:
        var t = s.cached[s.pos + (-1)];
        throw s.error("Unexpected \"" + (t.term != 0 ? s.lex.getString(t.pmin,t.pmax - t.pmin) : "Eof") + "\"",t);
    }
};
var Std = function() { };
Std.parseInt = function(x) {
    if(x != null) {
        var _g = 0;
        var _g1 = x.length;
        while(_g < _g1) {
            var i = _g++;
            var c = x.charCodeAt(i);
            if(c <= 8 || c >= 14 && c != 32 && c != 45) {
                var nc = x.charCodeAt(i + 1);
                var v = parseInt(x,nc == 120 || nc == 88 ? 16 : 10);
                if(isNaN(v)) {
                    return null;
                } else {
                    return v;
                }
            }
        }
    }
    return null;
};
var lm_Tok = function(t,min,max) {
    this.term = t;
    this.pmin = min;
    this.pmax = max;
};
var lm_Stream = function(l) {
    this.lex = l;
    this.cached = new Array(128);
    this.cached[0] = new lm_Tok(0,0,0);
    this.right = 1;
    this.pos = 1;
};
lm_Stream.prototype = {
    recycle: function(tok) {
        tok.nxt = this.cached[0].nxt;
        this.cached[0].nxt = tok;
    }
    ,newTok: function(term,min,max) {
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
        if(n <= 0) {
            return;
        }
        if(this.right - this.pos >= n) {
            var i = n;
            while(i-- > 0) this.recycle(this.cached[this.pos + i]);
            i = this.pos;
            this.right -= n;
            while(i < this.right) {
                this.cached[i] = this.cached[i + n];
                ++i;
            }
        } else {
            n -= this.right - this.pos;
            while(n-- > 0) this.lex.token();
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
            this.cached[this.right++] = this.newTok(t,this.lex.pmin,this.lex.pmax);
        }
        return this.cached[this.pos++];
    }
    ,reduce: function(lvw) {
        var w = lvw & 255;
        if(w == 0) {
            return this.reduceEP(lvw >>> 8);
        }
        var pmax = this.cached[this.pos + (-1)].pmax;
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
        var t = this.newTok(lv,prev.pmax,prev.pmax);
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
    var char = 1;
    var i = 0;
    while(i < pmin) if(input.charCodeAt(i++) == 10) {
        char = 1;
        ++line;
    } else {
        ++char;
    }
    return " at line: " + line + ", column: " + char;
};
Lexer.raw = "ÿÿÿÿÿÿÿÿÿ\x01ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ\x01ÿÿÿÿÿÿÿ\t\x08\x07\x06ÿ\x05ÿ\x04\x03\x02\x02\x02\x02\x02\x02\x02\x02\x02ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ\x01ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ\x01ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ\x06\x07\x04\x02\x03\x05\x01\x01\x00ÿ";
Parser.raw = "ÿ\x0Eÿ\x01ÿÿ\x02ÿÿ\x08ÿÿÿÿÿÿÿ\x0Eÿ\x01ÿÿ\x02ÿÿ\nÿÿÿÿÿÿÿ\x0Eÿ\x01ÿÿ\x02ÿÿ\x03ÿÿÿÿÿÿÿÿ\x04\x04\x06\x07ÿ\x0Bÿÿÿÿÿÿÿÿÿ\x0Eÿ\x01ÿÿ\x02ÿÿ\x05ÿÿÿÿÿÿÿÿÿÿ\x06\x07ÿÿÿÿÿÿÿÿÿÿÿ\x0Eÿ\x01ÿÿ\x02ÿÿ\rÿÿÿÿÿÿÿ\x0Eÿ\x01ÿÿ\x02ÿÿ\x0Cÿÿÿÿÿÿ\tÿ\x04\x04\x06\x07ÿÿÿÿÿÿÿÿÿÿÿ\x06\x02\x03\x04\x05\x00ÿÿÿ\x01ÿÿÿÿÿ";
Parser.lvs = [2050,2307,2307,2307,2307,2306,2305];
Demo.main();
})({});
```
