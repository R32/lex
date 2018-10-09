Lex
------

Build lightweight lexer/parser(LR0) state transition tables in macro(compile phase).

## status

* [x] Lexer: Does not support unicode(The maximum char is 254)

* [x] Parser: **Rollback-Able LR(0)**. **(WIP)**.

  - [x] Operator Precedence. Only operators in the same position.

    > I don't know how yacc implements it, I used my own way. so there might be bugs
    >
    > In fact, this LR0 parser just modifies the table that LexEngine built. So as parser it's may also be unstable.

  - [x] Independent LHS. *I don't know what it should be called.* in the [Usage](#usage) example:

    ```haxe
    // skip the main entry(par.main()) and call expr() independently
    static function main() {
        var str = '1 - 2 * (3 + 4) + 5 * 6';
        var lex = new Lexer(lms.ByteData.ofString(str));
        var par = new Parser(lex);
        var s = @:privateAccess par.stream;
        trace(@:privateAccess Parser.expr(s) == (1 - 2 * (3 + 4) + 5 * 6) );
        var t = s.peek(0);
        trace(t.term == Eof);
    }
    ```

    In fact you should use it in `actions`, something like:

    ```haxe
    static var main = switch(s) {
        case [e = expr_list, Eof];  // use expr_list instead of expr
    }
    static var expr_list = switch(s) {
        case []:                    // epsilon
            var e = expr(s);        // call LHS expr.
            var t = s.peek(0);
            if (t.term == Eof) {
                e;
            } else if (t.term == Comma) {
                s.junk(1);          // discard Comma
                var e2 = expr(s);
                //......
                e + e2;
            } else {}
    }
    static var expr = switch(s)     // .....
    ```

  - [x] Guard, If the production(rhs) have a "left sub rhs".

    ```haxe
    class Main {
        static function main() {
            var str = 'ab';
            var lex = new Lexer(lms.ByteData.ofString(str));
            var par = new Parser(lex);
            trace(par.main());
        }
    }
    enum abstract Token(Int) to Int {
        var Eof = 0;
        var A;
        var B;
    }
    @:rule(Eof, 127) class Lexer implements lm.Lexer<Token> {
        static var tok =  [
            "a" => A,
            "b" => B,
        ];
    }
    class Parser implements lm.LR0<Lexer, Int> {
        static var main = switch(s) {
            case [e1 = expr, Eof]: e1;
            case [e1 = expr, e2 = expr, Eof]: e1 + e2;
        }
        static var expr = switch(s) {
            case [A, B] if (Math.random() > 0.5):  // if false then rollback to case [A]
                trace("A: " + _t1.pstr() + ", B: " + _t2.pstr());
                303;
            case [A]:
                trace("A: " + _t1.pstr());
                101;
            case [B]:
                trace("B: " + _t1.pstr());
                202;
        }
    }
    ```
  Inside the actions, you could use `_t1~_tN` to access the position.

  ```hx
  _t1.pmax - _t1.pmin;
  _t1.pstr();
  ```

  Since the Parser can only be used with `enum abstract(Int)`, So here are 2 ways to combine Tokens

  ```haxe
  // 1. same prefix(At least 2 characters).
  switch(s) {
  case [e1=expr, Op(t), e2=expr]: switch(t) { case OpPlus: .... }
  }

  // 2.
  switch(s) {
  case [e1=expr, t=[OpPlus, OpMinus], e2=expr]: t == OpPlus ? e1 + e2 : e1 - e2;
  }
  ```
  NOTICE: If you put tokens together with **different priorities**, you will get a conflict error.

### CHANGES

* `0.4.0`: Independent LHS
* `0.3.0`: Automatically grows to 16 bits when *number of States* exceeds 8bit.
* `0.2.0`: Operator Precedence
* `0.1.x`: init

### Defines

It looks very messy here.

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

  actually you can use `--remap <package:target>` to override it.

* `-D lex_lr0table`: for debug. it will generate a LR0 table save as `lr0-table.txt`. for example:

  > You need to modify the `mmap` field in `debug.Print`

  ```
  Production:
    (R0)  MAIN --> EXPR $
    (R1)  EXPR --> EXPR [+-] EXPR
    (R2)       --> EXPR * EXPR
    (R3)       --> EXPR / EXPR
    (R4)       --> ( EXPR )
    (R5)       --> - EXPR
    (R6)       --> int
  -------------------------------------------------------------------------------------------------------------
  |  (S)   |  (RB)  |  (EP)  |   $    |  int   |   +    |   -    |   *    |   /    |   (    |   )    |  EXPR  |
  -------------------------------------------------------------------------------------------------------------
  |   0    |  NULL  |  NULL  |        | R6,S15 |        |   3    |        |        |   4    |        |   1    |
  -------------------------------------------------------------------------------------------------------------
  |   1    |  NULL  |  NULL  | R0,S16 |        |   7    |   7    |   8    |   9    |        |        |        |
  -------------------------------------------------------------------------------------------------------------
  |   2    |  NULL  |  NULL  |        | R6,S15 |        |   3    |        |        |   4    |        |   6    |
  -------------------------------------------------------------------------------------------------------------
  |   3    |  NULL  |  NULL  |        | R6,S15 |        |   3    |        |        |   4    |        | R5,S14 |
  -------------------------------------------------------------------------------------------------------------
  |   4    |  NULL  |  NULL  |        | R6,S15 |        |   3    |        |        |   4    |        |   5    |
  -------------------------------------------------------------------------------------------------------------
  |   5    |  NULL  |  NULL  |        |        |   7    |   7    |   8    |   9    |        | R4,S13 |        |
  -------------------------------------------------------------------------------------------------------------
  |   6    |  NULL  |  NULL  |        |        |   7    |   7    |   8    |   9    |        |        |        |
  -------------------------------------------------------------------------------------------------------------
  |   7    |  NULL  |  NULL  |        | R6,S15 |        |   3    |        |        |   4    |        | R1,S10 |
  -------------------------------------------------------------------------------------------------------------
  |   8    |  NULL  |  NULL  |        | R6,S15 |        |   3    |        |        |   4    |        | R2,S11 |
  -------------------------------------------------------------------------------------------------------------
  |   9    |  NULL  |  NULL  |        | R6,S15 |        |   3    |        |        |   4    |        | R3,S12 |
  -------------------------------------------------------------------------------------------------------------
  -------------------------------------------------------------------------------------------------------------
  |   10   |  NULL  |   R1   |        |        |        |        |   8    |   9    |        |        |        |
  -------------------------------------------------------------------------------------------------------------
  |   11   |  NULL  |
  -------------------
  |   12   |  NULL  |
  -------------------
  |   13   |  NULL  |
  -------------------
  |   14   |  NULL  |
  -------------------
  |   15   |  NULL  |
  -------------------
  |   16   |  NULL  |
  -------------------
  ```

## Usage

copy from [demo/Demo.hx](demo/Demo.hx)

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
    var CStr;
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
    static var r_int = "-?[1-9][0-9]*";
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
        '"' => CStr,          // do escape in Parser @:rule(CStr)
    ];
}

@:rule({
    left: [OpPlus, OpMinus],
    left: [OpTimes, OpDiv],   // the lower have higher Priority.
}) class Parser implements lm.LR0<Lexer, Int> {

    static var main = switch(s) {
        case [e = expr, Eof]: e;
    }

    static var expr = switch(s) {
        case [e1 = expr, op = [OpPlus,OpMinus], e2 = expr]: op == OpPlus ? e1 + e2 : e1 - e2;
        case [e1 = expr, OpTimes, e2 = expr]: e1 * e2;
        case [e1 = expr, OpDiv, e2 = expr]: Std.int(e1 / e2);
        case [LParen, e = expr, RParen]: e;
        case [OpMinus, e = expr]: -e;
        case [CInt(n)]: n;
    }

    // for extract n from CInt(n), NOTICE: If you don't define @:rule(CInt) function, then the "n" type is Token.
    @:rule(CInt) static inline function int_of_string(s: String):Int return Std.parseInt(s);

    // if the @:rule function has 3 params then the macro will auto pass it the following parameters.
    // Note: This function does not handle escape
    @:rule(CStr) static function unescape(input: lms.ByteData, pmin: Int, pmax: Int):String {
        return input.readString(pmin + 1, pmax - pmin - 2); // trim quotes
    }

    // if the @:rule function has 2 params then the type of the second argument is lm.Stream.Tok<AUTO>.
    //@:rule(CStr) static function unescape(input: lms.ByteData, t):String {
    //  return input.readString(t.pmin + 1, t.pmax - t.pmin - 2);
    //}
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
// Generated by Haxe 4.0.0-preview.4+1ba7d2a3c
(function () { "use strict";
var Demo = function() { };
Demo.main = function() {
    console.log("Demo.hx:8:",Parser._entry(new Parser(new Lexer("1 - 2 * (3 + 4) + 5 * 6")).stream,0,9) == 17);
};
var lm_Lexer = function() { };
var Lexer = function(s) {
    this.input = s;
    this.pmin = 0;
    this.pmax = 0;
};
Lexer.cases = function(s,lex) {
    switch(s) {
    case 0:
        return lex._token(0);
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
        var t = lex._token(4);
        lex.pmin = pmin;
        return t;
    case 9:
        return lex._token(4);
    case 10:
        return lex._token(4);
    default:
        return 8;
    }
};
Lexer.prototype = {
    getString: function(p,len) {
        return this.input.substr(p,len);
    }
    ,_token: function(state) {
        var i = this.pmax;
        var len = this.input.length;
        this.pmin = i;
        if(i >= len) {
            return 0;
        }
        var prev = state;
        while(i < len) {
            var c = this.input.charCodeAt(i++);
            state = Lexer.raw.charCodeAt(128 * state + c);
            if(state >= 7) {
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
        var q = Lexer.raw.charCodeAt(943 - state);
        if(q < 12) {
            this.pmax = i - prev;
        } else {
            q = Lexer.raw.charCodeAt(state + 896);
            if(q < 12) {
                this.pmax = i - prev - Lexer.raw.charCodeAt(state + 912);
            } else {
                throw new Error("UnMatached: " + this.pmin + "-" + this.pmax + ": \"" + this.input.substr(this.pmin,i - this.pmin) + "\"");
            }
        }
        return Lexer.cases(q,this);
    }
    ,token: function() {
        return this._token(0);
    }
};
var Parser = function(lex) {
    this.stream = new lm_Stream(lex,0);
};
Parser._entry = function(stream,state,exp) {
    var prev = state;
    var t = null;
    var dx = 0;
    var keep = stream.pos;
    while(true) {
        while(true) {
            t = stream.next();
            state = Parser.raw.charCodeAt(16 * prev + t.term);
            t.state = state;
            if(state >= 10) {
                break;
            }
            prev = state;
        }
        if(state == 255) {
            state = prev;
            dx = 1;
        } else {
            dx = 0;
        }
        var q = Parser.raw.charCodeAt(271 - state);
        if(q < 7) {
            stream.pos -= dx;
        } else {
            q = Parser.raw.charCodeAt(state + 176);
            if(q < 7) {
                stream.rollback(dx + Parser.raw.charCodeAt(state + 208),9);
            } else {
                break;
            }
        }
        while(true) {
            var value = Parser.cases(q,stream);
            t = stream.cached[stream.pos + -1];
            if(t.term == exp) {
                --stream.pos;
                stream.junk(1);
                return value;
            }
            t.val = value;
            t.state = Parser.raw.charCodeAt(16 * stream.cached[stream.pos + -2].state + t.term);
            prev = t.state;
            if(prev < 11) {
                break;
            }
            q = Parser.raw.charCodeAt(271 - prev);
        }
    }
    if(exp == -1 && stream.pos - dx > keep) {
        return stream.cached[keep].val;
    }
    t = stream.cached[stream.pos + -1];
    throw new Error("Unexpected \"" + (t.term != 0 ? stream.lex.getString(t.pmin,t.pmax - t.pmin) : "Eof") + "\"" + stream.errpos(t.pmin));
};
Parser.cases = function(f,s) {
    var __r = 0;
    var __v;
    switch(f) {
    case 0:
        __r = 2306;
        __v = s.cached[s.pos + -2].val;
        break;
    case 1:
        __r = 2563;
        var e1 = s.cached[s.pos + -3].val;
        var e2 = s.cached[s.pos + -1].val;
        __v = s.cached[s.pos + -2].term == 2 ? e1 + e2 : e1 - e2;
        break;
    case 2:
        __r = 2563;
        __v = s.cached[s.pos + -3].val * s.cached[s.pos + -1].val;
        break;
    case 3:
        __r = 2563;
        __v = s.cached[s.pos + -3].val / s.cached[s.pos + -1].val | 0;
        break;
    case 4:
        __r = 2563;
        __v = s.cached[s.pos + -2].val;
        break;
    case 5:
        __r = 2562;
        __v = -s.cached[s.pos + -1].val;
        break;
    default:
        __r = 2561;
        __v = Std.parseInt(s.stri(-1));
    }
    s.reduce(__r);
    return __v;
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
    junk: function(n) {
        if(n <= 0) {
            this.right = this.pos;
        } else if(this.right - this.pos >= n) {
            this.right -= n;
            var _g = this.pos;
            var _g1 = this.right;
            while(_g < _g1) {
                var i = _g++;
                this.cached[i] = this.cached[i + n];
            }
        } else {
            n -= this.right - this.pos;
            while(n-- > 0) this.lex.token();
            this.right = this.pos;
        }
    }
    ,errpos: function(pmin) {
        var input = this.lex.input;
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
    }
    ,stri: function(dx) {
        var t = this.cached[this.pos + dx];
        return this.lex.getString(t.pmin,t.pmax - t.pmin);
    }
    ,next: function() {
        if(this.right == this.pos) {
            var t = this.lex.token();
            this.cached[this.right++] = new lm_Tok(t,this.lex.pmin,this.lex.pmax);
        }
        return this.cached[this.pos++];
    }
    ,rollback: function(dx,maxv) {
        this.pos -= dx;
        dx = this.pos;
        while(dx < this.right) {
            if(this.cached[dx].term >= maxv) {
                this.right = dx;
                this.lex.pmax = this.cached[dx].pmin;
                break;
            }
            ++dx;
        }
    }
    ,reduce: function(lvw) {
        var pmax = this.cached[this.pos + -1].pmax;
        var w = lvw & 255;
        this.pos -= w;
        var t = this.cached[this.pos];
        t.term = lvw >>> 8;
        t.pmax = pmax;
        ++this.pos;
        --w;
        this.right -= w;
        var _g = this.pos;
        var _g1 = this.right;
        while(_g < _g1) {
            var i = _g++;
            this.cached[i] = this.cached[i + w];
        }
    }
};
Lexer.raw = "\xff\xff\xff\xff\xff\xff\xff\xff\xff\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x01\xff\x0f\xff\xff\xff\xff\xff\x0e\x0d\x0c\x0b\xff\x02\xff\x0a\x09\x03\x03\x03\x03\x03\x03\x03\x03\x03\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x03\x03\x03\x03\x03\x03\x03\x03\x03\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x08\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x06\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\xff\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\xff\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\x05\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x07\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x03\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x08\x06\x07\x04\x02\x05\x01\x0b\x09\xff\x0a\xff\x01\x03\x00\xff";
Parser.raw = "\xff\x0f\xff\x03\xff\xff\x04\xff\xff\xff\x01\xff\xff\xff\xff\xff\x10\xff\x07\x07\x08\x09\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\xff\x03\xff\xff\x04\xff\xff\xff\x06\xff\xff\xff\xff\xff\xff\x0f\xff\x03\xff\xff\x04\xff\xff\xff\x0e\xff\xff\xff\xff\xff\xff\x0f\xff\x03\xff\xff\x04\xff\xff\xff\x05\xff\xff\xff\xff\xff\xff\xff\x07\x07\x08\x09\xff\x0d\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x07\x07\x08\x09\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\xff\x03\xff\xff\x04\xff\xff\xff\x0a\xff\xff\xff\xff\xff\xff\x0f\xff\x03\xff\xff\x04\xff\xff\xff\x0b\xff\xff\xff\xff\xff\xff\x0f\xff\x03\xff\xff\x04\xff\xff\xff\x0c\xff\xff\xff\xff\xff\xff\xff\xff\xff\x08\x09\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x06\x05\x04\x03\x02\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff";
Demo.main();
})();
```
