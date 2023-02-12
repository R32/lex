Lex
------

Build lexer and simple parser(SimpleLR) in macro.

## Samples

* [hello world](#Usage)

* [Lexer tool for c langauge](/tools/CLexer.hx)

  > run `haxelib run lex` [test.lex](/tools/test/test.lex) and then you will get a [`test.c`](/tools/test/test.c) file

* [hscript parser](/demo/)

* [css selector](https://github.com/R32/css-selector/blob/master/csss/LRParser.hx)

## Status

LIMITS : you can't use it in [`macro-in-macro`](https://github.com/HaxeFoundation/haxe/pull/7496)

* Lexer

  ```haxe
  static function main()  {
      var str = lms.ByteData.ofString('exec(123 + 456)');
      var lex = new Lexer(str);
      var t = lex.token();
      while (t != Eof) {
          trace(s_token(t));
          t = lex.token();
      }
  }

  enum Op {
      Add;
      Sub;
      Mul;
      Div;
  }

  enum Token {
      Eof;
      LParen;
      RParen;
      Op( op : Op );
      CIdent( id : String);
      CInt( i : Int);
      CString( s : String);
  }

  /*
   * The following meta characters are supported in string:
   * `*`: zero or more
   * `+`: one or more
   * `?`: zero or one
   * `[`: begin char range
   * `]`: end char range
   * `\`: escape next char
   * `|`: or  (Not recommended, e.g: "abc|xyz" should be replaced by: "abc" | "xyz" )
   *
   * The new syntax is added as follows:
   *
   * "a" | "b"    => /a|b/
   * "a" + "b"    => /ab/  "+" has a higher priority than "|"
   * Opt("abc")   => /(abc)?/
   * Star("abc")  => /(abc)*
   * Plus("abc")  => /(abc)+/
   */
  @:rule(Eof) class Lexer implements lm.Lexer<Token> {

      // You could add "@:skip" to avoid being parsed into a pattern

      var r_int = "0" | "-?[1-9][0-9]*";

      var r_ident = "[a-zA-Z_][a-zA-Z_0-9]+";

      // Matching 1, And the first one will be automatically renamed as "token"
      var tok =  [
          "[ \t]+" => this.token(),   // Recursive Matching 1
          "+" => Op(Add),
          "-" => Op(Sub),
          "*" => Op(Mul),
          "/" => Op(Div),
          "(" => LParen,
          ")" => RParen,
          r_int =>
              CInt(Std.parseInt(this.current)),
          r_ident =>
              CIdent(this.current),
          '"' => {
              var i = this.pmax;      // save start position
              var t = this.string();  // goto Matching 2
              if (t == Eof)
                  throw "UnClosed \"";
              this.pmin = i;          // restore pmin
              CString(this.current);
          },
          "//[^\n]*" =>
              this.token(),           // Recursive Matching 1
          _ => {
              // error handing, when error occurs, this.pmin will be >= this.pmax
              throw "UnExpected : " + this.getString(pmax, pmin - pmax);
              // NOTE: Unlike SLR parser, For lexer, only "Matching 1"
              // has the ability to handle errors, the others can only do empty(epsilon) matching
          }
      ];

      // Matching 2
      var string = [
          '"'     => CString(""),
          '[^"]+' => this.string()    // Recursive Matching 2
      ];

      // Matching N...
  }
  ```

* Parser: Only SimpleLR is available.

  Unlike normal LR parser, there is no *action-table*, all are *jump-table*.

  Some conflicts may be resolved in normal *LALR/LR1*, But here the conflicts error will be thrown directly.

  - **Position**: Inside the actions, you could use `T1~TN` to access the position, which is the instance of `lm.Stream.Tok`

    ```hx
    T1.pmax - T1.pmin;
    ```

    And inside the actions, you could use the vairalbe `stream`

    ```hx
    var tok = stream.peek(0);
    if (tok.term == SomeToken)
        stream.junk(1);
    ```

  - Combine multiple Tokens:

    ```haxe
    /*
     * This feature has been removed.
     *
     *// 0. the same prefix(At least 2 characters).
     * switch(s) {
     *case [e1=expr, Op(t), e2=expr]: switch(t) { case OpPlus: .... }
     *}
     */

    // 1. uses "[]", NOTE: if you put tokens with **different precedence**, a conflict error will be thrown.
    switch(s) {
    case [e1=expr, op = [OpAdd, OpSub], e2=expr]: op == OpAdd ? e1 + e2 : e1 - e2;
    }

    // 2. uses production(switch), NOTE: This will ignore all token **precedence** from predefine.
    // But you can use "@:prec(XXX)" to enforce the **precedence** for it
    case [e1=expr, op = op, e2=expr]: trace(op == OpAdd) ... //


    var op : Token = switch(s) {
    case [OpAdd] : OpAdd;
    case [OpSub] : OpSub;
    case [OpDiv] : OpDiv;
    case [OpMul] : OpMul;
    }
    ```

  - You can use string literals instead of simple terminators in *stream match*.

    ```haxe
    switch(s) {
    case [e1=expr, op = ["+", "-"], e2=expr]: op == OpPlus ? e1 + e2 : e1 - e2;
    case ["(", e = expr, ")"]: e;
    }
    ```

  - **Operator Precedence**:

    ```haxe
    // the operator precedence definitions:
    @:rule({
        left: ["+", "-"],         // The parser could auto reflect(str) => Token
        left: [OpTimes, OpDiv],   // The lower have higher Priority.
        nonassoc: [UMINUS],       // All characters of the placeholder must be UPPERCASE
    }) class MyParser implements lm.SLR<MyLexer> {
    ```

<pre><details>
    <summary>details...</summary><p>
    ```
    // UPPERCASE == "non-terml", LOWERCASE == "terml"
    [..., op, E]: if defined(op)     then case.left.lval = E.value
    [..., op, E]: if not defined(op) then case.left = null
    [..., T, E] | [E] | [..., t]:    then case.left = null
    [E, op, ...]: if defined(op)     then case.right.own = E.value
    [E, op, ...]: if not defined(op) then case.right.prio = -1
    [E, T, ...]                      then case.right.prio = -1
    [t, ...]                         then case.right = null
    ```
</p></details></pre>


### CHANGES

* `x.x.x`:

  - [slr] Refactored LR0 code to SLR

  - [lexer] Added new syntax Opt(), Star(), Plus() for group.

### Defines

* `-D lex_slrtable`: for debug. it will generate a SimpleLR table save as `slr-table.txt`. for example:

  > You may have to modify the `mmap` field in `debug.Print`

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
        var str = '1 - 2 * (3 + 4) + 5 * Unexpected 6';
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
    var CIdent;
}

/**
* @:rule(EOF, cmax = 255) See the example below:
*   Eof is a custom terminator which is defined in "<Token>" (required)
*   127 is the custom maximum char value. (optional, default is 255)
*/
@:rule(Eof, 127) class Lexer implements lm.Lexer<Token> {
    var inter = "0|[1-9][0-9]*";         // 0 or ...
    var r_zero = "0";                    // string variable will be treated as pattern if there is no `@:skip`
    var r_int = "[1-9][0-9]*";
    var tok =  [                         // a rule set definition, the first one will become .token()
        "[ \t]+" => this.token(),
        r_zero | r_int => CInt,          // zero or int
     // inter + Opt("[eE][+-]?[0-9]+")   // exponent
        "+" => OpPlus,
        "-" => OpMinus,
        "*" => OpTimes,
        "/" => OpDiv,
        "(" => LParen,
        ")" => RParen,
        "[a-zA-Z_]+" => CIdent,
    ];
}

@:rule({
    start: [main],            // Specify start, like the "%start" in ocamlyacc, If not specified, the first "switch" will be selected
    left: ["+", "-"],         // The parser could auto reflect(str) => Token
    left: [OpTimes, OpDiv],   // The lower have higher priority.
    nonassoc: [UMINUS],       // The placeholder must be uppercase
}) class Parser implements lm.SLR<Lexer> {

    var main = switch(s) {
        case [e = expr, Eof]:
            e;
        default:              // place handling error code here
            var t = stream.peek(0);
            switch(t.term) {
            case Eof:
                return 0;
            case CIdent:                // Show recovery from errors, Note: this ability is very weak
                stream.junk(1);         // Discard current token
                slrloop( -1, MAIN_EXP); // main => MAIN_EXP, NOTE: Only the entry switch-case (Specified in "start") has an EXP value
            default:
                throw "Unexpected: " + stream.str(t);
            }
    }

    var expr : Int = switch(s) {        // Specify Type explicitly
        case [e1 = expr, op = [OpPlus,OpMinus], e2 = expr]: op == OpPlus ? e1 + e2 : e1 - e2;
        case [e1 = expr, OpTimes, e2 = expr]: e1 * e2;
        case [e1 = expr, OpDiv, e2 = expr]: Std.int(e1 / e2);
        case [LParen, e = expr, RParen]: e;
        case [@:prec(UMINUS) OpMinus, e = expr]: -e;   // %prec UMINUS
        case [CInt(n)]: n;
    }

    // Define custom extract function for CInt(n)
    @:rule(CInt) inline function int_of_string( s : String ) : Int return Std.parseInt(s);
    // OR @:rule(CInt) inline function int_of_string( input : lms.ByteData, t : lm.Stream.Tok ) : Int {
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
// Generated by Haxe 4.3.0-rc.1
(function ($global) { "use strict";
var Demo = function() { };
Demo.main = function() {
    var str = "1 - 2 * (3 + 4) + 5 * 6";
    var lex = new Lexer(str);
    var par = new Parser(lex);
    console.log("Demo.hx:9:",par._entry(0,8) == 17);
};
var Lexer = function(s) {
    this.input = s;
    this.pmin = 0;
    this.pmax = 0;
};
Lexer.prototype = {
    getString: function(p,len) {
        return this.input.substr(p,len);
    }
    ,_token: function(init,right) {
        if(this.pmax >= right) {
            return 0;
        }
        var raw = Lexer.raw;
        var i = this.pmax;
        var state = init;
        var prev = init;
        var c;
        while(i < right) {
            c = this.input.charCodeAt(i++);
            state = raw.charCodeAt(128 * state + c);
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
        var q = raw.charCodeAt(399 - state);
        if(i > this.pmax && q < 8) {
            this.pmin = this.pmax;
            this.pmax = i;
        } else {
            q = raw.charCodeAt(399 - init);
        }
        return this.cases(q);
    }
    ,token: function() {
        return this._token(0,this.input.length);
    }
    ,cases: function(s) {
        switch(s) {
        case 0:
            return this._token(0,this.input.length);
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
            throw new Error("UnMatached: '" + this.input.substr(this.pmax,this.pmin - this.pmax) + "'");
        }
    }
};
var Parser = function(lex) {
    this.stream = new lm_Stream(lex);
};
Parser.prototype = {
    _entry: function(state,exp) {
        var t = this.stream.newTok(0,0,0);
        t.state = state;
        var _this = this.stream;
        var i = _this.right;
        while(--i >= _this.pos) _this.cached[i + 1] = _this.cached[i];
        _this.cached[_this.pos] = t;
        ++_this.pos;
        ++_this.right;
        var raw = Parser.raw;
        while(true) {
            while(true) {
                t = this.stream.next();
                state = raw.charCodeAt(16 * state + t.term);
                if(state >= 9) {
                    break;
                }
                t.state = state;
            }
            if(state == 255) {
                this.stream.pos -= 1;
                var _this = this.stream;
                state = _this.cached[_this.pos + (-1)].state;
            }
            while(true) {
                var q = raw.charCodeAt(159 - state);
                var value = this.cases(q);
                if(q >= 7) {
                    return value;
                }
                t = this.stream.reduce(Parser.lvs[q]);
                if(t.term == exp) {
                    this.stream.pos -= 2;
                    this.stream.junk(2);
                    return value;
                }
                t.val = value;
                var _this1 = this.stream;
                state = raw.charCodeAt(16 * _this1.cached[_this1.pos + (-2)].state + t.term);
                t.state = state;
                if(state < 9) {
                    break;
                }
            }
        }
    }
    ,cases: function(q) {
        var __s = this.stream;
        switch(q) {
        case 0:
            return __s.cached[__s.pos + (-2)].val;
        case 1:
            var e1 = __s.cached[__s.pos + (-3)].val;
            var e2 = __s.cached[__s.pos + (-1)].val;
            if(__s.cached[__s.pos + (-2)].term == 2) {
                return e1 + e2;
            } else {
                return e1 - e2;
            }
            break;
        case 2:
            return __s.cached[__s.pos + (-3)].val * __s.cached[__s.pos + (-1)].val;
        case 3:
            return __s.cached[__s.pos + (-3)].val / __s.cached[__s.pos + (-1)].val | 0;
        case 4:
            return __s.cached[__s.pos + (-2)].val;
        case 5:
            return -__s.cached[__s.pos + (-1)].val;
        case 6:
            return Std.parseInt(__s.stri(-1));
        default:
            var t = this.stream.peek(0);
            throw new Error("Unexpected \"" + (t.term != 0 ? this.stream.lex.getString(t.pmin,t.pmax - t.pmin) : "Eof") + "\"");
        }
    }
};
var Std = function() { };
Std.parseInt = function(x) {
    var v = parseInt(x);
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
var lm_Stream = function(l) {
    this.lex = l;
    var this1 = new Array(128);
    this.cached = this1;
    this.right = 0;
    this.pos = 0;
};
lm_Stream.prototype = {
    reclaim: function(tok) {
        tok.nxt = this.h;
        this.h = tok;
    }
    ,newTok: function(term,min,max) {
        if(this.h == null) {
            return new lm_Tok(term,min,max);
        } else {
            var t = this.h;
            this.h = this.h.nxt;
            t.term = term;
            t.pmin = min;
            t.pmax = max;
            return t;
        }
    }
    ,peek: function(i) {
        while(this.right - this.pos <= i) {
            var t = this.lex.token();
            this.cached[this.right++] = this.newTok(t,this.lex.pmin,this.lex.pmax);
        }
        return this.cached[this.pos + i];
    }
    ,junk: function(n) {
        if(n <= 0) {
            return;
        }
        if(this.right - this.pos >= n) {
            var i = n;
            while(i-- > 0) this.reclaim(this.cached[this.pos + i]);
            i = this.pos;
            this.right -= n;
            while(i < this.right) {
                this.cached[i] = this.cached[i + n];
                ++i;
            }
        } else {
            n -= this.right - this.pos;
            while(n-- > 0) this.lex.token();
            while(this.right > this.pos) this.reclaim(this.cached[--this.right]);
        }
    }
    ,stri: function(dx) {
        var t = this.cached[this.pos + dx];
        return this.lex.getString(t.pmin,t.pmax - t.pmin);
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
        --w;
        this.pos -= w;
        this.right -= w;
        var t = this.cached[this.pos + (-1)];
        t.term = lvw >>> 8;
        t.pmax = pmax;
        if(w == 0) {
            return t;
        }
        var i = w;
        while(i-- > 0) this.reclaim(this.cached[this.pos + i]);
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
Lexer.raw = "ÿÿÿÿÿÿÿÿÿ\x01ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ\x01ÿÿÿÿÿÿÿ\t\x08\x07\x06ÿ\x05ÿ\x04\x03\x02\x02\x02\x02\x02\x02\x02\x02\x02ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ\x01ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ\x01ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ\x06\x07\x04\x02\x03\x05\x01\x01\x00ÿ";
Parser.raw = "ÿ\x0Eÿ\x01ÿÿ\x02ÿÿ\x08ÿÿÿÿÿÿÿ\x0Eÿ\x01ÿÿ\x02ÿÿ\nÿÿÿÿÿÿÿ\x0Eÿ\x01ÿÿ\x02ÿÿ\x03ÿÿÿÿÿÿÿÿ\x04\x04\x06\x07ÿ\x0Bÿÿÿÿÿÿÿÿÿ\x0Eÿ\x01ÿÿ\x02ÿÿ\x05ÿÿÿÿÿÿÿÿÿÿ\x06\x07ÿÿÿÿÿÿÿÿÿÿÿ\x0Eÿ\x01ÿÿ\x02ÿÿ\rÿÿÿÿÿÿÿ\x0Eÿ\x01ÿÿ\x02ÿÿ\x0Cÿÿÿÿÿÿ\tÿ\x04\x04\x06\x07ÿÿÿÿÿÿÿÿÿÿÿ\x06\x02\x03\x04\x05\x00ÿÿÿ\x01ÿÿÿÿÿ";
Parser.lvs = [2050,2307,2307,2307,2307,2306,2305];
Demo.main();
})({});
```
