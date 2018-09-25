Lex
------

Build lightweight lexer/parser(LR0) state transition tables in macro(compile phase).

## status

* [x] Lexer: Does not support unicode(The maximum char is 254)
* [x] Parser: **Rollback-Able LR(0)** that use rollback `O(1) + S(2NStates)` to resolve conflicts.
  - [x] Guard, If the production(rhs) have a "left sub rhs" or can be epsilon.
    ```hx
    switch(s) {
    case [A, B, C] if(expr): 0; // if false then rollback to case [A]:
    case [A]: 1;
    }
    ```
  - [ ] Operator Priority

*Need More Tests in Parser*

### Defines

It looks very messy here.

* use `-D lex_switch` or the numble of rules is less then 6 then the function jump table will be converted as `switch` expr

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

  ```
  Production:
    (R0)  MAIN --> EXPR $
    (R1)  EXPR --> EXPR + EXPR
    (R2)       --> EXPR - EXPR
    (R3)       --> EXPR * EXPR
    (R4)       --> EXPR / EXPR
    (R5)       --> ( EXPR )
    (R6)       --> - EXPR
    (R7)       --> int
  -------------------------------------------------------------------------------------------------------------
  |  (S)   |  (RB)  |  (EP)  |   $    |  int   |   +    |   -    |   *    |   /    |   (    |   )    |  EXPR  |
  -------------------------------------------------------------------------------------------------------------
  |   0    |  NULL  |  NULL  |        | R7,S17 |        |   3    |        |        |   4    |        |   1    |
  -------------------------------------------------------------------------------------------------------------
  |   1    |  NULL  |  NULL  | R0,S18 |        |   7    |   8    |   9    |   10   |        |        |        |
  -------------------------------------------------------------------------------------------------------------
  |   2    |  NULL  |  NULL  |        | R7,S17 |        |   3    |        |        |   4    |        |   6    |
  -------------------------------------------------------------------------------------------------------------
  |   3    |  NULL  |  NULL  |        | R7,S17 |        |   3    |        |        |   4    |        | R6,S16 |
  -------------------------------------------------------------------------------------------------------------
  |   4    |  NULL  |  NULL  |        | R7,S17 |        |   3    |        |        |   4    |        |   5    |
  -------------------------------------------------------------------------------------------------------------
  |   5    |  NULL  |  NULL  |        |        |   7    |   8    |   9    |   10   |        | R5,S15 |        |
  -------------------------------------------------------------------------------------------------------------
  |   6    |  NULL  |  NULL  |        |        |   7    |   8    |   9    |   10   |        |        |        |
  -------------------------------------------------------------------------------------------------------------
  |   7    |  NULL  |  NULL  |        | R7,S17 |        |   3    |        |        |   4    |        | R1,S14 |
  -------------------------------------------------------------------------------------------------------------
  |   8    |  NULL  |  NULL  |        | R7,S17 |        |   3    |        |        |   4    |        | R2,S13 |
  -------------------------------------------------------------------------------------------------------------
  |   9    |  NULL  |  NULL  |        | R7,S17 |        |   3    |        |        |   4    |        | R3,S12 |
  -------------------------------------------------------------------------------------------------------------
  |   10   |  NULL  |  NULL  |        | R7,S17 |        |   3    |        |        |   4    |        | R4,S11 |
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
  |   17   |  NULL  |
  -------------------
  |   18   |  NULL  |
  -------------------
  ```

## Usage

copy from [demo/Demo.hx](demo/Demo.hx)

```hx
package;

class Demo {
  static function main() {
    var str = '1 + 2 + 3 + -1';
    var lex = new Lexer(lms.ByteData.ofString(str));
    var par = new Parser(lex);
    trace(par.main());
  }
}

// NOTICE: the lm.LR0 only works with "enum abstract (Int)"
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
  ];
}

class Parser implements lm.LR0<Lexer> {

  static var main = switch(s) {
    case [e = expr, Eof]: e;
  }
  // TODO: "Operator Priority" hasn't been implemented yet.
  static var expr = switch(s) {
    case [e1 = expr, OpPlus, e2 = expr]: e1 + e2;
    case [e1 = expr, OpMinus, e2 = expr]: e1 - e2;
    case [e1 = expr, OpTimes, e2 = expr]: e1 * e2;
    case [e1 = expr, OpDiv, e2 = expr]: e1 / e2;
    case [LParen, e = expr, RParen]: e;
    case [OpMinus, e = expr]: -e;
    case [CInt(n)]: n;
  }

  // for extract n from CInt(n)
  @:ofStr(CInt) static inline function int_of_string(s: String):Int return Std.parseInt(s);
}
```

the you can compile it use:

`haxe -dce full -D lex_switch -D analyzer-optimize -D nodejs -lib lex -main Demo -js demo.js`

> NOTE: `-D nodejs` is used to remove js.compat.TypedArray

<br />

#### js output

```js
// Generated by Haxe 4.0.0-preview.4+0cfe8f712
(function () { "use strict";
var $hxEnums = $hxEnums || {};
var Demo = function() { };
Demo.main = function() {
  console.log("Demo.hx:8:",new Parser(new Lexer("1 + 2 + 3 + -1"))._entry(0,8));
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
  default:
    return 7;
  }
};
Lexer.prototype = {
  getString: function(p,len) {
    return this.input.substr(p,len);
  }
  ,_token: function(state) {
    var i = this.pmax;
    var len = this.input.length;
    if(i >= len) {
      return 0;
    }
    this.pmin = i;
    var prev = state;
    while(i < len) {
      var c = this.input.charCodeAt(i++);
      state = Lexer.raw.charCodeAt(128 * state + c);
      if(state >= 4) {
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
    var q = Lexer.raw.charCodeAt(559 - state);
    if(q < 8) {
      this.pmax = i - prev;
    } else {
      q = Lexer.raw.charCodeAt(state + 512);
      if(q < 8) {
        this.pmax = i - prev - Lexer.raw.charCodeAt(state + 528);
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
Parser.cases = function(f,s) {
  switch(f) {
  case 0:
    var e = s.cached[s.pos + -2].val;
    s.reduce(8,2);
    return e;
  case 1:
    var e1 = s.cached[s.pos + -3].val;
    var e2 = s.cached[s.pos + -1].val;
    s.reduce(9,3);
    return e1 + e2;
  case 2:
    var e11 = s.cached[s.pos + -3].val;
    var e21 = s.cached[s.pos + -1].val;
    s.reduce(9,3);
    return e11 - e21;
  case 3:
    var e12 = s.cached[s.pos + -3].val;
    var e22 = s.cached[s.pos + -1].val;
    s.reduce(9,3);
    return e12 * e22;
  case 4:
    var e13 = s.cached[s.pos + -3].val;
    var e23 = s.cached[s.pos + -1].val;
    s.reduce(9,3);
    return e13 / e23;
  case 5:
    var e3 = s.cached[s.pos + -2].val;
    s.reduce(9,3);
    return e3;
  case 6:
    var e4 = s.cached[s.pos + -1].val;
    s.reduce(9,2);
    return -e4;
  default:
    var t = s.cached[s.pos + -1];
    var n = Std.parseInt(s.lex.getString(t.pmin,t.pmax - t.pmin));
    s.reduce(9,1);
    return n;
  }
};
Parser.prototype = {
  _entry: function(state,exp) {
    var prev = state;
    var t;
    var dx = 0;
    while(true) {
      while(true) {
        t = this.stream.next();
        state = Parser.raw.charCodeAt(16 * prev + t.term);
        t.state = state;
        if(state >= 11) {
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
      if(q < 8) {
        this.stream.pos -= dx;
      } else {
        q = Parser.raw.charCodeAt(state + 176);
        if(q < 8) {
          this.stream.rollback(dx + Parser.raw.charCodeAt(state + 208));
        } else {
          break;
        }
      }
      var value = Parser.cases(q,this.stream);
      var _this = this.stream;
      t = _this.cached[_this.pos + -1];
      if(t.term == exp) {
        --this.stream.pos;
        this.stream.junk(0);
        return value;
      }
      t.val = value;
      var _this1 = this.stream;
      t.state = Parser.raw.charCodeAt(16 * _this1.cached[_this1.pos + -2].state + t.term);
      prev = t.state;
    }
    var _this2 = this.stream;
    var last = _this2.cached[_this2.pos + -1];
    throw new Error("Unexpected \"" + this.stream.lex.getString(last.pmin,last.pmax - last.pmin) + "\" at " + last.pmin + "-" + last.pmax);
  }
};
var HxOverrides = function() { };
HxOverrides.cca = function(s,index) {
  var x = s.charCodeAt(index);
  if(x != x) {
    return undefined;
  }
  return x;
};
var Std = function() { };
Std.parseInt = function(x) {
  var v = parseInt(x,10);
  if(v == 0 && (HxOverrides.cca(x,1) == 120 || HxOverrides.cca(x,1) == 88)) {
    v = parseInt(x);
  }
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
  ,next: function() {
    if(this.right == this.pos) {
      var t = this.lex.token();
      this.cached[this.right++] = new lm_Tok(t,this.lex.pmin,this.lex.pmax);
    }
    return this.cached[this.pos++];
  }
  ,rollback: function(dx) {
    this.pos -= dx;
    this.right = this.pos;
    this.lex.pmax = this.cached[this.pos].pmin;
  }
  ,reduce: function(lv,w) {
    var pmax = this.cached[this.pos + -1].pmax;
    this.pos -= w;
    var t = this.cached[this.pos];
    t.term = lv;
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
if( String.fromCodePoint == null ) String.fromCodePoint = function(c) { return c < 0x10000 ? String.fromCharCode(c) : String.fromCharCode((c>>10)+0xD7C0)+String.fromCharCode((c&0x3FF)+0xDC00); }
Lexer.raw = "\xff\xff\xff\xff\xff\xff\xff\xff\xff\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x01\xff\xff\xff\xff\xff\xff\xff\x09\x08\x07\x06\xff\x02\xff\x05\x04\x03\x03\x03\x03\x03\x03\x03\x03\x03\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x03\x03\x03\x03\x03\x03\x03\x03\x03\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x03\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x06\x07\x04\x02\x05\x01\x01\x03\x00\xff";
Parser.raw = "\xff\x11\xff\x03\xff\xff\x04\xff\xff\x01\xff\xff\xff\xff\xff\xff\x12\xff\x07\x08\x09\x0a\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x11\xff\x03\xff\xff\x04\xff\xff\x06\xff\xff\xff\xff\xff\xff\xff\x11\xff\x03\xff\xff\x04\xff\xff\x10\xff\xff\xff\xff\xff\xff\xff\x11\xff\x03\xff\xff\x04\xff\xff\x05\xff\xff\xff\xff\xff\xff\xff\xff\x07\x08\x09\x0a\xff\x0f\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x07\x08\x09\x0a\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x11\xff\x03\xff\xff\x04\xff\xff\x0e\xff\xff\xff\xff\xff\xff\xff\x11\xff\x03\xff\xff\x04\xff\xff\x0d\xff\xff\xff\xff\xff\xff\xff\x11\xff\x03\xff\xff\x04\xff\xff\x0c\xff\xff\xff\xff\xff\xff\xff\x11\xff\x03\xff\xff\x04\xff\xff\x0b\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x07\x06\x05\x01\x02\x03\x04\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff";
Demo.main();
})();
```
