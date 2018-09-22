Lex Macro
------

Build State Transition Table in macro(compile phase) and the table format is:

## status

* [x] Lexer: Does not support unicode(The maximum char is 254)
* [ ] Parser: **Rollback-Able LR0**(Not available yet)

## Usage

...

### Defines

It looks very messy here.

* use `-D lex_switch` or the numble of rules is less then 6 then the function jump table will be converted as `switch` expr

* `-D lex_charmax`: to simply handle for utf16 char, Because the State Transition Table is 8-bit, If the input value is too large, there may be got out-of-range error.

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
    (R2)       --> int
  ----------------------------------------------------------------
  |  (S)   |  (RB)  |  (EP)  |   $    |  int   |   +    |  EXPR  |
  ----------------------------------------------------------------
  |   0    |  NULL  |  NULL  |        | R2,S6  |        |   1    |
  ----------------------------------------------------------------
  |   1    |  NULL  |  NULL  | R0,S7  |        |   4    |        |
  ----------------------------------------------------------------
  |   2    |  NULL  |  NULL  |        | R2,S6  |        |   3    |
  ----------------------------------------------------------------
  |   3    |  NULL  |  NULL  |        |        |   4    |        |
  ----------------------------------------------------------------
  |   4    |  NULL  |  NULL  |        | R2,S6  |        | R1,S5  |
  ----------------------------------------------------------------
  |   5    |  NULL  |
  -------------------
  |   6    |  NULL  |
  -------------------
  |   7    |  NULL  |
  -------------------
  ```