Lex Macro
------

Build State Transition Table in macro(compile phase)

## status

* [x] Lexer: Does not support unicode(The maximum char is 254)
* [ ] Parser: Not available yet

## Usage

...

### Defines

It looks very messy here.

* use `-D lex_switch` or the numble of rules is less then 6 then the function jump table will be converted as `switch` expr

* `-D lex_charmax`: to simply handle the utf16 char, Because the State Transition Table is 8-bit, If the input value is too large, there may be got out-of-range error.

  ```hx
  // source code from lm.LexBuilder
  var c = input.readByte(i++);
  #if lex_charmax
  if (c > CMAX) c = CMAX; // CMAX default value is 255.
  #end
  state = trans(state, c);
  ```
* `-D lex_rawtable or -D lex_strtable`: use `Bytes`(*No encoding specified*) or `String` as table format.

  By default, `String` format is used for **JS**, other platforms use `Bytes` format.

* `-D lex_rawinput`: then force use `Bytes` as the input format, default is `String`. see `lms.ByteData`

  actually you can use `--remap <package:target>` to override it.

* `-D lex_lr0table`: for debug. it will generate a LR0 table save as `lr0-table.txt`. Example:

  ```
  Production:
    (R0)  MAIN --> EXPR $
    (R1)  EXPR --> EXPR + EXPR
    (R2)       --> int
  -------------------------------------------------------
  |  (S)   |  (EP)  |   $    |  int   |   +    |  EXPR  |
  -------------------------------------------------------
  |   0    |  NULL  |        |   R2   |        |   1    |
  -------------------------------------------------------
  |   1    |  NULL  |   R0   |        |   4    |        |
  -------------------------------------------------------
  |   2    |  NULL  |        |   R2   |        |   3    |
  -------------------------------------------------------
  |   3    |  NULL  |        |        |   4    |        |
  -------------------------------------------------------
  |   4    |  NULL  |        |   R2   |        |   R1   |
  -------------------------------------------------------
  ```