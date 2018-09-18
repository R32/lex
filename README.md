Lex Macro
------

Build State Transition Table in macro(compile phase)

## status

* [x] Lexer: Does not support unicode(The maximum char is 254)
* [ ] Parser: Not available yet

## Usage

...

### Defines

* `-D lex_switch`: then the function jump table will be converted as `switch` expr
* `-D lex_lr0table`: generate LR0 table to "lr0-table.txt"

  example:

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