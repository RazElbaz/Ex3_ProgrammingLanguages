#lang pl
;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#|
Task 2. Expanding the FLANG BNF language :

We have extended our BNF and parser to support this syntax.
**'then-do' and 'else-do' are terminals and are part of the syntax of the special form "if".

Problems we encountered: We had to refresh our memory regarding the grammar writing and therefore we had to repeat the last lectures.
Time:


 BNF :
 
#| The grammar:
 <FLANG> ::= <num>                             ;; Rule 1 
          | { + <FLANG> <FLANG> }              ;; Rule 2
          | { - <FLANG> <FLANG> }              ;; Rule 3
          | { * <FLANG> <FLANG> }              ;; Rule 4
          | { / <FLANG> <FLANG> }              ;; Rule 5
          | { with { <id> <FLANG> } <FLANG> }  ;; Rule 6
          | <id>                               ;; Rule 7
          | { fun { <id> } <FLANG> }           ;; Rule 8
          | { call <FLANG> <FLANG> }           ;; Rule 9
          | True ;; add rule for True ;; Rule 10
          | False ;; Rule 11
          |{= <FLANG> <FLANG>} ;; add rule for = ;; Rule 12
          |{> <FLANG> <FLANG>} ;; Rule 13
          |{< <FLANG> <FLANG>} ;; Rule 14
          |{not <FLANG>} ;; Rule 15
          |{if <FLANG> {then-do <FLANG>}
                       {else-do <FLANG>}}  ;; add rule 16 for (the above) if expressions

|#
           

|#
