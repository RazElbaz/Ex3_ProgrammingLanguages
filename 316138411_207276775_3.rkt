#lang pl
;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#|
Task 2. Expanding the FLANG BNF language :

We have extended our BNF and parser to support this syntax.
**'then-do' and 'else-do' are terminals and are part of the syntax of the special form "if".

Problems we encountered: We had to refresh our memory regarding the grammar writing and therefore we had to repeat the last lectures.
Time: 20 minutes


FLANG BNF :
 
 The grammar:
 <FLANG> ::= <num>                             ;; Rule 1 
          | { + <FLANG> <FLANG> }              ;; Rule 2
          | { - <FLANG> <FLANG> }              ;; Rule 3
          | { * <FLANG> <FLANG> }              ;; Rule 4
          | { / <FLANG> <FLANG> }              ;; Rule 5
          | { with { <id> <FLANG> } <FLANG> }  ;; Rule 6
          | <id>                               ;; Rule 7
          | { fun { <id> } <FLANG> }           ;; Rule 8
          | { call <FLANG> <FLANG> }           ;; Rule 9
          | True                               ;; add rule for True  ;; Rule 10
          | False                              ;; add rule for False ;; Rule 11
          |{= <FLANG> <FLANG>}                 ;; add rule for =     ;; Rule 12
          |{> <FLANG> <FLANG>}                 ;; add rule for >     ;; Rule 13
          |{< <FLANG> <FLANG>}                 ;; add rule for <     ;; Rule 14
          |{not <FLANG>}                       ;; add rule for not   ;; Rule 15
          |{if <FLANG> {then-do <FLANG>}
                       {else-do <FLANG>}}      ;; add rule 16 for (the above) if expressions

          
|#
;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#|
Task 3. Extending the Parser :

We used the test examples above to complete the missing parts of FLANG
Type definition and parse-expr procedure.

Problems we encountered:-We had to refresh our memory regarding the grammar writing and therefore we had to repeat the last lectures.
                        -In define-type FLANG we didn't add id and therefore we got an error in parse-sexpr
Time: 40 minutes
      
|#

(define-type FLANG
 [Num Number]
 [Call FLANG FLANG]
 [Bool Boolean]
 [Bigger FLANG FLANG]
 [Smaller FLANG FLANG]
 [Equal FLANG FLANG]
 [Not FLANG]
 [If FLANG FLANG FLANG]
 [Id Symbol])

(: parse-sexpr : Sexpr -> FLANG)
 ;; to convert s-expressions into FLANGs
 (define (parse-sexpr sexpr)
  (match sexpr
 [(number: n) (Num n)]
 ['True (Bool true)]
 ['False (Bool false)]
 [(symbol: name) (Id name)]

 [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
 [(list '= lhs rhs) (Equal (parse-sexpr lhs)(parse-sexpr rhs))]
 [(list '> lhs rhs) (Bigger (parse-sexpr lhs)(parse-sexpr rhs))]
 [(list '< lhs rhs) (Smaller (parse-sexpr lhs)(parse-sexpr rhs))]
 [(list 'not exp) (Not (parse-sexpr exp))]
 [(cons 'if more)
 (match sexpr    ;;Match is used for built-in types in Racket 
       [(list 'if cond (list (symbol: then-do) T-body) (list (symbol: else-do) E-body)) ;; We used Said's code from lectures
       (If (parse-sexpr cond)  (parse-sexpr T-body) (parse-sexpr E-body))]
       [else (error 'parse-sexpr "bad if syntax!!")])]  
 [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#|
Task 4. Extending subst and eval  :

We used the formal rules to complete the code for the subst procedure. 


Problems we encountered:-We had to refresh our memory regarding the grammar writing and therefore we had to repeat the last lectures.
                        -We encountered an error:
                            Cases: Missing cases for the following versions: (number ...), (call ...), (id ...)
                         Then we realized that we were missing the cases for Id, Num, Call in the subst function and added them.
* We used the code from the lectures
Time: 60 minutes



Formal Substitution rules:

subst:
      N[v/x] = N
      {+ E1 E2}[v/x] = {+ E1[v/x] E2[v/x]}
      {- E1 E2}[v/x] = {- E1[v/x] E2[v/x]}
      {* E1 E2}[v/x] = {* E1[v/x] E2[v/x]}
      {/ E1 E2}[v/x] = {/ E1[v/x] E2[v/x]}
      y[v/x] = y
      x[v/x] = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]} ; if y =/= x
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
      {call E1 E2}[v/x] = {call E1[v/x] E2[v/x]}
      {fun {y} E}[v/x] = {fun {y} E[v/x]} ; if y =/= x
      {fun {x} E}[v/x] = {fun {x} E}
      B[v/x] = B ;; B is Boolean
      {= E1 E2}[v/x] = {= E1[v/x] E2[v/x]}
      {> E1 E2}[v/x] = {> E1[v/x] E2[v/x]}
      {< E1 E2}[v/x] = {< E1[v/x] E2[v/x]}
      { not E}[v/x] = {not E[v/x]}
      {if Econd {then-do Edo} {else-do Eelse}}[v/x]
      = {if Econd[v/x] {then-do Edo[v/x]} {else-do
      Eelse[v/x]}}

|#

(: subst : FLANG Symbol FLANG -> FLANG)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
  (cases expr
      [(Num n) expr]
      [(Bool b) expr]
      [(Id name) (if (eq? name from) to expr)]
      [(Call l r) (Call (subst l from to) (subst r from to))]
      [(Equal l r) (Equal (subst l from to) (subst r from to))]
      [(Bigger l r) (Bigger (subst l from to) (subst r from to))]
      [(Smaller l r) (Smaller (subst l from to) (subst r from to))]
      [(Not flag) (Not (subst flag from to))]
      [(If Econd Edo Eelse)(If (subst Econd from to) (subst Edo from to) (subst Eelse from to))])) 

