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
                        -Parentheses problem
Time: 40 minutes
      
|#

(define-type FLANG
    ;; We used the code from the lectures
    [Num  Number]
    [Add  FLANG FLANG]
    [Sub  FLANG FLANG]
    [Mul  FLANG FLANG]
    [Div  FLANG FLANG]
    [Id   Symbol]
    [With Symbol FLANG FLANG]
    [Fun  Symbol FLANG]
    [Call FLANG FLANG]
    [Bool Boolean]
    [Bigger FLANG FLANG]
    [Smaller FLANG FLANG]
    [Equal FLANG FLANG]
    [Not FLANG]
    [If FLANG FLANG FLANG])

(: parse-sexpr : Sexpr -> FLANG)
 ;; to convert s-expressions into FLANGs
 (define (parse-sexpr sexpr)
  (match sexpr
 [(number: n) (Num n)]
 ['True (Bool true)]
 ['False (Bool false)]       
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
 

 ;; We used the code from the lectures
 [(symbol: name) (Id name)]
 [(cons 'with more)
          ( match sexpr
          [(list 'with (list (symbol: name) named-expr) body)
                                 (With name (parse-sexpr named-expr)
                                             (parse-sexpr body))]
             [else (error 'parse-sexpr "bad with syntax!!")])]
 [(cons 'fun more)
          ( match sexpr
          [(list 'fun (list (symbol: name)) body)
                                 (Fun name (parse-sexpr body))]
             [else (error 'parse-sexpr "bad fun syntax!!")])]
 [(list '+ l r) (Add (parse-sexpr l) (parse-sexpr r))]
          [(list '- l r) (Sub (parse-sexpr l) (parse-sexpr r))]
          [(list '* l r) (Mul (parse-sexpr l) (parse-sexpr r))]
          [(list '/ l r) (Div (parse-sexpr l) (parse-sexpr r))]
[else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))
;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#|
Task 4. Extending subst and eval  :

We used the formal rules to complete the code for the subst procedure. 


Problems we encountered:-We had to refresh our memory regarding the grammar writing and therefore we had to repeat the last lectures.
                        -We encountered an error:
                            Cases: Missing cases for the following versions: (number ...), (call ...), (id ...)
                         Then we realized that we were missing the cases for Id, Num, Call in the subst function and added them.
                        -Parentheses problem

! The biggest problem was that we didn't notice in the first definition of the grammar that all the operations of addition, subtraction, multiplication, division, fun and with should be added,
  therefore we had to add them wherever it was needed and we used the codes from the lectures
                        
* We used the code from the lectures
Time: 3 hours



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
      [(Add l r) (Add (subst l from to) (subst r from to))]
      [(Sub l r) (Sub (subst l from to) (subst r from to))]
      [(Mul l r) (Mul (subst l from to) (subst r from to))]
      [(Div l r) (Div (subst l from to) (subst r from to))]
      [(With name named body)
                  (With name (subst named from to)
                            (if (eq? from name)
                                body
                                (subst body from to)))]
      [(Fun name body)
                     (Fun name (if (eq? name from)
                                body
                               (subst  body from to)))]
      [(Bool b) expr]
      [(Id name) (if (eq? name from) to expr)]
      [(Call l r) (Call (subst l from to) (subst r from to))]
      [(Equal l r) (Equal (subst l from to) (subst r from to))]
      [(Bigger l r) (Bigger (subst l from to) (subst r from to))]
      [(Smaller l r) (Smaller (subst l from to) (subst r from to))]
      [(Not flag) (Not (subst flag from to))]
      [(If Econd Edo Eelse)(If (subst Econd from to) (subst Edo from to) (subst Eelse from to))])) 


;; The following function is used in multiple places below,
;; hence, it is now a top-level definition
(: Num->number : FLANG -> Number)
;; gets a FLANG -- presumably a Num variant -- and returns the
;; unwrapped number
(define (Num->number e)
 (cases e
 [(Num n) n]
 [else (error 'Num->number "expected a number, got: ~s" e)]))


(: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
 ;; gets a Racket numeric binary operator, and uses it within a FLANG
 ;; `Num' wrapper
(define (arith-op op expr1 expr2)
 (Num (op (Num->number expr1) (Num->number expr2))))

(: logic-op : (Number Number -> Boolean) FLANG FLANG -> FLANG)
 ;; gets a Racket Boolean binary operator (on numbers), and applies it
 ;; to two `Num' wrapped FLANGs
(define (logic-op op expr1 expr2)
 (Bool (op (Num->number expr1) (Num->number expr2))))





(: flang->bool : FLANG -> Boolean)
;; gets a Flang E (of any kind) and returns a its appropiate
;; Boolean value -- which is true if and only if E does not
;; represent false
;; Remark: the `flang->bool` function will also be top-level
;; since it's used in more than one place.
(define (flang->bool e)
 (cases e
 [(Bool boolean) boolean]
 [else (error 'flang->bool "expects a boolean, got: ~s" e)]))

;;TEST - flang->bool
(test (flang->bool (Bool true))=> #t)
(test (flang->bool (Bool false))=> #f)




#|
We used the procedures defined above and the formal rules below to complete the code for the evaluation procedure and refer to the tests provided in the introduction
part of the mission.

 eval: Evaluation rules:
      eval(N) = N ;; N is an expression for a numeric value
      eval({+ E1 E2}) = eval(E1) + eval(E2) \ if both E1 and E2
      eval({- E1 E2}) = eval(E1) - eval(E2) \ evaluate to numbers
      eval({* E1 E2}) = eval(E1) * eval(E2) / otherwise error!
      eval({/ E1 E2}) = eval(E1) / eval(E2) /
      eval(id) = error! eval({with {x E1} E2}) =
      eval(E2[eval(E1)/x])
      eval(FUN) = FUN ; assuming FUN is a function expression
      eval({call E1 E2}) = eval(Ef[eval(E2)/x])
      if eval(E1)={fun {x} Ef}
       = error! otherwise

      eval(B) = B ;; B is an expression for a Boolean value
      eval({= E1 E2}) = eval(E1) = eval(E2) \ if both E1 and E2
      eval({> E1 E2}) = eval(E1) > eval(E2) \ evaluate to
       numbers
      eval({< E1 E2}) = eval(E1) < eval(E2) / otherwise error!
      eval({not E}) = not(eval(E)) /E may be anything
      eval({if Econd {then-do Edo} {else-do Eelse}})
      = eval(Edo) if eval(Econd) =/= false,
      eval(Eelse), otherwise.
|#


(: eval : FLANG -> FLANG)
 ;; evaluates FLANG expressions by reducing them to *expressions*
 (define (eval expr)
 (cases expr
  ;;We used Said's code 
 [(Num n) expr]
 [(Add l r) (arith-op + (eval l) (eval r))]
 [(Sub l r) (arith-op - (eval l) (eval r))]
 [(Mul l r) (arith-op * (eval l) (eval r))]
 [(Div l r) (arith-op / (eval l) (eval r))]
 [(Id name) (error 'eval "free identifier ~s" name)]
 [(Fun name body) expr]
 [(Call fun-expr arg-expr)
     (let([fval (eval fun-expr)])
       (cases fval
         [(Fun bound-id bound-body)
          (eval (subst bound-body
                       bound-id
                       (eval arg-expr)))]
         [else (error 'eval "`call' expects a function, got: ~s" fval)]))]

 [(With name named body) (eval (subst body name (eval named)))]
 [(Bool b) expr]
 [(Bigger l r) (logic-op > (eval l) (eval r))]
 [(Smaller l r) (logic-op < (eval l) (eval r))]
 [(Equal l r) (logic-op = (eval l) (eval r))]
 ;;We used Said's code from lecture 10
 [(If l m r)
    (let ([cond (eval l)])
    (cases cond
        [(Bool boolean) (if (eq? boolean #t) (eval m) (eval r))]
        [else (error 'eval "expected a boolean, got: ~s" cond)]))]
 [(Not exp) (Bool (not (flang->bool (eval exp))))]))

