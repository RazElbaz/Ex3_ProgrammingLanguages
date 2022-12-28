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
 [(list '= l r) (Equal (parse-sexpr l) (parse-sexpr r))]
 [(list '> l r) (Bigger (parse-sexpr l) (parse-sexpr r))]
 [(list '< l r) (Smaller (parse-sexpr l) (parse-sexpr r))]
 [(list 'not exp) (Not (parse-sexpr exp))]
 [(cons 'if more)
 (match sexpr    ;;Match is used for built-in types in Racket 
       [(list 'if cond (list (symbol: then-do) T-body) (list (symbol: else-do) E-body)) ;; We used Said's code from lectures
       (If (parse-sexpr cond)  (parse-sexpr T-body) (parse-sexpr E-body))]
       [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])]  
 

 ;; We used the code from the lectures
 [(symbol: name) (Id name)]
 [(cons 'with more)
          ( match sexpr
          [(list 'with (list (symbol: name) named-expr) body)
                                 (With name (parse-sexpr named-expr)
                                             (parse-sexpr body))]
             [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
 [(cons 'fun more)
          ( match sexpr
          [(list 'fun (list (symbol: name)) body)
                                 (Fun name (parse-sexpr body))]
             [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
 [(list '+ l r) (Add (parse-sexpr l) (parse-sexpr r))]
 [(list '- l r) (Sub (parse-sexpr l) (parse-sexpr r))]
 [(list '* l r) (Mul (parse-sexpr l)  (parse-sexpr r))]
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
Time: 4 hours



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
 [(Bool boolean)(if boolean #t #f)]
 [else #t]))

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
 [(Id name) (error 'eval "free identifier: ~s" name)]
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
   (let ([cond (flang->bool (eval l))])
         (if (eq? cond true) (eval m) (eval r)))]
 [(Not exp) (Bool (not (flang->bool (eval exp))))]))

;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#|
Task 5. Extending the run procedure  :

Finally, we will allow the interface procedure to return any of the three possible types of the extended language.
We used the test examples to complete the code for the run procedure

Problems we encountered:We got the error:
  parse: unbound identifier in: parse

Then we accessed the code that Said uploaded: Twoards_FLANG .rkt and took the parse function from there
Time: 1 hours

|#


;; from Twoards_FLANG .rkt
(: parse : String -> FLANG)
(define (parse code)
       (parse-sexpr (string->sexpr code)))

(: run : String -> (U Number Boolean FLANG))
;; evaluate a FLANG program contained in a string
(define (run str)
  (let ([result (eval (parse str))])
    (cases result
      [(Num n) n]
      [(Bool b) b]
      [else result])))



;; tests
(test (run "True") => true)
(test (run "{not True}") => false)
(test (run "{> 3 44}") => false)
(test (run "{if {- 3 3} {then-do 4} {else-do 5}}") => 4)
(test (run "{with {x 8}
 {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 1/4)
(test (run "{with {x 0}
 {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 0)
(test (run "{if {> 2 1} {then-do True} {else-do {+ 2 2}}}") => true)
(test (run "{with {c True}
 {if c {then-do {> 2 1}} {else-do 2}}}")
 => true)
(test (run "{with {foo {fun {x}
 {if {< x 2} {then-do x} {else-do {/ x 2}}}}} foo}")
 => (Fun 'x (If (Smaller (Id 'x) (Num 2)) (Id 'x) (Div (Id 'x) (Num 2)))))
(test (run "{with {x 0}
 {if {> x 0} {/ 2 x} x}}")
 =error> "parse-sexpr: bad `if' syntax in (if (> x 0) (/ 2 x) x)")
 (test (run "true") =error> "eval: free identifier: true")
(test (run "{< false 5}") =error> "eval: free identifier: false")
(test (run "{< False 5}")
 =error> "Num->number: expected a number, got: #(struct:Bool #f)")

;;TEST-constructors
(test (Add (Num 1) (Num 1)) => (Add (Num 1) (Num 1)))
(test (Sub   (Num 1) (Num 1)) => (Sub   (Num 1) (Num 1)))
(test (Mul (Num 1) (Num 1)) => (Mul (Num 1) (Num 1)))
(test (Div (Num 1) (Num 1)) => (Div (Num 1) (Num 1)))
(test (Id 'eval) => (Id 'eval))
(test (With 'razgal100 (Num 1) (Num 1)) => (With 'razgal100 (Num 1) (Num 1)))
(test (Fun  'razgal100 (Num 1)) => (Fun 'razgal100 (Num 1)))
(test (Call (Num 1) (Num 1)) => (Call (Num 1) (Num 1)))
(test (Bigger (Num 2) (Num 1)) => (Bigger (Num 2) (Num 1)))
(test (Smaller (Num 2) (Num 1)) => (Smaller (Num 2) (Num 1)))
(test (Equal (Num 1) (Num 1)) => (Equal (Num 1) (Num 1)))
(test (Not (Bool  #t)) => (Not (Bool  #t)))
(test (If (Bool #t) (Bool #t) (Bool #t)) => (If (Bool #t) (Bool #t) (Bool #t)))

;;TEST-parse-sexpr
(test (parse-sexpr "1") =error>  "parse-sexpr: bad syntax in")
(test (parse-sexpr "100") =error>  "parse-sexpr: bad syntax in")
(test (parse-sexpr 1) =>  (Num 1))
(test (parse-sexpr 'True) =>  (Bool true))
(test (parse-sexpr 'False) =>  (Bool false))
(test (parse-sexpr 'eval) =>  (Id 'eval))
(test (parse-sexpr {+ 1 2}) =>   (Num 3))
(test (parse-sexpr {- 1 1}) =>   (Num 0))
(test (parse-sexpr {* 1 2}) =>   (Num 2))
(test (parse-sexpr {/ 1 1}) =>   (Num 1))
(test (parse-sexpr (list '= 1 2) )=> (Equal (Num 1) (Num 2)))
(test (parse-sexpr (list '< 1 2)) =>   (Smaller (Num 1) (Num 2)))

;;TEST-parse
(test (parse "1") =>  (Num 1))
(test (parse "{+ 1 2}") =>  (Add (Num 1) (Num 2)))
(test (parse "{- 1 2}") =>  (Sub (Num 1) (Num 2)))
(test (parse "{* 1 2}") =>  (Mul (Num 1) (Num 2)))
(test (parse "{/ 1 2}") =>  (Div (Num 1) (Num 2)))

;;TEST-parse-from lecture
(test (parse "4") => (Num 4))
(test (parse "{+ 3 5}") => (Add (Num 3) (Num 5)))
(test (parse "{+ 3 {- 8 {+ 2 1}}}") => (Add (Num 3) (Sub (Num 8) (Add (Num 2) (Num 1)))))
(test (parse "{+ 1 2 3}") =error> "bad syntax")

(test (parse "{with {x {+ 4 2}} {* x x}}") => (With 'x (Add (Num 4) (Num 2))
                                              (Mul (Id 'x) (Id 'x))))

(test (parse "{fun {x} x}") => (Fun 'x (Id 'x)))
(test (parse "{fun {x} {/ x 5}}") => (Fun 'x (Div (Id 'x) (Num 5))))
(test (parse "{call {fun {x} {/ x 5}} 8}") => (Call {Fun 'x (Div (Id 'x) (Num 5))} (Num 8)))
(test (parse "{with {sqr {fun {x} {* x x}}}
                                    {+ {call sqr 5}
                                        {call sqr 6}}}") =>
                 (With 'sqr (Fun 'x (Mul (Id 'x) (Id 'x)))
                       (Add (Call (Id 'sqr) (Num 5))
                            (Call (Id 'sqr) (Num 6)))))
(test (parse "{fun x {* x x}}")=error> "bad `fun' syntax in ")


;;TEST-subst-from lecture
(test (subst (Mul (Id 'x) (Id 'x)); ==> e
       'x ;==> i
       (Num 6) ;==> v
       ) => (Mul (Num 6) (Num 6)))


(test (subst (Id 'x)
             'x
             (Num 8)) => (Num 8))

(test (subst (Id 'y)
             'x
             (Num 8)) => (Id 'y))
(test (subst (With 'x (Num 3)
                   (Id 'x))
             'x
             (Num 5)) => (With 'x (Num 3)
                   (Id 'x)))
(test (subst (With 'y
                   (Add (Id 'x) (Num 3))
                   (Add (Id 'x) (Num 5)))
             'x
             (Num 4)) => (With 'y
                               (Add (Num 4) (Num 3))
                               (Add (Num 4) (Num 5))))

(test (subst (Fun 'x (Add (Id 'x) (Id 'y)))
             'x
             (Num 4)) => (Fun 'x (Add (Id 'x) (Id 'y))))

(test (subst (Fun 'x (Add (Id 'x) (Id 'y)))
             'y
             (Num 4)) => (Fun 'x (Add (Id 'x) (Num 4))))
(test (subst (Call (Fun 'x (Div (Id 'x) (Id 'y)))
                   (Add (Id 'x) (Id 'y)))
                   'x
                   (Num 3)) => (Call (Fun 'x (Div (Id 'x) (Id 'y)))
                   (Add (Num 3) (Id 'y))))

(test (subst (Call (Fun 'x (Div (Id 'x) (Id 'y)))
                   (Add (Id 'x) (Id 'y)))
                   'y
                   (Num 3)) => (Call (Fun 'x (Div (Id 'x) (Num 3)))
                   (Add (Id 'x) (Num 3))))

;;TEST - flang->bool
(test (flang->bool (Bool true))=> true)
(test (flang->bool (Bool false))=> false)
(test (flang->bool (eval (parse "{> 2 2}")))=> false)
(test (flang->bool (eval (Smaller (Num 1) (Num 1)) ))=> false)
(test (flang->bool (eval (Equal (Num 1) (Num 1)) ))=> true)
(test (flang->bool (eval (Equal (Num 1) (Num 2)) ))=> false)
(test (flang->bool (eval (Bigger (Num 1) (Num 2)) ))=> false)

;;TEST - Num->number
(test (Num->number (Num 4))=> 4)
(test (Num->number (Num 123))=> 123)
(test (Num->number (Bool true)) =error> "Num->number: expected a number, got: ")
(test (Num->number (Bool true))=error>  "Num->number: expected a number, got: ")

;;TEST -logic-op
(test (logic-op = (Num 2) (Num 1)) => (Bool false))
(test (logic-op = (Num 1) (Num 1)) => (Bool true))

;;TEST -arith-op
(test (arith-op + (Num 1) (Num 1)) => (Num 2))
(test (arith-op - (Num 1) (Num 1)) => (Num 0))
(test (arith-op * (Num 1) (Num 1)) => (Num 1))
(test (arith-op / (Num 1) (Num 1)) => (Num 1))

;;TEST - eval
(test (eval (Num 1)) => (Num 1))
(test (eval (Num 2)) => (Num 2))
(test (eval (Bool true)) => (Bool true))
(test (eval (Bool false)) => (Bool false))
(test (eval (Not(Bool false))) => (Bool true))
(test (eval (Not(Bool true))) => (Bool false))
(test (eval ( parse "{= 1 2}"))=> (Bool false))
(test (eval ( parse "{= 2 2}"))=> (Bool true))
(test (eval (Add (Num 1) (Num 1)))=> (Num 2))
(test (eval (Mul (Num 1) (Num 1)))=> (Num 1))

;;TEST - run
(test (run "1") => 1)
(test (run "{= 1 1}") => true)
(test (run "{fun {eval} sqr}") => (Fun 'eval (Id 'sqr)))
(test (run "{fun {eval} eval}") => (Fun 'eval (Id 'eval)))
(test (run "{Fun {+ 1 1} 2}") =error> "bad syntax in ")