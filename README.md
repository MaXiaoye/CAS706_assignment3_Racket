# CAS706_assignment3_Racket

Assignment 3
Typed Lambda-calculus interpreter.
Your term language should be based on the same term language as A2. The operational semantics is the same as for A2, and the same as in the textbook.

You should first write down the typing rules for your language. You are allowed, even highly encouraged, to improve the A2 grammar to make your life easier, so that typing rules can be made simpler. Your task is to have your interpreter first do type reconstruction for all inputs before any reduction is done. Decent error messages should be returned for untypable terms. The interpreter should then proceed as in assignment 2.

You should also be providing some test cases for your interpreter - make sure to test higher-order functions as well as cases that do not type (such as old stuck cases from assignment 2) and cases that work but are rejected in your typed language.

You should try to leverage the host system as much as you can for this assignment. This can mean having the type inference algorithm return a typed term, from a different ADT, than the original source. Then your interpreter can be much much simpler (and it should be) because all the 'impossible' cases have been removed already.

In other words, you are writing a type inference routine as the most important part of this assignment. The unification algorithm is thus key.

Bonus: implement an interpreter for your language but using the finally tagless method (in whatever language you want, but this is easiest in Haskell/Scala/Ocaml; more bonus for more languages. Can even be done in Agda/Idris). Note that this is cheating, in the sense that even though it is typed, the types are maintained by the host language, so that there is no need to implement unification (or even a separate typing pass) at all!

-----------------------------------

Again, the same procedure and method to do type inference:
Just run a3_racket.rkt

(define t2 (reconstruct f3 e1))
(define cSet2 (collect t2))
(define sSet2 (unify cSet2))

Example: Take the same test case: \y.\x. x+y

(define e1
  (new TypeEnv%))

(define i1
  (new INT% [v 10]))

(define v1
  (new VAR% [n "x"]))

(define v2
  (new VAR% [n "y"]))

(define c1
  (new CALC% [o "+"] [a1 v1] [a2 i1]))

(define c2
  (new CALC% [o "+"] [a1 v1] [a2 v2]))

(define fresher1
  (new fresher%))

(define f1
  (new FUN% [p "x"] [b c1]))

(define f2
  (new FUN% [p "x"] [b c2]))

(define f3
  (new FUN% [p "y"] [b f2]))

(define a1
  (new APP% [f f1] [a i1]))

(define t1 (reconstruct a1 e1))

(define cSet1 (collect t1))

(define sSet1 (unify cSet1))

(define t2 (reconstruct f3 e1))

(define cSet2 (collect t2))

(define sSet2 (unify cSet2))

Racket has interact section. So we can do test here.
Here I show only 2 test cases or the readme file will be too long . Actually type inference works well for other test cases. 
;This line shows type of \y.\x. x+y is TyFun
> (display (send sSet2 applyOneType (get-field ty t2)))
#(struct:object:TyFun% ...)
;This line shows return type of \y.\x. x+y is still a TyFun function2
> (display (get-field returnTy (send sSet2 applyOneType (get-field ty t2))))
#(struct:object:TyFun% ...)

;These line shows type of function2 is TyInt -> TyInt
> (display (get-field returnTy (get-field returnTy (send sSet2 applyOneType (get-field ty t2)))))
#(struct:object:TyInt% ...)
> (display (get-field paramTy (get-field returnTy (send sSet2 applyOneType (get-field ty t2)))))
#(struct:object:TyInt% ...)
>  
