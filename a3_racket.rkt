#lang racket


;; ----- data structures -----
(struct Closure (fun env))

;; ----- untyped term
(define term%
  (class object%
    (super-new)
   )
 )

(define INT%
  (class term%
    (init v)
    (super-new)
    ;(define value v)
    (field [value v])
    (define/public (getValue)
      value)
  )
 )

(define BOOL%
  (class term%
    (init v)
    (super-new)
    (field [value v])
    (define/public (getValue)
      value)
  )
 )

(define VAR%
  (class term%
    (init n)
    (super-new)
    (field [name n])
    (define/public (getName)
      name)
  )
 )

(define FUN%
  (class term%
    (init p b)
    (super-new)
    (field [param p] [body b])
    ;(define body b)
  )
 )

(define APP%
  (class term%
    (init f a)
    (super-new)
    (field [fun f] [arg a])
    ;(define body b)
  )
 )

(define CALC%
  (class term%
    (init o a1 a2)
    (super-new)
    (field [op o] [arg1 a1] [arg2 a2])
    ;(define body b)
  )
 )

;; ----- typed term
(define typedTerm%
  (class object%
    (super-new)
   )
 )

(define T_INT%
  (class typedTerm%
    (init t v)
    (super-new)
    ;(define value v)
    (field [ty t] [value v])
    (define/public (getValue)
      value)
  )
 )

(define T_BOOL%
  (class typedTerm%
    (init t v)
    (super-new)
    ;(define value v)
    (field [ty t] [value v])
    (define/public (getValue)
      value)
  )
 )

(define T_VAR%
  (class typedTerm%
    (init t n)
    (super-new)
    (field [ty t] [name n])
    (define/public (getName)
      name)
  )
 )

(define T_FUN%
  (class typedTerm%
    (init t p b)
    (super-new)
    (field [ty t] [param p] [body b])
    ;(define body b)
  )
 )

(define T_APP%
  (class typedTerm%
    (init t f a)
    (super-new)
    (field [ty t] [fun f] [arg a])
    ;(define body b)
  )
 )

(define T_CALC%
  (class term%
    (init t o a1 a2)
    (super-new)
    (field [ty t] [op o] [arg1 a1] [arg2 a2])
    ;(define body b)
  )
 )

; ----- type
(define Type%
  (class object%
    (super-new)
   )
 )

(define TyInt%
  (class Type%
    (super-new)
  )
 )

(define TyBool%
  (class Type%
    (super-new)
  )
 )

(define TyFun%
  (class Type%
    (init t1 t2)
    (super-new)
    (field [paramTy t1] [returnTy t2])
  )
 )

(define TyVar%
  (class Type%
    (init n)
    (super-new)
    (field [num n])
  )
 )

;----- type environment
(define TypeEnv%
  (class object%
    (init [e (make-hash)])
    (super-new)
    (field [Env e])
    (define/public (set k v)
      (hash-set! Env k v)
     )
    (define/public (get k)
      (hash-ref Env k)
     )
  )
 )

;----- type reconstruction
(define (reconstruct term tenv)
    (cond [(is-a? term INT%)
           (new T_INT% [t (send fresher1 freshVar)] [v (get-field value term)])
          ]
      [(is-a? term BOOL%)
           (new T_BOOL% [t (send fresher1 freshVar)] [v (get-field value term)])
          ]
      [(is-a? term FUN%)
           (define paramTy (send fresher1 freshVar))
           (send tenv set (get-field param term) paramTy)
           (new T_FUN% [t (send fresher1 freshVar)]
                [p (new T_VAR% [t paramTy] [n (get-field param term)])]
                [b (reconstruct (get-field body term) tenv)]
            )
          ]
      [(is-a? term VAR%)
           (new T_VAR% [t (send tenv get (get-field name term))] [n (get-field name term)])
          ]
      [(is-a? term APP%)
           (new T_APP% [t (send fresher1 freshVar)] [f (reconstruct (get-field fun term) tenv)] [a (reconstruct (get-field arg term) tenv)])
          ]
      [(is-a? term CALC%)
           (new T_CALC% [t (send fresher1 freshVar)] [o (get-field op term)] [a1 (reconstruct (get-field arg1 term) tenv)] [a2 (reconstruct (get-field arg2 term) tenv)])
          ]
      [else 'err]
     )
 )
; ----- get a new var type
(define fresher%
  (class object%
    (init [n 0])
    (super-new)
    (field [num n])
    (define/public (freshVar)
      (set! num (+ num 1))
      ;(define newVar(new TyVar% [n num]))
      ;(newVar))
      (new TyVar% [n num])
     )
   )
 )

;----- constraint
(define Constraint%
  (class object%
    (init a b)
    (super-new)
    (field [typeA a] [typeB b])
  )
 )

(define (collect term)
    (cond [(is-a? term T_INT%)
           (define c1 (new Constraint% [a (get-field ty term)] [b (new TyInt%)]))
           (list c1)
          ]
      [(is-a? term T_BOOL%)
       (define c1 (new Constraint% [a (get-field ty term)] [b (new TyBool%)]))
       (list c1)
          ]
      [(is-a? term T_FUN%)
       (define cSet1 (append (collect (get-field body term)) (collect (get-field param term))))
       (define c1 (new Constraint% [a (get-field ty term)] [b (new TyFun% [t1 (get-field ty (get-field param term))] [t2 (get-field ty (get-field body term))])]))
       (append cSet1 (list c1))
          ]
      [(is-a? term T_VAR%)
           '()
          ]
      [(is-a? term T_APP%)
           (define cSet1 (append (collect (get-field fun term)) (collect (get-field arg term))))
           (define c1 (new Constraint% [a (get-field ty (get-field fun term))] [b (new TyFun% [t1 (get-field ty (get-field arg term))] [t2 (get-field ty term)])]))
           (append cSet1 (list c1))
          ]
      [(is-a? term T_CALC%)
           (define cSet1 (append (collect (get-field arg1 term)) (collect (get-field arg2 term))))
           (define c1 (new Constraint% [a (get-field ty term)] [b (get-field ty (get-field arg1 term))]))
           (define c2 (new Constraint% [a (get-field ty term)] [b (get-field ty (get-field arg2 term))]))
           (define c3 (new Constraint% [a (get-field ty term)] [b (new TyInt%)]))
           (define cSet2 (list c1 c2 c3))
           (append cSet1 cSet2)
          ]
      [else 'err]
     )
 )

;----- substitution
(define Substitution%
  (class object%
    (init [s (make-hash)])
    (super-new)
    (field [solutions s])
    (define/public (applyToSet constraints)
      (define newSet '())
      (for ([t (in-list constraints)])
        (define cNew (new Constraint% [a (applyOneType (get-field typeA t))] [b (applyOneType (get-field typeB t))]))
        (set! newSet (append newSet (list cNew)))
        )
     newSet)
    (define/public (applyOneType ty)
      (cond [(is-a? ty TyInt%) ty]
            [(is-a? ty TyBool%) ty]
            [(is-a? ty TyFun%)
             (new TyFun% [t1 (applyOneType (get-field paramTy ty))] [t2 (applyOneType (get-field returnTy ty))])]
            [(is-a? ty TyVar%)
             (define replaceTy ty)
             (for ([(k v) (in-hash solutions)])
               (when (= k (get-field num ty))
                   (set! replaceTy v)
                 )
               )
             replaceTy]
       )
     )
    (define/public (compose other)
      (for ([(k v) (in-hash solutions)])
        (hash-set! solutions k (send other applyOneType v)))
      (for ([(k v) (in-hash (get-field solutions other))])
        (hash-set! solutions k v))
      (define s1 (new Substitution% [s solutions]))
     s1)
  )
)

;-----generate a solution
(define (fromPair t1 t2)
  (define solution (make-hash))
  (hash-set! solution t1 t2)
  (new Substitution% [s solution])
 )

;----- unification
(define (unify constraints)
  (cond
      [(null? constraints) (new Substitution%)]
      [else (define solutionOne (unifyOne (first constraints)))
            (define updatedConstraints (send solutionOne applyToSet (rest constraints)))
            (define solutionRest (unify updatedConstraints))
            (send solutionOne compose solutionRest)
       ]
   )
 )

;unify one constraint.
(define (unifyOne constraint)
  (define tyA (get-field typeA constraint))
  (define tyB (get-field typeB constraint))
  (cond
      [(and (is-a? tyA TyInt%) (is-a? tyB TyInt%)) (new Substitution%)]
      [(and (is-a? tyA TyBool%) (is-a? tyB TyBool%)) (new Substitution%)]
      [(and (is-a? tyA TyFun%) (is-a? tyB TyFun%))
       (define c1 (new Constraint% [a (get-field paramTy tyA)] [b (get-field paramTy tyB)]))
       (define c2 (new Constraint% [a (get-field returnTy tyA)] [b (get-field returnTy tyB)]))
       (unify (list c1 c2))]
      [(is-a? tyA TyVar%) (unifyVar (get-field num tyA) tyB)]
      [(is-a? tyB TyVar%) (unifyVar (get-field num tyB) tyA)]
      [else (error "wrong!")]
   )
 )

(define (unifyVar tvar ty)
  (cond
      [(is-a? ty TyVar%)
       (cond
         [(= tvar (get-field num ty)) (new Substitution%)]
         [else (fromPair tvar ty)]
        )]
      [else
       (cond
         [(occurs tvar ty) (error "wrong!")]
         [else (fromPair tvar ty)]
        )]
   )
 )

;occurs check
(define (occurs tvar ty)
  (cond
    [(is-a? ty TyFun%) (or (occurs tvar (get-field paramTy ty)) (occurs tvar (get-field returnTy ty)))]
    [(is-a? ty TyVar%) (= tvar (get-field num ty))]
    [else #f]
   )
 )

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

;; ----- interpreter -----
(define r2
  (lambda (exp)
    (interp exp env0)))

(define interp
  (lambda (exp env)
    (match exp
      [(? symbol? x)
       (let ([v (lookup x env)])
         (cond
          [(not v)
           (error "undefined variable" x)]
          [else v]))]
      [(? number? x) x]
      [`(lambda (,x) ,e)
       (Closure exp env)]
      [`(let ([,x ,e1]) ,e2)
       (let ([v1 (interp e1 env)])
         (interp e2 (ext-env x v1 env)))]
      [`(,e1 ,e2)
       (let ([v1 (interp e1 env)]
             [v2 (interp e2 env)])
         (match v1
           [(Closure `(lambda (,x) ,e) env-save)
            (interp e (ext-env x v2 env-save))]))]
      [`(,op ,e1 ,e2)
       (let ([v1 (interp e1 env)]
             [v2 (interp e2 env)])
         (match op
           ['+ (+ v1 v2)]
           ['- (- v1 v2)]
           ['* (* v1 v2)]
           ['/ (/ v1 v2)]))])))


;; ----- environment for interp -----
(define env0 '())

(define ext-env
  (lambda (x v env)
    (cons `(,x . ,v) env)))

(define lookup
  (lambda (x env)
    (let ([p (assq x env)])
      (cond
       [(not p) #f]
       [else (cdr p)]))))