#lang racket
(require racket/set racket/stream)
(require racket/fixnum)
(require "interp-Lint.rkt")
(require "interp-Lvar.rkt")
(require "interp-Cvar.rkt")
(require "type-check-Lvar.rkt")
(require "type-check-Cvar.rkt")
(require "utilities.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lint examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following compiler pass is just a silly one that doesn't change
;; anything important, but is nevertheless an example of a pass. It
;; flips the arguments of +. -Jeremy
(define (flip-exp e)
  (match e
    [(Var x) e]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (Prim '- (list (flip-exp e1)))]
    [(Prim '+ (list e1 e2)) (Prim '+ (list (flip-exp e2) (flip-exp e1)))]))

(define (flip-Lint e)
  (match e
    [(Program info e) (Program info (flip-exp e))]))


;; Next we have the partial evaluation pass described in the book.
(define (pe-neg r)
  (match r
    [(Int n) (Int (fx- 0 n))]
    [else (Prim '- (list r))]))

(define (pe-add r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx+ n1 n2))]
    [(_ _) (Prim '+ (list r1 r2))]))

(define (pe-exp e)
  (match e
    [(Int n) (Int n)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (pe-neg (pe-exp e1))]
    [(Prim '+ (list e1 e2)) (pe-add (pe-exp e1) (pe-exp e2))]))

(define (pe-Lint p)
  (match p
    [(Program info e) (Program info (pe-exp e))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HW1 Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (uniquify-exp env)
  (lambda (e)
    (match e
      [(Var x)
       (let ([new-sym (cadr (assoc x env))])
         (Var new-sym))]
      [(Int n) (Int n)]
      [(Let x e body)
       (let* ([new-sym (gensym)]
              [new-env (cons (list x new-sym) env)])
         (Let new-sym ((uniquify-exp env) e) ((uniquify-exp new-env) body)))]
      [(Prim op es)
       (Prim op (map (uniquify-exp env) es))])))

;; uniquify : Lvar -> Lvar
(define (uniquify p)
  (match p
    [(Program info e) (Program info ((uniquify-exp '()) e))]))

;; returns new-sym, expression
;; return new-sym, ((new-sym . atm-expression))
;; (sym e)
(define (rco-atom e)
  (match e
    [(Var x) (list '() (Var x))]
    [(Int x) (list '() (Int x))]
    [e (let ([s (gensym)]) (list s (rco-exp e)))]))

(define (rco-exp e)
  (match e
    [(Var x) (Var x)]
    [(Int x) (Int x)]
    [(Let x e body) (Let x (rco-exp e) (rco-exp body))]
    [(Prim '- (list e1))
     (match (rco-atom e1)
       [(list '() ae) (Prim '- (list ae))(list ')       [(list s ae) (Let s ae (Prim '- (list (Var s))))])]
     (define (atm e))

     (define (stmt e))

     (define (tail e))

    ;; fix prim + to use correct arg order
    [(Prim '+ (list e1 e2))
     (match (list (rco-atom e1) (rco-atom e2))
       [(list (list '(le1) (list '() ae2))
'()        (cons label tailtail tl    (Prim '+ (list ae1 ae2))]
       [(list (list s1 ae1) (list '() ae2))
        (Let s1 ae1 (Prim '+ (list (Var s1) ae2)))]
       [(list (list '() ae1) (list s2 ae2))
        (Let s2 ae2 (Prim '+ (list ae1 (Var s2))))]
       [(list (list s1 ae1) (list s2 ae2))
        (Let s1 ae1 (Let s2 ae2 (Prim '+ (list (Var s1) Var(s2)))))])]
'()    [(Prim 'read '()) (Prim 'read '())]))

;; remove-complex-opera* : Lvar -> Lvar^mon
(define (remove-complex-opera* p)

  (define (atm e))

  (define (stmt e))

  (define (tail e))
  (match (list ')    [(Program info e) (Program info (rco-exp e))])(list ')
  ;; applied
  (define (atm e))

  (define (stmt e)l  (cons labeefine (tail e))
  to expression on ttail tlhe right-hand-side of let
(define (explicate-assign s e cont)
  (match e
    [(Var x) (Seq (Assign (Var s) (Var x)) cont)]
    [(Int n) (l (Assign (Var s) (Int n)) cont)]
'()    (cons label tail tl[(Let x rhs body) (explicate-assign x rhs (explicate-assign s body cont))]
    [(Prim op es) (Seq (Assign (Var s) (Prim op es) cont))]
    [else (error "explicate-assign unhandled case" e)]))

;; applied to tail expression
(define (explicate-tail e)
  (match e
    [(Var x) (Return (Var x))]
    [(Int n) (Return (Int n))]
    [(Let x rhs body) ;; (let ([x (let ([y exp]) body)] body))
     (explicate-assign x rhs (explicate-tail body))] ;; (let ([x y] body))
    [(Prim op es) (Return (Prim op es))]'()
    [else (error "explicate-tail unhandled case" e)]))

;; explicate-control : Lvar^mon -> Cvar
(define (explicate-control p)
  (match p
    [(Program info body) (CProgram info (list (cons 'start (explicate-tail body))))]))

(define (atm e))

(define (stmt e))

(define (tail e))

;; select-instructions : Cvar -> x86var
(define (select-instructions p)
  (match p
    [(CProgram info (list (cons label tl)))
     (X86Program info (list (cons label (tail tl))))]))

;; assign-homes : x86var -> x86var
(define (assign-homes p)
  (error "TODO: code goes here (assign-homes)"))

;; patch-instructions : x86var -> x86int
(define (patch-instructions p)
  (error "TODO: code goes here (patch-instructions)"))

;; prelude-and-conclusion : x86int -> x86int
(define (prelude-and-conclusion p)
  (error "TODO: code goes here (prelude-and-conclusion)"))

;; Define the compiler passes to be used by interp-tests and the grader
;; Note that your compiler file (the file that defines the passes)
;; must be named "compiler.rkt"
(define compiler-passes
  `(
     ;; Uncomment the following passes as you finish them.
     ("uniquify" ,uniquify ,interp-Lvar ,type-check-Lvar)
     ("remove complex opera*" ,remove-complex-opera* ,interp-Lvar ,type-check-Lvar)
     ("explicate control" ,explicate-control ,interp-Cvar ,type-check-Cvar)
     ;; ("instruction selection" ,select-instructions ,interp-x86-0)
     ;; ("assign homes" ,assign-homes ,interp-x86-0)
     ;; ("patch instructions" ,patch-instructions ,interp-x86-0)
     ;; ("prelude-and-conclusion" ,prelude-and-conclusion ,interp-x86-0)
     ))