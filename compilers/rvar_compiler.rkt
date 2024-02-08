#lang racket

(provide Int Prim Program pe-Rvar interp-Rvar)

(require racket/dict)
(require racket/fixnum)

(require "public-student-support-code/utilities.rkt")

;; (struct Int (value))
;; (struct Prim (op args))
;; (struct Program (info body))
;; (struct Var (var))
;; (struct Let (var exp exp))

(define interp-Rvar-class
  (class object%
    (super-new)

    (define/public ((interp-exp env) e)
      (match e
        [(Int n) n]
        [(Prim 'read '())
         (define r (read))
         (cond [(fixnum? r) r]
               [else (error 'interp-exp "read expected an integer" r)])]
        [(Prim '- (list e)) (fx- 0 ((interp-exp env) e))]
        [(Prim '+ (list e1 e2))
         (fx+ ((interp-exp env) e1) ((interp-exp env) e2))]
        [(Var x) (dict-ref env x)]
        [(Let x e body)
         (define new-env (dict-set env x ((interp-exp env) e)))
         ((interp-exp new-env) body)]))

    (define/public (interp-program p)
      (match p
        [(Program '() e) ((interp-exp '()) e)]))))

(define (interp-Rvar p)
  (send (new interp-Rvar-class) interp-program p))

(define (pe-neg r)
  (match r
    [(Int n) (Int (fx- 0 n))]
    [else (Prim -' (list r))]))

(define (pe-add r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx+ n1 n2))]
    [(_ _) (Prim '+ (list r1 r2))]))

(define (pe-exp e)
  (match e
    [(Int n) (Int n)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (pe-neg (pe-exp e1))]
    [(Prim '+ (list e1 e2)) (pe-add (pe-exp e1) (pe-exp e2))]
    [(Var x) (Var x)]
    [(Let x e body) (Let x (pe-exp e) (pe-exp body))]))

(define (pe-Rvar p)
  (match p
    [(Program '() e) (Program '() (pe-exp e))]))
