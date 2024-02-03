#lang racket

(require racket/fixnum)
(require "compiler.rkt")

(define rd (Prim 'read '()))

(define eight (Int 8))
(define neg-eight (Prim '- (list eight)))
(define ast1.1 (Prim '+ (list rd neg-eight)))

(match ast1.1
  [(Prim op (list child child2))
   ;; book has (print op) but I'd rather just return value here
   op])

(define (leaf? arith)
  (match arith
    [(Int n) #t]
    [(Prim 'read '()) #t]
    [(Prim '- (list e1)) #f]
    [(Prim '+ (list e1 e2)) #f]))

(leaf? (Prim 'read '()))
(leaf? (Prim '- (list (Int 8))))
(leaf? (Int 8))

(define (exp? ast)
  (match ast
    [(Int n) #t]
    [(Prim 'read '()) #t]
    [(Prim '- (list e)) (exp? e)]
    [(Prim '+ (list e1 e2))
     (and (exp? e1) (exp? e2))]
    [else #f]))

(define (Rint? ast)
  (match ast
    [(Program '() e) (exp? e)]
    [else #f]))

(Rint? (Program '() ast1.1))
(Rint? (Program '()
                (Prim '- (list (Prim 'read '())
                               (Prim '+ (list (Int 8)))))))

(define (interp-exp e)
  (match e
    [(Int n) n]
    [(Prim 'read '())
     (define r (read))
     (cond [(fixnum? r) r]
           [else (error 'interp-exp "read expected an integer" r)])]
    [(Prim '- (list e))
     (define v (interp-exp e))
     (fx- 0 v)]
    [(Prim '+ (list e1 e2))
     (define v1 (interp-exp e1))
     (define v2 (interp-exp e2))
     (fx+ v1 v2)]))

(define (interp-Rint p)
  (match p
    [(Program '() e) (interp-exp e)]))
