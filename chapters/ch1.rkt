#lang racket

(provide ast1.1)

(require "compiler.rkt")
(require (prefix-in u: "public-student-support-code/utilities.rkt"))

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

(println (pe-Rint (Program '() (Prim '+ (list (Int 10) (Int 32))))))

(define (compile filename)
  (let* ([ast (u:read-program "program.rkt")]
         [_ (println ast)]
         [out (pe-Rint ast)])
    out))

(compile "program.rkt")
