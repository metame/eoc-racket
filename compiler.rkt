#lang racket

(provide Int Prim Program)

(struct Int (value))
(struct Prim (op args))
(struct Program (info body))
