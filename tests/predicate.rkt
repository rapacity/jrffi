#lang racket
(require rackunit)

(require "../main.rkt")

(define _int (_jobject "java/lang/Integer"))
(define int? (jtype-predicate _int))

(test-true "jtype predicate, true" (int? (new-integer 33333)))
(test-false "jtype predicate, false" (int? (new-string "fffff")))




