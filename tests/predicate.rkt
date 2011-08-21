#lang racket
(require rackunit)

(require "../main.rkt")

(define _int (_jobject "java/lang/Integer"))
(define int? (jtype-predicate _int))

(test-true "jtype predicate, true" (int? (new-integer 33333)))
(test-false "jtype predicate, false" (int? (new-string "fffff")))

(define _jdouble-list  (_jlist _jdouble))
(define _jdouble-list?  (jtype-predicate _jdouble-list))

(test-true "jlist predicate, true" (_jdouble-list? (map real->double-flonum (list 1 2 4))))


