#lang racket
(require rackunit)

(require "../main.rkt"  (java java/lang/Integer))


(test-true "jtype predicate, true" (Integer? (new-Integer 33333)))
(test-false "jtype predicate, false" (Integer? (new-string "fffff")))

(define _jdouble-list  (_jlist _jdouble))
(define _jdouble-list?  (jtype-predicate _jdouble-list))

(test-true "jlist predicate, true" (_jdouble-list? (map real->double-flonum (list 1 2 4))))


