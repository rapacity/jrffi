#lang racket

(require rackunit "require.rkt" "../jvector.rkt" "../core.rkt")

(define x (make-jvector _jboolean 2))
(jvector-set! x 0 #f)
(jvector-set! x 1 #t)

(test-equal? "jvector + require method" (Arrays-toString x) "[false, true]")
(test-equal? "require constructor/method" (Integer-toString (new-Integer "444")) "444")
(test-equal? "require field-accessor" (get-Integer-MAX_VALUE) 2147483647)
(test-equal? "require constructor/method" (Integer-toString (new-Integer "444")) "444")
(test-equal? "require field-accessor" (get-Integer-MAX_VALUE) 2147483647)
(test-equal? "require constructor/method #:rktfy" (integer->string (make-integer 100)) "100")
(test-equal? "require static method #:rktfy" (integer->string 666) "666")
(test-equal? "require field-accessor #:rktfy" (get-integer-max-value) 2147483647)
(test-true "require predicate? #:rktfy" (integer? (make-integer 100)))
(test-true "require class #:rktfy" (jtype? integer))
