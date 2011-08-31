#lang racket

(require rackunit (except-in "../main.rkt" Integer) "../auto.rkt")


(jrequire java.util.Arrays java.lang.Integer)


(define x (make-jvector _jboolean 2))
(jvector-set! x 0 #f)
(jvector-set! x 1 #t)

(test-equal? "jrequire method" "[false, true]" (java.util.Arrays-toString x))

(test-equal? "jrequire constructor/method" "444" (java.lang.Integer-toString (new-java.lang.Integer "444")))

(test-equal? "jrequire field-accessor" 2147483647 (get-java.lang.Integer-MAX_VALUE))

(jrequire java.lang.Integer #:import)

(test-equal? "jrequire constructor/method #:import" "444" (Integer-toString (new-Integer "444")))
(test-equal? "jrequire field-accessor #:import" 2147483647 (get-Integer-MAX_VALUE))

(jrequire java.lang.Integer #:import #:racketify)


(test-equal? "jrequire constructor/method #:import #:racketify" "100"
             (integer->string (make-integer 100)))
(test-equal? "jrequire field-accessor #:import #:racketify" 2147483647
             (get-integer-max-value))
(test-true "jrequire predicate? #:import #:racketify"
           (integer? (make-integer 100)))

(require ffi/unsafe)

(test-true "jrequire class #:import #:racketify"
           (cpointer-has-tag? integer 'jclass))