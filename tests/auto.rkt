#lang racket

(require rackunit "../main.rkt" (java java/util/Arrays java/lang/Integer))

; currently broken jrequire deprecated
#|
(define x (make-jvector _jboolean 2))
(jvector-set! x 0 #f)
(jvector-set! x 1 #t)

(test-equal? "jrequire method" "[false, true]" (java.util.Arrays-toString x))

(test-equal? "jrequire constructor/method" "444" (java.lang.Integer-toString (new-java.lang.Integer "444")))

(test-equal? "jrequire field-accessor" 2147483647 (get-java.lang.Integer-MAX_VALUE))

(test-equal? "jrequire constructor/method #:import" "444" (Integer-toString (new-Integer "444")))
(test-equal? "jrequire field-accessor #:import" 2147483647 (get-Integer-MAX_VALUE))


(test-equal? "jrequire constructor/method #:import #:racketify" "100"
             (integer->string (make-integer 100)))

(test-equal? "jrequire static method #:import #:racketify" "666"
             (integer->string 666))

(test-equal? "jrequire field-accessor #:import #:racketify" 2147483647
             (get-integer-max-value))
(test-true "jrequire predicate? #:import #:racketify"
           (integer? (make-integer 100)))

(test-true "jrequire class #:import #:racketify"
           (jtype? integer))
|#