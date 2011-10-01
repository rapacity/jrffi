#lang racket/base

(require rackunit "require.rkt" "../jvector.rkt" "../core.rkt")

(define/provide-test-suite auto-tests "Tests for automated binding"
  
  (test-case "jvector + require method"
    (define x (make-jvector _jboolean 2))
    (jvector-set! x 0 #f)
    (jvector-set! x 1 #t)
    (check-equal? (Arrays-toString x) "[false, true]"))
  
  (test-case "require constructor/method"
    (check-equal? (Integer-toString (new-Integer "444")) "444"))
  
  (test-case "require field-accessor"
    (check-equal? (get-Integer-MAX_VALUE) 2147483647))
  
  (test-case "require constructor/method"
    (check-equal? (Integer-toString (new-Integer "444")) "444"))
  
  (test-case "require field-accessor"
    (check-equal? (get-Integer-MAX_VALUE) 2147483647))
  
  (test-case "require constructor/method #:rktfy"
    (check-equal? (integer->string (make-integer 100)) "100"))
  
  (test-case "require static method #:rktfy"
    (check-equal? (integer->string 666) "666"))
  
  (test-case "require field-accessor #:rktfy"
    (check-equal?  (get-integer-max-value) 2147483647))
  
  (test-case "require predicate? #:rktfy"
    (check-true (integer? (make-integer 100))))
  
  (test-case "require class #:rktfy"
    (check-true (jtype? integer)))
  
  )