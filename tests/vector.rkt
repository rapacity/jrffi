#lang racket/base
(require rackunit "../base.rkt")

(define/provide-test-suite vector-tests "Tests for automated binding"
  (test-case "homogeneous jobject-vector"
    (define vec (make-jobject-vector 3 _jstring))
    (jobject-vector-set! vec 2 (new-string "asdfg"))
    (check-equal? (get-string (jobject-vector-ref vec 2)) "asdfg"))
  
  (test-case "homogeneous jint-vector"
    (define vec (make-jint-vector 5))
    (check-not-exn (λ () (jint-vector-set! vec 4 1234567)))
    (check-equal? (jint-vector-ref vec 4) 1234567))
  
  (test-case "homogeneous jshort-vector"
    (define vec (make-jshort-vector 5))
    (check-not-exn (λ () (jshort-vector-set! vec 4 123)))
    (check-equal? (jshort-vector-ref vec 4) 123))
  
  (test-case "homogeneous jlong-vector"
    (define vec (make-jlong-vector 5))
    (check-not-exn (λ () (jlong-vector-set! vec 4 1234567891011)))
    (check-equal? (jlong-vector-ref vec 4) 1234567891011))
  
  (test-case "homogeneous jboolean-vector"
    (define vec (make-jboolean-vector 5))
    (check-not-exn (λ () (jboolean-vector-set! vec 4 #t)))
    (check-equal? (jboolean-vector-ref vec 4) #t))
  
  (test-case "homogeneous jbyte-vector"
    (define vec (make-jbyte-vector 5))
    (check-not-exn (λ () (jbyte-vector-set! vec 4 9)))
    (check-equal? (jbyte-vector-ref vec 4) 9))
  
  (test-case "homogeneous jchar-vector"
    (define vec (make-jchar-vector 5))
    (check-not-exn (λ () (jchar-vector-set! vec 4 #\♥)))
    (check-equal? (jchar-vector-ref vec 4) #\♥))
  
  (test-case "homogeneous jfloat-vector"
    (define vec (make-jfloat-vector 5))
    (check-not-exn (λ () (jfloat-vector-set! vec 4 666.23425)))
    (check-equal? (jfloat-vector-ref vec 4) 666.23425))
  
  (test-case "homogeneous jdouble-vector"
    (define vec (make-jdouble-vector 5))
    (check-not-exn (λ () (jdouble-vector-set! vec 4 666.2342324234235)))
    (check-equal? (jdouble-vector-ref vec 4) 666.2342324234235))
  )