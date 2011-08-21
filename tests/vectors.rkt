#lang racket
(require rackunit)

(require "../main.rkt" "../vector.rkt" "../jvector.rkt")

(define java.util.Arrays (find-class "java/util/Arrays"))
(define java.lang.Integer (find-class "java/lang/Integer"))

(define array->string (jmethod java.util.Arrays toString : _jobject ... -> _jstring #:static))

(define obj-vec (make-jobject-vector 3 Integer))
(define int-vec (make-jint-vector 10))

(jint-vector-set! int-vec 4 666)
(test-equal? "jint-vector-ref" 666 (jint-vector-ref int-vec 4))

(jobject-vector-set! obj-vec 2 (new-integer 9001))
(test-equal? "jobject-vector-ref" 9001 (primitive-jobject->racket (jobject-vector-ref obj-vec 2)))

(define obj-jvec (make-jvector _jobject 10))
(jvector-set! obj-jvec 4 (new-integer 42))
(test-equal? "jvector-ref" 42 (primitive-jobject->racket (jobject-vector-ref obj-jvec 4)))