#lang racket
(require rackunit "../main.rkt" "../extra.rkt" (java java/util/Arrays java/lang/Integer))

(define array->string Arrays-toString)

(define obj-vec (make-jobject-vector 3 Integer))
(define int-vec (make-jint-vector 10))

(jint-vector-set! int-vec 4 666)
(test-equal? "jint-vector-ref" (jint-vector-ref int-vec 4) 666)

(jobject-vector-set! obj-vec 2 (new-Integer 9001))
(test-equal? "jobject-vector-ref" 9001 (primitive-jobject->racket (jobject-vector-ref obj-vec 2)))

(define obj-jvec (make-jvector _jobject 10))
(jvector-set! obj-jvec 4 (new-Integer 42))
(test-equal? "jvector-ref" 42 (primitive-jobject->racket (jobject-vector-ref obj-jvec 4)))