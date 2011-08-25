#lang racket

(require rackunit "../main.rkt" "../jvector.rkt")

(define java.util.Arrays (find-class "java/util/Arrays"))

(define toString
  (jmethod/overload/check java.util.Arrays "toString" #t
   (#f ((_jvector _jlong)) _jstring)
   (#f ((_jvector _jint)) _jstring)
   (#f ((_jvector _jshort)) _jstring)
   (#f ((_jvector _jchar)) _jstring)
   (#f ((_jvector _jbyte)) _jstring)
   (#f ((_jvector _jboolean)) _jstring)
   (#f ((_jvector _jfloat)) _jstring)
   (#f ((_jvector _jdouble)) _jstring)
   (#f ((_jvector _jobject)) _jstring)))

(define x (make-jvector _jboolean 2))
(jvector-set! x 0 #f)
(jvector-set! x 1 #t)

(test-equal? "jmethod/overload/check" "[false, true]" (toString x))

