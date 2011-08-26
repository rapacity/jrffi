#lang racket

(require rackunit "../main.rkt" "../jvector.rkt")

(define java.util.Arrays (find-class "java/util/Arrays"))

(define toString
  (jmethod/overload/check java.util.Arrays "toString" 
   (#t #f ((_jvector _jlong)) _jstring)
   (#t #f ((_jvector _jint)) _jstring)
   (#t #f ((_jvector _jshort)) _jstring)
   (#t #f ((_jvector _jchar)) _jstring)
   (#t #f ((_jvector _jbyte)) _jstring)
   (#t #f ((_jvector _jboolean)) _jstring)
   (#t #f ((_jvector _jfloat)) _jstring)
   (#t #f ((_jvector _jdouble)) _jstring)
   (#t #f ((_jvector _jobject)) _jstring)))

(define x (make-jvector _jboolean 2))
(jvector-set! x 0 #f)
(jvector-set! x 1 #t)

(test-equal? "jmethod/overload/check" "[false, true]" (toString x))


(define java.lang.Integer (find-class "java/lang/Integer"))

(define newInteger
  (jconstructor/overload/check java.lang.Integer
   (#f (_jint) _jvoid)
   (#f (_jstring) _jvoid)
   ))

(define y (make-jvector (_jobject "java/lang/Integer") 2))
(jvector-set! y 0 (newInteger "666"))
(jvector-set! y 1 (newInteger 999))

(test-equal? "jconstructor/overload.check" "[666, 999]" (toString y))


(define java.lang.Integer-toString
  (jmethod/overload/check
   java.lang.Integer
   "toString"
   (#t #f (_jint _jint) _jstring)
   (#t #f (_jint) _jstring)
   (#f #f () _jstring)))

(test-equal? "jmethod/overload/check" "333" (java.lang.Integer-toString (newInteger "333")))


