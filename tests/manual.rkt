#lang racket

(require rackunit "../base.rkt")

(define toString
  (get-java-method
   (_jobject "java/util/Arrays") "toString"
   (_jmethod [(_jvector _jlong)    -> _jstring #:static]
             [(_jvector _jint)     -> _jstring #:static]
             [(_jvector _jshort)   -> _jstring #:static]
             [(_jvector _jchar)    -> _jstring #:static]
             [(_jvector _jbyte)    -> _jstring #:static]
             [(_jvector _jboolean) -> _jstring #:static]
             [(_jvector _jfloat)   -> _jstring #:static]
             [(_jvector _jdouble)  -> _jstring #:static]
             [(_jvector _jobject)  -> _jstring #:static])))

(define toString-boolean-jinst (jinst toString (_jvector _jboolean)))

(define x (make-jvector _jboolean 2))
(jvector-set! x 0 #f)
(jvector-set! x 1 #t)

(test-equal? "method overload" (toString x) "[false, true]")

(define c (make-jvector _jchar 2))
(jvector-set! c 0 #\a)
(jvector-set! c 1 #\d)

(test-equal? "jvector + method overload" (toString c) "[a, d]")
(test-equal? "method overload" (toString x) "[false, true]")
(test-equal? "jinst" (toString-boolean-jinst x) "[false, true]")


(define new-integer
  (get-java-constructor
   (_jobject "java/lang/Integer")
   (_jconstructor [_jint]
                  [_jstring])))

(define y (make-jvector (_jobject "java/lang/Integer") 2))
(jvector-set! y 0 (new-integer "666"))
(jvector-set! y 1 (new-integer 999))

(test-equal? "constructor overload" (toString y) "[666, 999]")


(define java.lang.Integer-toString
  (get-java-method
   (_jobject "java/lang/Integer") "toString"
   (_jmethod [_jint _jint -> _jstring #:static]
             [_jint       -> _jstring #:static]
             [            -> _jstring])))

(test-equal? "method overload" (java.lang.Integer-toString (new-integer "333")) "333")


(define java.util.Arrays (_jobject "java/util/Arrays"))
(define java.lang.Integer (_jobject "java/lang/Integer"))


(define array->string 
  (get-java-method java.util.Arrays "toString"
    (_jmethod [_jobject ... -> _jstring #:static])))


(test-equal? "static method vararg no-args" (array->string) "[]")

(test-equal? "static method vararg with-args" (array->string (new-integer 1) (new-integer 35)) "[1, 35]")


