#lang racket

(require rackunit "../main.rkt")

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

(test-equal? "method overload" "[false, true]" (toString x))
(test-equal? "jinst" "[false, true]" (toString-boolean-jinst x))


(define newInteger
  (get-java-constructor
   (_jobject "java/lang/Integer")
   (_jconstructor [_jint]
                  [_jstring])))

(define y (make-jvector (_jobject "java/lang/Integer") 2))
(jvector-set! y 0 (newInteger "666"))
(jvector-set! y 1 (newInteger 999))

(test-equal? "jconstructor/overload.check" "[666, 999]" (toString y))


(define java.lang.Integer-toString
  (get-java-method
   (_jobject "java/lang/Integer") "toString"
   (_jmethod [_jint _jint -> _jstring #:static]
             [_jint       -> _jstring #:static]
             [            -> _jstring])))

(test-equal? "jmethod/overload/check" "333" (java.lang.Integer-toString (newInteger "333")))



