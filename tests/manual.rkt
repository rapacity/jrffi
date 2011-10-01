#lang racket

(require rackunit "../base.rkt")

(define/provide-test-suite manual-tests  "Tests for manual binding"
  (test-case "overloaded method type"
    (_jmethod [(_jvector _jlong)    -> _jstring #:static]
              [(_jvector _jint)     -> _jstring #:static]
              [(_jvector _jshort)   -> _jstring #:static]
              [(_jvector _jchar)    -> _jstring #:static]
              [(_jvector _jbyte)    -> _jstring #:static]
              [(_jvector _jboolean) -> _jstring #:static]
              [(_jvector _jfloat)   -> _jstring #:static]
              [(_jvector _jdouble)  -> _jstring #:static]
              [(_jvector _jobject)  -> _jstring #:static]))
  
  (test-case "non-overloaded method type"
    (_jmethod [(_jvector _jlong)    -> _jstring #:static]))
  
  (test-case "instantiate method overload"
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
    
    (check-equal? (toString x) "[false, true]")

    (define c (make-jvector _jchar 2))
    (jvector-set! c 0 #\a)
    (jvector-set! c 1 #\d)
        
    (check-equal? (toString c) "[a, d]" "jvector + method overload")
    (check-equal? (toString x) "[false, true]" "method overload")
    (check-equal? (toString-boolean-jinst x) "[false, true]" "jinst")
  
  
    (define new-integer
      (get-java-constructor
       (_jobject "java/lang/Integer")
       (_jconstructor [_jint]
                      [_jstring])))
    
    (define y (make-jvector (_jobject "java/lang/Integer") 2))
    (jvector-set! y 0 (new-integer "666"))
    (jvector-set! y 1 (new-integer 999))
    
    (check-equal? (toString y) "[666, 999]" "constructor overload")

    (define java.lang.Integer-toString
      (get-java-method
       (_jobject "java/lang/Integer") "toString"
       (_jmethod [_jint _jint -> _jstring #:static]
                 [_jint       -> _jstring #:static]
                 [            -> _jstring])))

    (check-equal? (java.lang.Integer-toString (new-integer "333")) "333" "method overload")
    
    (define java.util.Arrays (_jobject "java/util/Arrays"))
    (define java.lang.Integer (_jobject "java/lang/Integer"))
    
    (define array->string 
      (get-java-method java.util.Arrays "toString"
                       (_jmethod [_jobject ... -> _jstring #:static])))
    
    (check-equal? (array->string) "[]" "static method vararg no-args")
    
    (check-equal? (array->string (new-integer 1) (new-integer 35)) "[1, 35]" "static method vararg with-args"))
  
  )