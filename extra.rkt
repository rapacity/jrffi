#lang racket/base


(require "core.rkt" "c.rkt" "overload.rkt")
(require "require.rkt")
 



(require (java java/lang/Boolean
               java/lang/Short
               java/lang/Byte
               java/lang/Integer
               java/lang/Long
               java/lang/Double
               java/lang/Float
               java/lang/Character
               java/lang/String
               ))

(define (primitive-jobject->racket obj)
  (cond
    [(String? obj)    (get-string obj)]
    [(Boolean? obj)   (Boolean-booleanValue obj)]
    [(Byte? obj)      (Byte-byteValue obj)]
    [(Short? obj)     (Short-shortValue obj)]
    [(Integer? obj)   (Integer-intValue obj)]
    [(Long? obj)      (Long-longValue obj)]
    [(Float? obj)     (Float-floatValue obj)]
    [(Double? obj)    (Double-doubleValue obj)]    
    [(Character? obj) (Character-charValue obj)]
    [else (error "Not a primitive jobject")]))




(define racket->primitive-jobject
  (let ([new-boolean   (jinst new-Boolean _jboolean)]
        [new-byte      (jinst new-Byte _jbyte)]
        [new-short     (jinst new-Short _jshort)]
        [new-integer   (jinst new-Integer _jint)]
        [new-long      (jinst new-Long _jlong)]
        [new-float     (jinst new-Float _jfloat)]
        [new-double    (jinst new-Double _jdouble)])
    (lambda (obj)
      (cond [(jstring? obj)  (new-string obj)]
            [(jboolean? obj) (new-Boolean obj)]
            [(jbyte? obj)    (new-byte obj)] 
            [(jshort? obj)   (new-short obj)]
            [(jint? obj)     (new-integer obj)]
            [(jlong? obj)    (new-long obj)]
            [(jfloat? obj)   (new-float obj)]
            [(jdouble? obj)  (new-double obj)]
            [(jchar? obj)    (new-Character obj)]
            [else (error "Not a primitive jobject")]))))


(define _jobject-primitive
  (_jobject "java/lang/Object" racket->primitive-jobject primitive-jobject->racket))


(provide (all-defined-out))


