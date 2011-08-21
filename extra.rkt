#lang racket

(require "core.rkt" "misc.rkt" (only-in "c.rkt" find-class instance-of? get-string new-string))

(define Boolean (find-class "java/lang/Boolean"))
(define Short (find-class "java/lang/Short"))
(define Byte (find-class "java/lang/Byte"))
(define Integer (find-class "java/lang/Integer"))
(define Long (find-class "java/lang/Long"))
(define Double (find-class "java/lang/Double"))
(define Float (find-class "java/lang/Float"))
(define Character (find-class "java/lang/Character"))
(define String (find-class "java/lang/String"))



(define new-integer (jconstructor Integer _jint))
(define new-boolean (jconstructor Boolean _jboolean))
(define new-float (jconstructor Float _jfloat))
(define new-byte (jconstructor Byte _jbyte))
(define new-character (jconstructor Character _jchar))


(define Boolean.booleanValue (jmethod Boolean booleanValue : -> _jboolean))
(define Byte.byteValue (jmethod Byte byteValue : -> _jbyte))
(define Short.shortValue (jmethod Short shortValue : -> _jshort))
(define Integer.intValue (jmethod Integer intValue : -> _jint))
(define Long.longValue (jmethod Long longValue : -> _jlong))
(define Double.doubleValue (jmethod Double doubleValue : -> _jdouble))
(define Float.floatValue (jmethod Float floatValue : -> _jfloat))
(define Character.charValue (jmethod Character charValue : -> _jchar))

(define (primitive-jobject->racket obj)
  (cond [(instance-of? obj Integer)   (Integer.intValue obj)]
        [(instance-of? obj String)    (get-string obj)]
        [(instance-of? obj Boolean)   (Boolean.booleanValue obj)]
        [(instance-of? obj Float)     (Float.floatValue obj)]
        [(instance-of? obj Short)     (Short.shortValue obj)]
        [(instance-of? obj Byte)      (Byte.byteValue obj)]
        [(instance-of? obj Character) (Character.charValue obj)]
        [(instance-of? obj Long)      (Long.longValue obj)]
        [(instance-of? obj Double)    (Double.doubleValue obj)]
        [else (error "Not a primitive jobject")]))

(define (racket->primitive-jobject obj)
  (cond [(byte? obj)    (new-byte obj)]
        [(integer? obj) (new-integer obj)]
        [(string? obj)  (new-string obj)]
        [(boolean? obj) (new-boolean obj)]
        [(char? obj)    (new-character obj)]
        [(real? obj)    (new-float obj)]
        [else (error "Not a primitive jobject")]))


(define _jobject-primitive
  (_jobject "java/lang/Object" racket->primitive-jobject primitive-jobject->racket))


(provide (all-defined-out))


