#lang scribble/doc
@(require 
  (for-label racket
    (except-in "../main.rkt" ->))
  
  
  scribble/racket
  scribble/eval
  scribble/base
  scribble/manual
  
  "../main.rkt" )

@defmodule[jrffi]



@title[#:style 'quiet #:tag "examples"]{Examples}

Automatic Binding example
@examples[
(require jrffi (java java/lang/Integer)) ; requiring the Integer class
(define n (new-Integer 6765))            ; constructing an Integer object
(Integer-toString n)                     ; calling a method
(get-Integer-MAX_VALUE)                  ; accessing a static field
(Integer? n)                             ; checking if n is an instance of the Integer class
]

Manual binding example
@examples[
(require jrffi)

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

(toString x)
(toString-boolean-jinst x)


(define newInteger
  (get-java-constructor
   (_jobject "java/lang/Integer")
   (_jconstructor [_jint]
                  [_jstring])))

(define y (make-jvector (_jobject "java/lang/Integer") 2))
(jvector-set! y 0 (newInteger "666"))
(jvector-set! y 1 (newInteger 999))

(toString y)


(define java.lang.Integer-toString
  (get-java-method
   (_jobject "java/lang/Integer") "toString"
   (_jmethod [_jint _jint -> _jstring #:static]
             [_jint       -> _jstring #:static]
             [            -> _jstring])))

(java.lang.Integer-toString (newInteger "333"))
 ]