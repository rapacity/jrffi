#lang racket/base
(require "core.rkt" "c.rkt")

(struct jvector (cpointer type info))

(define jvector->cpointer jvector-cpointer)

(define (make-jvector-predicate element)
  (define element-tag (jtype-tag element))
  (λ (obj) (and (jvector? obj) (eq? element-tag (jtype-tag (jvector-type obj))))))

(define _jvector
  ((lambda ()
     (struct _jvector jtype/vector ()
       #:property prop:procedure
       (lambda (self element)
         (define array-info (tag->array-info (jtype-tag element)))
         (define make-array (array-info-make array-info))
         (define signature  (make-vector-signature (jtype-signature element)))
         (struct-copy jtype/vector self
           [signature    #:parent jtype signature]
           [racket->java #:parent jtype jvector-cpointer]
           [java->racket #:parent jtype (lambda (e) (jvector e element (array-length e)))]
           [predicate    #:parent jtype (make-jvector-predicate element)]
           [class        #:parent jtype/object (find-class signature)]
           [element               element])))
     (let ([class-id (find-class "[Ljava/lang/Object;")])
       (_jvector "[Ljava/lang/Object;" 'object (make-jvector-predicate _jobject) __jobject 
                 jvector-cpointer
                 (lambda (e) (jvector e _jobject))
                 class-id
                 _jobject)))))

(define (make-jvector type length)
  (define array-info (tag->array-info (jtype-tag type)))
  (define array-make (array-info-make array-info))
  (define pointer
    (if (jtype/object? type)
        (array-make length (jtype/object-class type) #f)
        (array-make length)))
  (jvector pointer type array-info))

(define (jvector-ref v i)
  (define array-info   (jvector-info v))
  (define array-ref    (array-info-ref array-info))
  (define java->racket (or (jtype-java->racket (jvector-type v)) values))
  (java->racket (array-ref (jvector->cpointer v) i)))

(define (jvector-set! v i n)
  (define array-info   (jvector-info v))
  (define array-set!   (array-info-set! array-info))
  (define racket->java (or (jtype-racket->java (jvector-type v)) values))
  (define pointer      (jvector->cpointer v))
  (array-set! pointer i (racket->java n)))

(provide (struct-out jvector) _jvector jvector->cpointer make-jvector jvector-set! jvector-ref)