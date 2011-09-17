#lang racket/base

(require "core.rkt" "c.rkt")

(struct jvector (cpointer type length))

(define jvector->cpointer jvector-cpointer)

(define (make-jvector-predicate element)
  (let ([element-tag (jtype-tag element)])
    (λ (obj) (and (jvector? obj) (eq? element-tag (jtype-tag (jvector-type obj)))))))

(define _jvector
  ((lambda ()
     (struct _jvector jtype/vector ()
       #:property prop:procedure
       (lambda (self element)
         (define-values (make-array array-ref array-set!) (tag->array-info (jtype-tag element)))
         (let ([signature (make-vector-signature (jtype-signature element))])
           (struct-copy jtype/vector self
             [signature    #:parent jtype signature]
             [racket->java #:parent jtype jvector-cpointer]
             [java->racket #:parent jtype (lambda (e) (jvector e element (array-length e)))]
             [predicate    #:parent jtype (make-jvector-predicate element)]
             [class        #:parent jtype/object (find-class signature)]
             [element               element]))))
     (let ([class-id (find-class "[Ljava/lang/Object;")])
       (_jvector "[Ljava/lang/Object;" 'object (make-jvector-predicate _jobject) __jobject 
                 jvector-cpointer
                 (lambda (e) (jvector e _jobject))
                 class-id
                 _jobject)))))

(define (make-jvector type length)
  (define pointer
    (if (jtype/object? type) (new-object-array length (jtype/object-class type) #f)
        (let-values ([(make _3 _4) (tag->array-info (jtype-tag type))]) (make length))))
  (jvector pointer type length))

(define (jvector-ref v i)
  (let*-values ([(type) (jvector-type v)]
                [(_2 ref _4) (tag->array-info (jtype-tag type))]
                [(java->racket) (or (jtype-java->racket type) values)])
    (java->racket (ref (jvector->cpointer v) i))))

(define (jvector-set! v i n)
  (let*-values ([(type) (jvector-type v)]
                [(_2 _3 set) (tag->array-info (jtype-tag type))]
                [(racket->java) (or (jtype-racket->java type) values)])
    (set (jvector->cpointer v) i (racket->java n))))

(provide 
 
 (struct-out jvector)
 _jvector jvector->cpointer make-jvector jvector-set! jvector-ref)
