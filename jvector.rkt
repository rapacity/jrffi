#lang racket/base

(require "core.rkt" "c.rkt")

(define jvector->cpointer jvector-cpointer)

(define _jvector
  ((lambda ()
     (struct _jvector jtype/vector ()
       #:property prop:procedure
       (lambda (self element)
         (define tag (jtype-tag element))
         (define-values (make-array array-ref array-set!) (tag->array-info tag))
         (let ([signature (make-vector-signature (jtype-signature element))])
           (struct-copy jtype/vector self
             [signature    #:parent jtype signature]
             [racket->java #:parent jtype jvector-cpointer]
             [java->racket #:parent jtype (lambda (e) (jvector e element))]
             [predicate    #:parent jtype (λ (o) (and (jvector? o)
                                                      (eq? tag (jtype-tag (jvector-type o)))))]
             [class        #:parent jtype/object (find-class signature)]
             [element               element]))))
     (let ([class-id (find-class "[Ljava/lang/Object;")])
       (_jvector "[Ljava/lang/Object;" 'object (make-jobject-predicate class-id) __jobject 
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
  (let-values ([(_2 ref _4) (tag->array-info (jtype-tag (jvector-type v)))])
    (ref (jvector->cpointer v) i)))

(define (jvector-set! v i n)
  (let-values ([(_2 _3 set) (tag->array-info (jtype-tag (jvector-type v)))])
    (set (jvector->cpointer v) i n)))

(provide _jvector jvector->cpointer make-jvector jvector-set! jvector-ref)
