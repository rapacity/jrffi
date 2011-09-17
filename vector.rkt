#lang racket/base

(require "core.rkt" "jvector.rkt" "c.rkt" (for-syntax racket/base))


(require (for-syntax racket/function racket/syntax))


(require ffi/unsafe)

(begin-for-syntax
  (define (id:array-make name)
    (list (format-id name "_j~a" name)
          (format-id name "_j~a-vector" name)
          (format-id name "j~a-vector" name)
          (format-id name "make-j~a-vector" name)
          (format-id name "j~a-vector-set!" name)
          (format-id name "j~a-vector-ref" name)
          (format-id name "j~a-vector-length" name))))

(define-syntax (unpack-homogenous-vector stx)
  (syntax-case stx ()
    [(x type ...)
     (with-syntax ([((_jelement _type _struct _make _set! _ref _length) ...)
                    (map id:array-make (syntax-e #`(type ...)))]
                   [(_ctype ...) (generate-temporaries #'(type ...))])
       #`(begin
            (begin
              (struct _struct jvector ())
              (define _type
                (let* ([signature (make-vector-signature (jtype-signature _jelement))]
                       [class-id  (find-class signature)])
                  (jtype/vector signature 'object (make-jobject-predicate class-id) 
                                _jobject #f #f class-id _jelement)))
              (define-values (_make _ref _set!)
                (let-values ([(m r s!) (tag->array-info 'type)])
                  (values (lambda (i) (_struct (m i) _jelement i))
                          (lambda (o i) (r (jvector-cpointer o) i))
                          (lambda (o i v) (s! (jvector-cpointer o) i v))))))
            ...))]))


(struct jobject-vector jvector ())
(define _jobject-vector
  (let* ([signature (make-vector-signature (jtype-signature _jobject))]
         [class-id  (find-class signature)])
    (jtype/vector signature 'object (make-jobject-predicate class-id) 
                  _jobject #f #f class-id _jobject)))
(define-values (make-jobject-vector jobject-vector-ref jobject-vector-set!)
  (let-values ([(m r s!) (tag->array-info 'object)])
    (values (lambda (i obj)
              (unless (jtype/object? obj)
                (error "not a valid object type"))
              (jobject-vector (m i (jtype/object-class obj) #f) _jobject i))
            (lambda (o i) (r (jvector-cpointer o) i))
            (lambda (o i v) (s! (jvector-cpointer o) i v)))))

(unpack-homogenous-vector boolean byte char short int long float double)


(provide (all-defined-out))





