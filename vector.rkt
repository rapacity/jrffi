#lang racket/base
(require "core.rkt" "jvector.rkt" "c.rkt" (for-syntax racket/base racket/syntax syntax/parse))

(define-syntax (unpack-homogenous-vector stx)
  (define-syntax-class array
    (pattern name:id
             #:with element (format-id #'name "_j~a" #'name)
             #:with type    (format-id #'name "_j~a-vector" #'name)
             #:with struct  (format-id #'name "j~a-vector" #'name)
             #:with make    (format-id #'name "make-j~a-vector" #'name)
             #:with set!    (format-id #'name "j~a-vector-set!" #'name)
             #:with ref     (format-id #'name "j~a-vector-ref" #'name)
             #:with length  (format-id #'name "j~a-vector-length" #'name)
             #:with ctype   (generate-temporary)))
  (syntax-parse stx
    [(_ a:array ...)
     #'(begin
         (begin
           (struct a.struct jvector ())
           (define a.type
             (let* ([signature (make-vector-signature (jtype-signature a.element))]
                    [class-id  (find-class signature)])
               (jtype/vector signature 'object (make-jobject-predicate class-id) 
                             _jobject #f #f class-id a.element)))
           (define-values (a.make a.ref a.set!)
             (let* ([info (tag->array-info 'a)]
                    [ref  (array-info-ref info)]
                    [set  (array-info-set! info)]
                    [make (array-info-make info)])
               (values (lambda (i) (a.struct (make i) a.element i))
                       (lambda (o i) (ref (jvector-cpointer o) i))
                       (lambda (o i v) (set (jvector-cpointer o) i v)))))
           (provide a.make a.ref a.set! (struct-out a.struct)))
         ...)]))

(struct jobject-vector jvector ())
(define _jobject-vector
  (let* ([signature (make-vector-signature (jtype-signature _jobject))]
         [class-id  (find-class signature)])
    (jtype/vector signature 'object (make-jobject-predicate class-id) 
                  _jobject #f #f class-id _jobject)))
(define-values (make-jobject-vector jobject-vector-ref jobject-vector-set!)
  (let* ([info (tag->array-info 'object)]
         [ref  (array-info-ref info)]
         [set  (array-info-set! info)]
         [make (array-info-make info)])
    (values (lambda (i obj)
              (unless (jtype/object? obj)
                (error "not a valid object type"))
              (jobject-vector (make i (jtype/object-class obj) #f) _jobject i))
            (lambda (o i) (ref (jvector-cpointer o) i))
            (lambda (o i v) (set (jvector-cpointer o) i v)))))

(unpack-homogenous-vector boolean byte char short int long float double)

(provide make-jobject-vector jobject-vector-ref jobject-vector-set! (struct-out jobject-vector))