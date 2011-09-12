#lang racket/base

(require (only-in ffi/unsafe _cprocedure)
         "core.rkt" "c.rkt"
         racket/contract/base)
(require (for-syntax syntax/parse racket/base) racket/contract/region)



(struct jtype/field (type static?) #:transparent)


(define-syntax (_jfield stx)
  (syntax-parse stx
    [(_ type (~optional (~and #:static static-kw)))
     (with-syntax ([static? (if (attribute static-kw) #`#t #`#f)])
       (quasisyntax/loc stx
         (jtype/field type static?)))]))


(define (get-java-accessor class-type field-name ftype)
  (let* ([type      (jtype/field-type ftype)]
         [static?   (jtype/field-static? ftype)]
         [signature (jtype-signature type)]
         [class-id  (jtype/object-class class-type)]
         [field-id  (get-field-id class-id field-name signature static?)]
         [ctype     (jtype-ctype type)]
         [ffi-func  (get-jrffi-obj
                     (format "get-~a~a-field" (if static? "static-" "") (jtype-tag type))
                     (_cprocedure (list __jnienv (if static? __jclass __jobject) __jfieldID) ctype))])
    (if static? (λ () (ffi-func current-jnienv class-id field-id))
        (λ (obj) (ffi-func current-jnienv obj field-id)))))

(define (get-java-mutator class-type field-name ftype)
  (let* ([type      (jtype/field-type ftype)]
         [static?   (jtype/field-static? ftype)]
         [signature (jtype-signature type)]
         [class-id  (jtype/object-class class-type)]
         [field-id  (get-field-id class-id field-name signature static?)]
         [ctype     (jtype-ctype type)]
         [ffi-func (get-jrffi-obj 
                    (format "set-~a~a-field" (if static? "static-" "") (jtype-tag type))
                    (_cprocedure (list __jnienv (if static? __jclass __jobject) __jfieldID ctype) 
                                 ctype))])
    (if static? (λ (new-value) (ffi-func current-jnienv class-id field-id new-value))
        (λ (obj new-value) (ffi-func current-jnienv obj field-id new-value)))))

(define (get-java-parameter class-id field-name ftype)
  (let* ([accessor (get-java-accessor class-id field-name ftype)]
         [mutator  (get-java-mutator class-id field-name ftype)])
    (if (jtype/field-static? ftype)
        (case-lambda
          [() (accessor)]
          [(new-value) (mutator new-value)])
        (case-lambda
          [(obj) (accessor obj)]
          [(obj new-value) (mutator obj new-value)]))))


(provide _jfield get-java-accessor get-java-mutator get-java-parameter)