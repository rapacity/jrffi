#lang racket/base



(require racket/stxparam)
(require (for-syntax syntax/parse racket/syntax racket) racket/file racket/list)
(require ffi/unsafe racket/runtime-path racket/function)



(define-syntax-parameter :                                                     
  (lambda (stx) #f))
    
; Helper macros for importing function from the C jrffi library
(define-syntax (cimports stx)
  (syntax-parse stx #:literals (= ->)
    [(_) #`(begin)]
    [(_ #:with-env) #`(begin)]
    [(_ [name (~optional (~seq = ffi-name)) args ...] rest ...)
     #`(begin (define name (cimport #,(if (attribute ffi-name) #'ffi-name #'name) args ...))
              (cimports rest ...))]
    [(_ #:with-env [name (~optional (~seq = ffi-name)) args ...] rest ...)
     #`(begin (define name (cimport/env #,(if (attribute ffi-name) #'ffi-name #'name) args ...))
              (cimports #:with-env rest ...))]))
(define-syntax (cimport stx)
  (syntax-parse stx #:literals (: ->)
    [(_ name : args ... -> return)
     #`(get-jrffi-obj 'name (_fun args ... -> return))]))
; like cimport but the first argument of the imported function is __jnienv the imported function is
; then wrapped in a lambda with the user-specified args and the first argument provided current-jnienv
(define-syntax (cimport/env stx)
  (syntax-parse stx #:literals (: ->)
    [(_ name : arg-types ... -> return)
     (with-syntax ([(args ...) (generate-temporaries #`(arg-types ...))])
       #`(let ([f (cimport name : __jnienv arg-types ... -> return)])
           (lambda (args ...) (f current-jnienv args ...))))]))

; if false then false else make pointer a java global ref, register it with a finalizer, and tag it
(define (_jpointer/null tag)
  (make-ctype _pointer #f
    (λ (obj)
      (if (not obj) #f
          (let ([return (new-global-ref obj)])
            (delete-local-ref obj)
            (register-finalizer return delete-global-ref)
            (cpointer-push-tag! return tag)
            return)))))

(define-cstruct _message
  ([status _int]
   [string _string]))

(define-cstruct _JavaVMOption
  ([option _string]
   [extra _pointer]))

(define __jfieldID  _pointer)
(define __jmethodID _pointer)
(define __signature _string)
(define __jnienv    _pointer)
(define __jsize     _sint32)
(define __jclass  (_jpointer/null 'jclass))
; basic java types
(define __jobject (_jpointer/null 'jobject))
(define __jstring  __jobject)
(define __jboolean (make-ctype _uint8 (λ (e) (if e 1 0)) (λ (e) (if (zero? e) #f #t))))
(define __jbyte    _int8)
(define __jchar    _ushort)
(define __jshort   _sint16)
(define __jint     _sint32)
(define __jlong    _sint64)
(define __jfloat   _float)
(define __jdouble  _double*)
(define __jvoid    _void)

(define-runtime-path jrffi-path "./jrffi")
(define jrffi (ffi-lib jrffi-path))

(define (get-jrffi-obj name type)
  (get-ffi-obj (regexp-replaces name '((#rx"-" "_"))) jrffi type))

; holds the jnienv for the current pthread
(define current-jnienv #f)

(define (set-current-jnienv! v)
  (set! current-jnienv v))

(cimports
 [attach-current-thread                          : -> __jnienv]
 [create-jvm                                     : (_list i _JavaVMOption) _int -> _message-pointer]
 [jvm-initialized?    = is-jvm-initialized       : -> _bool]
 #:with-env
 [new-object-array                               : __jsize __jclass __jobject -> __jobject]
 [set-object-array-element                       : __jobject __jsize __jobject -> _void]
 [get-object-array-element                       : __jobject __jsize  -> __jobject]
 [get-array-length                               : _pointer -> __jsize]
 [find-class*           = find-class             : _string -> __jclass]
 [get-object-class                               : __jobject -> __jclass] 
 [instance-of?          = is-instance-of         : __jobject __jclass -> _bool]
 [get-method-id*        = get-method-id          : __jclass _string _string -> __jmethodID]
 [get-static-method-id* = get-static-method-id   : __jclass _string _string -> __jmethodID]
 [get-field-id*         = get-field-id           : __jclass _string _string -> __jfieldID] 
 [get-static-field-id*  = get-static-field-id    : __jclass _string _string -> __jfieldID]
 [delete-local-ref                               : _pointer -> _void]
 [new-global-ref                                 : _pointer -> _pointer]
 [delete-global-ref                              : _pointer -> _void]
 [get-string-length                              : _pointer -> __jsize]
 [new-string                                     : _string  -> __jobject]
 [get-string*           = get-string             : __jstring -> _pointer]
 [release-string-chars                           : __jobject _pointer -> _void]
 )

(define (get-string x)
  (let* ([str (get-string* x)]
         [return (cast str _pointer _string)])
    (release-string-chars x str)
    return))

; caching of fetch-class class ids
(define class-ids (make-hash))

; fetch class-id and error on fail
(define (find-class clss)
  (or (hash-ref! class-ids clss (thunk (find-class* clss))) 
      (error (string-append "class not found " clss))))

(define (get-method-id clss sig name #:static? [static? #f])
  (let ([return ((if static? get-static-method-id* get-method-id*) clss sig name)])
    (if return return (error "Method not found"))))

(define (get-field-id clss sig name #:static? [static? #f])
  (let ([return ((if static? get-static-field-id* get-field-id*) clss sig name)])
    (if return return (error "Field not found"))))


(begin-for-syntax
  (define (id:array-make name)
    (list (format-id name "__j~a" name)
          (format-id name "new-~a-array" name)
          (format-id name "set-~a-array-element" name)
          (format-id name "get-~a-array-element" name))))


(define-syntax (unpack-cside-jvector stx)
  (syntax-case stx ()
    [(x type ...)
     (with-syntax ([tag->array-info (format-id #'x "tag->array-info")]
                   [((__element ffi-make ffi-set! ffi-ref) ...)
                    (map id:array-make (syntax-e #`(type ...)))])
       #`(begin
           (cimports
            #:with-env
            [ffi-make : __jsize                     -> __jobject] ...
            [ffi-set! : __jobject __jsize __element -> __jvoid] ...
            [ffi-ref  : __jobject __jsize           -> __element] ...)
           (define (tag->array-info tag)
             (case tag
               [(object) (values new-object-array get-object-array-element set-object-array-element)]
               [(type)   (values ffi-make ffi-ref ffi-set!)] ...
               [else     (error "this type cannot be used in an array")]))))]))



(unpack-cside-jvector boolean byte char short int long float double)


(provide (all-defined-out))








