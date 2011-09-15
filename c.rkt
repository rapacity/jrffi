#lang racket/base

(require (for-syntax syntax/parse racket/syntax racket))
(require ffi/unsafe racket/runtime-path racket/function)

(define-syntax (: stx)                                                     
  (raise-syntax-error #f "can only be used in a cimports context" stx))
    
; Helper macro for importing function from the C jrffi library
(define-syntax (cimports stx)
  (define-syntax-class row #:literals (= -> :)
    (pattern (name:id (~optional (~seq = ffi-name:id) #:defaults ([ffi-name #'name])) : arg:expr ...)))
  (syntax-parse stx #:literals (= ->)
    [(_ o:row ... #:with-env p:row ...)
     (quasisyntax/loc stx
       (begin 
         (define o.name (get-jrffi-obj 'o.ffi-name (_fun o.arg ...))) ...
         (define p.name (get-jrffi-obj 'p.ffi-name (_fun [__jnienv = current-jnienv] p.arg ...))) ...))]))

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

(define-cstruct _JavaVMOption
  ([option _string]
   [extra _pointer]))

(define-cstruct _JNINativeMethod
  ([name      _string]
   [signature _string]
   [function  _pointer]))

(define __jfieldID  _pointer)
(define __jmethodID _pointer)
(define __signature _string)
(define __jnienv    _pointer)
(define __jsize     _sint32)
(define __jclass  (_jpointer/null 'jclass))
; basic java-type's ctypes
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

(define-runtime-path jrffi-path "jrffi")
(define jrffi (ffi-lib jrffi-path))

(define (get-jrffi-obj name type)
  (get-ffi-obj (regexp-replaces name '((#rx"-" "_"))) jrffi type))

; holds the jnienv for the current pthread
(define current-jnienv #f)

(define (set-current-jnienv! v)
  (set! current-jnienv v))

(cimports
 [attach-current-thread                          : -> __jnienv]
 [create-jvm                                     : (_list i _JavaVMOption) _int -> _int]
 [jvm-initialized?      = is-jvm-initialized     : -> _bool]
 [has-exception?        = exception-check        : __jnienv -> __jboolean]
 [exception-occurred                             : __jnienv -> __jobject]
 [exception-clear                                : __jnienv -> _void]
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
 [register-natives                               : __jclass (_list i _JNINativeMethod) __jint -> __jint]
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

(define (get-method-id clss name sig static?)
  (let ([return ((if static? get-static-method-id* get-method-id*) clss name sig)])
    (if return return 
        (begin
          (when (has-exception? current-jnienv)
            (exception-clear current-jnienv))
          (error (format "Method not found ~a: ~a" name sig))))))

(define (get-field-id clss name sig static?)
  (let ([return ((if static? get-static-field-id* get-field-id*) clss name sig)])
    (if return return
        (begin
          (when (has-exception? current-jnienv)
            (exception-clear current-jnienv))
          (error (format "Field not found ~a: ~a" name sig))))))
  
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