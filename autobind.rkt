#lang racket

(require "core.rkt" (for-syntax syntax/parse))
(require racket/system)
(require srfi/13)

(struct method-signature (name args return) #:transparent)
(struct field-signature (name type) #:transparent)

(define (parse-signature name sig)
  (match sig
    [(regexp #rx"^\\(([^)]+)\\)(.+)$" (list _ args return))
     (method-signature name
      (parse-types (open-input-string args))
      (parse-type (open-input-string return)))]
    [type (field-signature name (parse-type (open-input-string type)))]))


(struct jclass-signature (name fields methods) #:transparent)

(define (find-class-signature clss)
  (define extract-name (match-lambda [(regexp #rx" ([^ ]+?)\\(" (list _ name)) name]))
  (define extract-signature (match-lambda [(regexp #rx"^Signature: (.+)$" (list _ signature)) signature]))
  (define (extract-names/signatures port)
    (let ([lines (filter (negate (curry string=? "")) (drop-right (drop (port->lines port) 2) 1))])
      (let loop ([lines lines] [output null])
        (if (null? lines) output
            (loop (cddr lines) 
                  (cons (parse-signature 
                         (extract-name (string-trim (first lines)))
                         (extract-signature (string-trim (second lines))))
                        output))))))
  (let* ([javap (process (string-append "javap -s -public " clss))]
         [input-port (first javap)]
         [error-port (fourth javap)])
    (let ([error-string (read-line error-port)])
      (unless (eof-object? error-string)
        (error error-string)))
    (call-with-values (thunk (partition field-signature? (extract-names/signatures input-port)))
                      (curry jclass-signature (string-trim clss)))))

(define (read-until port pred?)
  (define (aux)
    (let ([char (read-char port)])
      (if (pred? char) null
          (cons char (aux)))))
  (list->string (aux)))

(define (parse-type port)
  (case (read-char port)
    ; FIXME should use _jvector not _jlist
    [(#\[) `(_jlist ,(parse-type port))]
    [(#\Z) `_jboolean]
    [(#\B) `_jbyte]
    [(#\C) `_jchar]
    [(#\S) `_jshort]
    [(#\I) `_jint]
    [(#\J) `_jlong]
    [(#\F) `_jfloat]
    [(#\D) `_jdouble]
    [(#\V) `_jvoid]
    [(#\L) (let ([class-name (read-until port (curry char=? #\;))])
             (cond [(string=? class-name "java/lang/String") `_jstring]
                   [(string=? class-name "java/lang/Object") `_jobject]
                   [else `(_jobject ,class-name)]))]
    [else #f]))

(define (parse-types port)
  (let loop ()
    (let ([type (parse-type port)])
      (if type (cons type (loop))
          null))))

(find-class-signature "java.util.Arrays")
