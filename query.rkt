#lang racket/base

(require racket/system srfi/13 racket/match racket/function racket/list racket/port)

(struct constructor-signature (vararg? args return) #:transparent)
(struct method-signature (name abstract? static? final? vararg? args return) #:transparent)
(struct field-signature (name static? final? type) #:transparent)


(struct jclass-signature (name fields methods constructors) #:transparent)

;#;(match ""
;  [(list (regexp #rx"public( static)?( final)?(?:[^ ]+) ([^(]+)\\((?![.]{3})*([.]{3})?[^)]*\\)"))])
;^ *(public|protected)( abstract)?( static)?( final)?

(define (find-class-signature clss)
  (define (parse-signature name sig properties vararg?)
    (match (list sig properties)
      [(list (regexp #rx"^\\(([^)]*)\\)(.+)$" (list _ args return))
             (list public/protected abstract? static? final?))
       (let* ([arg-types* (parse-types (open-input-string args))]
              [return-type (parse-type (open-input-string return))]
              [arg-types (if (not vararg?) arg-types*
                             (match arg-types* [(list head ... tail) `(,@head (vararg ,@(cdr tail)))]))])
         (if (string=? clss name)
             (constructor-signature vararg? arg-types return-type)
             (method-signature name abstract? static? final? vararg? arg-types return-type)))]
      [(list type (list public/protected abstract? static? final?))
       (field-signature name final? static? (parse-type (open-input-string type)))]))
  (define extract-name (match-lambda [(regexp #rx" ([^ ]*)\\(" (list _ name)) name]
                                     [(regexp #rx" ([^ ]*);" (list _ name)) name]))
  (define vararg? (curry regexp-match? #rx"[.][.][.]"))
  (define extract-properties
    (match-lambda
      [(regexp #rx"^ *(public|protected)( abstract)?( static)?( final)?"
               (list _ public/protected abstract? static? final?))
       (list public/protected (and abstract? #t) (and static? #t) (and final? #t))]))
  (define extract-signature (match-lambda [(regexp #rx" *Signature: (.+) *"(list _ signature)) signature]))
  (define (extract-names/signatures port)
    (let ([lines (filter (negate (curry string=? "")) (drop-right (drop (port->lines port) 2) 1))])
      (let loop ([lines lines] [output null])
        (if (null? lines) output
            (loop (cddr lines) 
                  (cons (parse-signature 
                         (extract-name (first lines))
                         (extract-signature (second lines))
                         (extract-properties (first lines))
                         (vararg? (first lines)))
                        output))))))
  (define (quote-sigil str)
    (regexp-replace* #rx"[$]" str "\\\\$"))
  (let* ([javap (process (string-append "javap -s -protected " (quote-sigil clss)))]
         [input-port (first javap)]
         [error-port (fourth javap)])
    (let ([error-string (read-line error-port)])
      (unless (eof-object? error-string)
        (error error-string)))
    (let ([signatures (extract-names/signatures input-port)])
      (jclass-signature
       (string-trim clss)
       (filter field-signature? signatures)
       (filter method-signature? signatures)
       (filter constructor-signature? signatures)))))

(define (read-until port pred?)
  (define (aux)
    (let ([char (read-char port)])
      (if (pred? char) null
          (cons char (aux)))))
  (list->string (aux)))

(define (parse-type port)
  (let ([msg (read-char port)])
    (if (eof-object? msg) #f
        (case msg 
          [(#\[) `(vector ,@(parse-type port))]
          [(#\Z) `(boolean)]
          [(#\B) `(byte)]
          [(#\C) `(char)]
          [(#\S) `(short)]
          [(#\I) `(int)]
          [(#\J) `(long)]
          [(#\F) `(float)]
          [(#\D) `(double)]
          [(#\V) `(void)]
          [(#\L) (let ([class-name (read-until port (curry char=? #\;))])
                   `(object ,class-name))]
          [else (error 'parse-type (format "Unrecognized Type: ~a" msg))]))))


(define (parse-types port)
  (let loop ()
    (let ([type (parse-type port)])
      (if type (cons type (loop))
          null))))

(provide (all-defined-out))


