#lang racket

(require (for-syntax syntax/parse racket/function racket/syntax srfi/26/cut) "core.rkt" "c.rkt")
(require racket/system)
(require srfi/13)

(struct constructor-signature (vararg? args) #:transparent)
(struct method-signature (name static? vararg? args return) #:transparent)
(struct field-signature (name static? type) #:transparent)


(struct jclass-signature (name fields methods constructors) #:transparent)

(define (find-class-signature clss)
  (define (parse-signature name sig static? vararg?)
    (match sig
      [(regexp #rx"^\\(([^)]*)\\)(.+)$" (list _ args return))
       (let* ([arg-types* (parse-types (open-input-string args))]
              [return-type (parse-type (open-input-string return))]
              [arg-types (if (not vararg?) arg-types*
                             (match arg-types* [(list head ... tail) `(,@head (vararg ,@(cdr tail)))]))])
         (if (string=? clss name)
             (constructor-signature vararg? arg-types)
             (method-signature name static? vararg? arg-types return-type)))]
      [type (field-signature name static?
              (parse-type (open-input-string type)))]))
  (define extract-name (match-lambda [(regexp #rx" ([^ ]*)\\(" (list _ name)) name]
                                     [(regexp #rx" ([^ ]*);" (list _ name)) name]))
  (define vararg? (curry regexp-match? #rx"[.][.][.]"))
  (define static? (curry regexp-match? #rx"^ *[^ ]+ static"))
  (define extract-signature (match-lambda [(regexp #rx" *Signature: (.+) *"(list _ signature)) signature]))
  (define (extract-names/signatures port)
    (let ([lines (filter (negate (curry string=? "")) (drop-right (drop (port->lines port) 2) 1))])
      (let loop ([lines lines] [output null])
        (if (null? lines) output
            (loop (cddr lines) 
                  (cons (parse-signature 
                         (extract-name (first lines))
                         (extract-signature (second lines))
                         (static? (first lines))
                         (vararg? (first lines)))
                        output))))))
  (let* ([javap (process (string-append "javap -s -public " clss))]
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

(define (partition-by proc lst)
  (for/fold ([buckets null]) ([e (in-list lst)])
    (dict-update buckets (proc e) (curry cons e) null)))

(define (parse-types port)
  (let loop ()
    (let ([type (parse-type port)])
      (if type (cons type (loop))
          null))))

(provide (all-defined-out))


