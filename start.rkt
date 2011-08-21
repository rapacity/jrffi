#lang racket/base
(require racket/string racket/list)
(require ffi/unsafe)
(require (only-in (planet "sutil.scm" ("oesterholt" "ho-utils.plt" 1 6)) glob))
(require "c.rkt")

; for internal use only
(define (start-jvm options)
  (let* ([container (create-jvm options (length options))]
         [status (message-status container)]
         [message (message-string container)])
    (free container)
    (unless (zero? status)
      (error 'start-jvm message))))

(define (expand-class-paths paths)
  (string-join (append* (map glob (regexp-split ":" paths))) ":"))

(define (default-start-jvm)
  (start-jvm 
   (list (make-JavaVMOption 
          (string-append "-Djava.class.path=" (expand-class-paths (getenv "CLASSPATH"))) #f))))

; in drracket once the shared library is run, the same jvm instance will be available for all other
; tabs, also when using places we reuse the current-jvm instance, and attach a thread below
(unless (jvm-initialized?)
  (default-start-jvm))

; TODO, detach java thread on exit
(set-current-jnienv! (attach-current-thread))
