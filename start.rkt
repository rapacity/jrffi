#lang racket/base
(require racket/string racket/list
         (only-in (planet "sutil.scm" ("oesterholt" "ho-utils.plt" 1 6)) glob)
         "c.rkt")

; for internal use only
(define (start-jvm options)
  (let* ([status (create-jvm options (length options))])
    (unless (zero? status)
      (error 'start-jvm 
        (case status
          [(-1) "unknown error"]
          [(-2) "thread detached from the VM"]
          [(-3) "JNI version error"]
          [(-4) "not enough memory"]
          [(-5) "VM already created"]
          [(-6) "invalid arguments"]
          [else "unknown error"])))))

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
