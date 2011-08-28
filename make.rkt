#lang racket/base

(require dynext/compile)

(define java-home (getenv "JAVA_HOME"))
(define arch      (getenv "ARCH"))

(unless (and java-home (not (string=? java-home "")))
  (error "ERROR: JAVA_HOME environment variable not set"))

(unless (and arch (not (string=? arch "")))
  (error "ERROR: ARCH environment variable not set"))

(define includes
  (list
   (build-path java-home "include")
   (build-path java-home "include" "linux")))

(define libs
  (list
   "-ljava"
   (format "-L~a" (build-path java-home "jre" "lib" arch))
   (format "-L~a" (build-path java-home "jre" "lib" arch "server"))))
  
(current-extension-compiler-flags (list* "-fPIC" "-shared" "-O2" libs))
 
(define (pre-installer top home)
  (compile-extension #f "jrffi.c" "jrffi.so" includes))

(provide pre-installer)
