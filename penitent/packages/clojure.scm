(define-module (penitent packages clojure)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix utils) #:select (substitute-keyword-arguments)))

(define-public (with-man-pages clojure-tools)
  (package
    (inherit clojure-tools)
    (arguments
     (substitute-keyword-arguments (package-arguments clojure-tools)
       ((#:install-plan install-plan)
        #~(append #$install-plan '(("clojure.1" "/share/man/man1/")
                                   ("clj.1" "/share/man/man1/"))))))))
