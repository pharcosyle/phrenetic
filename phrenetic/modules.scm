(define-module (phrenetic modules)
  #:use-module ((guix modules) #:select (guix-module-name?))
  #:use-module (ice-9 match)
  #:export (phrenetic-module-name?))

(define (phrenetic-module-name? name)
  (or (guix-module-name? name)
      (match name
        (('phrenetic _ ...) #t)
        (_ #f))))
