(define-module (phrenetic stateless)
  #:use-module (guix gexp)
  #:use-module ((guix modules) #:select (source-module-closure))
  #:use-module (guix packages)
  #:use-module (gnu services)
  #:use-module (phrenetic modules)
  #:use-module (phrenetic utils)
  #:export (service-fn
            extend-proc
            tool-package))

(define (service-fn service-type)
  (lambda* (name #:key state ignore)
    (simple-service
     (symbol-append name '-stateless)
     service-type
     `((#:state . ,state)
       (#:ignore . ,ignore)))))

(define (extend-proc config exts)
  (let ((exts* (apply merge-with append exts)))
    (->> (apply list->alist config)
         (acons #:state (assoc-ref exts* #:state))
         (acons #:ignore (assoc-ref exts* #:ignore)))))

(define* (tool-package bin-name #:rest opts)
  (package
    (inherit simple-package)
    (name "stateless")
    (source (tool-program opts))
    (arguments
     `(#:builder
       ,#~(begin
            (let ((bin (string-append #$output "/bin")))
              (mkdir #$output)
              (mkdir bin)
              (symlink #$source (string-append bin "/" #$bin-name))))))))

(define (tool-program opts)
  (program-file
   "stateless"
   (with-imported-modules (source-module-closure
                           '((phrenetic build stateless))
                           #:select? phrenetic-module-name?)
     #~(begin
         (use-modules ((phrenetic build stateless) #:select (tool)))
         (apply tool '#$opts)))))
