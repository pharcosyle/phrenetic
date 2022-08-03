(define-module (ebullient utils)
  #:use-module (guix git-download)
  #:use-module ((guix inferior) #:select (inferior-for-channels lookup-inferior-packages))
  #:use-module (guix packages)
  #:use-module (gnu services)
  #:use-module ((ice-9 match) #:select (match-lambda))
  #:use-module ((srfi srfi-1) #:select (take drop remove fold first))
  #:export (update-list
            when-let
            if-let
            when-not
            if-not
            ->
            ->>
            as->
            keyvals
            update-services*
            update-services
            ;; get-inferior-package
            with-git-version
            with-git-commit
            with-git-url))

(define (update-list l pred f)
  (map (lambda (x)
         (if (pred x) (f x) x))
       l))

(define-syntax when-let
  (syntax-rules ()
    ((_ ((var expr)) body ...)
     (let ((var expr))
       (when var body ...)))))

(define-syntax if-let
  (syntax-rules ()
    ((_ ((var expr)) then else)
     (let ((var expr))
       (if var then else)))))

(define-syntax when-not
  (syntax-rules ()
    ((_ test body ...)
     (when (not test) body ...))))

(define-syntax if-not
  (syntax-rules ()
    ((_ test then else)
     (if (not test) then else))))

(define-syntax ->
  (syntax-rules ()
    ((_ x) x)
    ((_ x (f args ...) expr ...) (-> (f x args ...) expr ...))
    ((_ x f expr ...) (-> (f x) expr ...))))

(define-syntax ->>
  (syntax-rules ()
    ((_ x) x)
    ((_ x (f args ...) expr ...) (->> (f args ... x) expr ...))
    ((_ x f expr ...) (->> (f x) expr ...))))

(define-syntax as->
  (lambda (x)
    (syntax-case x ()
      ((_ exp name) #'exp)
      ((_ exp name form form* ...)
       (with-syntax ((x (datum->syntax #'form (syntax->datum #'name))))
         #'(let ((x exp))
             (as-> form name form* ...)))))))

(define (keyvals m)
  (->> m
       (map (match-lambda
              ((k . v)
               (list k v))))
       (apply append)))

(define (update-services* services pred update-fn)
  (update-list
   services
   pred
   (lambda (svc)
     (service (service-kind svc)
              (update-fn (service-value svc))))))

(define (update-services services kind update-fn)
  (update-services*
   services
   (lambda (svc)
     (eq? (service-kind svc) kind))
   update-fn))

;; (define (get-inferior-package channels package)
;;   (-> (inferior-for-channels channels)
;;       (lookup-inferior-packages package)
;;       first))

(define* (with-git pkg
                   #:key
                   (upstream-version (package-version pkg))
                   revision
                   commit
                   hash)
  (package
    (inherit pkg)
    (version (if revision
                 (git-version upstream-version revision commit)
                 upstream-version))
    (source
     (origin
       (inherit (package-source pkg))
       (uri (git-reference
             (inherit (-> pkg package-source origin-uri))
             (commit commit)))
       (sha256
        (base32
         hash))
       (file-name (git-file-name (package-name pkg) version))))))

(define* (with-git-version pkg #:key version commit hash)
  (with-git pkg
            #:upstream-version version
            #:commit commit
            #:hash hash))

(define* (with-git-commit pkg #:key upstream-version (revision "0") commit hash)
  (with-git pkg
            #:upstream-version upstream-version
            #:revision revision
            #:commit commit
            #:hash hash))

(define (with-git-url pkg url)
  (package
    (inherit pkg)
    (source
     (origin
       (inherit (package-source pkg))
       (uri (git-reference
             (inherit (-> pkg package-source origin-uri))
             (url url)))))))
