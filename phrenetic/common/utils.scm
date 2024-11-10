(define-module (phrenetic common utils)
  #:use-module ((ice-9 match) #:select (match-lambda))
  #:use-module ((srfi srfi-1) #:select (append-map fold fold-right take drop))
  #:export (re-export-public-interface
            when-let
            if-let
            when-not
            if-not
            partial
            rpartial
            ->
            ->>
            as->
            list->alist
            alist->list
            merge
            merge-with
            select-keys
            update-keys
            update-vals
            interpose
            around
            quoted
            single-quoted
            KiB
            MiB
            GiB))

;; Copied this from the "Guile CV" project. The gnu.scm file has something like this too.
(define-macro (re-export-public-interface . args)
  "Re-export the public interface of a module or modules. Invoked as
@code{(re-export-public-interface (mod1) (mod2)...)}."
  (if (null? args)
      '(if #f #f)
      `(begin
         ,@(map (lambda (mod)
                  (or (list? mod)
                      (error "Invalid module specification" mod))
                  `(module-use! (module-public-interface (current-module))
                    (resolve-interface ',mod)))
                args))))

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

(define* (partial f #:rest args)
  (lambda* (#:rest more)
    (apply f (append args more))))

(define* (rpartial f #:rest args)
  (lambda* (#:rest more)
    (apply f (append more args))))

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

(define* (list->alist #:rest kvs)
  (let loop ((res '())
             (more kvs))
    (if (null? more)
        res
        (loop (cons (as-> more $
                          (take $ 2)
                          (apply cons $))
                    res)
              (drop more 2)))))

(define (alist->list alist)
  (append-map
   (match-lambda
     ((k . v)
      (list k v)))
   alist))

(define* (merge #:rest alists)
  (apply merge-with
         (match-lambda
           ((_ . latter) latter))
         alists))

(define* (merge-with f #:rest alists)
  (fold
   (lambda (a res)
     (map
      (lambda (k)
        (cons k
              (let ((res-v (assoc-ref res k))
                    (v (assoc-ref a k)))
                (if (and res-v v)
                    (f res-v v)
                    (or res-v v)))))
      (let ((keys (lambda (alist)
                    (map car alist))))
        (append (keys res)
                (filter (lambda (k)
                          (not (member k (keys res))))
                        (keys a))))))
   '()
   alists))

(define (select-keys alist ks)
  (filter (match-lambda
            ((k . v)
             (member k ks)))
          alist))

(define (update-keys alist f)
  (map (match-lambda
         ((k . v)
          (cons (f k) v)))
       alist))

(define (update-vals alist f)
  (map (match-lambda
         ((k . v)
          (cons k (f v))))
       alist))

(define (interpose sep lst)
  (fold-right (lambda (e acc)
                (cons e
                      (if (null? acc)
                          acc
                          (cons sep acc))))
              '() lst))

(define (around s x)
  (string-append x s x))

(define (quoted s)
  (string-append "\"" s "\""))

(define (single-quoted s)
  (string-append "'" s "'"))

(define KiB (expt 2 10))
(define MiB (expt 2 20))
(define GiB (expt 2 30))
