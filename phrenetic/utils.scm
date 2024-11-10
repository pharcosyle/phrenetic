(define-module (phrenetic utils)
  #:use-module (guix build-system trivial)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu services)
  #:use-module (phrenetic common utils)
  #:use-module ((srfi srfi-1) #:select (remove))
  #:export (simple-package
            with-git-url
            with-git-version
            with-git-commit
            remove-services-by-types
            only-extensions
            without-extensions))

(re-export-public-interface (phrenetic common utils))

(define simple-package
  (package
    (name #f)
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (synopsis #f)
    (description #f)
    (home-page #f)
    (license #f)))

(define (with-git-url pkg url)
  (package
    (inherit pkg)
    (source
     (origin
       (inherit (package-source pkg))
       (uri (git-reference
             (inherit (-> pkg package-source origin-uri))
             (url url)))))))

(define* (with-git-version pkg
                           #:key
                           version
                           commit-proc
                           hash)
  (with-git pkg
            #:upstream-version version
            #:commit (if commit-proc
                         (commit-proc version)
                         version)
            #:hash hash))

(define* (with-git-commit pkg
                          #:key
                          upstream-version
                          (revision "0")
                          commit
                          hash)
  (with-git pkg
            #:upstream-version upstream-version
            #:revision revision
            #:commit commit
            #:hash hash))

(define* (with-git pkg
                   #:key
                   upstream-version
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

(define* (remove-services-by-types services #:rest kinds)
  (remove (lambda (s)
            (member (service-kind s) kinds))
          services))

(define* (change-extensions f st #:rest exts)
  (service-type
   (inherit st)
   (extensions
    (f (lambda (candidate)
         (member (service-extension-target candidate) exts))
       (service-type-extensions st)))))

(define only-extensions
  (partial change-extensions filter))

(define without-extensions
  (partial change-extensions remove))
