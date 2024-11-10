(define-module (nonphrenetic packages clojure)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix utils) #:select (substitute-keyword-arguments))
  #:use-module ((gnu packages clojure) #:select (clojure-tools) #:prefix clojure:)
  #:use-module ((gnu packages readline) #:select (rlwrap)))

(define-public clojure-tools
  (package
    (name (package-name clojure:clojure-tools))
    (version "1.11.1.1435")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.clojure.org/install/clojure-tools-" version ".tar.gz"))
       (sha256
        (base32
         "1h4v762agzhnrqs3mj7a84xlw51xv6jh8mvlc5cc83q4n9wwabs5"))))
    (build-system copy-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments clojure:clojure-tools)
       ((#:install-plan install-plan)
        #~(append #$install-plan
                  (list
                   `(,(string-append "clojure-tools-" #$version ".jar") "lib/clojure/libexec/"))))
       ((#:phases phases)
        #~(modify-phases #$phases
            (delete 'copy-tools-deps-alpha-jar)))))
    (inputs
     (list rlwrap))
    (home-page (package-home-page clojure:clojure-tools))
    (synopsis (package-synopsis clojure:clojure-tools))
    (description (package-description clojure:clojure-tools))
    (license (package-license clojure:clojure-tools))))
