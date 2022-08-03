(define-module (penitent packages datomic)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((gnu packages compression) #:select (unzip))
  #:use-module ((nonguix licenses) #:prefix license:))

(define-public datomic-cli-tools
  (package
    (name "datomic-cli-tools")
    (version "1.0.91")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://datomic-releases-1fc2183a.s3.amazonaws.com/tools/datomic-cli/datomic-cli-" version ".zip"))
       (sha256
        (base32
         "1xicmbsig8f1p5r9rxkhndi0f9l9w421zf49rbx44yc6v0db523b"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("." "bin" #:include-regexp ("^\\./datomic")) ; Files that start with "datomic" are the scripts. Restrict to this subset to automatically pick up any added in the future without being too inclusive about what we add to bin.
         ("README.txt" "share/doc/datomic/"))
       #:phases
       ,#~(modify-phases %standard-phases
            (add-after 'install 'make-scripts-executable
              (lambda _
                (for-each (lambda (f) (chmod f #o555))
                          (find-files (string-append #$output "/bin"))))))))
    (native-inputs
     (list unzip))
    (home-page "https://docs.datomic.com/cloud/index.html")
    (synopsis "CLI tools for Datomic")
    (description synopsis)
    (license (license:nonfree "https://www.datomic.com/cloud-eula.html"))))
