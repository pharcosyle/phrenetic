(define-module (ebullient home packages emacs-xyz)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system emacs)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix utils) #:select (substitute-keyword-arguments))
  #:use-module ((gnu packages emacs-xyz) #:select (emacs-tldr) #:prefix emacs-xyz:)
  #:use-module ((gnu packages compression) #:select (unzip))
  #:use-module (ebullient utils))

(define-public emacs-guix-contributing
  (package
    (name "emacs-guix-contributing")
    (version "0")
    (source
     (local-file "guix-contributing.el"))
    (build-system emacs-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-guix-source-path
           (lambda* (#:key inputs #:allow-other-keys)
             (make-file-writable "guix-contributing.el")
             (emacs-substitute-variables "guix-contributing.el"
               ("guix-contributing-source-path" (search-input-directory inputs "share/guix-emacs-development"))))))))
    (inputs
     (list guix-emacs-development))
    (home-page #f)
    (synopsis "Some Emacs integration to assist in contributing to Guix")
    (description "See https://guix.gnu.org/manual/en/guix.html#The-Perfect-Setup")
    (license license:gpl3+)))

(define guix-emacs-development*
  (let ((commit "bf0389a3806509650b7a8425973ac5aac722901a")
        (revision "0"))
    (package
      (name "guix-emacs-development")
      (version (git-version "1.3.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.savannah.gnu.org/git/guix.git")
               (commit commit)))
         (sha256
          (base32
           "0x4mpw017q4l14aimlyzxxa68jz2rn6zb8m8l7s791fkkdn2paja"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         '(("etc/copyright.el" "share/guix-emacs-development/etc/copyright.el")
           ("etc/snippets" "share/guix-emacs-development/etc/snippets"))))
      (home-page #f)
      (synopsis #f)
      (description #f)
      (license #f))))

(define guix-emacs-development
  (with-git-url guix-emacs-development* "https://github.com/guix-mirror/guix"))

(define-public emacs-tldr
  (package
    (inherit emacs-xyz:emacs-tldr)
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-xyz:emacs-tldr)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'patch-tldr-directory-path
              (lambda* (#:key inputs #:allow-other-keys)
                (emacs-substitute-variables "tldr.el"
                  ("tldr-directory-path" (search-input-directory inputs "share/tldr-pages")))))))))
    (inputs (modify-inputs (package-inputs emacs-xyz:emacs-tldr)
              (prepend tldr-pages)))))

(define tldr-pages
  (let ((commit "7ee7ed0f4afc90ef05b1dde87f6fec5b462a0394")
        (revision "0"))
    (package
      (name "tldr-pages")
      (version (git-version "1.5b" revision commit))
      (source
       (origin
         (method url-fetch/zipbomb)
         (uri (string-append "https://raw.githubusercontent.com/tldr-pages/tldr-pages.github.io/" commit "/assets/tldr.zip"))
         (sha256
          (base32
           "0a09ycfrxiaxv0hzjrkwi56l0ga5d7ydrlclmk4vd6ndb242vhgr"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         '(("." "share/tldr-pages/"))))
      (home-page "https://tldr.sh")
      (synopsis "A collection of community-maintained help pages for command-line tools")
      (description synopsis)
      (license license:cc-by4.0))))
