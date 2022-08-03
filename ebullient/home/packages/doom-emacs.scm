(define-module (ebullient home packages doom-emacs)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages rust-apps) #:select (fd ripgrep))
  #:use-module ((gnu packages version-control) #:select (git))
  #:use-module ((ebullient home packages all-the-icons-fonts) #:select (all-the-icons-fonts)))

(define-public doom-emacs
  (let ((commit "a570ffe16c24aaaf6b4f8f1761bb037c992de877")
        (revision "0"))
    (package
      (name "doom-emacs")
      (version (git-version "3.0.0-dev" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/doomemacs/doomemacs")
               (commit commit)))
         (sha256
          (base32
           "17xns3y3cjpmccdzpzab81770v8v4ivhn7iwvpf1ap8g5x41inav"))
         (file-name (git-file-name name version))
         (patches
          (list (local-file "patches/change-paths.patch")
                ;; (local-file "patches/ligatures.patch")
                ))))
      (build-system copy-build-system) ; TODO LICENSE file gets installed to a "share/" folder, not hurting anything but maybe remove. More generally perhaps check to see what other changes there might be between a simple checkout of the repo and post- copy-build-system
      (arguments
       (list
        ;; #:install-plan
        ;; '(("." "share/doom-emacs/"))
        ;; #:tests? #t
        ;; #:test-command '("./bin/doom" "test")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'install 'symlink-bin
              (lambda _
                (mkdir #$output:bin)
                (symlink (string-append #$output "/bin")
                         (string-append #$output:bin "/bin")))))))
      (propagated-inputs
       (list git
             ripgrep
             fd
             all-the-icons-fonts))
      (outputs '("out" "bin"))
      (synopsis "An Emacs framework for the stubborn martian hacker")
      (description "Doom is a configuration framework for GNU Emacs tailored for
Emacs bankruptcy veterans who want less framework in their frameworks, a modicum
of stability (and reproducibility) from their package manager, and the
performance of a hand rolled config (or better).")
      (home-page "https://github.com/doomemacs/doomemacs")
      (license license:expat))))
