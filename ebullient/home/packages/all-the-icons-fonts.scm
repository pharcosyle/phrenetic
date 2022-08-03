(define-module (ebullient home packages all-the-icons-fonts)
  #:use-module (guix build-system font)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((gnu packages emacs-xyz) #:select (emacs-all-the-icons))
  #:use-module ((gnu packages fonts) #:select (font-google-material-design-icons))
  #:use-module ((ebullient home packages fonts) #:select (font-awesome))
  #:use-module (ebullient utils))

(define-public all-the-icons-fonts
  (let ((commit "7193403b47ad8b65347f9bafc939a152f0fe448a")
        (revision "0"))
    (package
      (name "all-the-icons-fonts")
      (version (git-version (package-version emacs-all-the-icons) revision commit))
      (source
       (origin
         (inherit (package-source emacs-all-the-icons))
         (uri (git-reference
               (inherit (-> emacs-all-the-icons package-source origin-uri))
               (commit commit)))
         (sha256
          (base32
           "1mwh493312xgfra3grgnqncf2ihf7rr6rpwfx8234d46xplf0879"))
         (file-name (git-file-name name version))))
      (build-system font-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-before 'install 'remove-unbundled-fonts
             (lambda _
               (with-directory-excursion "fonts"
                 (for-each delete-file
                           (list "fontawesome.ttf"
                                 "material-design-icons.ttf"))))))))
      (propagated-inputs
       (list font-awesome
             font-google-material-design-icons))
      (home-page (package-home-page emacs-all-the-icons))
      (synopsis (package-synopsis emacs-all-the-icons))
      (description (package-description emacs-all-the-icons))
      (license (package-license emacs-all-the-icons)))))
