(define-module (ebullient home packages emacs)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix utils) #:select (substitute-keyword-arguments))
  #:use-module ((gnu packages compression) #:select (zstd))
  #:use-module ((gnu packages emacs) #:select (emacs emacs-next-pgtk))
  #:use-module ((gnu packages gcc) #:select (gcc-12))
  #:use-module (ebullient utils)
  #:export (emacs-fully-loaded))

(define emacs-with-native-comp (@@ (flat packages emacs) emacs-with-native-comp))

(define* (emacs-fully-loaded #:key (pgtk? #t))
  (-> (if pgtk?
          (-> emacs-next-pgtk
              with-emacs-latest
              with-wayland-super-fix)
          emacs)
      with-native-comp
      with-findable-C-source
      with-zstd
      with-path-integration-improvements))

(define-public (with-emacs-latest emacs)
  (-> emacs
      (with-git-commit #:upstream-version "29.0.50"
                       #:commit "d92fb1592a02f7e34fb82069fc8d61d85dac8a48"
                       #:hash "12xhfiikz349vndfj992mkm0hjvbarc8l4731sk8i76a17nnfkz5")
      (with-git-url "https://github.com/emacs-mirror/emacs")))

(define-public (with-wayland-super-fix emacs)
  (package
    (inherit emacs)
    (source
     (origin
       (inherit (package-source emacs))
       (patches
        (cons "/home/pharcosyle/projects/phrenetic/ebullient/home/packages/patches/wayland-super-fix.patch" ; (local-file "patches/wayland-super-fix.patch") ; TODO why isn't this working with local-file?
              (-> emacs package-source origin-patches)))))))

(define-public (with-native-comp emacs)
  (emacs-with-native-comp emacs gcc-12 'full-aot))

(define-public (with-findable-C-source emacs)
  (package
    (inherit emacs)
    (arguments
     (substitute-keyword-arguments (package-arguments emacs)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'patch-C-source-directory
              (lambda _
                (substitute* "lisp/emacs-lisp/find-func.el"
                  (("\\(expand-file-name \"src\" source-directory\\)")
                   (string-append "\"" #$(file-append (package-source emacs) "/src") "\"")))))))))))

(define-public (with-zstd emacs)
  (package
    (inherit emacs)
    (propagated-inputs
     (modify-inputs (package-propagated-inputs emacs)
       (prepend zstd)))))

(define-public (with-path-integration-improvements emacs)
  (package
    (inherit emacs)
    (source
     (origin
       (inherit (package-source emacs))
       (snippet
        `(begin
           ,(origin-snippet (package-source emacs))
           (let ((wrap-in-quotes (lambda (s)
                                   (string-append "\"" s "\""))))
             (with-directory-excursion "lisp"
               (substitute* "net/tramp.el"
                 (("\\(tramp-default-remote-path" start-of-list)
                  (string-join
                   (cons start-of-list
                         (map wrap-in-quotes
                              (list "~/.config/guix/current/bin"
                                    "~/.guix-home/profile")))
                   " ")))
               (substitute* "man.el"
                 (("\"/usr/local/include\"" last-item)
                  (string-join
                   (list last-item
                         (wrap-in-quotes "~/.guix-home/profile/include"))
                   " ")))))))))))
