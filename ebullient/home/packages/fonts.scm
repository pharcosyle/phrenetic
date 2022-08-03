(define-module (ebullient home packages fonts)
  #:use-module (guix build-system font)
  #:use-module (guix packages)
  #:use-module ((gnu packages fonts) #:select (font-awesome) #:prefix fonts:)
  #:use-module (ebullient utils))

(define-public font-awesome
  (let ((version "6.1.1"))
    (with-git-version
     (package
       (inherit fonts:font-awesome)
       (build-system font-build-system)
       (arguments '()))
     #:version version
     #:commit version
     #:hash "01y8ys9k9cmf1pz4xqya4280yf942dm2h378qsvgip1pla2fms5f")))
