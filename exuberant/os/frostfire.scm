(define-module (exuberant os frostfire)
  #:use-module (ebullient system base-os))

(define os
  (base-os #:host-name "frostfire"
           #:timezone "America/Los_Angeles"
           #:accounts '(((#:name . "pharcosyle")
                         (#:comment . "Krzysztof Baranowski")
                         (#:admin? . #t))
                        ((#:name . "pcoulson")
                         (#:comment . "Phil Coulson")
                         (#:admin? . #t))
                        ((#:name . "gandalf")
                         (#:comment . "Speak Friend and Enter")
                         (#:password . "mellon")))
           ;; #:auto-login "pharcosyle" ; REVIEW Enable this later.
           #:disk-encryption? #t
           #:luks-uuid "bdda56af-6ca0-4953-bc13-d5af8715e0e5"
           #:stateless? #t
           #:nix? #t
           #:console? #t
           #:desktop? #t
           #:pipewire? #t
           #:bluetooth? #t
           #:sessions '(#:session/gnome
                        #:session/sway)
           #:laptop? #t
           #:macbook? #t))

os
