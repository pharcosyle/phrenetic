(define-module (nonphrenetic io-devices)
  #:use-module (gnu system keyboard)
  #:export (%macbook-keyboard
            %macbook-display))

(define %macbook-keyboard
  `((#:keyboard/name . "Apple Inc. Apple Internal Keyboard / Trackpad")
    (#:keyboard/vendor-id . 1452)
    (#:keyboard/product-id . 610)
    (#:keyboard/layout . ,(keyboard-layout "us" #:model "macbook78"))))

(define %macbook-display
  '((#:display/make . "Apple Computer Inc")
    (#:display/model . "Color LCD")
    (#:display/hidpi? . #t)))
