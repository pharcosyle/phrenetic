(define-module (nonphrenetic system)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module ((gnu services linux) #:select (kernel-module-loader-service-type))
  #:use-module (gnu system)
  #:use-module ((nongnu packages firmware) #:select (facetimehd-calibration facetimehd-firmware nouveau-firmware))
  #:use-module ((nongnu packages linux) #:select (linux linux-firmware linux-xanmod-version linux-xanmod-revision linux-xanmod-source broadcom-bt-firmware broadcom-sta facetimehd))
  #:use-module ((nongnu system linux-initrd) #:select (microcode-initrd))
  #:use-module (phrenetic system)
  #:use-module (phrenetic utils)
  #:use-module (nonphrenetic io-devices)
  #:use-module ((srfi srfi-1) #:select (any))
  #:export (%nonguix-authorized-keys
            %nonguix-substitute-urls
            linux-kernel
            apple-macbook
            with-macbook-hardware))

(define make-linux-xanmod (@@ (nongnu packages linux) make-linux-xanmod))

(define nonguix-public-key
  '((#:public-key/type . #:public-key.type/ecc)
    (#:public-key.ecc/curve . "Ed25519")
    (#:public-key.ecc/q . "C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98")))

(define %nonguix-authorized-keys
  (list (plain-file "substitutes.nonguix.org.pub"
                    (public-key-serialize nonguix-public-key))))

(define %nonguix-substitute-urls
  (list "https://substitutes.nonguix.org"))

(define-system-comp (linux-kernel #:key
                                  system
                                  xanmod?
                                  xanmod-config-version
                                  linux-customization-params)
  (kernel (linux-with-customizations (get-linux #:system system
                                                #:xanmod? xanmod?
                                                #:xanmod-config-version xanmod-config-version)
                                     linux-customization-params))
  (initrd (if (member system '("x86_64-linux" "i686-linux"))
              (lambda* (file-systems #:rest rest)
                (apply microcode-initrd file-systems
                       #:initrd (operating-system-initrd os)
                       rest))
              (operating-system-initrd os)))
  (firmware (append
             (operating-system-firmware os)
             (list linux-firmware))))

(define* (get-linux #:key
                    system
                    xanmod?
                    xanmod-config-version)
  (or (assoc-ref `(;; ("aarch64-linux" . linux-arm64-generic)
                   ;; ...
                   )
                 system)
      (if xanmod?
          (make-linux-xanmod linux-xanmod-version
                             linux-xanmod-revision
                             linux-xanmod-source
                             #:xanmod-defconfig
                             (string-append "config_x86-64-v"
                                            xanmod-config-version))
          linux)))

(define* (apple-macbook os #:key bluetooth?)
  (-> os
      (apple-broadcom-wireless #:bluetooth? bluetooth?)
      apple-facetime-camera))

(define-system-comp (apple-broadcom-wireless #:key bluetooth?)
  (kernel-loadable-modules (cons* broadcom-sta
                                  (operating-system-kernel-loadable-modules os)))
  (kernel-arguments (add-or-update-kernel-argument
                     (operating-system-user-kernel-arguments os)
                     "modprobe.blacklist"
                     '("b43" "b43legacy" "ssb" "bcm43xx" "brcm80211" "brcmfmac" "brcmsmac" "bcma")))
  (firmware (append
             (operating-system-firmware os)
             (if bluetooth?
                 (list broadcom-bt-firmware) '()))))

(define-system-comp (apple-facetime-camera)
  (kernel-loadable-modules (cons* facetimehd
                                  (operating-system-kernel-loadable-modules os)))
  (kernel-arguments (add-or-update-kernel-argument
                     (operating-system-user-kernel-arguments os)
                     "modprobe.blacklist"
                     '("bdc_pci")))
  (firmware (cons* facetimehd-firmware
                   facetimehd-calibration
                   (operating-system-firmware os)))
  (services
   (cons* (simple-service 'facetimehd
                          kernel-module-loader-service-type
                          '("facetimehd"))
          (operating-system-user-services os))))

(define (add-or-update-kernel-argument kargs param-name vs)
  (let ((param? (lambda (p)
                  (and (string? p)
                       (string-prefix? param-name p))))
        (vs* (string-join vs ",")))
    (if (any param? kargs)
        (map (lambda (p)
               (if (param? p)
                   (string-append p "," vs*)
                   p))
             kargs)
        (cons (string-append param-name "=" vs*)
              kargs))))

(define-system-comp (nvidia-graphics)
  (firmware (cons* nouveau-firmware
                   (operating-system-firmware os))))

(define* (with-macbook-hardware os
                                #:key
                                efi-label
                                xanmod?
                                (xanmod-config-version "3")
                                virtualization-kvm-users
                                virtualization-binfmt-platforms
                                (console-input %macbook-keyboard)
                                (console-output %macbook-display)
                                number-of-ttys
                                bluetooth?
                                stateless?)
  (as-> os $
    (grub-efi-boot $ #:label efi-label
                   #:stateless? stateless?)
    (linux-kernel $ #:system "x86_64-linux"
                    #:xanmod? xanmod?
                    #:xanmod-config-version xanmod-config-version)
    (virtualization $ #:kvm-support? #t
                    #:kvm-users virtualization-kvm-users
                    #:native-platform "x86_64"
                    #:binfmt-platforms virtualization-binfmt-platforms)
    (console-keyboard-layouts $ #:kb-layout (assoc-ref console-input #:keyboard/layout))
    (console-fonts $ #:number-of-ttys number-of-ttys
                   #:hidpi? (assoc-ref console-output #:display/hidpi?))
    (apple-macbook $ #:bluetooth? bluetooth?)
    (nvidia-graphics $)))
