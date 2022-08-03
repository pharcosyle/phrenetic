(define-module (ebullient system components)
  #:use-module ((guix modules) #:select (source-module-closure))
  #:use-module (guix gexp)
  #:use-module ((guix store) #:select (%default-substitute-urls))
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  ;; #:use-module ((gnu packages bash) #:select (bash))
  #:use-module ((gnu packages certs) #:select (nss-certs))
  #:use-module ((gnu packages fonts) #:select (font-terminus))
  #:use-module ((gnu packages gnupg) #:select (guile-gcrypt))
  #:use-module ((gnu packages linux) #:select (brightnessctl pipewire-0.3))
  ;; #:use-module ((gnu packages shells) #:select (zsh))
  #:use-module ((gnu packages wm) #:select (swaylock))
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services nix)
  #:use-module (gnu services sound)
  #:use-module (gnu services xorg)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system mapped-devices)
  #:use-module ((nongnu packages linux) #:select (linux linux-firmware broadcom-sta broadcom-bt-firmware))
  #:use-module ((nongnu system linux-initrd) #:select (microcode-initrd))
  #:use-module ((rde packages) #:select ((sway-latest . sway)))
  #:use-module (ebullient utils)
  #:use-module ((ice-9 match) #:select (match-lambda))
  #:use-module ((ice-9 rdelim) #:select (read-string))
  #:use-module ((srfi srfi-1) #:select (any remove iota))
  #:export (barebones-os
            host-info
            grub-efi
            base-services
            nss
            linux-nonfree
            disk-encryption
            stateless
            stateless-persist-dir
            btrfs
            btrfs-subvols
            users
            nonguix-substitutes
            nix
            console-keyboard-layouts
            hidpi-console-font
            desktop-services
            gdm
            gnome-desktop
            wm
            sway-wm
            macbook-wireless
            macbook-kbl))

(define-syntax-rule (system-comp os field ...)
  (operating-system
    (inherit os)
    field ...))

(define-syntax define-system-comp
  (lambda (x)
    (syntax-case x ()
      ((_ (name arg ...) field ...)
       (with-syntax ((os (datum->syntax x 'os)))
         #'(define* (name os arg ...)
             (system-comp os field ...)))))))

(define barebones-os
  (operating-system
    (host-name #f)
    (timezone #f)
    (bootloader #f)
    (services '())
    (file-systems %base-file-systems)))

(define-system-comp (host-info #:key
                               host-name
                               timezone
                               locale)
  (host-name host-name)
  (timezone timezone)
  (locale (or locale "en_US.utf8")))

(define-system-comp (grub-efi #:key
                              (label "genesis")
                              (target "/boot/efi"))
  (bootloader (bootloader-configuration
               (bootloader grub-efi-bootloader)
               (targets (list target))
               (timeout 1)))
  (file-systems
   (cons* (file-system
            (device (file-system-label label))
            (mount-point target)
            (type "vfat"))
          (operating-system-file-systems os))))

(define-system-comp (base-services)
  (services
   (append %base-services
           (operating-system-user-services os))))

(define-system-comp (nss)
  (packages
   (cons* nss-certs
          (operating-system-packages os))))

(define-system-comp (linux-nonfree)
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (cons* linux-firmware
                   (operating-system-firmware os))))

(define* (disk-encryption os
                          #:key
                          source-uuid
                          (target "ciphered")
                          encrypted-mount-points)
  (let ((encrypted-device (mapped-device
                           (source (uuid source-uuid))
                           (targets (list target))
                           (type luks-device-mapping))))
    (system-comp
     os
     (mapped-devices
      (cons* encrypted-device
             (operating-system-mapped-devices os)))
     (file-systems
      (map (lambda (fs)
             (if (member (file-system-mount-point fs) encrypted-mount-points)
                 (file-system
                   (inherit fs)
                   (dependencies (cons* encrypted-device
                                        (file-system-dependencies fs))))
                 fs))
           (operating-system-file-systems os))))))

(define-system-comp (stateless #:key persist-dir bluetooth?)
  (initrd (lambda (file-systems . rest)
            (apply (operating-system-initrd os) file-systems
                   #:volatile-root? #t
                   rest)))
  (file-systems
   (map (lambda (fs)
          (if (member (file-system-mount-point fs)
                      `("/var/guix"
                        "/var/log"
                        ,persist-dir))
              (file-system
                (inherit fs)
                (needed-for-boot? #t))
              fs))
        (operating-system-file-systems os)))
  (services
   (append
    (operating-system-user-services os)
    (list
     (service stateless-service-type
              `((#:persist-dir . ,persist-dir)
                (#:paths . ,(append
                             '("/etc/machine-id"
                               "/var/lib/random-seed"
                               "/etc/NetworkManager/system-connections"
                               "/var/lib/NetworkManager/secret_key"
                               "/etc/guix/signing-key.pub"
                               "/etc/guix/signing-key.sec")
                             (if bluetooth?
                                 '("/var/lib/bluetooth") '())))))))))

(define stateless-persist-dir "/.persist")

(define save-or-restore-users+groups
  #~(lambda (persist-dir save-or-restore)
      (for-each (lambda (path)
                  (let ((files (list path (string-append persist-dir path))))
                    (apply copy-file (case save-or-restore
                                       ((#:restore) (reverse files))
                                       ((#:save) files)))))
                '("/etc/group"
                  "/etc/passwd"
                  "/etc/shadow"))))

(define (populate-gexp config)
  (with-extensions (list guile-gcrypt)
    (with-imported-modules (source-module-closure
                            '((gnu build install)))
      #~(begin
          (use-modules ((gnu build install) #:select (populate-root-file-system)))

          (define* (populate #:key persist-dir #:allow-other-keys)
            (let* ((system-path "/var/guix/profiles/system")
                   (system-1-path "/var/guix/profiles/system-1-link")
                   (system-1-exists? (file-exists? system-1-path))
                   (system-link (readlink system-path))
                   (system-1-link (if system-1-exists?
                                      (readlink system-1-path)
                                      system-link)))
              (populate-root-file-system system-1-link ""
                                         #:extras `((,system-path -> ,system-link)))
              (unless system-1-exists?
                (delete-file system-1-path)))
            (#$save-or-restore-users+groups persist-dir #:restore))

          (apply populate (keyvals '#$config))))))

(define (activate-state-gexp config)
  #~(begin
      (use-modules ((ice-9 match) #:select (match)))

      (define* (activate-state #:key persist-dir paths)

        (define (install path)
          (let loop ((components (string-tokenize path (char-set-complement (char-set #\/))))
                     (base persist-dir)
                     (target-base ""))
            (match components
              ((head tail ...)
               (let* ((path (string-append base "/" head))
                      (target-path (string-append target-base "/" head)))
                 (if (null? tail)
                     ;; The Guix code makes a point of doing stuff like this atomically. I don't really know why but I will too.
                     (let ((pivot (string-append target-path ".new")))
                       (symlink path pivot)
                       (rename-file pivot target-path))
                     (begin
                       (catch 'system-error
                         (lambda ()
                           (mkdir target-path))
                         (lambda args
                           (unless (= EEXIST (system-error-errno args))
                             (apply throw args))))
                       (let ((st (lstat path)))
                         (chown target-path (stat:uid st) (stat:gid st))
                         (chmod target-path (stat:perms st)))
                       (loop tail path target-path))))))))

        (for-each install paths)
        (#$save-or-restore-users+groups persist-dir #:save))

      (apply activate-state (keyvals '#$config))))

(define stateless-service-type
  (service-type
   (name 'stateless)
   (extensions
    (list (service-extension boot-service-type
                             populate-gexp)
          (service-extension activation-service-type
                             activate-state-gexp)))
   (description "Initialize a stateless system.")))

(define-system-comp (btrfs #:key
                           (label "firmament")
                           subvols)
  (file-systems
   (append
    (map (match-lambda
           ((subvol . mount-point)
            (file-system
              (device (file-system-label label))
              (mount-point mount-point)
              (type "btrfs")
              (options (string-append "subvol=" subvol ",compress=zstd,autodefrag")))))
         subvols)
    (operating-system-file-systems os))))

(define* (btrfs-subvols #:key stateless? persist-dir nix?)
  (append
   '(("root-blank" . "/")
     ("boot" . "/boot")
     ("guix-store" . "/gnu/store")
     ("guix-var" . "/var/guix")
     ("log" . "/var/log")
     ("home" . "/home"))
   (if stateless?
       `(("persist" . ,persist-dir)) '())
   (if nix?
       '(("nix" . "/nix")) '())))

(define* (account #:key
                  name
                  comment
                  shell
                  admin?
                  password
                  (salt "toosimple")
                  bluetooth?)
  (user-account
   (name name)
   (comment (or comment ""))
   (group "users")
   (supplementary-groups (append '("netdev" "audio" "video")
                                 (if admin? '("wheel") '())
                                 (if bluetooth? '("lp") '())))
   (shell (case shell
            ;; ((#:shell/bash) (file-append bash "/bin/bash"))
            ;; ((#:shell/zsh) (file-append zsh "/bin/zsh"))
            (else (default-shell))))
   (password (if password
                 (crypt password salt)
                 #f))))

(define-system-comp (users #:key
                           who
                           bluetooth?)
  (users
   (append (map (lambda (user)
                  (apply account (->> user
                                      (acons #:bluetooth? bluetooth?)
                                      keyvals)))
                who)
           (operating-system-users os))))

(define-system-comp (nonguix-substitutes)
  (services
   (update-services
    (operating-system-user-services os)
    guix-service-type
    (lambda (config)
      (guix-configuration
       (inherit config)
       (substitute-urls
        (append (guix-configuration-substitute-urls config)
                (list "https://substitutes.nonguix.org")))
       (authorized-keys
        (append (guix-configuration-authorized-keys config)
                (list (local-file "substitutes.nonguix.org.pub")))))))))

(define-system-comp (nix)
  (services
   (cons* (service nix-service-type)
          (operating-system-user-services os))))

(define-system-comp (console-keyboard-layouts #:key keyboard-layout)
  (keyboard-layout keyboard-layout)
  (bootloader (bootloader-configuration
               (inherit (operating-system-bootloader os))
               (keyboard-layout keyboard-layout))))

(define-system-comp (hidpi-console-font)
  (services
   (update-services
    (operating-system-user-services os)
    console-font-service-type
    (lambda (config)
      (map (lambda (tty-and-font)
             (cons (car tty-and-font)
                   (file-append font-terminus "/share/consolefonts/ter-132n")))
           config)))))

(define-system-comp (desktop-services #:key pipewire? bluetooth?)
  (services
   (as-> (operating-system-user-services os) $
     (append selective-desktop-services $)
     (if pipewire?
         (with-pipewire $) $)
     ;; (if bluetooth?
     ;;     (cons* (bluetooth-service #:auto-enable? #t) $))
     )))

(define (with-pipewire services)
  (as-> services $
    (remove-services $ (list pulseaudio-service-type
                             alsa-service-type))
    (cons* (udev-rules-service 'pipewire-add-udev-rules
                               pipewire-0.3)
           $)))

(define (remove-services services kinds)
  (remove (lambda (s)
            (member (service-kind s) kinds))
          services))

(define selective-desktop-services
  (remove-services %desktop-services
                   (append (map service-kind %base-services)
                           (list gdm-service-type
                                 screen-locker-service-type))))

(define-system-comp (gdm #:key
                         (wayland? #t)
                         auto-login)
  (services
   (cons* (service gdm-service-type
                   (gdm-configuration
                    (auto-login? (if auto-login #t #f))
                    (default-user auto-login)
                    (wayland? wayland?)))
          (operating-system-user-services os))))

(define-system-comp (gnome-desktop)
  (services
   (cons* (service gnome-desktop-service-type)
          (operating-system-user-services os))))

(define-system-comp (wm #:key package services)
  (packages
   (cons* package
          (operating-system-packages os)))
  (services
   (append services
           (operating-system-user-services os))))

(define* (sway-wm os #:key laptop?)
  (wm os
      #:package sway
      #:services (cons* swaylock-service
                        (if laptop?
                            (list brightnessctl-udev-rules) '()))))

(define swaylock-service
  (screen-locker-service swaylock))

(define brightnessctl-udev-rules
  (udev-rules-service 'brightnessctl-add-udev-rules brightnessctl))

(define-system-comp (macbook-wireless #:key bluetooth?)
  (kernel-arguments (with-blacklist (operating-system-user-kernel-arguments os)
                                    "b43,b43legacy,ssb,bcm43xx,brcm80211,brcmfmac,brcmsmac,bcma"))
  (kernel-loadable-modules (cons* broadcom-sta
                                  (operating-system-kernel-loadable-modules os)))
  (firmware (append
             (if bluetooth? (list broadcom-bt-firmware) '())
             (operating-system-firmware os))))

(define (with-blacklist kernel-args new)

  (define blacklist-param "modprobe.blacklist")
  (define (blacklist? ka)
    (string-prefix? blacklist-param ka))

  (if (any blacklist? kernel-args)
      (update-list kernel-args blacklist? (lambda (blacklist)
                                            (string-append blacklist "," new)))
      (cons* (string-append blacklist-param "=" new) kernel-args)))

(define macbook-kbl
  (keyboard-layout "us" #:model "macbook78"))
