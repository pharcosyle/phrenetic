(define-module (configs core)
  #:use-module ((phrenetic home) #:select (create-he he-entire))
  #:use-module ((phrenetic install) #:select (installer-package))
  #:use-module ((phrenetic system) #:select (blank-os btrfs disk-encryption host-info os-base stateless swap tmpfs zswap))
  #:use-module (phrenetic utils)
  #:use-module ((nonphrenetic io-devices) #:select (%macbook-display %macbook-keyboard))
  #:use-module ((nonphrenetic home) #:select (%nixpkgs-allow-unfree))
  #:use-module ((nonphrenetic system) #:select (%nonguix-authorized-keys %nonguix-substitute-urls with-macbook-hardware))
  #:use-module ((ice-9 match) #:select (match-lambda))
  #:use-module ((srfi srfi-1) #:select (delete-duplicates))
  #:export (system
            installer
            primary-he
            aux-he
            guest-he))

(define (system)
  (let ((primary-name "pharcosyle")
        (aux-name "pcoulson")
        (guest-name "mellon")
        (number-of-ttys 6)
        (bluetooth? #t)
        (stateless? #t))
    (as-> blank-os $
      (base-os $ #:who `(((#:name . ,primary-name)
                          (#:comment . "Krzysztof Baranowski")
                          (#:shell . ,primary-he-shell)
                          (#:admin? . #t))
                         ((#:name . ,aux-name)
                          (#:comment . "Phil Coulson")
                          (#:shell . ,aux-he-shell)
                          (#:admin? . #t))
                         ((#:name . ,guest-name)
                          (#:comment . "Mellon")
                          (#:shell . ,guest-he-shell)))
               #:login-number-of-ttys number-of-ttys
               #:guix-authorized-keys %nonguix-authorized-keys
               #:guix-substitute-urls %nonguix-substitute-urls
               #:kmonad? #t
               #:kmonad-users (feature-users `((,primary-name . ,primary-he-kmonad?)
                                               (,aux-name . ,aux-he-kmonad?)
                                               (,guest-name . ,guest-he-kmonad?)))
               #:avahi-users (list primary-name
                                   aux-name
                                   guest-name)
               #:pipewire-users (list primary-name
                                      aux-name
                                      guest-name)
               #:wireless-users (list primary-name
                                      aux-name
                                      guest-name)
               #:bluetooth? bluetooth?
               #:seat-users (list primary-name
                                  aux-name
                                  guest-name)
               #:auto-login? disk-encryption?
               #:auto-login-user primary-name
               #:sessions (->> (list primary-he-sessions
                                     aux-he-sessions
                                     guest-he-sessions)
                               (apply append)
                               delete-duplicates)
               #:hyprland-users (session-users #:session/hyprland
                                               `((,primary-name . ,primary-he-sessions)
                                                 (,aux-name . ,aux-he-sessions)
                                                 (,guest-name . ,guest-he-sessions)))
               #:sway-users (session-users #:session/sway
                                           `((,primary-name . ,primary-he-sessions)
                                             (,aux-name . ,aux-he-sessions)
                                             (,guest-name . ,guest-he-sessions)))
               #:stateless? stateless?)
      (host-info $ #:host-name "frostfire"
                 #:timezone "America/Los_Angeles")
      (tmpfs $ #:mount-point "/"
             ;; TODO trying without at the moment
             ;; #:size (let ((virtual-memory (+ physical-memory swap-size)))
             ;;          (/ virtual-memory 2))
             )
      (case filesystem
        ;; ((#:filesystem/ext4)
        ;;  (ext4 $ #:label root-label
        ;;           #:mount-point "/"))
        ((#:filesystem/btrfs)
         (btrfs $ #:label root-label
                #:mounts (map (match-lambda
                                ((subvol . mount-point)
                                 `((#:mount-point . ,mount-point)
                                   (#:subvol . ,subvol))))
                              mountables))))
      (if disk-encryption?
          (disk-encryption $ #:device-uuid luks-uuid
                           #:mount-points (case filesystem
                                            ;; ((#:filesystem/ext4)
                                            ;;  ...)
                                            ((#:filesystem/btrfs)
                                             (map (match-lambda ((_ . mount-point) mount-point))
                                                  mountables))))
          $)
      (swap $ #:file? #t
            #:file-size swap-size
            #:file-no-cow? (equal? filesystem #:filesystem/btrfs)
            #:stateless? stateless?)
      ;; (zswap $)
      (if stateless?
          (stateless $ #:storage-paths %stateless-storage-paths
                     #:hes `((,primary-name . ,(primary-he))
                             (,aux-name . ,(aux-he))
                             (,guest-name . ,(guest-he)))
                     #:state-users (list primary-name)
                     #:password-users (list primary-name
                                            aux-name
                                            guest-name))
          $)
      (with-macbook-hardware $ #:efi-label efi-label
                             ;; #:xanmod? #t
                             #:virtualization-kvm-users (feature-users `((,primary-name . ,primary-he-virtualization?)
                                                                         (,aux-name . ,aux-he-virtualization?)
                                                                         (,guest-name . ,guest-he-virtualization?)))
                             ;; #:virtualization-binfmt-platforms '("aarch64")
                             #:number-of-ttys number-of-ttys
                             #:bluetooth? bluetooth?
                             #:stateless? stateless?))))

(define physical-memory (* 16 GiB))
;; TODO trying without at the moment
;; (define swap-size (* 32 GiB))
(define swap-size (* 16 GiB)) ; Made this large to accommodate big Guix builds in /tmp, namely the linux kernel.

(define (feature-users flags)
  (->> flags
       (filter (match-lambda ((_ . enable?) enable?)))
       (map (match-lambda ((user . _) user)))))

(define (session-users session user-sessions)
  (->> user-sessions
       (filter (match-lambda
                 ((_ . sessions)
                  (member session sessions))))
       (map (match-lambda ((user . _) user)))))



(define* (base-os os #:rest opts)
  (apply os-base
         os
         (append opts
                 (os+he-shared-opts))))



(define default-shell #:shell/bash)


(define (primary-he)
  (let ((stateless? #t))
    (create-he
     (apply
      he-entire
      ;; TODO
      #:stateless-storage-paths '((#:storage/machine . "/.machine")
                                  (#:storage/persist . "/.persist"))
      #:login-shell primary-he-shell
      #:email "pharcosyle@gmail.com"
      #:kmonad? primary-he-kmonad?
      #:sessions primary-he-sessions
      #:virtualization? primary-he-virtualization?
      #:projects `(("phrenetic" . ((#:doom? . ,doom?)))
                   ("Krush/hyperdrive" . ((#:stateless? . ,stateless?)
                                          (#:doom? . ,doom?)
                                          (#:doom-org? . ,(assoc-ref doom-flags #:doom/org?))
                                          (#:doom-tree-sitter? . ,(assoc-ref doom-flags #:doom/tree-sitter?)))))
      (he-shared-opts)))))

(define primary-he-shell default-shell)
(define primary-he-kmonad? #t)
(define primary-he-sessions '(#:session/hyprland
                              #:session/sway))
(define primary-he-virtualization? #t)


(define (aux-he)
  (create-he
   (apply
    he-entire
    ;; TODO
    #:stateless-storage-paths '((#:storage/machine . "/home/pcoulson/.machine")
                                (#:storage/persist . "/home/pcoulson/.persist"))
    #:login-shell aux-he-shell
    #:email "pharcosyle@gmail.com"
    #:kmonad? aux-he-kmonad?
    #:sessions aux-he-sessions
    #:virtualization? aux-he-virtualization?
    (he-shared-opts))))

(define aux-he-shell default-shell)
(define aux-he-kmonad? #f)
(define aux-he-sessions primary-he-sessions)
(define aux-he-sessions '(;; #:session/gnome
                          #:session/hyprland
                          #:session/sway))
(define aux-he-virtualization? primary-he-virtualization?)


(define (guest-he)
  (create-he
   (apply
    he-entire
    ;; TODO
    #:stateless-storage-paths '((#:storage/machine . "/home/mellon/.machine")
                                (#:storage/persist . "/home/mellon/.persist"))
    #:login-shell guest-he-shell
    #:kmonad? guest-he-kmonad?
    #:sessions guest-he-sessions
    #:virtualization? guest-he-virtualization?
    (he-shared-opts))))

(define guest-he-shell default-shell)
(define guest-he-kmonad? #f)
(define guest-he-sessions '(;; #:session/gnome
                            #:session/hyprland
                            #:session/sway))
(define guest-he-virtualization? #f)



;; HE shared opts
(define doom? #t)
(define doom-flags '((#:doom/calendar? . #t)
                     (#:doom/org? . #t)
                     (#:doom/evil? . #t)
                     (#:doom/tree-sitter? . #t)))

(define (he-shared-opts)
  (append
   (os+he-shared-opts)
   `(#:stateless? #t
     #:nix-nixpkgs-config-settings ,%nixpkgs-allow-unfree
     #:doom? ,doom?
     #:doom-flags ,(acons #:doom/icons? #t doom-flags)
     #:doom-ui? #t
     #:bluetooth? #t
     #:sessions-inputs ,(list %macbook-keyboard)
     #:sessions-outputs ,(list %macbook-display)

     ;; NEW package parameterization
     #:emacs-pgtk ,(@ (gnu packages emacs) emacs-pgtk)
     #:sessions-swayidle ,(@ (gnu packages wm) swayidle)
     #:apps-make-google-chrome ,(@ (nongnu packages chrome) make-google-chrome-stable))))



;; OS+HE shared opts
(define (os+he-shared-opts)
  `(#:package-managers (#:pm/guix #:pm/nix)
    #:console? #t
    #:desktop? #t
    #:media #:media/pipewire
    ;; #:login-manager #:lm/gdm

    ;; NEW package parameterization
    #:kmonad ,(@ (gnu packages haskell-apps) kmonad)
    #:media-pipewire ,(@ (gnu packages linux) pipewire)
    #:sessions-swaylock-effects? #t
    #:sessions-swaylock ,(@ (gnu packages wm) swaylock)
    #:sessions-swaylock-effects ,(@ (gnu packages wm) swaylock-effects)))



(define (installer)
  #f
  ;; (installer-package
  ;;  #:os-name "frostfire"
  ;;  #:boot-target boot-target
  ;;  #:efi-label efi-label
  ;;  #:root-label root-label
  ;;  #:disk-encryption? disk-encryption?
  ;;  #:luks-uuid luks-uuid
  ;;  #:filesystem filesystem
  ;;  #:mountables mountables
  ;;  #:btrfs-mount-flags btrfs-mount-flags
  ;;  #:btrfs-mount-options btrfs-mount-options)
  )





;; TODO temporarily putting these two here as I removed them from system component
(define %stateless-storage-paths
  '((#:storage/machine . "/.machine")
    (#:storage/persist . "/.persist")))

(define %stateless-mountables
  (append
   `(("boot" . "/boot")
     ("guix-store" . "/gnu/store")
     ("guix-var" . "/var/guix"))
   (map (match-lambda
          ((storage . dir)
           (cons (case storage
                   ((#:storage/machine) "machine")
                   ((#:storage/persist) "persist"))
                 dir)))
        %stateless-storage-paths)))





;; System+installer shared opts

(define efi-label "genesis")
(define filesystem #:filesystem/btrfs)
(define root-label "firmament")
(define mountables %stateless-mountables) ; TODO nb: had this in `(if stateless? ...)' before.
(define disk-encryption? #t)
(define luks-uuid "bdda56af-6ca0-4953-bc13-d5af8715e0e5")



(define system (system))
(define installer (installer))
(define primary-he (primary-he))
(define aux-he (aux-he))
(define guest-he (guest-he))
