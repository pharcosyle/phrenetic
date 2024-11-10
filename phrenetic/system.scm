(define-module (phrenetic system)
  #:use-module ((guix modules) #:select (source-module-closure))
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix store) #:select (%default-substitute-urls))
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module ((gnu build accounts) #:select (%password-lock-file))
  #:use-module ((gnu packages bash) #:select (bash))
  #:use-module ((gnu packages base) #:select (coreutils))
  #:use-module ((gnu packages firmware) #:select (ath9k-htc-ar7010-firmware ath9k-htc-ar9271-firmware openfwwf-firmware))
  #:use-module ((gnu packages freedesktop) #:select (elogind libinput-minimal switchd))
  #:use-module ((gnu packages fonts) #:select (font-terminus))
  #:use-module ((gnu packages haskell-apps) #:select (kmonad))
  #:use-module ((gnu packages hyprland) #:select (hyprlock))
  #:use-module ((gnu packages libusb) #:select (libmtp))
  #:use-module ((gnu packages linux) #:select (brightnessctl customize-linux e2fsprogs kbd linux-libre util-linux))
  #:use-module ((gnu packages nfs) #:select (nfs-utils))
  #:use-module ((gnu packages shells) #:select (zsh))
  #:use-module ((gnu packages wm) #:select (swaylock swaylock-effects))
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services avahi)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services games)
  #:use-module ((gnu services linux) #:select (zram-device-service-type zram-device-configuration))
  #:use-module (gnu services networking)
  #:use-module (gnu services nix)
  #:use-module (gnu services shepherd)
  #:use-module ((gnu services sound) #:select (alsa-service-type pipewire-service-type pipewire-configuration pulseaudio-service-type))
  #:use-module (gnu services sysctl)
  #:use-module ((gnu services xorg) #:select (gdm-service-type gdm-configuration screen-locker-service-type screen-locker-configuration))
  #:use-module (gnu services virtualization)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system file-systems)
  #:use-module ((gnu system privilege) #:select (privileged-program))
  #:use-module ((gnu system shadow) #:select (account-service-type))
  #:use-module (gnu system mapped-devices)
  #:use-module ((rde gexp) #:select (slurp-file-like))
  #:use-module (rde system services networking)
  #:use-module (phrenetic modules)
  #:use-module ((phrenetic stateless) #:prefix stateless:)
  #:use-module (phrenetic utils)
  #:use-module ((ice-9 match) #:select (match match-lambda))
  #:use-module ((ice-9 string-fun) #:select (string-replace-substring))
  #:use-module ((srfi srfi-1) #:select (append-map delete-duplicates first remove iota))
  #:export (blank-os
            system-comp
            define-system-comp
            base-services
            classic-login
            greetd-login
            accounts
            guix-pm
            public-key-serialize
            nix-pm
            kmonad-tool
            desktop-base-services
            classic-media
            pipewire-media
            network-manager
            bluetooth
            gdm
            desktop-tty
            gnome-desktop
            sway-wm
            host-info
            ext4
            btrfs
            tmpfs
            disk-encryption
            swap
            zram-swap
            zswap
            stateless
            linux-libre-kernel
            linux-with-customizations
            grub-efi-boot
            virtualization
            console-keyboard-layouts
            console-fonts
            os-base))

(define %random-seed-file (@@ (gnu services base) %random-seed-file))

(define blank-os
  (operating-system
    (host-name #f)
    (bootloader (bootloader-configuration
                 (bootloader #f)))
    (firmware (remove (lambda (p)
                        (member p (list ath9k-htc-ar7010-firmware
                                        ath9k-htc-ar9271-firmware
                                        openfwwf-firmware)))
                      %base-firmware))
    (services '())
    (file-systems %base-file-systems)))

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

(define-system-comp (base-services #:key stateless?)
  (services
   (append
    (operating-system-user-services os)
    (list (service virtual-terminal-service-type)
          (service syslog-service-type)
          (service static-networking-service-type
                   (list %loopback-static-networking))
          (service urandom-seed-service-type)
          (service nscd-service-type)
          (service rottlog-service-type)
          (service udev-service-type)
          (service sysctl-service-type)
          (service special-files-service-type
                   `(("/bin/sh" ,(file-append bash "/bin/sh"))
                     ("/usr/bin/env" ,(file-append coreutils "/bin/env")))))
    (if stateless?
        (list
         (stateless-service
          'base-services
          #:state `(((#:path . ,%random-seed-file)
                     (#:storage . #:storage/machine))
                    ((#:path . "/var/db/nscd/")
                     (#:storage . #:storage/machine))
                    ((#:path . "/var/lib/rottlog/")
                     (#:storage . #:storage/machine)))
          #:ignore '(((#:path . "/etc/resolv.conf"))
                     ((#:path . "/etc/resolv.conf.bak"))
                     ((#:path . "/bin/sh")
                      (#:preds . ((#:symlink-to-store))))
                     ((#:path . "/usr/bin/env")
                      (#:preds . ((#:symlink-to-store)))))))))))

(define-system-comp (classic-login #:key number-of-ttys)
  (services
   (append
    (operating-system-user-services os)
    (list (service login-service-type)
          (service agetty-service-type (agetty-configuration
                                        (extra-options '("-L"))
                                        (term "vt100")
                                        (tty #f)
                                        (shepherd-requirement '(syslogd)))))
    (map (lambda (n)
           (service mingetty-service-type (mingetty-configuration
                                           (tty (string-append "tty" (number->string n))))))
         (iota number-of-ttys 1)))))

(define-system-comp (greetd-login #:key number-of-ttys)
  (services
   (cons* (service greetd-service-type
                   (greetd-configuration
                    (terminals
                     (map (lambda (n)
                            (greetd-terminal-configuration
                             (terminal-vt (number->string n))))
                          (iota number-of-ttys 1)))))
          (operating-system-user-services os))))

(define-system-comp (accounts #:key who)
  (services
   (cons* (service accounts-service-type
                   (map (lambda (user)
                          (apply account (alist->list user)))
                        who))
          (operating-system-user-services os))))

(define* (account #:key
                  name
                  comment
                  shell
                  admin?)
  (user-account
   (name name)
   (comment (or comment ""))
   (group "users")
   (supplementary-groups (if admin? '("wheel") '()))
   (shell (case shell
            ((#:shell/bash) (file-append bash "/bin/bash"))
            ((#:shell/zsh) (file-append zsh "/bin/zsh"))))))

(define accounts-service-type
  (service-type (name 'accounts-servce)
                (extensions
                 (list (service-extension account-service-type
                                          identity)))
                (compose identity)
                (extend (lambda (accounts groups)
                          (map (lambda (account)
                                 (account-with-groups account groups))
                               accounts)))
                (description "Add user accounts and extend them with supplementary groups.")))

(define (account-with-groups account groups)
  (user-account
   (inherit account)
   (supplementary-groups (-> (user-account-supplementary-groups account)
                             (append (->> groups
                                          (filter (match-lambda
                                                    ((users . _)
                                                     (member (user-account-name account) users))))
                                          (append-map (match-lambda
                                                        ((_ . groups) groups)))))
                             delete-duplicates))))

(define-system-comp (guix-pm #:key
                             authorized-keys
                             substitute-urls
                             (dev? #t)
                             stateless?)
  (services
   (append
    (operating-system-user-services os)
    (list
     (service guix-service-type
              (guix-configuration
               (authorized-keys (append
                                 %default-authorized-guix-keys
                                 (or authorized-keys '())))
               (substitute-urls (append
                                 %default-substitute-urls
                                 (or substitute-urls '())))
               (extra-options (if dev?
                                   (list "--gc-keep-derivations=yes"
                                         "--gc-keep-outputs=yes")
                                   '()))))
     (service log-cleanup-service-type
              (log-cleanup-configuration
               (directory "/var/log/guix/drvs"))))
    ;; TODO Should these (at least e.g. "/var/guix") be in Stateless > Guix System? Update: or in both places?
    (if stateless?
        (list
         (stateless-service
          'guix-pm
          #:state '(((#:path . "/etc/guix/signing-key.pub")
                     (#:storage . #:storage/machine)
                     (#:parent-dir-perms . ((#:mode . #o111))))
                    ((#:path . "/etc/guix/signing-key.sec")
                     (#:storage . #:storage/machine)
                     (#:parent-dir-perms . ((#:mode . #o111)))))
          #:ignore `(;((#:path . "/gnu/store/")) ; TODO inscrutable error on system build: "guix system: error: path `/gnu/store/' is not in the store"
                     ((#:path . "/var/guix/"))
                     ((#:path . "/etc/guix/acl")
                      (#:preds . ((#:symlink-to-store))))
                     ((#:path . ,(string-append (user-account-home-directory %root-account) "/.cache/guix/"))))))
        '()))))

(define (public-key-serialize pk)
  (let ((sexp `(public-key
                ,(case (assoc-ref pk #:public-key/type)
                   ((#:public-key.type/ecc)
                    `(ecc
                      (curve ,(assoc-ref pk #:public-key.ecc/curve))
                      (q ,(around (assoc-ref pk #:public-key.ecc/q) "#"))))
                   ;; ...
                   ))))
    (call-with-output-string
      (lambda (port)
        (display sexp port)))))

(define-system-comp (nix-pm #:key stateless?)
  (services
   (append
    (operating-system-user-services os)
    (list
     (service (without-profile-extension nix-service-type)
              (nix-configuration
               (extra-config (if stateless?
                                 (list
                                  (nix-conf-serialize '((allow-symlinked-store . #t))))
                                 '()))))
     (service log-cleanup-service-type
              (log-cleanup-configuration
               (directory "/nix/var/log/nix/drvs"))))
    (if stateless?
        (list
         (stateless-service
          'nix-pm
          #:state '(((#:path . "/nix/store/")
                     (#:storage . #:storage/machine))
                    ((#:path . "/nix/var/nix/")
                     (#:storage . #:storage/machine))
                    ((#:path . "/nix/var/log/nix/")
                     (#:storage . #:storage/machine)))
          #:ignore `(((#:path . ,(string-append (user-account-home-directory %root-account) "/.cache/nix/"))))))
        '()))))

(define (nix-conf-serialize config)
  (apply string-append
         (append-map (match-lambda
                       ((n . v)
                        (list (symbol->string n)
                              " = "
                              (match v
                                (#t "true")
                                (#f "false")
                                ;; ((lst ...) (string-join lst))
                                (v v))
                              "\n")))
                     config)))

(define-system-comp (kmonad-tool #:key
                                 users
                                 kmonad)
  (services
   (append
    (operating-system-user-services os)
    (let ((groups '("uinput" "input")))
      (list
       (simple-service 'kmonad-users-add-groups
                       accounts-service-type
                       (cons users groups))
       (udev-rules-service 'kmonad-add-udev-rules
                           kmonad
                           ;; The input group is already part of %base-groups so we don't have to add it here. More importantly, if we do Guix issues a warning on build "guix system: warning: the following groups appear more than once: input" and I don't think it's dangerous but let's just be safe.
                           #:groups (delete "input" groups)))))))

(define-system-comp (desktop-base-services #:key
                                           avahi-users
                                           stateless?)
  (services
   (append
    (operating-system-user-services os)
    (list (simple-service 'libinput udev-service-type (list libinput-minimal))
          (simple-service 'mtp udev-service-type (list libmtp))
          (service sane-service-type)
          polkit-wheel-service
          (simple-service 'nfs-mount-privileged privileged-program-service-type
                          (list (privileged-program
                                 (program (file-append nfs-utils "/sbin/mount.nfs"))
                                 (setuid? #t))))
          fontconfig-file-system-service ; TODO Make a dedicated component for this so the Home fonts component has an analogue in System?
          (service udisks-service-type)
          ;; (service accountsservice-service-type)
          (service cups-pk-helper-service-type)
          (service colord-service-type)
          (service geoclue-service-type)
          (service polkit-service-type)
          (service rtkit-service-type)
          (service dbus-root-service-type)
          (service ntp-service-type)
          (service x11-socket-directory-service-type) ; TODO Gate this, only necessary for wayland with xwayland. In fact, determine if it's necessary at all or, ideally, if something equivalent can be done at the user level.
          ;; TODO Probably doesn't belong in desktop-base-services, sticking it here for the moment.
          ;; TODO Could this be home service instead (except for the udev rules part of course)? Does the daemon need to udev/system-y stuff?
          (service joycond-service-type))
    (avahi-services #:users avahi-users)
    (if stateless?
        (list
         (stateless-service
          'desktop-base-services
          #:state '(((#:path . "/etc/machine-id")
                     (#:storage . #:storage/machine))
                    ;; ((#:path . "/var/lib/AccountsService/")
                    ;;  (#:storage . #:storage/machine))
                    ;; ((#:path . "/var/lib/colord/")
                    ;;  (#:storage . #:storage/machine))
                    ;; ((#:path . "/var/lib/udisks2/")
                    ;;  (#:storage . #:storage/machine))
                    )
          #:ignore `(((#:path . ,(string-append (file-system-mount-point %fontconfig-file-system) "/"))
                      (#:preds . ((#:empty-dir)))))))
        '()))))

(define* (avahi-services #:key users)
  (let ((group "avahi-users"))
    (list
     (service avahi-service-type
              (avahi-configuration
               (dbus-privileged-group group)))
     (simple-service 'avahi-groups
                     account-service-type
                     (list (user-group (name group) (system? #t))))
     (simple-service 'avahi-users-add-groups
                     accounts-service-type
                     (cons users (list group))))))

(define-system-comp (upower-power #:key stateless?)
  (services
   (append
    (operating-system-user-services os)
    (list
     (service upower-service-type
              (upower-configuration
               ;; The defaults are quite low, set them higher. In particular `percentage-action' is 2 which isn't a lot of battery to have in reserve.
               (percentage-low 30)
               (percentage-critical 15)
               (percentage-action 10)
               (critical-power-action 'hybrid-sleep)))) ; This is the default but let's be explicit.
    (if stateless?
        (list
         (stateless-service
          'upower
          #:state '(((#:path . "/var/lib/upower/")
                     (#:storage . #:storage/machine)))))
        '()))))

(define-system-comp (classic-media)
  (services
   (append
    (operating-system-user-services os)
    (list (service pulseaudio-service-type)
          (service alsa-service-type)))))

(define-system-comp (pipewire-media #:key
                                    pipewire
                                    users)
  (services
   (append
    (operating-system-user-services os)
    (list
     (service pipewire-service-type
              (pipewire-configuration
               (pipewire pipewire)))
     (simple-service 'pipewire-users-add-groups
                     accounts-service-type
                     (cons users (list "pipewire")))))))

(define-system-comp (network-manager #:key
                                     (wireless #:wireless/iwd)
                                     wireless-users
                                     stateless?)
  (services
   (append
    (operating-system-user-services os)
    (list
     (service network-manager-service-type
              (network-manager-configuration
               (shepherd-requirement (if wireless
                                         (case wireless
                                           ((#:wireless/wpa-supplicant) '(wpa-supplicant))
                                           ((#:wireless/iwd) '(iwd)))
                                         '())))))
    (if wireless
        (wireless-services #:wireless wireless
                           #:users wireless-users
                           #:stateless? stateless?)
        '())
    (modem-services)
    (if stateless?
        (list
         (stateless-service
          'network-manager
          #:state '(((#:path . "/etc/NetworkManager/system-connections/")
                     (#:storage . #:storage/machine))
                    ((#:path . "/var/lib/NetworkManager/")
                     (#:storage . #:storage/machine)
                     (#:mode . #o700)))))
        '()))))

(define* (wireless-services #:key
                            wireless
                            users
                            stateless?)
  (case wireless
    ((#:wireless/wpa-supplicant) (list
                                  (service wpa-supplicant-service-type)))
    ((#:wireless/iwd) (iwd-services #:users users
                                    #:stateless? stateless?))))

(define* (iwd-services #:key
                       users
                       stateless?)
  (append
   (let ((group "iwd-users"))
     (list
      (service iwd-service-type
               (iwd-configuration
                (main-conf
                 '((Settings ((AutoConnect . #t)))))
                (dbus-users-group group)))
      (simple-service 'iwd-groups
                      account-service-type
                      (list (user-group (name group) (system? #t))))
      (simple-service 'iwd-users-add-groups
                      accounts-service-type
                      (cons users (list group)))))
   (if stateless?
       (list
        (stateless-service
         'iwd
         #:state '(((#:path . "/var/lib/iwd")
                    (#:storage . #:storage/machine)
                    (#:mode . #o700)))))
       '())))

(define (modem-services)
  (list (service modem-manager-service-type)
        (service usb-modeswitch-service-type)))

(define-system-comp (bluetooth #:key stateless?)
  (services
   (append
    (operating-system-user-services os)
    (list
     (service bluetooth-service-type
              (bluetooth-configuration
               (auto-enable? #t))))
    (if stateless?
        (list
         (stateless-service
          'bluetooth
          #:state '(((#:path . "/var/lib/bluetooth/")
                     (#:storage . #:storage/machine)
                     (#:mode . #o700)))))
        '()))))

(define-system-comp (switchd-acpi)
  (services
   (cons* (simple-service
           'switchd
           shepherd-root-service-type
           (list (switchd-shepherd-service)))
          (operating-system-user-services os))))

(define (switchd-shepherd-service)
  (let ((events->commands
         `((power-button . ,suspend-program)
           ;; (restart-button . ...)
           ;; (sleep-button . ...)
           ;; (suspend-button . ...)
           (lid-closed . ,suspend-program)
           ;; (lid-opened . ...)
           ;; (dock-attached . ...)
           ;; (dock-detached . ...)
           )))
    (shepherd-service
     (provision '(switchd))
     (requirement '(udev)) ; Requires '/dev/input' be populated.
     (respawn? #t) ; switchd exits when devices disappear and expects to be restarted when this happens, thus explicitly set respawning to true (even though that's already the default value).
     (start #~(make-forkexec-constructor
               (cons #$(file-append switchd "/bin/switchd")
                     '#$(alist->list
                         (update-keys events->commands
                                      (lambda (event)
                                        (-> event
                                            symbol->string
                                            (string-replace-substring  "-" "_"))))))
               #:log-file "/var/log/switchd.log"))
     (stop #~(make-kill-destructor)))))

(define suspend-program
  (program-file
   "suspend"
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules ((guix build utils) #:select (invoke)))
         ;; TODO For now I'm using the loginctl (elogind) command to implement suspending which presumably will break if I switch seat managers. Figure out a generic alternative (pm-utils...).
         (invoke #$(file-append elogind "/bin/loginctl") "suspend")))))

(define-system-comp (elogind-seats)
  (services
   (cons* (service elogind-service-type
                   (elogind-configuration
                    (kill-user-processes? #f) ; Guix default but state explicitly.
                    ;; (inhibit-delay-max-seconds 0) ; Find/implement an inhibitor lock system / PrepareForSleep + PrepareForShutdown DBus API before disabling this functionality in elogind.
                    (holdoff-timeout-seconds 0)
                    (idle-action 'ignore) ; Guix default but state explicitly.
                    (runtime-directory-size-percent 100) ; "Clearing" this field might be better but elogind-configuration validates that it's a number.
                    (remove-ipc? #f)

                    ;; Ignore all ACPI.
                    (handle-power-key                 'ignore)
                    (handle-suspend-key               'ignore)
                    (handle-hibernate-key             'ignore)
                    (handle-lid-switch                'ignore)
                    (handle-lid-switch-docked         'ignore)
                    (handle-lid-switch-external-power 'ignore)))
          (operating-system-user-services os))))

(define-system-comp (seatd-seats #:key users)
  (services
   (append
    (operating-system-user-services os)
    (let ((group "seat"))
      (list
       (service seatd-service-type
                (seatd-configuration
                 (group group)))
       (simple-service 'seatd-users-add-groups
                       accounts-service-type
                       (cons users (list group))))))))

(define-system-comp (gdm #:key
                         wayland?
                         auto-login?
                         auto-login-user
                         stateless?)
  (services
   (append
    (operating-system-user-services os)
    (list
     (service gdm-service-type
              (gdm-configuration
               (auto-login? auto-login?)
               (default-user auto-login-user)
               (wayland? wayland?)))
     gdm-file-system-service)
    (if stateless?
        (list
         (stateless-service
          'gdm
          #:ignore `(((#:path . ,(string-append (file-system-mount-point %gdm-file-system) "/"))))))
        '()))))

(define-system-comp (desktop-tty #:key
                                 login
                                 (tty-number 2)
                                 auto-login?
                                 auto-login-user)
  (services
   (cons* (switch-to-tty-service #:tty-number tty-number)
          (if auto-login?
              (with-auto-login-to-tty (operating-system-user-services os)
                                      #:login login
                                      #:tty-number tty-number
                                      #:user auto-login-user)
              (operating-system-user-services os)))))

(define* (switch-to-tty-service #:key tty-number)
  (simple-service
   'switch-to-tty
   shepherd-root-service-type
   (list
    (shepherd-service
     (provision '(switch-to-tty))
     (requirement '(virtual-terminal))
     (one-shot? #t)
     (start #~(lambda ()
                (invoke #$(file-append kbd "/bin/chvt")
                        (number->string #$tty-number))))))))

(define* (with-auto-login-to-tty services #:key login tty-number user)
  (case login
    ((#:login/classic)
     (modify-services services
       (mingetty-service-type
        config =>
        (if (equal? (mingetty-configuration-tty config)
                    (string-append "tty" (number->string tty-number)))
            (mingetty-configuration
             (inherit config)
             (auto-login user))
            config))))
    ((#:login/greetd)
     (modify-services services
       (greetd-service-type
        config =>
        (greetd-configuration
         (inherit config)
         (terminals
          (map (lambda (terminal-config)
                 (if (equal? (greetd-terminal-vt terminal-config)
                             (number->string tty-number))
                     (greetd-terminal-configuration
                      (inherit terminal-config)
                      ;; (initial-session-user user)
                      ;; (initial-session-command "default-session-start")
                      )
                     terminal-config))
               (greetd-terminals config)))))))))

(define-system-comp (gnome-desktop)
  (services
   (cons* (service (without-profile-extension gnome-desktop-service-type))
          (operating-system-user-services os))))

(define-system-comp (hyprland-wm #:key users)
  (services
   (append (operating-system-user-services os)
           (list
            (hyprlock-service))
           (brightnessctl-services #:users users))))

(define-system-comp (sway-wm #:key
                             users
                             swaylock-effects?
                             swaylock
                             swaylock-effects)
  (services
   (append (operating-system-user-services os)
           (list
            (swaylock-service #:effects? swaylock-effects?
                              #:swaylock swaylock
                              #:swaylock-effects swaylock-effects))
           ;; TODO Omit dup with hyprland
           ;; (brightnessctl-services #:users users)
           )))

(define* (hyprlock-service)
  (service screen-locker-service-type
           (screen-locker-configuration
            (name "hyprlock")
            (program (file-append hyprlock "/bin/hyprlock"))
            (allow-empty-password? #t)
            (using-setuid? #f))))

(define* (swaylock-service #:key
                           effects?
                           swaylock
                           swaylock-effects)
  (service screen-locker-service-type
           (screen-locker-configuration
            (name "swaylock")
            (program (file-append (if effects?
                                      swaylock-effects
                                      swaylock)
                                  "/bin/swaylock"))
            (allow-empty-password? #t)
            (using-setuid? #f))))

(define* (brightnessctl-services #:key users)
  (let ((groups '("backlight-brightness"
                  "leds-brightness")))
    (list
     (simple-service 'brightnessctl-users-add-groups
                     accounts-service-type
                     (cons users groups))
     (udev-rules-service 'brightnessctl-add-udev-rules
                         brightnessctl
                         #:groups groups))))

(define-system-comp (host-info #:key
                               host-name
                               timezone
                               locale)
  (host-name host-name)
  (timezone (or timezone
                (operating-system-timezone os)))
  (locale (or locale
              (operating-system-locale os))))

(define-system-comp (ext4 #:key
                          label
                          mount-point
                          flags
                          options)
  (file-systems
   (cons* (file-system
            (device (file-system-label label))
            (mount-point mount-point)
            (type "ext4")
            (flags (or flags '()))
            (options (alist->file-system-options (or options '()))))
          (operating-system-file-systems os))))

(define-system-comp (btrfs #:key
                           label
                           mounts)
  (file-systems
   (append
    (operating-system-file-systems os)
    (map (lambda (mount)
           (apply btrfs-mount
                  #:label label
                  (alist->list mount)))
         mounts))))

(define* (btrfs-mount #:key
                      label
                      mount-point
                      subvol
                      (flags '(no-atime))
                      (options '(("compress" . "zstd")
                                 "autodefrag")))
  (file-system
    (device (file-system-label label))
    (mount-point mount-point)
    (type "btrfs")
    (flags (or flags '()))
    (options (as-> (or options '()) $
                   (if subvol
                       (acons "subvol" subvol $) $)
                   (alist->file-system-options $)))))

(define-system-comp (tmpfs #:key
                           mount-point
                           size)
  (file-systems
   (cons* (file-system
            (device "none")
            (mount-point mount-point)
            (options (if size
                         (string-append "size=" (number->string size))
                         #f))
            (type "tmpfs")
            (check? #f))
          (operating-system-file-systems os))))

(define* (disk-encryption os
                          #:key
                          device-uuid
                          (target "deciphered")
                          mount-points)
  (let ((encrypted-device (mapped-device
                           (source (uuid device-uuid))
                           (targets (list target))
                           (type luks-device-mapping))))
    (system-comp
     os
     (mapped-devices
      (cons* encrypted-device
             (operating-system-mapped-devices os)))
     (file-systems
      (map (lambda (fs)
             (if (member (file-system-mount-point fs) mount-points)
                 (file-system
                   (inherit fs)
                   (dependencies (cons* encrypted-device
                                        (file-system-dependencies fs))))
                 fs))
           (operating-system-file-systems os))))))

(define-system-comp (swap #:key
                          (target "/.swap")
                          file?
                          file-size
                          file-no-cow?
                          stateless?)
  (swap-devices
   (cons* (swap-space
           (target target))
          (operating-system-swap-devices os)))
  (services
   (append
    (operating-system-user-services os)
    (let ((name (lambda (base)
                  (symbol-append base '- (string->symbol target)))))
      (if file?
          (cons* (simple-service (name 'swap-create-file)
                                 activation-service-type
                                 (create-swap-file-gexp target file-size file-no-cow?))
                 (if stateless?
                     (list
                      (stateless-service
                       (name 'swap-file)
                       #:state `(((#:path . ,target)
                                  (#:storage . #:storage/machine)
                                  (#:preemptively? . #t)))))
                     '()))
          (list
           (simple-service (name 'create-swap-device)
                           activation-service-type
                           (create-swap-device-gexp target))))))))

(define (create-swap-file-gexp file size no-cow?)
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules ((guix build utils) #:select (mkdir-p invoke symbolic-link?)))

        ;; TODO Forget what I said in the comment below, do do this just for stateless
        ;; Check if the swap file is a symblic link and create its target if so. I do this to make stateless setups work but don't gate it because hey having this functionality available can't hurt.
        (let ((file* (if (and (file-exists? #$file)
                              (symbolic-link? #$file))
                         (readlink #$file)
                         #$file)))
          (when (and (file-exists? file*)
                     (not (= (stat:size (stat file*))
                             #$size)))
            (delete-file file*))
          (when (not (file-exists? file*))
            (mkdir-p (dirname file*))
            (when #$no-cow?
              (invoke #$(file-append coreutils "/bin/truncate") "--size" "0" file*)
              (invoke #$(file-append e2fsprogs "/bin/chattr") "+C" file*))
            (invoke #$(file-append util-linux "/bin/fallocate") "--length" (number->string #$size) file*)
            (chmod file* #o600)
            (invoke #$(file-append util-linux "/sbin/mkswap") file*))))))

(define (create-swap-device-gexp target)
  #~(begin
      ;; ...
      ;; Find the swap device using the same (somewhat complex) logic as `swap-service-type' and run `mkswap' on it (if that hasn't been done already, donno how I'd check).
      ))

(define-system-comp (zswap #:key
                           (compressor "zstd")
                           (allocator "z3fold")
                           (max-pool-percent 20))
  (kernel-arguments (append
                     (operating-system-user-kernel-arguments os)
                     (map (match-lambda
                            ((k . v)
                             (string-append k "=" v)))
                          `(("zswap.enabled" . "1")
                            ("zswap.compressor" . ,compressor)
                            ("zswap.zpool" . ,allocator)
                            ("zswap.max_pool_percent" . ,(number->string max-pool-percent))))))
  (initrd-modules (append
                   (operating-system-initrd-modules os)
                   (list compressor
                         allocator))))

(define-system-comp (zram-swap #:key
                               (compression 'zstd)
                               size
                               memory-limit
                               priority)
  (services
   (cons* (service zram-device-service-type
                   (zram-device-configuration
                    (size size)
                    (compression-algorithm compression)
                    (memory-limit memory-limit)
                    (priority priority)))
          (operating-system-user-services os))))

(define* (stateless os
                    #:key
                    storage-paths
                    hes
                    state-users
                    password-users)
  (as-> os $
    (system-comp
     $
     (file-systems
      (->> (operating-system-file-systems os)
           (map (lambda (fs)
                  (if (member (file-system-mount-point fs)
                              (cons "/var/guix"
                                    (map (match-lambda ((_ . dir) dir))
                                         storage-paths)))
                      (file-system
                        (inherit fs)
                        (needed-for-boot? #t))
                      fs)))))
     (services
      ;; Important: put the stateless-service *after* the rest of the operating system services.
      (append
       (operating-system-user-services os)
       (list
        (service stateless-service-type
                 (let ((machine-dir (assoc-ref storage-paths #:storage/machine)))
                   `(#:storage-paths ,storage-paths
                     #:hes ,(or hes '())
                     #:state-users ,(or state-users '())
                     #:password-files ,(->> (or password-users '())
                                            (cons (user-account-name %root-account))
                                            (map (lambda (user)
                                                   `(,user . ,(string-append machine-dir
                                                                             "/.passwords/"
                                                                             user)))))
                     #:symlink-log-dir? #t
                     #:log-storage-dir ,machine-dir)))))))

    (stateless-guix-system $)))

(define stateless-service-type
  (service-type
   (name 'stateless)
   (extensions
    (list (service-extension boot-service-type
                             (lambda (config)
                               (boot-gexp
                                (assoc-ref config #:symlink-log-dir?)
                                (assoc-ref config #:log-storage-dir)
                                (assoc-ref config #:password-files))))
          (service-extension activation-service-type
                             (lambda (config)
                               (activation-gexp
                                (assoc-ref config #:state-users)
                                (assoc-ref config #:state)
                                (assoc-ref config #:storage-paths))))
          (service-extension shepherd-root-service-type
                             (lambda (config)
                               (activate-he-shepherd-services
                                (assoc-ref config #:hes))))
          (service-extension profile-service-type
                             (lambda (config)
                               (list (stateless:tool-package
                                      "stateless"
                                      (assoc-ref config #:state)
                                      (assoc-ref config #:ignore)
                                      "/"
                                      (assoc-ref config #:storage-paths)
                                      ;; #:additional-known
                                      ;; (system-additional-known (map car (assoc-ref config #:hes)))
                                      ))))))
   (compose identity)
   (extend (lambda (config exts)
             (stateless:extend-proc
              config

              exts
              ;; (append exts
              ;;         (list `((#:ignore . ,(map (lambda (store)
              ;;                                     (string-append store "/"))
              ;;                                   ;; (map (match-lambda ((_ . dir) dir)) (assoc-ref config #:storage-paths))
              ;;                                   '(list
              ;;                                     "/.persist"
              ;;                                     "/.machine")))))
              ;;         (if #t ; TODO (assoc-ref config #:symlink-log-dir?)
              ;;             (list '((#:ignore . ("/var/log"))))
              ;;             '()))
              )))
   (description "Initialize a stateless system.")))

(define (boot-gexp symlink-log-dir? log-storage-dir password-files)
  (gexps->gexp
   (list (populate-root-gexp symlink-log-dir? log-storage-dir)
         (populate-passwords-gexp password-files))))

(define (populate-root-gexp symlink-log-dir? log-storage-dir)
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules ((guix build utils) #:select (mkdir-p))
                     ((ice-9 match) #:select (match-lambda)))

        (for-each
         (match-lambda
           ((dir . mode)
            (mkdir-p dir)
            (chmod dir mode))
           (dir
            (mkdir-p dir)))
         ;; Taken from `populate-root-file-system'. Some of these might not be necessary (for example, "/tmp" and "/var/run" are deleted and recreated in `cleanup-service-type') but it's safest to do them here anyway just in case some early-running code assumes their presence.
         ;; TODO Update: /run and /var/run are probably no longer necessary but I remember some stateless population/activation ordering stuff being subtle so I'm not removing them just yet. Relevant commits:
         ;; - https://git.savannah.gnu.org/cgit/guix.git/commit/?id=e73db355b127b9ca2b5339f645f2d3eb6929531f
         ;; - https://git.savannah.gnu.org/cgit/guix.git/commit/?id=27ee6f06d0ecab58ca3b739c911bacefda440177
         '("/bin"
           "/etc"
           ("/gnu/store" . #o1775)
           "/home"
           "/mnt"
           "/run"
           ("/tmp" . #o1777)
           "/var/db"
           "/var/empty"
           ("/var/lock" . #o1777)
           "/var/run"
           ("/var/tmp" . #o1777)))

        (let ((log-dir "/var/log"))
          (if #$symlink-log-dir?
              (let ((log-state-dir (string-append #$log-storage-dir log-dir)))
                (mkdir-p log-state-dir)
                (symlink log-state-dir log-dir))
              (mkdir-p log-dir))))))

;; HACK Guix assumes /etc/shadow is persistent between reboots and doesn't allow specifying a password file (like Nix's `passwordFile') so imitate this functionality by writing a shadow file on every boot populated with our users and their password hashes. The file doesn't have to be complete: Guix will add the missing entries (for system accounts, etc).
(define (populate-passwords-gexp password-files)
  (with-imported-modules (source-module-closure
                          '((gnu build accounts)))
    #~(begin
        (use-modules ((gnu build accounts) #:select (shadow-entry write-shadow))
                     ((ice-9 match) #:select (match-lambda))
                     ((ice-9 textual-ports) #:select (get-line)))

        (write-shadow
         (map (match-lambda
                ((user . password-file)
                 (shadow-entry
                  (name user)
                  (password (if (file-exists? password-file)
                                (call-with-input-file password-file get-line)
                                ""))
                  (last-change #f))))
              '#$password-files)))))

(define (activation-gexp state-users state storage-paths)
  (gexps->gexp
   (list (create-state-homes-gexp state-users
                                  (map (match-lambda ((_ . dir) dir))
                                       storage-paths))
         (activate-gexp state storage-paths))))

(define (create-state-homes-gexp users storage-dirs)
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules ((guix build utils) #:select (mkdir-p)))

        (for-each
         (lambda (user)
           (let* ((pw (getpwnam user))
                  (home (passwd:dir pw)))
             (for-each (lambda (storage-dir)
                         (let ((state-home (string-append storage-dir home)))
                           (mkdir-p state-home)
                           (chmod state-home #o700)
                           (chown state-home (passwd:uid pw) (passwd:gid pw))))
                       '#$storage-dirs)))
         '#$users))))

(define (activate-gexp state storage-paths)
  (with-imported-modules (source-module-closure
                          '((phrenetic build stateless))
                          #:select? phrenetic-module-name?)
    #~(begin
        (use-modules ((phrenetic build stateless) #:select (activate)))
        (activate '#$state
                  ""
                  '#$storage-paths))))

(define (activate-he-shepherd-services hes)
  (cons (user-homes-shepherd-service (map (match-lambda ((user . _) user))
                                          hes))
        (map (match-lambda
               ((user . he)
                (activate-he-shepherd-service user he)))
             hes)))

(define (activate-he-shepherd-service user he)
  (shepherd-service
   (provision (list (symbol-append 'stateless-home- (string->symbol user))))
   (requirement '(stateless-user-homes))
   (one-shot? #t)
   (start #~(make-forkexec-constructor
             '(#$(file-append he "/activate"))
             #:user #$user
             #:group (group:name (getgrgid (passwd:gid (getpw #$user))))
             #:log-file (string-append "/var/log/stateless-home-" #$user ".log")
             #:environment-variables
             (list (string-append "HOME=" (passwd:dir (getpw #$user))))))
   (stop #~(make-kill-destructor))))

;; HACK Clear the home directories because the `user-homes' shepherd service puts skeleton files in them. Only do this when a new user is added and on startup activation, not reconfigure activation: determine which it is by checking for the presence of ".guix-home".
;; Update: possibly the shepherd servie `transient?` field could be of help here.
(define (user-homes-shepherd-service users)
  (shepherd-service
   (provision '(stateless-user-homes))
   (requirement '(user-homes))
   (one-shot? #t)
   (start (with-imported-modules '((guix build utils)) ; Donno if `with-imported-modules' is necessary. Most of the Shepherd services in Guix (that are defined this way, i.e. with a lambda gexp) don't have it but a few do and it doesn't seem to hurt anything.
            #~(lambda ()
                (define (run)
                  (for-each
                   (lambda (user)
                     (let ((home (passwd:dir (getpwnam user))))
                       (when (not (file-exists? (string-append home "/.guix-home")))
                         (delete-directory-contents home))))
                   '#$users))

                (define (delete-directory-contents dir)
                  (for-each (lambda (name)
                              (delete-file-recursively (string-append dir "/" name)))
                            (scandir dir (lambda (name)
                                           (not (member name '("." "..")))))))

                (run)
                #t)))
   (modules '(((guix build utils) #:select (delete-file-recursively))
              ((ice-9 ftw) #:select (scandir))))))

(define (system-additional-known users)
  #~(append
     #$etc-static-files
     ;; #$root-skeleton-files ; TODO commenting this until I fix / am sure doesn't matter: "warning: importing module (guix config) from the host"
     #$(user-homes users)))

(define etc-static-files
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules ((guix build utils) #:select (symbolic-link?))
                     ((ice-9 ftw) #:select (scandir)))

        (let ((etc-static-dir "/etc/static"))
          (map (lambda (file)
                 (let ((target (string-append "/etc/" file))
                       (source (string-append etc-static-dir "/" file))
                       ;; TODO maybe replace with `directory-exists?', for brevity.
                       (directory? (lambda (path)
                                     (eq? (stat:type (stat path)) 'directory))))
                   (cons target
                         (if (directory? source)
                             (lambda (f)
                               (and (symbolic-link? f)
                                    (string-prefix? etc-static-dir (readlink f))))
                             (const #t))))) ; TODO a file (not a directory or a symlink)
               (scandir etc-static-dir (lambda (name)
                                         (not (member name '("." ".."))))))))))

;; TODO old implementation, remove
;; (define etc-static-files
;;   (with-imported-modules '((guix build utils))
;;     #~(begin
;;         (use-modules ((guix build utils) #:select (find-files)))

;;         (let ((etc-static-dir "/etc/static"))
;;           (map (lambda (file)
;;                  (cons (string-append "/etc" (string-drop file (string-length etc-static-dir)))
;;                        (if (equal? (dirname file) etc-static-dir)
;;                            (const #t) ; TODO a file (not a directory or a symlink)
;;                            symlink-to-store?)))
;;                (find-files etc-static-dir))))))


;; (define root-skeleton-files
;;   (with-imported-modules (source-module-closure
;;                           '((guix build utils)
;;                             (guix utils)))
;;     #~(begin
;;         (use-modules ((guix build utils) #:select (find-files))
;;                      ((guix utils) #:select (readlink*)))

;;         (let ((skel-dir (readlink* "/etc/skel")))
;;           (map (lambda (file)
;;                  (cons (string-append "/root" (string-drop file (string-length skel-dir)))
;;                        (const #t))) ; TODO a file with contents equal to the corresponding skeleton file
;;                (find-files skel-dir))))))

(define (user-homes users)
  #~(map (lambda (user)
           (let ((directory? (lambda (path)
                               (eq? (stat:type (lstat path)) 'directory))))
             (cons (string-append (passwd:dir (getpwnam user)) "/")
                   directory?))) ; TODO stat or lstat probably not important
         '#$users))




;; passwd subcommand WIP

;; (use-modules ((guix build utils) #:select (invoke mkdir-p)))

;; (define (enter-pass)
;;   (let ((pass (getpass "New password: "))
;;         (retype (getpass "Retype new password: ")))
;;     (if (equal? pass retype)
;;         pass
;;         (begin
;;           (display "Passwords do not match.")
;;           #f
;;           ;; (exit)
;;           ))))


;; (let ((pass (enter-pass)))
;;   (when pass
;;     ;; (invoke "/run/privileged/bin/passwd")

;;     (crypt pass (string-append "$" "6" "$" "sosalty"))

;;     (mkdir-p "/.machine/.passwords")
;;     ))

(define (gexps->gexp gexps)
  #~(begin
      #$@gexps))

(define stateless-service
  (stateless:service-fn stateless-service-type))

(define* (stateless-guix-system os)
  (as-> os $
    (system-comp
     $
     (services
      (cons* (stateless-service
              'guix-system
              #:ignore `(;; General Linux system.
                         ((#:path . "/dev/"))
                         ((#:path . "/proc/"))
                         ((#:path . "/run/"))
                         ((#:path . "/sys/"))
                         ((#:path . "/tmp/"))
                         ((#:path . "/var/lock/"))
                         ((#:path . "/var/run/"))
                         ;; ((#:path . "/var/tmp/")) ; FHS specifies this should be preserved between reboots. I'm leaving it commented out for now because I want to know if something shows up. I'll decide then if I should be symlinking individual state files/dirs inside this directory or the directory entirely.
                         ((#:path . "/etc/group"))
                         ((#:path . "/etc/passwd"))
                         ((#:path . "/etc/shadow"))
                         ((#:path . ,%password-lock-file))
                         ;; Specifc to Guix system.
                         ((#:path . "/etc/mtab")
                          (#:preds . ((#:symlink-to "/proc/self/mounts"))))
                         ((#:path . "/etc/ssl")
                          (#:preds . ((#:symlink-to "/run/current-system/profile/etc/ssl"))))
                         ((#:path . "/etc/static")
                          (#:preds . ((#:symlink-to-store))))))
             (operating-system-user-services os))))

    (sudo-lectures $)))

(define-system-comp (sudo-lectures #:key lectures?)
  (services
   (append
    (operating-system-user-services os)
    (if lectures?
        (list
         (stateless-service
          'sudo-lectures
          #:state '(((#:path . "/var/db/sudo/lectured/")
                     (#:storage . #:storage/machine)))))
        '())))
  (sudoers-file
   (if (not lectures?)
       (mixed-text-file
        "sudoers"
        (slurp-file-like (operating-system-sudoers-file os))
        "Defaults lecture=never" "\n")
       (operating-system-sudoers-file os))))

;; ...

(define-system-comp (grub-efi-boot #:key
                                   label
                                   (target "/boot/efi")
                                   stateless?)
  (bootloader (bootloader-configuration
               (inherit (operating-system-bootloader os))
               (bootloader grub-efi-bootloader)
               (targets (list target))))
  (file-systems
   (cons* (file-system
            (device (file-system-label label))
            (mount-point target)
            (type "vfat"))
          (operating-system-file-systems os)))
  (services
   (append
    (operating-system-user-services os)
    (if stateless?
        (list
         (let ((grub-dir "/boot/grub"))
           (match (grub-efi-format+file-name)
             ((format . file-name)
              ;; I could be more exhaustive here if I wanted: locales and modles are in the GRUB package so I could map over them and add ignore entries for each. Unforunately there isn't an easy way to get the built grub.cfg contents for comparison.
              (stateless-service
               'grub-efi
               #:ignore `(((#:path . ,(string-append target "/EFI/Guix/" file-name)))
                          ((#:path . ,(string-append grub-dir "/" format "/")))
                          ((#:path . ,(string-append grub-dir "/locale/")))
                          ((#:path . ,(string-append grub-dir "/fonts/unicode.pf2")))
                          ((#:path . ,(string-append grub-dir "/grub.cfg")))
                          ((#:path . ,(string-append grub-dir "/grubenv"))
                           (#:preds . ((#:file-content ,grubenv))))))))))
        '()))))

(define* (grub-efi-format+file-name #:key removable?)
  (match (assoc-ref '(("x86_64" . ("x86_64-efi" . "x64"))
                      ;; ...
                      )
                    (-> %host-type ; Not sure this is the right way to do this. Will it fail under cross-compilation? What is `%current-(target-)system' in Guix code all about?
                        (string-split #\-)
                        first))
    ((format . base-name)
     (cons format
           (string-append (if removable? "boot" "grub")
                          base-name
                          ".efi")))))

(define (repeat n x)
  (map (lambda _ x) (iota n)))

(define grubenv
  (let* ((package "grub")
         (message (string-append "# GRUB Environment Block" "\n"
                                 "# WARNING: Do not edit this file by tools other than " package "-editenv!!!" "\n"))
         (envblk-size 1024)
         (padding-size (- envblk-size (string-length message))))
    (string-append message
                   (apply string-append (repeat padding-size "#")))))

;; ...

(define-system-comp (linux-libre-kernel #:key
                                        system
                                        linux-customization-params)
  (kernel (linux-with-customizations (get-linux system)
                                     linux-customization-params)))

(define (linux-with-customizations linux params)
  (if params
      (apply customize-linux
             #:linux linux
             params)
      linux))

(define (get-linux system)
  (or (assoc-ref `(;; ("aarch64-linux" . linux-libre-arm64-generic)
                   ;; ...
                   )
                 system)
      linux-libre))

(define-system-comp (virtualization #:key
                                    kvm-support?
                                    kvm-users
                                    native-platform
                                    binfmt-platforms)
  (services
   (append
    (operating-system-user-services os)
    (if kvm-support?
        (list
         (simple-service 'virtualization-users-add-groups
                         accounts-service-type
                         (cons kvm-users '("kvm"))))
        '())
    (let ((platforms (remove (lambda (p)
                               (equal? p native-platform))
                             (or binfmt-platforms '()))))
      (if (not (null? platforms))
          (list
           (service qemu-binfmt-service-type
                    (qemu-binfmt-configuration
                     (platforms (apply lookup-qemu-platforms platforms)))))
          '())))))

(define-system-comp (console-keyboard-layouts #:key kb-layout)
  (keyboard-layout kb-layout)
  (bootloader (bootloader-configuration
               (inherit (operating-system-bootloader os))
               (keyboard-layout kb-layout))))

(define-system-comp (console-fonts #:key
                                   number-of-ttys
                                   hidpi?)
  (services
   (cons* (service console-font-service-type
                   (map (lambda (n)
                          (cons (string-append "tty" (number->string n))
                                (if hidpi?
                                    (file-append font-terminus "/share/consolefonts/ter-132n")
                                    %default-console-font)))
                        (iota number-of-ttys 1)))
          (operating-system-user-services os))))

(define without-profile-extension
  (rpartial without-extensions profile-service-type))

(define* (os-base os
                  #:key
                  (login #:login/greetd)
                  login-number-of-ttys
                  who
                  package-managers
                  guix-authorized-keys
                  guix-substitute-urls
                  console?
                  kmonad?
                  kmonad-users
                  kmonad
                  desktop?
                  avahi-users
                  media
                  media-pipewire
                  pipewire-users
                  (connection-manager #:cm/network-manager)
                  wireless-users
                  bluetooth?
                  login-manager
                  auto-login?
                  auto-login-user
                  (seat-manager #:seats/elogind)
                  seat-users
                  sessions
                  sessions-swaylock-effects?
                  sessions-swaylock
                  sessions-swaylock-effects
                  hyprland-users
                  sway-users
                  stateless?)
  (as-> os $
    (base-services $ #:stateless? stateless?)
    (case login
      ((#:login/classic) (classic-login $ #:number-of-ttys login-number-of-ttys))
      ((#:login/greetd) (greetd-login $ #:number-of-ttys login-number-of-ttys)))
    (accounts $ #:who who)
    (if (member #:pm/guix (or package-managers '()))
        (guix-pm $ #:authorized-keys guix-authorized-keys
                 #:substitute-urls guix-substitute-urls
                 #:stateless? stateless?) $)
    (if (member #:pm/nix (or package-managers '()))
        (nix-pm $ #:stateless? stateless?) $)
    (if console?
        (if kmonad?
            (kmonad-tool $ #:users kmonad-users
                         #:kmonad kmonad)
            $)
        $)
    (if desktop?
        (as-> $ $
          (desktop-base-services $ #:avahi-users avahi-users
                                 #:stateless? stateless?)
          (upower-power $ #:stateless? stateless?)
          (if media
              (case media
                ((#:media/classic) (classic-media $))
                ((#:media/pipewire) (pipewire-media $ #:pipewire media-pipewire
                                                      #:users pipewire-users)))
              $)
          (if connection-manager
              (case connection-manager
                ((#:cm/network-manager) (network-manager $ #:wireless-users wireless-users
                                                         #:stateless? stateless?)))
              $)
          (if bluetooth?
              (bluetooth $ #:stateless? stateless?)
              $)
          (switchd-acpi $)
          (if seat-manager
              (case seat-manager
                ((#:seats/elogind) (elogind-seats $))
                ((#:seats/seatd) (seatd-seats $ #:users seat-users)))
              $)
          (if login-manager
              (case login-manager
                ((#:lm/gdm) (gdm $ #:auto-login? auto-login?
                                 #:auto-login-user auto-login-user
                                 #:stateless? stateless?)))
              (desktop-tty $ #:login login
                           #:auto-login? auto-login?
                           #:auto-login-user auto-login-user))
          (if (member #:session/gnome (or sessions '()))
              (gnome-desktop $) $)
          (if (member #:session/hyprland (or sessions '()))
              (hyprland-wm $ #:users hyprland-users) $)
          (if (member #:session/sway (or sessions '()))
              (sway-wm $ #:users sway-users
                       #:swaylock-effects? sessions-swaylock-effects?
                       #:swaylock sessions-swaylock
                       #:swaylock-effects sessions-swaylock-effects)
              $))
        $)))
