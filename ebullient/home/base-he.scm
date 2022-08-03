(define-module (ebullient home base-he)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (ebullient home components)
  #:use-module (ebullient utils)
  #:export (base-he))

(define* (base-he #:key
                  full-name
                  email
                  (login-shell #:shell/bash)
                  developer?
                  (interactive-shell (if developer?
                                         #:shell/zsh login-shell))
                  nix?
                  desktop?
                  pipewire?
                  sessions)
  (home-environment
   (services
    (filter
     service?
     (let ((font-mono font-monospace))
       (append
        (he-packages)
        (he-shells #:login-shell login-shell
                   #:interactive-shell interactive-shell)
        (he-fonts #:fonts (list font-mono))
        (if nix?
            (he-nix) '())
        (if developer?
            (append
             (he-emacs #:zsh-vterm? (eq? interactive-shell #:shell/zsh))
             (he-doom #:full-name full-name
                      #:email email
                      #:font font-mono)
             (he-git #:full-name full-name
                     #:email email)
             (he-ssh)
             (he-direnv #:shell interactive-shell))
            '())
        (if desktop?
            (append
             (he-desktop-packages)
             (he-xdg)
             (if pipewire?
                 (he-pipewire) '())
             (he-flatpak)
             (if (member #:session/sway sessions)
                 (let ((screen-locker swaylock-screen-locker))
                   (append
                    (he-sway #:pipewire? pipewire?
                             #:screen-locker screen-locker
                             #:idle-manager swayidle-idle-manager
                             #:application-launcher rofi-application-launcher
                             #:default-terminal alacritty-terminal
                             #:backup-terminal alacritty-terminal)
                    (he-swaylock)
                    (he-swayidle #:sway? #t
                                 #:screen-locker screen-locker)
                    (he-waybar #:sway? #t
                               #:font font-mono)
                    (he-rofi #:wayland? #t)
                    (he-alacritty #:font font-mono)
                    ;; TODO
                    ;; (list
                    ;;  (service gammastep-applet-service-type)
                    ;;  (service network-manager-applet-service-type)
                    ;;  (service udiskie-applet-service-type))
                    ))
                 '()))
            '())))))))
