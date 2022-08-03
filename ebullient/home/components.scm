(define-module (ebullient home components)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home-services terminals)
  #:use-module (gnu home-services version-control)
  ;; #:use-module ((gnu packages bash) #:select (bash))
  #:use-module ((gnu packages compression) #:select (zip unzip))
  #:use-module ((gnu packages curl) #:select (curl))
  #:use-module ((gnu packages emacs) #:select (emacs))
  #:use-module ((gnu packages fonts) #:select (font-adobe-source-code-pro font-google-noto))
  #:use-module ((gnu packages freedesktop) #:select (udiskie xdg-utils xdg-user-dirs desktop-file-utils xdg-desktop-portal xdg-desktop-portal-wlr))
  #:use-module ((gnu packages glib) #:select (dbus))
  #:use-module ((gnu packages gnome) #:select (network-manager-applet))
  #:use-module ((gnu packages image) #:select (grim slurp swappy))
  #:use-module ((gnu packages linux) #:select (brightnessctl psmisc pipewire-0.3 wireplumber))
  #:use-module ((gnu packages music) #:select (playerctl))
  #:use-module ((gnu packages package-management) #:select (flatpak))
  #:use-module ((gnu packages pulseaudio) #:select (pulseaudio))
  #:use-module ((gnu packages qt) #:select (qtwayland))
  #:use-module ((gnu packages rust-apps) #:select (swayhide))
  #:use-module ((gnu packages shells) #:select (zsh))
  #:use-module ((gnu packages shellutils) #:select (direnv))
  #:use-module ((gnu packages terminals) #:select (alacritty))
  #:use-module ((gnu packages version-control) #:select (git))
  #:use-module ((gnu packages web) #:select (jq))
  #:use-module ((gnu packages wm) #:select (swaylock swaylock-effects swayidle waybar))
  #:use-module ((gnu packages xdisorg) #:select (gammastep rofi wl-clipboard))
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (rde home services wm)
  #:use-module (rde home services xdisorg)
  #:use-module ((rde packages) #:select (rofi-wayland (sway-latest . sway)))
  #:use-module ((ebullient home packages emacs) #:select (emacs-fully-loaded))
  #:use-module ((ebullient home packages doom-emacs) #:select (doom-emacs))
  #:use-module ((ebullient home packages fonts) #:select (font-awesome))
  #:use-module (ebullient utils)
  #:use-module ((srfi srfi-1) #:select (append-map))

  #:export (he-packages
            he-shells
            he-fonts
            font-monospace
            he-nix
            he-emacs
            he-doom
            he-git
            he-ssh
            he-direnv
            he-xdg
            he-desktop-packages
            he-pipewire
            he-flatpak
            he-sway
            he-swaylock
            swaylock-screen-locker
            he-swayidle
            swayidle-idle-manager
            he-waybar
            he-rofi
            rofi-application-launcher
            he-alacritty
            alacritty-terminal
            gammastep-applet-service-type
            network-manager-applet-service-type
            udiskie-applet-service-type))

(define serialize-elisp-config (@@ (gnu home-services emacs) serialize-elisp-config))

(define* (he-packages #:key
                      (packages (list curl
                                      zip
                                      unzip)))
  (list
   (simple-service
    'packages-add
    home-profile-service-type
    packages)))

(define* (he-shells #:key
                    login-shell
                    interactive-shell
                    ;; (bashrc (local-file "bashrc"))
                    (zshrc (local-file "zshrc")))
  (let ((setup-shell? (lambda (shell)
                         (memq shell (list login-shell interactive-shell)))))
     (list
      (when (setup-shell? #:shell/bash)
        (service home-bash-service-type
                 ;; (home-bash-configuration
                 ;;  (bashrc
                 ;;   (list (local-file "bashrc"))))
                 ))
      (when (setup-shell? #:shell/zsh)
        (service home-zsh-service-type
                 (home-zsh-configuration
                  (zshrc
                   (list zshrc)))))
      (when-not (eq? login-shell interactive-shell)
        (simple-service
         'set-shell-env-var
         home-environment-variables-service-type
         `(("SHELL" . ,(case interactive-shell
                         ;; ((#:shell/bash) (file-append bash "/bin/bash"))
                         ((#:shell/zsh) (file-append zsh "/bin/zsh"))))))))))

(define* (he-fonts #:key fonts)
  (list
   (simple-service
    'fonts-add-packages
    home-profile-service-type
    (map (lambda (font)
           (assoc-ref font #:package))
         fonts))))

(define font-monospace
  `((#:name . "Source Code Pro")
    (#:size . 12)
    (#:weight . ,'medium)
    (#:package . ,font-adobe-source-code-pro)))

(define* (he-nix #:key
                 (nix-channels (local-file "nix-channels"))
                 (nixpkgs-config (local-file "nixpkgs-config.nix")))
  (list
   (simple-service
    'nix-add-channels
    home-files-service-type
    `((".nix-channels"
       ,nix-channels)))

   (simple-service
    'nix-add-nixpkgs-config
    home-xdg-configuration-files-service-type
    `(("nixpkgs/config.nix"
       ,nixpkgs-config)))))

(define* (he-emacs #:key
                   (emacs (emacs-fully-loaded))
                   zsh-vterm?)
  (list
   (simple-service
    'emacs-add-package
    home-profile-service-type
    (list emacs))

   (simple-service
    'emacs-set-editor-env-vars
    home-environment-variables-service-type
    `(("VISUAL" . ,(file-append emacs "/bin/emacsclient"))
      ("EDITOR" . "$VISUAL")))

   (when zsh-vterm?
     (simple-service
      'emacs-zsh-vterm
      home-zsh-service-type
      (home-zsh-extension
       (zshrc
        (list (local-file "zsh-vterm"))))))))

(define* (he-doom #:key
                  (doom-private (local-file "doom-private" #:recursive? #t))
                  full-name
                  email
                  font)
  (list
   (simple-service
    'doom-add-packages
    home-profile-service-type
    (list `(,doom-emacs "bin")))

   (simple-service
    'doom-add-config
    home-xdg-configuration-files-service-type
    `(("emacs"
       ,doom-emacs)
      ("doom"
       ,(directory-union
         "doom-private-with-params"
         `(,doom-private
           ,(file-union
             "params-dir"
             `(("params.el"
                ,(mixed-text-file
                  "params.el"
                  (serialize-elisp-config
                   #f
                   `((provide 'params)
                     (setq param-full-name ,full-name)
                     (setq param-email ,email)
                     (setq param-font-name ,(assoc-ref font #:name))
                     (setq param-font-size ,(assoc-ref font #:size))
                     (setq param-font-weight ,(assoc-ref font #:weight)))))))))))))))

(define* (he-git #:key
                 full-name
                 email
                 sign-commits? ; TODO make this (sign-commits? #t) once I have my gpg stuff set up and I'm passing a gpg-sign-key.
                 gpg-sign-key
                 ;; git-send-email?
                 extra-config)
  (list
   ;; (when git-send-email?
   ;;   (simple-service
   ;;    'git-send-email-package
   ;;    home-profile-service-type
   ;;    (list (list git "send-email"))))
   (service
    home-git-service-type
    (home-git-configuration
     (config
      `((user
         ((name . ,full-name)
          (email . ,email)
          ,@(if sign-commits?
                `((signingkey . ,gpg-sign-key)) '())))
        ;; TODO stuff from feature-git, figure it out later
        ;; (merge
        ;;  ;; diff3 makes it easier to solve conflicts with smerge, zdiff3
        ;;  ;; should make a conflict scope smaller, but guile-git fails if
        ;;  ;; this option is set.
        ;;  ((conflictStyle . diff3)))
        ;; (diff
        ;;  ;; histogram should be smarter about diff generation.
        ;;  ((algorithm . histogram)))
        (commit
         (,@(if sign-commits?
                '((gpgsign . #t)) '())))
        ;; (sendemail
        ;;  ((annotate . #t)))
        ;;  TODO these two (http and gpg) sections were in the RDE example home environment (but notably not in the rde feature-git). Figure out what they're for after I do gpg.
        ;; (http "https://weak.example.com"
        ;;       ((ssl-verify . #f)))
        ;; (gpg
        ;;  ((program . ,(file-append gnupg "/bin/gpg"))))
        ,@(if extra-config
              extra-config '())))))))

(define (he-ssh)
  (list
   (service home-openssh-service-type)))

(define* (he-direnv #:key shell)
  (list
   ;; REVIEW This might not be necessary once I integrate emacs-envrc during my Doom/Straight/Guix integration (clarification: the envrc emacs package needs to have direnv on the command line, the emacs-direnv Guix package bakes it in)
   (simple-service
    'direnv-add-package
    home-profile-service-type
    (list direnv))

   (let ((direnv-hook
          (lambda (shell)
            (let ((direnv-bin (file-append direnv "/bin/direnv")))
              (mixed-text-file
               "direnv-hook"
               "command -v " direnv-bin " > /dev/null && eval \"$(" direnv-bin " hook " shell ")\"")))))
     (case shell
       ;; ((#:shell/bash) (simple-service
       ;;                  'direnv-bash-hook
       ;;                  home-bash-service-type
       ;;                  (home-bash-extension
       ;;                   (bashrc
       ;;                    (list (direnv-hook "bash"))))))
       ((#:shell/zsh) (simple-service
                       'direnv-zsh-hook
                       home-zsh-service-type
                       (home-zsh-extension
                        (zshrc
                         (list (direnv-hook "zsh"))))))))))

(define* (he-desktop-packages
          #:key
          (packages (list ;; "protonvpn-cli" ; TODO Appears to be broken after the big Guix frozen-updates upgrade.

                          ;; nyxt
                          ;; gst-libav
                          ;; gst-plugins-bad
                          ;; gst-plugins-base
                          ;; gst-plugins-good
                          ;; gst-plugins-ugly
                          )))
  (list
   (simple-service
    'desktop-packages-add
    home-profile-service-type
    packages)))

(define* (he-xdg
          #:key
          (xdg-user-directories-configuration
           (let* ((xdg-user-subdir "/files")
                  (xdg-dir (lambda (path)
                             (string-append "$HOME" xdg-user-subdir path))))
             (home-xdg-user-directories-configuration
              (desktop "$HOME/desktop")
              (documents (xdg-dir "/docs"))
              (download "$HOME/dl")
              (music (xdg-dir "/music"))
              (pictures (xdg-dir "/pics"))
              (publicshare (xdg-dir "/public"))
              (templates (xdg-dir "/templates"))
              (videos (xdg-dir "/vids"))))))
  (list
   (simple-service
    'add-xdg-packages
    home-profile-service-type
    (list xdg-utils
          xdg-user-dirs ; I don't think I need this (the only reason would be if programs ever call the `xdg-user-dir' shell command and this package isn't an input to theirs).
          desktop-file-utils)) ; I don't think I need this at all but it adds elisp code (a mode for editing .desktop files). Andrew Tropin has it, I'll keep it just in case.

   (service
    home-xdg-user-directories-service-type
    xdg-user-directories-configuration)))

(define (he-pipewire)
  (append
   (pipewire-services)
   (wireplumber-services)))

(define (pipewire-services)
  (list
   (simple-service
    'pipewire-add-package
    home-profile-service-type
    (list pipewire-0.3))

   (simple-service
    'pipewire-add-shepherd-daemons
    home-shepherd-service-type
    (list
     (shepherd-service
      (provision '(pipewire))
      (start #~(make-forkexec-constructor
                (list #$(file-append pipewire-0.3 "/bin/pipewire"))
                #:log-file (string-append
                            (or (getenv "XDG_LOG_HOME")
                                (format #f "~a/.local/var/log"
                                        (getenv "HOME")))
                            "/pipewire.log")
                #:environment-variables
                (append (list "DISABLE_RTKIT=1")
                        (default-environment-variables))))
      (stop  #~(make-kill-destructor))
      (auto-start? #f))
     (shepherd-service
      (provision '(pipewire-pulse pulseaudio))
      (requirement '(pipewire pipewire-media-session))
      (start #~(make-forkexec-constructor
                (list #$(file-append pipewire-0.3 "/bin/pipewire-pulse"))
                #:log-file (string-append
                            (or (getenv "XDG_LOG_HOME")
                                (format #f "~a/.local/var/log"
                                        (getenv "HOME")))
                            "/pipewire-pulse.log")
                #:environment-variables
                (append (list "DISABLE_RTKIT=1")
                        (default-environment-variables))))
      (stop  #~(make-kill-destructor))
      (auto-start? #f))))

   (simple-service
    'pipewire-add-alsa-config
    home-xdg-configuration-files-service-type
    `(("alsa/asoundrc"
       ,(mixed-text-file
         "asoundrc"
         "<" (file-append pipewire-0.3 "/share/alsa/alsa.conf.d/50-pipewire.conf") ">" "\n"
         "<" (file-append pipewire-0.3 "/share/alsa/alsa.conf.d/99-pipewire-default.conf") ">" "\n"
         "\n"
         "pcm_type.pipewire {" "\n"
         "  lib " (file-append pipewire-0.3 "/lib/alsa-lib/libasound_module_pcm_pipewire.so") "\n"
         "}" "\n"
         "\n"
         "ctl_type.pipewire {" "\n"
         "  lib " (file-append pipewire-0.3 "/lib/alsa-lib/libasound_module_ctl_pipewire.so") "\n"
         "}" "\n"))))))

(define (wireplumber-services)
  (list
   (simple-service
    'wireplumber-add-package
    home-profile-service-type
    (list wireplumber))

   (simple-service
    'wireplumber-add-shepherd-daemon
    home-shepherd-service-type
    (list
     (shepherd-service
      (provision '(wireplumber pipewire-media-session))
      (requirement '(pipewire))
      (start #~(make-forkexec-constructor
                (list #$(file-append wireplumber "/bin/wireplumber"))
                #:log-file (string-append
                            (or (getenv "XDG_LOG_HOME")
                                (format #f "~a/.local/var/log"
                                        (getenv "HOME")))
                            "/wireplumber.log")
                #:environment-variables
                (append (list "DISABLE_RTKIT=1")
                        (default-environment-variables))))
      (stop  #~(make-kill-destructor))
      (auto-start? #f))))))

(define (he-flatpak)
  (list
   (simple-service
    'flatpak-add-packages
    home-profile-service-type
    (list flatpak
          xdg-desktop-portal
          xdg-desktop-portal-wlr))))

;; #:use-module (gnu system keyboard)
;;
;; (define (keyboard-layout-to-sway-config keyboard-layout)
;;   (let ((kb-options (string-join
;;                      (keyboard-layout-options keyboard-layout) ",")))
;;     `((input *
;;              ((xkb_layout  ,(keyboard-layout-name keyboard-layout))
;;               (xkb_variant ,(keyboard-layout-variant keyboard-layout))
;;               (xkb_options ,kb-options))))))

(define* (he-sway #:key
                  pipewire?
                  extra-config
                  (sway-mod 'Mod4)
                  ;; (add-keyboard-layout-to-config? #t)
                  (xwayland? #t)
                  (screen-locker "loginctl lock-session")
                  idle-manager
                  application-launcher
                  default-terminal
                  (backup-terminal default-terminal)
                  (bg-image (local-file "../../res/alucard_bg.png"))) ; TODO relative path is brittle

  (let* ((kb-layout #f ;; (get-value 'keyboard-layout config)
                    )
         (layout-config  (if (and #f ; add-keyboard-layout-to-config?
                                  kb-layout)
                             #f ; (keyboard-layout-to-sway-config kb-layout)
                             '())))
    (list
     (service
      home-sway-service-type
      (home-sway-configuration
       (config
        `((xwayland ,(if xwayland? 'enable 'disable))
          (,#~"")
          ;; ,@layout-config ; TODO [sway-wip]

          (,#~"\n\n# General settings:")
          (set $mod ,sway-mod)
          (set $term ,default-terminal)
          (set $backup-term ,backup-terminal)
          (set $menu ,application-launcher)
          (set $lock ,screen-locker)

          (floating_modifier $mod normal)

          (bindsym --to-code $mod+Shift+r reload)

          ;; TODO [sway-wip] I think I need to do gexp/ungexp with `dbus'
          ;; (,#~"\n\n# Update dbus environment variables:")
          ;; (exec ,(file-append dbus "/bin/dbus-update-activation-environment")
          ;;       WAYLAND_DISPLAY XDG_CURRENT_DESKTOP)

          (,#~"\n\n# Launching external applications:")
          (bindsym $mod+Control+Shift+Return exec $backup-term)
          (bindsym $mod+Return exec $term)

          (bindsym --to-code $mod+Shift+d exec $menu)
          (bindsym --to-code $mod+Shift+l exec $lock)

          (,#~"\n\n# Manipulating windows:")
          (bindsym --to-code $mod+Shift+c kill)
          (bindsym --to-code $mod+Shift+f fullscreen)
          (bindsym $mod+Shift+space floating toggle)
          (bindsym $mod+Ctrl+space focus mode_toggle)

          (bindsym $mod+Left focus left)
          (bindsym $mod+Down focus down)
          (bindsym $mod+Up focus up)
          (bindsym $mod+Right focus right)

          (bindsym $mod+Shift+Left move left)
          (bindsym $mod+Shift+Down move down)
          (bindsym $mod+Shift+Up move up)
          (bindsym $mod+Shift+Right move right)

          (,#~"\n\n# Moving around workspaces:")
          (bindsym $mod+tab workspace back_and_forth)
          ,@(append-map
             (lambda (x)
               `((bindsym ,(format #f "$mod+~a" (modulo x 10))
                          workspace number ,x)
                 (bindsym ,(format #f "$mod+Shift+~a" (modulo x 10))
                          move container to workspace number ,x)))
             (iota 10 1))

          (,#~"\n\n# Scratchpad settings:")
          (bindsym --to-code $mod+Shift+minus move scratchpad)
          (bindsym --to-code $mod+minus scratchpad show)

          (,#~"")
          (default_border pixel)
          (default_floating_border pixel)
          (gaps inner 8)))))

     (when idle-manager
       (simple-service
        'sway-enable-idle-manager
        home-sway-service-type
        `((,#~"")
          (exec ,idle-manager))))

     (simple-service
      'sway-add-brightness-control
      home-sway-service-type
      (let* ((step 10)
             (step->symbol (lambda (op)
                             (symbol-append (string->symbol (number->string step)) '% op)))
             (brightnessctl (file-append brightnessctl "/bin/brightnessctl")))
        `((bindsym --locked XF86MonBrightnessUp exec ,brightnessctl set ,(step->symbol '+))
          (bindsym --locked XF86MonBrightnessDown exec ,brightnessctl set ,(step->symbol '-)))))

     (simple-service
      'sway-add-audio-control
      home-sway-service-type
      ;; TODO what's are the "\\\n"s about? Probably will be clear once I see the generated file.
      ;; TODO maybe parameterize out the step value like in the brightness control code (with `step->symbol')
      (let ((pactl (file-append pulseaudio "/bin/pactl")))
        `((bindsym --locked XF86AudioRaiseVolume "\\\n"
                   exec ,pactl set-sink-mute @DEFAULT_SINK@ "false; \\\n"
                   exec ,pactl set-sink-volume @DEFAULT_SINK@ +5%)
          (bindsym --locked XF86AudioLowerVolume "\\\n"
                   exec ,pactl set-sink-mute @DEFAULT_SINK@ "false; \\\n"
                   exec ,pactl set-sink-volume @DEFAULT_SINK@ -5%)
          (bindsym --locked XF86AudioMute
                   exec ,pactl set-sink-mute @DEFAULT_SINK@ toggle)
          (bindsym --locked XF86AudioMicMute
                   exec ,pactl set-source-mute @DEFAULT_SOURCE@ toggle))))

     (simple-service
      'sway-add-player-controls
      home-sway-service-type
      (let ((playerctl (file-append playerctl "/bin/playerctl")))
        `((bindsym --locked XF86AudioPlay exec ,playerctl play-pause)
          (bindsym --locked XF86AudioPrev exec ,playerctl previous)
          (bindsym --locked XF86AudioNext exec ,playerctl next))))

     (when bg-image
       (simple-service
        'sway-add-bg-image
        home-sway-service-type
        `((output * bg ,bg-image fill))))

     (when extra-config
       (simple-service
        'sway-configuration
        home-sway-service-type
        `(,@extra-config
          (,#~""))))

     (simple-service
      'sway-reload-config-on-change
      home-run-on-change-service-type
      `(("files/.config/sway/config"
         ,#~(system* #$(file-append sway "/bin/swaymsg") "reload"))))

     (simple-service
      'xdg-desktop-portal-wlr-configuration
      home-xdg-configuration-files-service-type
      `(("xdg-desktop-portal-wlr/config"
         ,(mixed-text-file
           "xdg-desktop-portal-wlr-config"
           ;; TODO [sway-wip] do with newlines (note that I should string-join so it's still one arg to `format') (and do it for that sway-screenshot line too)
           ;; TODO [sway-wip] then comment this entire simple-service out.
           #~(format #f "[screencast]
output_name=
max_fps=30
chooser_cmd=~a -f %o -or -c ff0000
chooser_type=simple"
                     #$(file-append slurp "/bin/slurp"))))))

     (simple-service
      'packages-for-sway
      home-profile-service-type
      (list qtwayland ; Copied from RDE: I don't know what this is for.
            swayhide ; TODO I'm pretty sure this isn't required for anything. I'm keeping it around for now so I can try it out, though.
            xdg-desktop-portal
            xdg-desktop-portal-wlr))

     (simple-service
      'set-wayland-specific-env-vars
      home-environment-variables-service-type
      ;; Copied from RDE: I don't know what all of these do.
      (append
       '(("XDG_CURRENT_DESKTOP" . "sway")
         ("XDG_SESSION_TYPE" . "wayland")
         ("SDL_VIDEODRIVER" . "wayland")
         ("MOZ_ENABLE_WAYLAND" . "1")
         ("CLUTTER_BACKEND" . "wayland")
         ("ELM_ENGINE" . "wayland_egl")
         ("ECORE_EVAS_ENGINE" . "wayland-egl")
         ("QT_QPA_PLATFORM" . "wayland-egl")
         ("_JAVA_AWT_WM_NONREPARENTING" . "1"))
       (if pipewire?
           '(("RTC_USE_PIPEWIRE" . "true")) '())))

     (simple-service
      'packages-for-sway-fonts
      home-profile-service-type
      (list font-google-noto)) ; A font with good unicode coverage that should cover most needs.

     (sway-screenshot-service))))

;; TODO [sway-wip] curious what the 303030AA are, wait until I've built the config to check what the finished command looks like. Update: it's a grey color with high opacity I think.
(define (sway-screenshot-service)
  (simple-service
   'sway-screenshot
   home-sway-service-type
   ;; TODO [sway-wip] change these keys? I don't have a Print key (or do I?)
   `((bindsym $mod+Print exec ,shot-output)
     (bindsym $mod+Alt+Print exec ,swappy-clipboard)
     (bindsym $mod+Shift+Print exec ,shot-window-or-selection))))

(define subject-output
  #~(format #f "~a -t get_outputs | ~a -r '.[] | select(.focused) | .name'"
            #$(file-append sway "/bin/swaymsg")
            #$(file-append jq "/bin/jq")))

(define subject-window-or-selection
  #~(format #f "~a -t get_tree | ~a -r '.. | select(.pid? and .visible?) \
| .rect | \"\\(.x),\\(.y) \\(.width)x\\(.height)\"' | ~a -b ~a -B ~a"
            #$(file-append sway "/bin/swaymsg")
            #$(file-append jq "/bin/jq")
            #$(file-append slurp "/bin/slurp")
            "303030AA"
            "303030AA"))

(define* (shot-script subject #:key output geom (file "-"))
  (program-file
   (string-append "sway-shot-" subject)
   #~(system
      (format #f "~a ~a~a~a | ~a"
              #$(file-append grim "/bin/grim")
              #$(if output #~(string-append "-o \"$(" #$output ")\" ") "")
              #$(if geom #~(string-append "-g \"$(" #$geom ")\" ") "")
              #$file
              #$(file-append wl-clipboard "/bin/wl-copy")))))

(define shot-output
  (shot-script "output" #:output subject-output))

(define shot-window-or-selection
  (shot-script "window-or-selection" #:geom subject-window-or-selection))

(define swappy-clipboard
  (program-file
   "sway-swappy-clipboard"
   #~(system
      (format #f "~a | ~a -f -"
              #$(file-append wl-clipboard "/bin/wl-paste")
              #$(file-append swappy "/bin/swappy")))))

(define* (he-swaylock #:key
                      (swaylock-effects? #t)
                      extra-config)
  (list
   (service
    home-swaylock-service-type
    (home-swaylock-configuration
     (swaylock (if swaylock-effects?
                   swaylock-effects
                   swaylock))
     (config
      `((show-failed-attempts . #t)
        (daemonize . #t)
        ;; (show-keyboard-layout . ,show-keyboard-layout?) ; TODO [sway-wip]
        ;; TODO [sway-wip] Andrew Tropin: TODO: Source color from colorscheme
        (color . 3e3e3e)
        (indicator-caps-lock)
        ,@(if extra-config
              extra-config '())))))))

(define swaylock-screen-locker "/run/setuid-programs/swaylock") ; According to Andrew Tropin, this can be changed to a store path once https://issues.guix.gnu.org/53468 is resolved.

(define* (he-swayidle #:key
                      sway?
                      screen-locker
                      (lock-timeout 240)
                      extra-config)
  (list
   (service
    home-swayidle-service-type
    (home-swayidle-configuration
     (config
      `(,@(if screen-locker
              (let ((lock-cmd-quoted (format #f "'~a'" screen-locker)))
                `((lock ,lock-cmd-quoted)
                  (before-sleep ,lock-cmd-quoted)
                  (timeout ,lock-timeout ,lock-cmd-quoted)))
              '())
        ,@(if extra-config
              extra-config '())))))

   (when sway?
     (let* ((swaymsg (file-append sway "/bin/swaymsg"))
            (swaymsg-cmd (lambda (cmd)
                           #~(format #f "'~a \"~a\"'" #$swaymsg #$cmd)))
            (idle-timeout (+ lock-timeout 30)))
       (simple-service
        'swayidle-add-sway-dpms
        home-swayidle-service-type
        `((timeout ,idle-timeout ,(swaymsg-cmd "output * dpms off") resume ,(swaymsg-cmd "output * dpms on"))))))))

(define swayidle-idle-manager (file-append swayidle "/bin/swayidle -w"))

(define* (he-waybar #:key
                    sway?
                    (waybar-modules
                     (list
                      ;; TODO [sway-wip] conditionalize the sway ones?
                      (waybar-sway-workspaces)
                      ;; (waybar-sway-window)
                      (waybar-tray)
                      (waybar-idle-inhibitor)
                      ;; (waybar-temperature)
                      (waybar-sway-language)
                      (waybar-battery)
                      (waybar-clock)))
                    ;; TODO [sway-wip]
                    (base16-css (local-file "/gnu/store/gnp30nm80pd8rc5j7klvm95wkxd9xkqj-rde-5b823d7/rde/features/wm/waybar/base16-default-dark.css"))
                    font
                    ;; (base16-css (local-file "./wm/waybar/base16-default-dark.css"))
                    transitions?)
  (append
   (list
    (service
     home-waybar-service-type
     (home-waybar-configuration
      (config #(((position . top)
                 (name . main))))
      ;; TODO [sway-wip] Andrew Tropin: TODO: fix tray menu styles.
      (style-css
       `(,#~(format #f "@import \"~a\";\n" #$base16-css)
            (*
             ((font-family . #(,@(if font
                                     (list
                                      (-> font (assoc-ref #:name) string->symbol))
                                     '())
                               FontAwesome))
              ,@(if transitions? '() '((transition . none)))
              (box-shadow . none)
              (text-shadow . none)
              (min-height . 0)))

            (tooltip
             ((border . (solid @base02))
              (background . @base01)
              (opacity . 0.9)))

            ((tooltip label)
             ((color . @base05)
              (padding . 0)))

            (#{#waybar}#
             ((color . @base04)
              (background . @base01)))

            (#((.modules-right label)
               (.modules-right image))
             ((margin . (0.4em 0.2em))
              (padding . (0 0.4em))
              (background . @base02)
              (border-radius . 0.2em)))

            (.modules-left
             ((margin-left . 0.2em)))

            (.modules-right
             ((margin-right . 0.2em))))))))

   waybar-modules

   (list
    (simple-service
     'waybar-add-font-package
     home-profile-service-type
     (list font-awesome))

    (simple-service
     'waybar-reload-config-on-change
     home-run-on-change-service-type
     ;; TODO [sway-wip] factor out the command
     `(("files/.config/waybar/style.css"
        ,#~(system* #$(file-append psmisc "/bin/killall") "-SIGUSR2" "waybar"))
       ("files/.config/waybar/config"
        ,#~(system* #$(file-append psmisc "/bin/killall") "-SIGUSR2" "waybar"))))

    ;; I could pass the waybar command to the sway module (like I do with the screen locker and idle manager) instead of making this module aware of Sway, however the the `bar' command can take other arguments (see https://man.archlinux.org/man/sway-bar.5.en) that I might perhaps need in the future (though I think they only apply to swaybar, not waybar).
    (when sway?
      (simple-service
       'waybar-add-to-sway
       home-sway-service-type
       `((bar swaybar_command ,(file-append waybar "/bin/waybar"))))))))

(define* (waybar-module name
                        #:optional
                        config
                        style
                        #:key
                        (placement 'modules-right)
                        (bar-id 'main))
  (simple-service
   (symbol-append 'waybar-module- name)
   home-waybar-service-type
   (home-waybar-extension
    (config `#(((name . ,bar-id)
                (,placement . #(,name))
                (,name . ,(if config
                              config '())))))
    (style-css (if style
                   style '())))))

(define (waybar-sway-language)
  (waybar-module 'sway/language))

(define (waybar-sway-window)
  (waybar-module
   'sway/window
   `()
   `((#{#window}#
      ((margin-left . 1em)
       (margin-right . 1em))))
   #:placement 'modules-center))

(define* (waybar-sway-workspaces
          #:key
          persistent-workspaces
          (format-icons '(("1" . )
                          ("2" . )
                          ("3" . )
                          ("4" . )
                          ("6" . ) ; 
                          ("7" . ) ; 
                          ("8" . )
                          ("9" . )
                          ("10" . )

                          ("urgent" . )
                          ("focused" . )
                          ("default" . ))))
  (waybar-module
   'sway/workspaces
   `((disable-scroll . #t)
     (format . {icon})
     ;; TODO [sway-wip] Andrew Tropin: FIXME: Height becomes higher when icons are not used.
     (format-icons . ,format-icons)
     (persistent_workspaces . ,(if persistent-workspaces
                                   persistent-workspaces '())))
   `(((#{#workspaces}# button)
      ((background . none)
       (border-radius . 0.2em)
       (margin . (0.4em 0.2em))
       (padding . (0.1em 0.2em))
       (color . @base05)))

     ((#{#workspaces}# button:hover)
      ((background . none)
       (border-color . @base07)))

     ((#{#workspaces}# button.focused)
      ((background . @base02)
       (color . @base07)))

     ((#{#workspaces}# button.urgent)
      ((color . @base08))))
   #:placement 'modules-left))

(define (waybar-tray)
  (waybar-module
   'tray
   `()
   `(((#{#tray}# menu)
      ((color . @base05)
       (background . @base01)
       (border . (solid 1px))
       (border-color . @base02)))

     ((#{#tray}# menu menuitem)
      ((padding-top . 0px)
       (padding-bottom . 0px)
       (margin-top . 0.1em)
       (margin-bottom . 0em)))

     ((#{#tray}# menu menuitem:hover)
      ((background . none)))

     ((#{#tray}# menu separator)
      ((background . @base03)
       (padding-top . 1px)
       (margin-top . 0.2em)
       (margin-bottom . 0.2em))))))

(define (waybar-temperature)
  (waybar-module 'temperature))

(define (waybar-idle-inhibitor)
  (waybar-module
   'idle_inhibitor
   '((format . {icon})
     (format-icons . ((activated . )
                      (deactivated . ))))))

(define* (waybar-clock #:key
                       (format "{:%Y-%m-%d %H:%M}")
                       timezone)
  ;; (define emacsclient (get-value 'emacs-client config))
  ;; (define (emacsclient-cmd command)
  ;;   #~(format #f "\"~a --eval \\\"(~a)\\\"\"" #$emacsclient #$command))
  (waybar-module
   'clock
   `((tooltip-format . "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>")
     ,@(if timezone
           `((timezone . ,timezone)) '())
     (format . ,format)
     ;; ,@(if emacsclient
     ;;       `((on-click . ,(emacsclient-cmd "world-clock"))
     ;;         (on-click-right . ,(emacsclient-cmd "calendar")))
     ;;       '())
     )))

(define (waybar-battery)
  (waybar-module
   'battery
   `((format . "{capacity}% {icon}")
     (format-charging . ,(format #f "{capacity}~a {icon}" "⚡"))
     (states . ((empty . 10)
                (low . 20)
                (half-low . 40)
                (half . 60)
                (high . 90)
                (full . 100)))
     (format-icons . ((empty . )
                      (low . )
                      (half-low . )
                      (half . )
                      (high . )
                      (full . ))))
   `((#{#battery.discharging.empty}#
      ,(if #f ; TODO [sway-wip] was `intense?'
           `((color . @base02)
             (background . @base08))
           `((color . @base08))))
     (#{#battery.discharging.low}#
      ,(if #f ; TODO [sway-wip] was `intense?'
           `((color . @base02)
             (background . @base09))
           `((color . @base09)))))))

(define* (he-rofi #:key
                  wayland?
                  (theme "Arc-Dark"))
  (list
   (service
    home-rofi-service-type
    (home-rofi-configuration
     (rofi (if wayland?
               rofi-wayland
               rofi))
     ;; TODO Trying out Andrew Tropin's confguration as-is for now, revisit later.
     (config-rasi
      `((configuration
         ((modi . "run,ssh,drun")
          (drun-show-actions . #t)
          (show-icons . #t)
          (kb-row-tab . "")
          (kb-row-select . "Tab")
          (kb-secondary-paste . "Control+y")
          (kb-remove-word-forward . "Alt+d")
          (kb-remove-word-back . "Control+w,Control+BackSpace")
          (kb-clear-line . "Control+slash")
          (kb-page-next . "Control+v")
          (kb-page-prev . "Alt+v")))
        ,#~(format #f "@theme \"~a\"" #$theme)))))))

(define rofi-application-launcher (file-append rofi "/bin/rofi -show drun"))

(define* (he-alacritty #:key
                       font
                       config-file) ; (config-file (local-file "alacritty.yml"))
  (list
   (service
    home-alacritty-service-type
    (home-alacritty-configuration
     (config
      ;; TODO keeping this padding thing here for now, revisit it later
      `((window . ((padding . ((x . 10)
                               (y . 5)))))
        ,@(if font
              `((font . ((normal . ((style . , (font-weight->style
                                                (assoc-ref font #:weight)))
                                    (family . ,(assoc-ref font #:name))))
                         (size . ,(assoc-ref font #:size)))))
              '())
        ,@(if config-file
              `((import . #(,config-file)))
              '())))))))

(define (font-weight->style weight)
  (->> weight
       symbol->string
       (string-delete #\-)
       string-capitalize))

(define alacritty-terminal (file-append alacritty "/bin/alacritty"))

(define (gammastep-shepherd-service _)
  (list
   (shepherd-service
    (provision '(gammastep-applet))
    (respawn? #f)
    (start #~(lambda (wayland-display . _)
               ((make-forkexec-constructor
                 (list #$(file-append gammastep "/bin/gammastep-indicator") "-l 44:-123")
                 #:environment-variables
                 ;; (list (string-append "WAYLAND_DISPLAY=" wayland-display))
                 (call-with-input-file "/tmp/env-for-applets" read)))))
    (stop #~(make-kill-destructor))
    (auto-start? #f))))

(define gammastep-applet-service-type
  (service-type (name 'he-gammastep-applet)
                (extensions
                 (list (service-extension
                        home-shepherd-service-type
                        gammastep-shepherd-service)))
                (default-value #f)))

(define (network-manager-shepherd-service _)
  (list
   (shepherd-service
    (provision '(nm-applet))
    (respawn? #f)
    (start #~(lambda (wayland-display . _)
               ((make-forkexec-constructor
                 (list #$(file-append network-manager-applet "/bin/nm-applet") "--indicator")
                 #:environment-variables
                 ;; (list (string-append "WAYLAND_DISPLAY=" wayland-display))
                 (call-with-input-file "/tmp/env-for-applets" read)))))
    (stop #~(make-kill-destructor))
    (auto-start? #f))))

(define network-manager-applet-service-type
  (service-type (name 'he-network-manager-applet)
                (extensions
                 (list (service-extension
                        home-shepherd-service-type
                        network-manager-shepherd-service)))
                (default-value #f)))

(define (udiskie-shepherd-service _)
  (list
   (shepherd-service
    (provision '(udiskie-applet))
    (respawn? #f)
    (start #~(lambda (wayland-display . _)
               ((make-forkexec-constructor
                 (list #$(file-append udiskie "/bin/udiskie")
                       "--tray"
                       ;; (string-append "--file-manager="
                       ;;                #$(file-append xdg-utils "/bin/xdg-open"))
                       )
                 #:environment-variables
                 ;; (list (string-append "WAYLAND_DISPLAY=" wayland-display))
                 (call-with-input-file "/tmp/env-for-applets" read)))))
    (stop #~(make-kill-destructor))
    (auto-start? #f))))

(define udiskie-applet-service-type
  (service-type (name 'he-udiskie-applet)
                (extensions
                 (list (service-extension
                        home-shepherd-service-type
                        udiskie-shepherd-service)))
                (default-value #f)))
