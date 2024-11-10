(define-module (phrenetic home)
  #:use-module ((guix download) #:select (url-fetch))
  #:use-module (guix gexp)
  #:use-module ((guix modules) #:select (source-module-closure))
  #:use-module (guix packages)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services fontutils)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home-services version-control)
  #:use-module ((gnu packages browser-extensions) #:select (ublock-origin/chromium))
  #:use-module ((gnu packages chromium) #:select (ungoogled-chromium))
  ;; #:use-module ((gnu packages compression) #:select (zip unzip))
  #:use-module ((gnu packages bash) #:select (bash))
  #:use-module ((gnu packages clojure) #:select (clojure-tools))
  ;; #:use-module ((gnu packages curl) #:select (curl))
  #:use-module ((gnu packages emacs) #:select (emacs emacs-pgtk))
  #:use-module ((gnu packages emacs-xyz) #:select (doom-emacs))
  #:use-module ((gnu packages fonts) #:select (font-fira-code font-google-noto font-google-noto-emoji font-google-noto-sans-cjk font-google-noto-serif-cjk font-iosevka font-iosevka-aile font-iosevka-etoile font-liberation font-gnu-unifont))
  #:use-module ((gnu packages fontutils) #:select (fontmanager-no-googlefonts))
  #:use-module ((gnu packages freedesktop) #:select (desktop-file-utils poweralertd xdg-utils xdg-user-dirs xdg-desktop-portal xdg-desktop-portal-gtk xdg-desktop-portal-wlr))
  #:use-module ((gnu packages gimp) #:select (gimp))
  #:use-module ((gnu packages glib) #:select (dbus))
  #:use-module ((gnu packages gnome) #:select (adwaita-icon-theme dconf gnome-essential-extras gnome-meta-core-services gnome-meta-core-shell gnome-meta-core-utilities gnome-session gnome-themes-extra hicolor-icon-theme))
  #:use-module ((gnu packages gnome-xyz) #:select (arc-theme papirus-icon-theme))
  #:use-module ((gnu packages gnuzilla) #:select (icecat))
  #:use-module ((gnu packages gstreamer) #:select (gst-libav gst-plugins-base gst-plugins-good gst-plugins-bad gst-plugins-ugly))
  #:use-module ((gnu packages haskell-apps) #:select (kmonad))
  #:use-module ((gnu packages hyprland) #:select (hypridle hyprland hyprlock xdg-desktop-portal-hyprland/simple))
  #:use-module ((gnu packages image) #:select (grim slurp swappy))
  #:use-module ((gnu packages java) #:select (openjdk-lts))
  #:use-module ((gnu packages linux) #:select (brightnessctl pipewire wireplumber wireplumber))
  #:use-module ((gnu packages music) #:select (playerctl))
  #:use-module ((gnu packages node) #:select (node-lts))
  #:use-module ((gnu packages package-management) #:select (flatpak (guix . guix-package) nix))
  #:use-module ((gnu packages pulseaudio) #:select (pulseaudio pavucontrol))
  #:use-module ((gnu packages python-web) #:select (awscli))
  #:use-module ((gnu packages qt) #:select (qtwayland qtwayland-5))
  #:use-module ((gnu packages shells) #:select (zsh))
  #:use-module ((gnu packages shellutils) #:select (direnv))
  #:use-module ((gnu packages terminals) #:select (alacritty))
  #:use-module ((gnu packages tor-browsers) #:select (torbrowser))
  #:use-module ((gnu packages version-control) #:select (git))
  #:use-module ((gnu packages video) #:select (vlc))
  #:use-module ((gnu packages virtualization) #:select (qemu))
  #:use-module ((gnu packages vpn) #:select (protonvpn-cli))
  #:use-module ((gnu packages web) #:select (jq))
  #:use-module ((gnu packages web-browsers) #:select (nyxt))
  #:use-module ((gnu packages wm) #:select (mako sway swayidle swaylock swaylock-effects))
  #:use-module ((gnu packages xdisorg) #:select (rofi rofi-wayland wl-clipboard))
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system keyboard)
  #:use-module ((gnu system shadow) #:select (%default-dotguile %default-gdbinit %default-nanorc))
  #:use-module ((rde serializers elisp) #:select (elisp-serialize))
  #:use-module ((rde serializers ini) #:select (ini-serialize))
  #:use-module (phrenetic modules)
  #:use-module ((phrenetic stateless) #:prefix stateless:)
  #:use-module (phrenetic utils)
  ;; #:use-module ((sxml simple) #:select (sxml->xml))
  #:use-module ((ice-9 match) #:select (match match-lambda match-let))
  #:use-module ((ice-9 string-fun) #:select (string-replace-substring))
  #:use-module ((srfi srfi-1) #:select (append-map concatenate delete-duplicates every list-index remove))
  #:export (create-he
            stateless
            home-stateless-service
            shells
            xdg-base-directories
            xdg-trash
            emacs-editor
            emacs-interface
            emacs-new-frame
            doom
            doom-service
            doom-ts-lang
            doom-web
            guix-pm
            nix-pm
            virtualization
            kmonad-tool
            pipewire-media
            %colors
            %wallpapers
            font-library
            %fonts
            %date-formats
            %time-formats
            xdg-user-directories
            mesa
            doom-desktop
            dbus-ipc
            gdm
            desktop-tty
            gnome-desktop
            gnome-start
            sway-wm
            sway-start
            swaylock-wm-piece
            swaylock-screen-locker
            swayidle-wm-piece
            swayidle-idle-manager
            mako-wm-piece
            mako-notifier
            poweralertd-wm-piece
            poweralertd-monitor
            ;; kanshi-wm-piece
            portal-services/gtk
            portal-services/wlr
            portal-config-services
            flatpak-apps
            aws-prog
            clojure-prog
            doom-calendar-prog
            doom-org-prog
            direnv-prog
            direnv-service-type
            node-prog
            protonvpn-prog
            ssh-prog
            doom-dired-prog
            ;; nano-prog
            doom-trash-prog
            git-prog
            ;; misc-progs
            pavucontrol-app
            font-manager-app
            doom-menu-app
            doom-menu
            rofi-app
            rofi-menu
            gimp-app
            alacritty-app
            alacritty-terminal
            doom-vterm-app
            doom-vterm-terminal
            ungoogled-chromium-app
            chromium-wrapper
            icecat-app
            nyxt-app
            tor-browser-app
            vlc-app
            work
            services-only-packages
            services-sans-packages
            system-features
            sway-environment
            programs
            applications
            he-entire))

(define serialize-rasi-config (@@ (rde home services xdisorg) serialize-rasi-config))
(define serialize-sway-config (@@ (rde home services wm) serialize-sway-config))
(define serialize-swaylock-config (@@ (rde home services wm) serialize-swaylock-config))

(define (create-he services)
  (let ((he (home-environment)))
    (home-environment
     (inherit he)
     (essential-services (remove-services-by-types
                          (home-environment-essential-services he)
                          home-fontconfig-service-type))
     (services
      (filter service? services))))) ; A convenience to allow passing a list of services with #f, <unspecified> not filtered out.

(define* (stateless #:key storage-paths)
  (append
   (list
    (service home-stateless-service-type
             `(#:storage-paths ,storage-paths)))
   (stateless-guix-home)))

(define home-stateless-service-type
  (service-type
   (name 'home-stateless)
   (extensions
    (list (service-extension home-activation-service-type
                             (lambda (config)
                               (with-imported-modules (source-module-closure
                                                       '((phrenetic build stateless))
                                                       #:select? phrenetic-module-name?)
                                 #~(begin
                                     (use-modules ((phrenetic build stateless) #:select (activate)))
                                     (activate (#$state-with-home-expansions '#$(assoc-ref config #:state))
                                               #$homedir-gexp
                                               '#$(assoc-ref config #:storage-paths))))))
          ;; (service-extension home-profile-service-type
          ;;                    (lambda (config)
          ;;                      (list (stateless:tool-package
          ;;                             "home-stateless"
          ;;                             (state-with-home-expansions (assoc-ref config #:state))
          ;;                             (ignore-with-home-expansions (assoc-ref config #:ignore))
          ;;                             homedir-gexp
          ;;                             (assoc-ref config #:storage-paths)
          ;;                             #:additional-known
          ;;                             guix-home-files))))
          ))
   (compose identity)
   (extend stateless:extend-proc)
   (description "Initialize a stateless home.")))

(define homedir-gexp #~(getenv "HOME"))

(define path-with-home-expansion
  #~(lambda (path)
      (cond
       ((string? path) (string-append (getenv "HOME") "/" path))
       ((list? path)
        (apply
         (lambda* (#:optional tail #:key xdg-base)
           (string-append (getenv xdg-base) "/" (or tail "")))
         path)))))

(define state-with-home-expansions
  #~(lambda (state)
      (map (lambda (i)
             (acons #:path (#$path-with-home-expansion (assoc-ref i #:path)) i))
           state)))

(define ignore-with-home-expansions
  #~(lambda (ignore)
      (map #$path-with-home-expansion ignore)))

(define guix-home-files
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules ((guix build utils) #:select (find-files)))

        (let ((home-files
               (let* ((home-dir (getenv "HOME"))
                      (guix-home-dir (string-append
                                      home-dir "/.guix-home/" #$home-files-directory "/"))) ; TODO maybe do a `readlink' here instead of appending a slash, for clarity.
                 (map (lambda (file)
                        (string-append
                         home-dir
                         (string-drop file (string-length guix-home-dir))))
                      (find-files guix-home-dir)))))
          (make-known home-files symlink-to-store?)))))

;; (define (make-known files pred)
;;   (map (lambda (file)
;;          (cons file pred))
;;        files))

;; (define (symlink-to-store? file)
;;   (and (symbolic-link? file)
;;        (store-file-name? (readlink file))))

(define home-stateless-service
  (stateless:service-fn home-stateless-service-type))

(define (stateless-guix-home)
  (list
   (home-stateless-service
    'guix-home
    #:state
    `(((#:path . ,(list (assoc-ref log-home #:path)
                        #:xdg-base
                        (assoc-ref log-home #:xdg-base)))
       (#:storage . #:storage/machine)))
    #:ignore
    '(((#:path . ".guix-home")
       (#:preds . ((#:symlink-to-store))))))))

(define* (shells #:key
                 stateless?
                 doom?
                 doom-tree-sitter?
                 login-shell
                 interactive-shells
                 (shell-configs `((#:shell/bash . ((#:bashrc . ,(list (local-file "bashrc")))))
                                  (#:shell/zsh . ((#:zshrc . ,(list (local-file "zshrc"))))))))
  (append
   (append-map
    (lambda (shell)
      (case shell
        ((#:shell/bash)
         (cons* (service home-bash-service-type
                         (let ((config (assoc-ref shell-configs #:shell/bash)))
                           (home-bash-configuration
                            (bashrc (or (assoc-ref config #:bashrc) '()))
                            ;; ... other fields
                            )))
                (if stateless?
                    (list
                     (home-stateless-service
                      'bash
                      #:state '(((#:path . ("bash/" #:xdg-base "XDG_STATE_HOME"))
                                 (#:storage . #:storage/persist))))
                     (simple-service 'bash-history
                                     home-bash-service-type
                                     (home-bash-extension
                                      (bashrc
                                       (list
                                        ;; TODO Line that sets HISTFILE to match stateless path.
                                        )))))
                    '())))
        ((#:shell/zsh)
         (cons* (service home-zsh-service-type
                         (let ((config (assoc-ref shell-configs #:shell/zsh)))
                           (home-zsh-configuration
                            (zshrc (or (assoc-ref config #:zshrc) '()))
                            ;; ... other fields
                            )))
                (if stateless?
                    (list
                     (home-stateless-service
                      'zsh
                      #:state '(((#:path . ("zsh/" #:xdg-base "XDG_STATE_HOME"))
                                 (#:storage . #:storage/persist))
                                ((#:path . ("zsh/" #:xdg-base "XDG_CACHE_HOME"))
                                 (#:storage . #:storage/machine))))
                     (simple-service 'zsh-history
                                     home-zsh-service-type
                                     (home-zsh-extension
                                      (zshrc
                                       (list
                                        ;; TODO Line that sets HISTFILE to match stateless path.
                                        )))))
                    '())))))
    (delete-duplicates
     (cons login-shell interactive-shells)))

   (list
    (let ((name 'shells-setup-login-shell)
          (shepherd-stop-cmd
           (let ((shepherd (home-shepherd-configuration-shepherd (home-shepherd-configuration))))
             (mixed-text-file
              "shepherd-stop"
              #~(string-join
                 (list
                  #$(file-append shepherd "/bin/herd") "stop" "root"))))))
      (case login-shell
        ((#:shell/bash)
         (simple-service name
                         home-bash-service-type
                         (home-bash-extension
                          (bash-logout
                           (list shepherd-stop-cmd)))))
        ((#:shell/zsh)
         (simple-service name
                         home-zsh-service-type
                         (home-zsh-extension
                          (zlogout
                           (list shepherd-stop-cmd)))))))

    (match-let (((default-interactive-shell _ ...) interactive-shells))
      (when-not (equal? login-shell default-interactive-shell)
                (simple-service
                 'shells-set-shell-env-var
                 home-environment-variables-service-type
                 `(("SHELL" . ,(case default-interactive-shell
                                 ((#:shell/bash) (file-append bash "/bin/bash"))
                                 ((#:shell/zsh) (file-append zsh "/bin/zsh")))))))))

   (if doom?
       (doom-shells #:stateless? stateless?
                    #:tree-sitter? doom-tree-sitter?)
       '())))

(define* (doom-shells #:key
                      stateless?
                      tree-sitter?)
  (append
   (doom-ts-lang 'sh #:tree-sitter? tree-sitter?)
   (doom-eshell #:stateless? stateless?)))

(define* (doom-eshell #:key stateless?)
  (let ((name 'eshell))
    (list
     (doom-service
      name
      #:modules '((#:term
                   eshell))
      #:config `((after! eshell
                         (setq eshell-history-size ,very-big-history)))) ; Setting this to `nil' to inherit envvar HISTSIZE is another option.

     (when stateless?
       (doom-stateless-service
        name
        #:state '(((#:path . ("eshell/history" #:doom-base #:data))
                   (#:storage . #:storage/persist))
                  ((#:path . ("eshell/lastdir" #:doom-base #:data))
                   (#:storage . #:storage/persist))
                  ((#:path . ("eshell/z" #:doom-base #:data))
                   (#:storage . #:storage/persist))))))))

(define* (xdg-base-directories)
  (list
   ;; Allowing parameterization not done. I'm unlikely to want to change any of these dirs and I'm lazy.
   ;; (simple-service
   ;;  'xdg-base-directories
   ;;  home-xdg-base-directories-service-type
   ;;  (xdg-base-directories-configuration
   ;;   ...))
   ))

(define* (xdg-trash #:key stateless?)
  (list
   (when stateless?
     (home-stateless-service
      'xdg-trash
      #:state
      '(((#:path . ("Trash/" #:xdg-base "XDG_DATA_HOME"))
         (#:storage . #:storage/machine)))))))

(define* (emacs-editor #:key
                       wayland?
                       (emacs emacs)
                       (emacs-pgtk emacs-pgtk))
  (let ((emacs (get-emacs #:wayland? wayland?
                          #:emacs emacs
                          #:emacs-pgtk emacs-pgtk)))
    (list
     (simple-service
      'emacs-add-packages
      home-profile-service-type
      (list emacs))

     (simple-service
      'emacs-set-editor-env-vars
      home-environment-variables-service-type
      `(("VISUAL" . ,(file-append emacs "/bin/emacsclient"))
        ("EDITOR" . "$VISUAL"))))))

(define* (emacs-interface #:key
                          wayland?
                          (emacs emacs)
                          (emacs-pgtk emacs-pgtk))
  (let ((emacs (get-emacs #:wayland? wayland?
                          #:emacs emacs
                          #:emacs-pgtk emacs-pgtk)))
    `((#:emacs/program . ,(list
                           (file-append emacs "/bin/emacs")))
      (#:emacs/new-frame . ,(list
                             (file-append emacs "/bin/emacsclient")
                             "--create-frame"))
      (#:emacs/handler . ,(partial emacs-handler emacs)))))

(define* (get-emacs #:key
                    wayland?
                    emacs
                    emacs-pgtk)
  (if wayland?
      emacs-pgtk
      emacs))

(define* (emacs-handler emacs
                        name
                        exprs
                        #:key
                        modal?
                        modal-title
                        modal-width
                        modal-height
                        minibuffer?
                        input?)
  (program-file
   (string-append "emacs-handler-" name)
   #~(begin
       (use-modules ((ice-9 popen) #:select (close-pipe open-pipe*))
                    ((ice-9 textual-ports) #:select (get-string-all put-string)))

       (let* ((args (cdr (command-line)))
              (input (if #$input?
                         (string-drop-right (get-string-all (current-input-port)) 1)
                         #f))
              (pipe (let ((cmd (list
                                #$(file-append emacs "/bin/emacsclient")
                                "--eval"
                                (#$serialize-eval-expr
                                 (#$(eval-expr exprs
                                               modal?
                                               modal-title
                                               modal-width
                                               modal-height
                                               minibuffer?)
                                    args input)))))
                      (apply open-pipe* OPEN_READ cmd))))
         (put-string (current-output-port)
                     (let* ((res* (get-string-all pipe))
                            (end (string-take-right res* 1)) ; EOF or newline or something? Donno if this matters, doing it just in case.
                            (res (string-trim-both (string-drop-right res* 1) #\"))) ; `emacsclient' output is a sexp so strings will have quotes around them. Work around just this particular case, for now it's the only one that matters (for the dmenu-like functionality).
                       (string-append res end)))
         (close-pipe pipe)))))

(define (eval-expr exprs modal? modal-title modal-width modal-height minibuffer?)
  #~(lambda (args input)
      `(with-selected-frame
        (make-frame '(,@(if #$modal?
                            `((name . ,(string-append #$modal-title
                                                      #$emacs-modal-title-tail))
                              (width . ,(or #$modal-width 100))
                              (height . ,(or #$modal-height 20))
                              (alpha-background . 90))
                            '())
                      ,@(if #$minibuffer?
                            '((minibuffer . only)) '())))
        (let ((args ',args)
              (input ,(or input 'nil)))
          ,@(if #$minibuffer?
                '((unwind-protect
                   (progn
                    #$@exprs)
                   (delete-frame)))
                '#$exprs)))))

(define serialize-eval-expr
  #~(lambda (expr)
      (call-with-output-string
        (lambda (port)
          (write expr port)))))

(define emacs-modal-title-tail (string-append " - " "Emacs Modal"))

(define emacs-modal-window-props
  `((#:window-prop.criteria/title . ,(string-append ".*" emacs-modal-title-tail))
    (#:window-prop/floating? . #t)))

(define* (emacs-xdg-service name
                            xdg-name
                            program
                            #:key
                            (exec-argument "%u")
                            default-for)
  (let ((file (symbol-append 'emacs- name)))
    (simple-service
     (symbol-append 'emacs-xdg- name)
     home-xdg-mime-applications-service-type
     (home-xdg-mime-applications-configuration
      (desktop-entries
       (list
        (xdg-desktop-entry
         (file (symbol->string file))
         (name (string-append "Emacs [" xdg-name "]"))
         (config `((exec . ,#~(string-append #$program " " #$exec-argument))
                   (icon . "emacs")))
         (type 'application))))
      (default (map (lambda (mime-type)
                      `(,mime-type . ,(symbol-append file '.desktop)))
                    (or default-for '())))))))

(define* (doom #:key
               stateless?
               services)
  (append
   (list
    (service doom-service-type)

    (simple-service
     'doom-set-paths
     home-environment-variables-service-type
     (let ((doom-local "$XDG_STATE_HOME/doom"))
       `(("DOOMLOCALDIR" . ,doom-local)
         ("DOOMPROFILELOADFILE" . ,(string-append doom-local "/profile-load.el"))))))

   (or services '())

   (if stateless?
       (list
        (home-stateless-service
         'doom
         #:state '(((#:path . ("doom/straight/" #:xdg-base "XDG_STATE_HOME"))
                    (#:storage . #:storage/machine))
                   ((#:path . ("doom/profile-load.el" #:xdg-base "XDG_STATE_HOME"))
                    (#:storage . #:storage/machine))))
        (doom-stateless-service
         'doom
         #:state '(((#:path . ("eln/" #:doom-base #:cache))
                    (#:storage . #:storage/machine))
                   ;; This is a temp directory for eln compilation and could probably just be ignored and not stored at all but let's be safe.
                   ((#:path . ("comp/" #:doom-base #:cache))
                    (#:storage . #:storage/machine))
                   ((#:path . ("profiles.@.el" #:doom-base #:cache))
                    (#:storage . #:storage/machine))
                   ((#:path . ("@/" #:doom-base #:data))
                    (#:storage . #:storage/machine))
                   ((#:path . ("sync" #:doom-base #:data))
                    (#:storage . #:storage/machine))
                   ((#:path . ("logs/" #:doom-base #:state))
                    (#:storage . #:storage/machine)))))
       '())))

(define doom-service-type
  (service-type
   (name 'home-doom)
   (extensions
    (list (service-extension home-profile-service-type
                             (const
                              (list `(,doom-emacs "bin"))))
          (service-extension home-xdg-configuration-files-service-type
                             (lambda (config)
                               `(("emacs" ,doom-emacs)
                                 ("doom" ,(doom-private config)))))))
   (compose identity)
   (extend (lambda (_ exts)
             (doom-extend exts)))
   (default-value #f)
   (description "Install and configure Doom.")))

(define (doom-extend exts)
  (let ((field (lambda (k)
                 (map (lambda (i)
                        (or (assoc-ref i k) '()))
                      exts))))
    `((#:init-file . ,(init-file (field #:modules)))
      (#:config-file . ,(config-file (field #:config)))
      (#:packages-file . ,(packages-file (field #:packages)))
      (#:theme-files . ,(theme-files (field #:themes)))
      (#:snippets . ,(apply append (field #:snippets))))))

(define (init-file exts)
  (as-> exts $
    (map (lambda (i)
           (update-vals i (lambda (modules)
                            (map (lambda (m)
                                   (cond
                                    ((symbol? m) (list m))
                                    ((list? m) m)))
                                 modules))))
         $)
    (apply merge-with
           (lambda (ms ms*)
             (merge-with
              (lambda (flags flags*)
                (delete-duplicates (append flags flags*)))
              ms ms*))
           $)
    (sort $ (lambda (a b)
              (let ((idx (match-lambda
                           ((cat _ ...)
                            (list-index
                             (lambda (category)
                               (equal? cat category))
                             '(#:completion #:ui #:editor #:emacs #:term #:checkers #:tools #:lang #:app #:config))))))
                (< (idx a) (idx b)))))
    (update-vals $ (lambda (modules)
                     (let ((mod-name (match-lambda
                                       ((name _ ...)
                                        (symbol->string name)))))
                       (sort modules (lambda (a b)
                                       (string<? (mod-name a) (mod-name b)))))))
    (update-keys $ (lambda (category)
                     (symbol-append ': (keyword->symbol category))))
    (update-vals $ (lambda (modules)
                     (map (match-lambda
                            ((name) name)
                            (m m))
                          modules)))
    (apply append $)
    (elisp-serialize
     `(,#~";;; -*- lexical-binding: t; -*-"
          (doom! ,@$)))))

(define (config-file exts)
  (as-> exts $
    (delete-duplicates $)
    (apply append $)
    (elisp-serialize*
     (append
      `(,#~";;; -*- lexical-binding: t; -*-")
      $))))

(define (packages-file exts)
  (as-> exts $
    (delete-duplicates $)
    (apply append $)
    (elisp-serialize*
     (append
      `(,#~";; -*- no-byte-compile: t; -*-")
      $))))

(define (theme-files exts)
  (as-> exts $
    (apply append $)
    (map (match-lambda
           ((name defs extra-faces)
            (list name
                  (elisp-serialize*
                   `(,#~";;; -*- lexical-binding: t; no-byte-compile: t; -*-"
                     (require 'doom-themes)
                     (def-doom-theme ,name ,(symbol->string name) ,defs ,extra-faces))))))
         $)))

(define (elisp-serialize* exprs)
  #~(begin
      (use-modules ((ice-9 string-fun) #:select (string-replace-substring)))
      (string-replace-substring #$(elisp-serialize exprs) "(syntax " "(function ")))

(define (doom-private config)
  (file-union
   "doom-private"
   (append
    (map (match-lambda
           ((name file)
            `(,(string-append name)
              ,(mixed-text-file (string-append "doom-" name) (assoc-ref config file)))))
         '(("init.el" #:init-file)
           ("config.el" #:config-file)
           ("packages.el" #:packages-file)))
    (map (match-lambda
           ((theme-name file)
            (let ((name (string-append (symbol->string theme-name) "-theme.el")))
              `(,(string-append "themes/" name)
                ,(mixed-text-file name file)))))
         (assoc-ref config #:theme-files))
    (append-map (match-lambda
                  ((mode snippets)
                   (map (match-lambda
                          ((name file)
                           `(,(string-append "snippets/" mode "/" name)
                             ,(mixed-text-file (string-append "emacs-snippet-" name) file))))
                        snippets)))
                (assoc-ref config #:snippets)))))

(define* (doom-service name
                       #:key
                       modules
                       packages
                       config
                       themes
                       snippets)
  (simple-service
   (symbol-append name '-doom)
   doom-service-type
   `((#:modules . ,modules)
     (#:packages . ,packages)
     (#:config . ,config)
     (#:themes . ,themes)
     (#:snippets . ,snippets))))

(define* (doom-stateless-service name #:key state ignore)
  (home-stateless-service
   (symbol-append name '-doom)
   #:state (state-with-doom-dir state)
   #:ignore ignore))

(define (state-with-doom-dir state)
  (map (lambda (i)
         (acons #:path (path-with-doom-dir (assoc-ref i #:path)) i))
       state))

(define (path-with-doom-dir path)
  (let ((doom-dir
         (lambda (base subpath)
           `(,(string-append
               "doom/"
               (case base
                 ((#:cache) "cache")
                 ((#:data) "etc")
                 ((#:state) "state"))
               "/" subpath)
             #:xdg-base "XDG_STATE_HOME"))))
    (apply (lambda* (tail #:key doom-base)
             (doom-dir doom-base tail))
           path)))

(define* (doom-core #:key
                    stateless?
                    evil?
                    icons?
                    email)
  (append
   (doom-general #:stateless? stateless?
                 #:evil? evil?
                 #:icons? icons?)
   (doom-user #:email email)
   (doom-keymaps)
   (doom-undo #:stateless? stateless?)
   (doom-emacs-lisp)
   (doom-markdown)))

(define* (doom-general #:key
                       stateless?
                       evil?
                       icons?
                       (childframe? #t))
  (let ((name 'general))
    (cons
     (doom-service
      name
      #:modules
      `((#:completion
         (corfu ,@(if icons? '(+icons) '())
                +orderless
                +dabbrev)
         ;; (company ,@(if childframe? '(+childframe) '()))
         (vertico ,@(if icons? '(+icons) '())))

        (#:ui
         doom
         doom-dashboard
         (emoji +unicode)
         hl-todo
         indent-guides
         (ligatures +extra)
         modeline
         nav-flash
         ophints
         (popup +defaults +all)
         vi-tilde-fringe
         window-select
         workspaces
         zen)

        (#:editor
         ,@(if evil? '((evil +everywhere)) '())
         file-templates
         fold
         format
         lispy
         multiple-cursors
         rotate-text
         snippets
         word-wrap)

        (#:emacs
         electric
         eww
         (ibuffer ,@(if icons? '(+icons) '())))

        (#:checkers
         (syntax ,@(if icons? '(+icons) '())
                 ,@(if childframe? '(+childframe) '())))

        (#:tools
         editorconfig
         (eval +overlay)
         lookup
         (pass +auth)
         pdf
         prodigy)

        (#:config
         (default +bindings +smartparens)))

      #:packages
      '((package! expand-region :pin "e8f4e0fe9c9a80a6a26e2b438502aba9a799d580")
        (package! tldr :pin "1b09d2032491d3904bd7ee9bf5ba7c7503db6593")
        ;; (package! 0x0 :pin "63cd5eccc85e527f28e1acc89502a53245000428") ; TODO `elisp-serialize' makes the `0x0' into `#{0x0}#'.

        (package! fireplace :pin "f6c23e259349922aae25cf2898ba815a7d8f2527")

        ;; TODO TEMPORARY add this to play around with it.
        (package! org-tanglesync :pin "af83a73ae542d5cb3c9d433cbf2ce1d4f4259117"))

      #:config
      (append
       '((setq scroll-margin 10
               save-interprogram-paste-before-kill t)

         ;; I like having line numbers on but hlissner says they're slow so I might want to disable them at some point. Keep in mind I use them to determine what lines are continuation lines so I might have to make the right fringe bigger if I do this and set visual-line-fringe-indicators.
         ;; (setq display-line-numbers-type nil)

         (setq-default indent-tabs-mode t) ; Doom sets this to nil, reset it.

         ;; I'd like to have this on but in the Doom code it says it's more efficient not to.
         ;; (setq-default cursor-in-non-selected-windows t)

         (pixel-scroll-precision-mode)

         (after! bookmark
                 (setq bookmark-save-flag 1))

         (after! doom-modeline
                 ;; (setq doom-modeline-checker-simple-format nil)
                 (setq doom-modeline-major-mode-icon t)
                 (setq doom-modeline-persp-name t))

         ;; I don't want indent guides enabled automatically.
         (add-hook! '+indent-guides-inhibit-functions
                    (lambda () t))

         (use-package! lispy
                       :defer t
                       :init
                       ;; Not using lispy, remove all the Doom module's hooks.
                       (remove-hook! '(lisp-mode-hook
                                       emacs-lisp-mode-hook
                                       ielm-mode-hook
                                       scheme-mode-hook
                                       racket-mode-hook
                                       hy-mode-hook
                                       lfe-mode-hook
                                       dune-mode-hook
                                       clojure-mode-hook
                                       fennel-mode-hook)
                                     #'lispy-mode)
                       (remove-hook! 'eval-expression-minibuffer-setup-hook #'doom-init-lispy-in-eval-expression-h))

         (use-package! lispyville
                       :hook (prog-mode . lispyville-mode)
                       :init
                       (setq lispyville-key-theme nil) ; Prevent Doom module's invocation of `lispyville-set-key-theme' from doing anything.
                       :config
                       (lispyville-set-key-theme
                        '(operators
                          c-w
                          c-u
                          commentary))
                       (map! :map lispyville-mode-map
                             "C-s-j" #'lispyville-beginning-of-next-defun
                             "C-s-k" #'lispyville-beginning-of-defun
                             "C-s-," #'lispyville-end-of-defun
                             "C-s-a" #'lispyville-drag-backward
                             "C-s-g" #'lispyville-drag-forward
                             "C-s-p" #'lispyville-prettify
                             (:prefix "C-s-;"
                              "R" #'lispyville-raise-list)))

         (use-package! expand-region
                       :defer t
                       :init
                       (map! :nv "s-e" #'er/expand-region
                             :nv "s-E" #'er/contract-region)
                       :config
                       (setq expand-region-fast-keys-enabled nil) ; My mapping is conventient enough and I don't want the repeat key to conflict with anything.
                       ;; Copied from Doom config: ~/.config/emacs/modules/config/default/+emacs.el:12
                       (defadvice! biome--quit-expand-region-a (&rest _)
                         "Properly abort an expand-region region."
                         :before '(evil-escape doom/escape) ; TODO `evil-escape': maybe gate with `evil?'
                         (when (memq last-command '(er/expand-region er/contract-region))
                           (er/contract-region 0))))

         ;; REVIEW Trying out not having this so I can use avy dispatch commands.
         ;; (after! avy
         ;;   (setq avy-single-candidate-jump t))

         (after! paren
                 (setq! show-paren-delay 0))

         (add-hook! 'prog-mode-hook #'biome-sp-strict-h)

         (after! rainbow-delimiters
                 ;; Doom sets this to 4 for possible performance reasons. I like having more (and 9 is the rainbow-delimiters default). The Doom base theme defines 9 too so this should look okay on most themes (though if they define their own faces there might be some duplication/clash where their definitions stop and the base theme's start).
                 (setq rainbow-delimiters-max-face-count 9))

         (after! smartparens
                 (map! :map smartparens-mode-map
                       "C-s-h" #'sp-backward-sexp
                       "C-s-l" #'sp-forward-sexp
                       "C-s-u" #'sp-backward-up-sexp
                       "C-s-o" #'sp-up-sexp
                       :gn "C-s-m" #'sp-backward-down-sexp ; Bind in normal mode explicitly to override the Doom mapping in ~/.config/emacs/modules/config/default/config.el:447
                       "C-s-." #'sp-down-sexp
                       "C-s-c" #'sp-splice-sexp
                       "C-s-s" #'sp-splice-sexp-killing-backward
                       "C-s-f" #'sp-splice-sexp-killing-forward
                       "C-s-x" #'sp-backward-slurp-sexp
                       "C-s-v" #'sp-forward-slurp-sexp
                       "C-s-w" #'sp-backward-barf-sexp
                       "C-s-r" #'sp-forward-barf-sexp
                       (:prefix "C-s-;"
                        "(" #'sp-wrap-round
                        "[" #'sp-wrap-square
                        "{" #'sp-wrap-curly
                        "s" #'sp-split-sexp
                        "j" #'sp-join-sexp
                        "r" #'sp-raise-sexp
                        "c" #'sp-convolute-sexp
                        "w" #'sp-rewrap-sexp)))

         (defun biome-sp-strict-h ()
           (add-hook! 'smartparens-enabled-hook :local
                      #'turn-on-smartparens-strict-mode
                      (defun biome-modify-sp-strict-mode-map-h ()
                        (map! :map smartparens-strict-mode-map
                              :i "DEL" #'sp-backward-delete-char)))))

       (if evil?
           '((after! evil
                     (map! :m (vector 'C-i) nil)) ; Remove Doom's binding for `evil-jump-forward'.

             (after! evil-multiedit
                     (setq evil-multiedit-follow-matches t)))
           '())

       (wip-config))

      #:snippets
      `(("clojure-mode" (("bogus" "# -*- mode: snippet -*-\n# name: bogus\n# key: bogus\n# --\n(defn $1\n  \"$2\"$>\n  [$3]$>\n  $0)$>")))))

     (if stateless?
         (list
          (doom-service
           'bookmarks-indirection
           #:config
           ;; The Emacs bookmark package checks the modification time of the bookmarks file and offers to reload it if it changed. In the stateless case the file is a symlink and it's modified on every reconfigure (the symlink is recreated). This both breaks the funcationality (it should be checking the target file, not the symlink) and ressults in annoying prompts on reconfigure.
           '((after! bookmark
                     (setq bookmark-default-file (concat doom-data-dir "bookmarks/bookmarks")))))

          (doom-stateless-service
           name
           #:state '(((#:path . ("autosave/" #:doom-base #:cache))
                      (#:storage . #:storage/machine))
                     ((#:path . ("bookmarks/" #:doom-base #:data))
                      (#:storage . #:storage/persist))
                     ((#:path . ("recentf" #:doom-base #:cache))
                      (#:storage . #:storage/persist))
                     ((#:path . ("savehist" #:doom-base #:cache))
                      (#:storage . #:storage/persist))
                     ((#:path . ("saveplace" #:doom-base #:cache))
                      (#:storage . #:storage/persist))
                     ((#:path . ("tramp" #:doom-base #:cache))
                      (#:storage . #:storage/machine))
                     ((#:path . ("tramp-autosave/" #:doom-base #:cache))
                      (#:storage . #:storage/machine))

                     ((#:path . ("scratch/" #:doom-base #:data))
                      (#:storage . #:storage/persist))

                     ((#:path . ("projects" #:doom-base #:data))
                      (#:storage . #:storage/persist))
                     ((#:path . ("projectile.projects" #:doom-base #:cache))
                      (#:storage . #:storage/persist))
                     ((#:path . ("projectile.cache" #:doom-base #:cache))
                      (#:storage . #:storage/machine))

                     ((#:path . ("workspaces/" #:doom-base #:data))
                      (#:storage . #:storage/machine)))))
         '()))))



;; TODO WIP. Some of this stuff belongs in other components or gated.

(define (wip-config)
  '((after! emojify
            (setq emojify-display-style 'unicode))

    ;; TODO donno where this should go, ~+default-minibuffer-maps~ is a doom thing I guess
    (define-key! :keymaps +default-minibuffer-maps
      "s-J" #'scroll-up-command
      "s-K" #'scroll-down-command)

    ;; I don't think I'll ever need this with Sway. Is it worth keeping this and having an "I'm not on Sway / some WM" conditional around it?
    ;; (add-to-list 'initial-frame-alist '(fullscreen . fullboth))

    (map! "s-&" (lambda (command)
                  (interactive (list (read-shell-command "$ ")))
                  (call-process-shell-command command nil 0 nil)))

    (map! :leader
          "s-," (lookup-key doom-leader-map (kbd "<")))

    ;; (after! company
    ;;         (map! :map company-active-map
    ;;               "s-[" #'company-show-doc-buffer ; Currently opens Help, it would be better if I made it use Helpful.
    ;;               "s-]" #'company-show-location))

    (after! evil-org
            (map! :map evil-org-mode-map
                  (:prefix "g"
                   :nv "{" #'evil-backward-paragraph
                   :nv "}" #'evil-forward-paragraph)))

    ;; Doesn't work yet: you have to switch to the buffer "manually" once before it starts being treated as real
    (map! "s-d m" (cmd! (doom-set-buffer-real (current-buffer) t)))

    ;; `forward-char' in original definition is messing things up, do this for now
    ;; (defun +eshell/search-history ()
    ;;   (interactive)
    ;;   (consult-history))
    (after! esh-mode
            (map! :map eshell-mode-map
                  "C-s" #'consult-history))

    (after! vertico
            (setq vertico-count 20  ; Trying out, maybe too big.
                  vertico-scroll-margin 7))

    (map! (:leader
           "A" #'embark-dwim)
          ;; Trying out cycling (temp)
          "C-:" #'embark-act
          "s-q" #'embark-cycle)

    ;; REVIEW Might be useful if there end up being a lot more of these and they have a lot of similarities: https://www.gnu.org/software/emacs/manual/html_node/elisp/Extending-Rx.html
    ;; TODO `better-jumper-jump-backward' doesn't consider these buffers, that's probably not a problem with `doom-real-buffer-functions' but rather something I have to do specially for it.
    (use-package! s)
    (add-hook! 'doom-real-buffer-functions
               ;; (defun biome-new-buffer-p (buf)
               ;;   (s-matches?
               ;;    (rx bol "*new*"
               ;;        (* "<" (+ digit) ">")
               ;;        eol)
               ;;    (buffer-name buf)))
               ;; TODO This should be in my Org config section / module but wait to move it until I'm confident I'll be continuing with this marking-buffers-as-real approach.
               (defun biome-org-src-edit-buffer-p (buf)
                 (s-matches?
                  (rx bol "*Org Src " (+ anything) "*"
                      (* "<" (+ digit) ">")
                      eol)
                  (buffer-name buf))))

    ;;  TODO Might be easier to do this with file-local variables.
    ;; (setq biome--phrenetic-dir "~/work/phrenetic")
    ;; (add-hook! 'org-mode-hook
    ;;   (defun biome-add-org-autotangle-after-save-hook-h ()
    ;;     (add-hook! 'after-save-hook :local
    ;;       (defun biome-autotangle-h ()
    ;;         (when (file-in-directory-p buffer-file-name biome--phrenetic-dir)
    ;;           ;; TODO ensure there aren't files in the output directory that no longer correspond to the org file. Just wipe it?
    ;;           (let ((org-confirm-babel-evaluate nil))
    ;;             (org-babel-tangle)))))))

    ;; (use-package! org-tanglesync
    ;;   :hook ((org-mode . org-tanglesync-mode)
    ;;          ((prog-mode text-mode) . org-tanglesync-watch-mode))
    ;;   :config
    ;;   (setq org-tanglesync-watch-files '("/home/pharcosyle/work/phrenetic/phrenetic.org")))

    ;; Maybe `s-S' to save-and-tangle? If I don't get some sort of auto-tangling thing going
    ;; - maybe it could be "save and eval defun (C-M-x)" in lisp/programming modes?'
    ;; Maybe a hotkey that just jumps back and forth between my most recently focused browser window and eemacs?
    ;; Maybe bind `s-o' in `consult-buffer' to "close consult-buffer and open +vertico/switch-workspace-buffer"

    (map! "s-o" (lookup-key doom-leader-map (kbd "<")))
    ;; Get rid of binding for =s-r=?
    (map! "s-r" nil)

    ;; Also messes with my sexp-movement bindings
    ;; (map! "C-s-j" #'evil-scroll-down
    ;;       "C-s-k" #'evil-scroll-up)

    (after! evil
            (map! :map evil-motion-state-map
                  ;; Maybe "C-s-o" but then I'll have to change my sexp-movement command(s). There are other bindings for jump-forward though, maybe I'll just use those.
                  "C-S-o" #'evil-jump-forward))))

(define* (doom-user #:key email)
  (list
   (doom-service
    'user
    #:config
    `((setq user-mail-address ,email)))))

(define (doom-keymaps)
  (append
   (list
    (doom-service
     'keymaps
     #:config
     '((defun biome--trans (&rest rest)
         (-each (-partition 2 rest)
                (-lambda ((to from))
                         (define-key key-translation-map (kbd to) (kbd from)))))

       (biome--trans "C-h" "DEL"
                     "C-?" "C-h"

                     "s-h" "<left>"
                     "s-j" "<down>"
                     "s-k" "<up>"
                     "s-l" "<right>")

       (setq doom-leader-alt-key "s-SPC"
             doom-localleader-key "s-m"
             doom-localleader-alt-key "s-m")

       (defalias 'original-yank-pop #'yank-pop)

       ;; REVIEW Some of these should be in `:after' (or their respective package) sections but I'm not totally certain where I want to put bindings yet and I'm lazy.
       (map! "s-V" #'original-yank-pop

             "s-SPC" doom-leader-map

             ;; Adapted from Doom macOS bindings: ~/.config/emacs/modules/config/default/config.el:263
             "s-`" #'other-frame
             "s-n" #'+default/new-buffer
             "s-z" #'undo
             "s-Z" #'redo
             "s-c" (if (featurep 'evil) #'evil-yank #'copy-region-as-kill) ; TODO If keeping this, gate with `evil?'.
             "s-v" #'yank
             "s-s" #'save-buffer
             "s-x" #'execute-extended-command
             ;; REVIEW I don't think I need this, any time I'm in visual mode I can use `evil-delete` ("d")
             ;; :v "s-x" #'kill-region
             "s-/" (cmd! (save-excursion (comment-line 1)))
             ;; TODO If keeping this, gate with `evil?'.
             :n "s-/" #'evilnc-comment-or-uncomment-lines
             :v "s-/" #'evilnc-comment-operator

             "s-t" (lookup-key doom-leader-map (kbd "`"))
             "s-f" (lookup-key doom-leader-map (kbd "s b"))
             "s-r" (lookup-key doom-leader-map (kbd "f r"))
             "s-w" (lookup-key doom-leader-map (kbd "b k"))
             "s-d w" (lookup-key doom-leader-map (kbd "w d"))
             "s-d s-w" (cmd! (kill-current-buffer)
                             (+workspace/close-window-or-workspace))
             "s-g" (lookup-key doom-leader-map (kbd "g g"))
             "s-," (lookup-key doom-leader-map (kbd "w w"))
             "s-<" (lookup-key doom-leader-map (kbd "w W"))
             "s-y" (lookup-key doom-leader-map (kbd "i y"))
             "s-{" (lookup-key doom-leader-map (kbd "b p"))
             "s-}" (lookup-key doom-leader-map (kbd "b n"))
             "s-p" (lookup-key global-map (kbd "C-~")) ; TODO if I'm keeping this, improve it to first switch focus to a/the popup window if one isn't focused already

             "s-u" (lookup-key doom-leader-map (kbd "u"))
             "s-U" #'negative-argument ; REVIEW Trying this out.
             (:map universal-argument-map
              "s-u" #'universal-argument-more)

             ;; TODO Gate with `evil?'.
             (:after evil-easymotion
              "s-a" (lookup-key evilem-map (kbd "SPC")))

             "s-." #'repeat

             ;; TODO Gate with `evil?'.
             "s-J" #'evil-scroll-down
             "s-K" #'evil-scroll-up

             (:prefix "s-d"
              "." #'repeat-complex-command

              "h" #'git-gutter:popup-hunk
              ;; "o" #'+macos/open-in-default-program ;; TODO consider a Guix alternative? Meh.
              "r" #'projectile-replace
              "t" #'tldr
              "s" #'org-save-all-org-buffers
              "a" #'link-hint-open-multiple-links
              ;; TODO Gate with `evil?'.
              ;; "b" (cmd! (evil-local-mode 'toggle)
              ;;           (when evil-local-mode (evil-normal-state)))
              (:prefix "c"
               "f" #'org-gcal-fetch
               "s" #'org-gcal-sync
               "p" #'org-gcal-post-at-point)))

       ;; TODO wip stuff
       ;; If I keep this I can get rid of the "C-_" binding I have too. Update: maybe? Some places C-_ works and some s-h does (in vertico)? What about my key translation for C-? ? Straighten all this shit out.
       ;; - update: Guile won't accept the help-char string escape the way it is so I've removed it for now:
       ;;   (setq help-char (string-to-char "\C-_"))
       (biome--trans "s-h" "C-h")
       ;; Maybe "C-s-i" but then I'll have to change my sexp-movement command(s). Maybe keep this even if I do that, for consistency.
       (biome--trans "C-S-i" "<backtab>"))))

   (doom-dash)))

(define* (doom-undo #:key stateless?)
  (let ((name 'undo))
    (list
     (doom-service
      name
      #:modules '((#:emacs
                   undo))
      #:config
      '((after! undo-fu
                (setq undo-fu-ignore-keyboard-quit t))))

     (when stateless?
       (doom-stateless-service
        name
        #:state '(((#:path . ("undo-fu-session/" #:doom-base #:cache))
                   (#:storage . #:storage/machine))))))))

(define* (doom-ts-lang lang
                       #:key
                       tree-sitter?
                       config)
  (append
   (list
    (doom-service
     lang
     #:modules `((#:lang
                  (,lang ,@(if tree-sitter? '(+tree-sitter) '()))))
     #:config (or config '())))

   (if tree-sitter?
       (doom-tree-sitter) '())))

(define (doom-tree-sitter)
  (list
   (doom-service
    'tree-sitter
    #:modules `((#:tools
                 tree-sitter)))))

(define (doom-emacs-lisp)
  (list
   (doom-service
    'emacs-lisp
    #:modules `((#:lang
                 emacs-lisp))
    #:config
    '((after! elisp-mode
              (setq-hook! 'emacs-lisp-mode-hook indent-tabs-mode nil))))))

(define* (doom-javascript #:key tree-sitter?)
  (append
   (doom-ts-lang 'javascript #:tree-sitter? tree-sitter?)
   (doom-ts-lang 'json #:tree-sitter? tree-sitter?)))

(define (doom-markdown)
  (list
   (doom-service
    'markdown
    #:modules `((#:lang
                 markdown)))))

(define (doom-xml+csv)
  (list
   (doom-service
    'xml+csv
    #:modules `((#:lang
                 data)))))

(define* (doom-web #:key tree-sitter?)
  (append
   (doom-ts-lang 'web #:tree-sitter? tree-sitter?)
   (doom-javascript #:tree-sitter? tree-sitter?)

   (list
    (doom-service
     'rest
     #:modules `((#:lang
                  (rest +jq)))))))

(define (doom-dash)
  (list
   (doom-service
    'dash
    #:packages
    '((package! dash :pin "1de9dcb83eacfb162b6d9a118a4770b1281bcd84"))
    #:config
    '((use-package! dash)))))

(define* (doom-transient #:key stateless?)
  (let ((name 'transient))
    (list
     (doom-service
      name
      #:config
      `((after! transient
                (setq transient-history-limit ,very-big-history))))

     (when stateless?
       (doom-stateless-service
        name
        #:state '(((#:path . ("transient/history" #:doom-base #:data))
                   (#:storage . #:storage/persist))))))))

(define* (doom-desktop #:key
                       theme
                       light-colors
                       dark-colors
                       font
                       font-variable-pitch
                       font-size)
  (append
   (doom-themes #:theme theme
                #:light-colors light-colors
                #:dark-colors dark-colors)
   (doom-fonts #:font font
               #:font-variable-pitch font-variable-pitch
               #:font-size font-size)))

(define* (doom-themes #:key
                      theme
                      light-colors
                      dark-colors)
  (let ((light-theme-name 'doom-one-light)
        (dark-theme-name 'doom-nuclear))
    (list
     (doom-service
      'themes
      #:config
      `((setq doom-theme ',(case theme
                             ((#:theme/light) light-theme-name)
                             ((#:theme/dark) dark-theme-name))))
      #:themes
      (append
       (doom-light-theme #:name light-theme-name
                         #:colors light-colors)
       (doom-dark-theme #:name dark-theme-name
                        #:colors dark-colors))))))

;; Nothing here yet.
(define* (doom-light-theme #:key name colors)
  `())

(define* (doom-dark-theme #:key name colors)
  (let ((clr (lambda (color)
               (assoc-ref colors color))))
    `((,name
       ((fg        '(,(clr #:color/fg)      "#bfbfbf" "brightwhite"))
        (bg        '(,(clr #:color/bg)      "black"   "black"))

        (fg-alt    '("#63677f"              "#2d2d2d" "white"))
        (bg-alt    '("#262831"              "black"   "black"))

        (base0     '(,(clr #:color/base0)   "black"   "black"))
        (base1     '("#202229"              "#1e1e1e" "brightblack")) ; (doom-darken "#262831" 0.15)
        (base2     '("#22242c"              "#2e2e2e" "brightblack")) ; (doom-darken "#262831" 0.10)
        (base3     '("#24262e"              "#262626" "brightblack")) ; (doom-darken "#262831" 0.05)
        (base4     '("#484b5b"              "#3f3f3f" "brightblack"))
        (base5     `(,(car fg-alt)          "#525252" "brightblack"))
        (base6     '("#7c82a0"              "#6b6b6b" "brightblack")) ; (doom-darken "#939abd" 0.15)
        (base7     '("#939abd"              "#979797" "brightblack"))
        (base8     '(,(clr #:color/base8)   "#dfdfdf" "white"))

        (grey      base4)
        (red       '(,(clr #:color/red)     "#ff6655" "red"))
        (orange    '(,(clr #:color/orange)  "#dd8844" "brightred"))
        (green     '(,(clr #:color/green)   "#99bb66" "green"))
        (teal      '("#fec0cb"              "#44b9b1" "brightgreen"))
        (yellow    '(,(clr #:color/yellow)  "#ECBE7B" "yellow"))
        (blue      '(,(clr #:color/blue)    "#51afef" "brightblue" ))
        (dark-blue '("#5f68de"              "#2257A0" "blue"))
        (magenta   '(,(clr #:color/magenta) "#c678dd" "brightmagenta"))
        (violet    '(,(clr #:color/violet)  "#a9a1e1" "magenta"))
        (cyan      '(,(clr #:color/cyan)    "#46D9FF" "brightcyan"))
        (dark-cyan '("#cf8191"              "#5699AF" "cyan"))

        (highlight      ,(clr #:color/accent))
        (vertical-bar   base0)
        (selection      dark-blue)
        (builtin        blue)
        (comments       base5)
        (doc-comments   base7)
        (constants      yellow)
        (functions      blue)
        (keywords       magenta)
        (methods        teal)
        (operators      cyan)
        (type           orange)
        (strings        green)
        (variables      red)
        (numbers        dark-cyan)
        (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
        (error          red)
        (warning        yellow)
        (success        green)
        (vc-modified    orange)
        (vc-added       green)
        (vc-deleted     red))

       ;; Doom base theme overrides.
       ((cursor :background "#fdd94a")
        ((font-lock-comment-delimiter-face &override) :foreground base7)
        ((font-lock-doc-face &override) :foreground violet)
        ((line-number-current-line &override) :foreground violet)
        (mode-line :background base1 :foreground fg)
        (mode-line-inactive :background base3 :foreground base6)
        (org-block :background (doom-lighten bg-alt 0.02))
        (rainbow-delimiters-depth-1-face :foreground fg)
        (rainbow-delimiters-depth-2-face :foreground magenta)
        (rainbow-delimiters-depth-3-face :foreground blue)
        (rainbow-delimiters-depth-4-face :foreground cyan)
        (rainbow-delimiters-depth-5-face :foreground green)
        (rainbow-delimiters-depth-6-face :foreground yellow)
        (rainbow-delimiters-depth-7-face :foreground orange)
        (rainbow-delimiters-depth-8-face :foreground red)
        (rainbow-delimiters-depth-9-face :foreground teal)

        ;; Custom faces.
        (clojure-character-face :foreground teal :weight 'bold)
        (doom-modeline-bar :background highlight)
        (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
        (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
        (doom-modeline-buffer-project-root :foreground green :weight 'bold)
        (solaire-mode-line-face :inherit 'mode-line :background base0)
        (solaire-mode-line-inactive-face :inherit 'mode-line-inactive :background base2)

        ;; These are present in the `doom-one' theme on which mine is based but I'm not sure if I need or want them. I'm keeping them around for now.
        (css-proprietary-property :foreground orange)
        (css-property :foreground green)
        (css-selector :foreground blue)
        (font-latex-math-face :foreground green)
        (markdown-markup-face :foreground base5)
        (markdown-header-face :inherit 'bold :foreground red)
        ((markdown-code-face &override) :background (doom-lighten base3 0.05))
        (rjsx-tag :foreground red)
        (rjsx-attr :foreground orange))))))

(define* (doom-fonts #:key
                     font
                     font-variable-pitch
                     font-size)
  (append
   (list
    (doom-service
     'fonts
     #:config
     `((defun biome--set-font-safe (sym font)
         (when (find-font font)
           (set sym font)))

       (biome--set-font-safe
        'doom-font
        (font-spec :family ,(assoc-ref font #:font/name)
                   :size ,(+ font-size 2)
                   :weight ',(if-let ((weight (assoc-ref font #:font/weight)))
                                     (font-weight->emacs-face-weight weight)
                                     'normal)))
       (biome--set-font-safe
        'doom-variable-pitch-font
        (font-spec :family ,(assoc-ref font-variable-pitch #:font/name))))))

   (doom-dash)))

(define (font-weight->emacs-face-weight weight)
  (-> weight
      string-downcase
      string->symbol))

(define* (guix-pm #:key
                  stateless?
                  doom?
                  ;; channels
                  ;; channels-file
                  )
  (append
   ;; (list
   ;;  (let ((name 'guix-add-channels))
   ;;    (cond
   ;;     (channels
   ;;      (simple-service name
   ;;                      home-channels-service-type
   ;;                      channels))
   ;;     (channels-file
   ;;      (simple-service name
   ;;                      home-xdg-configuration-files-service-type
   ;;                      `(("guix/channels.scm"
   ;;                         ,channels-file)))))))

   (if stateless?
       (let ((guix-profile ".guix-profile")
             (guix-current "guix/current"))
         (list
          (simple-service
           'guix-ensure-profiles
           home-activation-service-type
           #~(#$stateless-ensure-profiles-gexp
              (let ((user-profiles-dir (string-append "/var/guix/profiles/per-user/" (passwd:name (getpwuid (getuid))))))
                (list (cons (string-append (getenv "HOME") "/" #$guix-profile)
                            (string-append user-profiles-dir "/guix-profile"))
                      (cons (string-append (getenv "XDG_CONFIG_HOME") "/" #$guix-current)
                            (string-append user-profiles-dir "/current-guix"))))))

          (home-stateless-service
           'guix
           #:state '(((#:path . ("guix/" #:xdg-base "XDG_CACHE_HOME"))
                      (#:storage . #:storage/machine)))
           #:ignore `(((#:path . ,guix-profile))
                      ((#:path . (,guix-current #:xdg-base "XDG_CONFIG_HOME")))))))
       '())

   (guile-services #:stateless? stateless?
                   #:doom? doom?)

   (if doom?
       (doom-guix #:stateless? stateless?) '())))

(define* (guile-services #:key
                         stateless?
                         doom?)
  (append
   (list
    ;; Typically we would add the package to the home profile to not depend on packages being installed at the system level, however in Guile's case it will (I think?) always be installed system-level and users probably want the same version of Guile as the one on the system anyway.
    ;; (simple-service
    ;;  'guile-add-packages
    ;;  home-profile-service-type
    ;;  (list guile))

    (simple-service
     'guile-add-config
     home-files-service-type
     `((".guile"
        ,%default-dotguile)))

    (simple-service
     'guile-add-gdb-config
     home-xdg-configuration-files-service-type
     `(("gdb/gdbinit"
        ,%default-gdbinit)))

    (when stateless?
      (home-stateless-service
       'guile
       #:state '(((#:path . ".guile_history")
                  (#:storage . #:storage/persist))
                 ((#:path . ("guile/" #:xdg-base "XDG_CACHE_HOME"))
                  (#:storage . #:storage/machine))))))

   (if doom?
       (doom-guile #:stateless? stateless?) '())))

(define* (doom-guile #:key stateless?)
  (append
   (let ((name 'guile))
     (list
      (doom-service
       name
       #:modules '((#:lang
                    (scheme +guile)))
       #:config
       '((after! scheme
                 (setq-hook! 'scheme-mode-hook indent-tabs-mode nil))

         ;; TODO Maybe do this if popping to the side is too annoying
         ;; (after! geiser-repl
         ;;   (setq geiser-repl-use-other-window nil))
         ))

      (when stateless?
        (doom-stateless-service
         name
         #:state '(((#:path . ("geiser-history.guile" #:doom-base #:cache))
                    (#:storage . #:storage/persist)))))))

   (doom-transient #:stateless? stateless?)))

(define* (doom-guix #:key stateless?)
  (append
   (let ((name 'guix))
     (list
      (doom-service
       name
       #:packages
       '((package! guix
                   :recipe (:fork
                            (:host nil
                             :repo "https://git.savannah.gnu.org/git/guix/emacs-guix.git"))
                   :pin "455272c5cc72ed4ba5bad13c669f024f51479a58")
         (package! build-farm :pin "5c268a3c235ace0d79ef1ec82c440120317e06f5") ; REVIEW trying out
         (package! guix-packaging ; REVIEW trying out
                   :recipe (:host github
                            :repo "ryanprior/emacs-guix-packaging"
                            :files (:defaults "snippets"))
                   :pin "5bbd1f1a268b3dfd813a75125ca88cbf0bef6529"))
       #:config
       '((after! guix
                 (set-popup-rules!
                  '(("^\\*Guix" :height 0.5))))
         (use-package! guix-devel
                       :hook (scheme-mode . guix-devel-mode))
         (use-package! guix-popup
                       :defer t
                       :init
                       (map! :leader
                             "l" #'guix))  ; REVIEW temporary binding?
         (use-package! guix-prettify
                       :hook (doom-first-buffer . global-guix-prettify-mode))
         (after! guix-pcomplete
                 (fmakunbound 'pcomplete/guix)) ; Don't use pcompletion, it doesn't work (I get an error: "Unknown # object: #~") plus my completions are already good (maybe even better than what it provides).

         (use-package! guix-packaging
                       :defer t
                       :init
                       (setq guix-packaging--data-dir (concat doom-cache-dir "guix-packaging")))

         ;; TODO Figure out how to do this with a gexp, e.g.:
         ;; #$(file-append guix-package "/etc")
         ;; and remove the inner safety `when'.
         (let ((guix-etc-dir (concat "/gnu/store/01q67ynhr3pw3gnl8pz7d8mm844hgfd5-guix-1.4.0-4.01fd830-checkout" "/etc")))
           (when (file-directory-p guix-etc-dir)
             (after! yasnippet
                     (add-to-list 'yas-snippet-dirs (concat guix-etc-dir "/snippets/yas")))
             (after! tempel
                     (unless (listp 'tempel-path)
                       (setq tempel-path (list tempel-path)))
                     (add-to-list 'tempel-path (concat guix-etc-dir "/snippets/tempel/*")))

             (load-file (concat guix-etc-dir "/copyright.el"))))

         (when (and user-full-name
                    user-mail-address)
           (setq copyright-names-regexp (format "%s <%s>" user-full-name user-mail-address)))))

      (when stateless?
        (doom-stateless-service
         name
         #:state '(((#:path . ("guix-packaging/" #:doom-base #:cache))
                    (#:storage . #:storage/machine)))))))

   (doom-guile #:stateless? stateless?)))

(define* (nix-pm #:key
                 stateless?
                 doom?
                 doom-tree-sitter?
                 login-shell
                 (channels `((,%nixpkgs-unstable-url . "nixpkgs")))
                 nixpkgs-config-settings
                 nixpkgs-config-expressions)
  (append
   (list
    (simple-service
     'nix-add-packages
     home-profile-service-type
     (list nix))

    (let ((name 'nix-source-profile)
          (source (mixed-text-file
                   "source-nix-profile"
                   #~(string-join
                      (list
                       "source" #$(file-append nix "/etc/profile.d/nix.sh"))))))
      (case login-shell
        ((#:shell/bash)
         (simple-service name
                         home-bash-service-type
                         (home-bash-extension
                          (bash-profile
                           (list source)))))
        ((#:shell/zsh)
         (simple-service name
                         home-zsh-service-type
                         (home-zsh-extension
                          (zprofile
                           (list source)))))))

    (simple-service
     'nix-add-channels
     home-files-service-type
     `((".nix-channels"
        ,(nix-channels-file "nix-channels" channels))))

    (simple-service
     'nix-add-nixpkgs-config
     home-xdg-configuration-files-service-type
     `(("nixpkgs/config.nix"
        ,(nixpkgs-config-file "nixpkgs-config.nix"
                              nixpkgs-config-settings
                              nixpkgs-config-expressions)))))

   (if stateless?
       (let ((nix-profile ".nix-profile")
             (nix-channels ".nix-defexpr/channels"))
         (list
          (simple-service
           'stateless-nix-ensure-profiles
           home-activation-service-type
           #~(#$stateless-ensure-profiles-gexp
              (let ((user-profiles-dir (string-append "/nix/var/nix/profiles/per-user/" (passwd:name (getpwuid (getuid))))))
                (list (cons (string-append (getenv "HOME") "/" #$nix-profile)
                            (string-append user-profiles-dir "/profile"))
                      (cons (string-append (getenv "HOME") "/" #$nix-channels)
                            (string-append user-profiles-dir "/channels"))))))

          (home-stateless-service
           'nix
           #:state '(((#:path . ("nix/" #:xdg-base "XDG_CACHE_HOME"))
                      (#:storage . #:storage/machine)))
           #:ignore `(((#:path . ,nix-profile))
                      ((#:path . ,nix-channels))))))
       '())

   (if doom?
       (doom-nix #:stateless? stateless?
                 #:tree-sitter? doom-tree-sitter?)
       '())))

(define (nix-channels-file name entries)
  (apply mixed-text-file name (nix-channels-serialize entries)))

(define (nix-channels-serialize entries)
  (append-map (match-lambda
                ((channel . name)
                 (list channel " " name "\n")))
              entries))

(define (nixpkgs-config-file name settings expressions)
  (apply mixed-text-file name
         (append
          (list "{" "\n")
          (append-map (match-lambda
                        ((n . v)
                         (list (symbol->string n)
                               " = "
                               (match v
                                 (#t "true")
                                 (#f "false")
                                 (v v))
                               ";" "\n")))
                      settings)
          (if expressions
              (list expressions) '())
          (list "}" "\n"))))

(define %nixpkgs-unstable-url "https://nixos.org/channels/nixpkgs-unstable")

(define* (doom-nix #:key
                   stateless?
                   tree-sitter?)
  (append
   (doom-ts-lang 'nix #:tree-sitter? tree-sitter?)
   (doom-transient #:stateless? stateless?)))

(define stateless-ensure-profiles-gexp
  (with-imported-modules (source-module-closure
                          '((guix build utils)
                            (phrenetic build utils))
                          #:select? phrenetic-module-name?)
    #~(lambda (profiles)
        (use-modules ((guix build utils) #:select (mkdir-p))
                     ((phrenetic build utils) #:select (no-follow-file-exists?)))

        (for-each
         (lambda (i)
           ;; I get an error if I try to use `match-lambda', I have no idea why.
           (let ((link (car i))
                 (profile (cdr i)))
             (when (and (no-follow-file-exists? profile)
                        (not (no-follow-file-exists? link)))
               (mkdir-p (dirname link))
               (symlink profile link))))
         profiles))))

(define* (kmonad-tool #:key
                      doom?
                      (device "/dev/input/by-id/usb-Apple_Inc._Apple_Internal_Keyboard___Trackpad-event-kbd")
                      (kbd (local-file "keyboard-config.kbd"))
                      kmonad)
  (list
   (simple-service
    'kmonad-add-packages
    home-profile-service-type
    (list kmonad))

   (simple-service
    'kmonad
    home-shepherd-service-type
    (kmonad-shepherd-service #:kbd kbd
                             #:kmonad kmonad))

   (when doom?
     (doom-service
      'kmonad
      #:packages '((package! kbd-mode
                             :recipe (:host github
                                      :repo "kmonad/kbd-mode")
                             :pin "a4c5f1c60ff392cb4037d53aeccef5163f91d91b"))))))

(define* (kmonad-shepherd-service #:key
                                  kbd
                                  kmonad)
  (list
   (shepherd-service
    (provision '(kmonad))
    (start #~(make-forkexec-constructor
              (list #$(file-append kmonad "/bin/kmonad") #$kbd)
              #:log-file #$(home-shepherd-service-log-file "kmonad.log")))
    (stop #~(make-kill-destructor)))))

(define* (pipewire-media #:key
                         stateless?
                         pipewire
                         (pipewire-pulse? #t))
  (list
   (service home-pipewire-service-type
            (home-pipewire-configuration
             (pipewire pipewire)
             (enable-pulseaudio? pipewire-pulse?)))

   (when (and stateless? pipewire-pulse?)
     (home-stateless-service
      'pipewire-pulse
      #:state '(((#:path . ("pulse/cookie" #:xdg-base "XDG_CONFIG_HOME"))
                 (#:storage . #:storage/machine)
                 (#:parent-dir-perms . ((#:mode . #o700)))))))

   (when stateless?
     (home-stateless-service
      'wireplumber
      #:state '(((#:path . ("wireplumber/" #:xdg-base "XDG_STATE_HOME"))
                 (#:storage . #:storage/machine)
                 (#:mode . #o700)))))))

(define (bluetooth)
  (append
   (list
    (doom-service
     'bluetooth
     #:packages
     '((package! bluetooth :pin "eaae9894353fe1bb32a276acdecb12b2d5e96ae3"))))

   (doom-dash)))

(define %colors
  '((#:colors/nuclear-light . ()) ; Nothing here yet.
    (#:colors/nuclear-dark . ((#:color/fg . "#dee2f8")
                              (#:color/bg . "#31343f")
                              (#:color/base0 . "#1e1f27")
                              (#:color/base8 . "#eef0fb") ; (doom-lighten "#dee2f8" 0.5)
                              (#:color/accent . "#8496ff")
                              (#:color/red . "#fb8578")
                              (#:color/orange . "#fdce5f")
                              (#:color/green . "#9fed9c")
                              (#:color/yellow . "#eddc91")
                              (#:color/blue . "#7db9fe")
                              (#:color/magenta . "#e29bf7")
                              (#:color/violet . "#aeb9f3")
                              (#:color/cyan . "#75e0f9")))))

(define %wallpapers
  `((#:wallpaper/alucard . ,(origin
                              (method url-fetch)
                              (uri "https://i.redd.it/ba5x648wvsu21.png")
                              (sha256
                               (base32
                                "1kjpbjxsrq6671his86z4grx7z06bkbg5025sxnmnwahibdfvhc1"))
                              (file-name "alucard.png")))))

(define* (font-library #:key
                       stateless?
                       font-sans
                       font-serif
                       font-mono
                       extra-fonts)
  (list
   (service home-fontconfig-service-type)
   ;; (service (as-> home-fontconfig-service-type $
   ;;            (without-extensions $ home-xdg-configuration-files-service-type)
   ;;            (service-type
   ;;             (inherit $)
   ;;             (extensions
   ;;              (cons* (service-extension
   ;;                      home-xdg-configuration-files-service-type
   ;;                      (partial add-fontconfig-config font-sans font-serif font-mono))
   ;;                     (service-type-extensions $))))))

   (simple-service
    'fonts-add-packages
    home-profile-service-type
    (append
     (map (lambda (font)
            (assoc-ref font #:font/package))
          (append
           (list font-sans
                 font-serif
                 font-mono)
           extra-fonts))))

   (when stateless?
     (home-stateless-service
      'fonts
      #:state '(((#:path . ("fontconfig/" #:xdg-base "XDG_CACHE_HOME"))
                 (#:storage . #:storage/machine)))))))

;; TODO Probably give up on fonts stuff and get rid of all of this code? If not there's some useful fonts conf examples at https://gitlab.archlinux.org/archlinux/packaging/packages/noto-fonts and less useful, more complex, ones in the google noto fedora package
;; TODO [2024-05-26 Sun] There's presets in the fontconfig package, share/fontconfig/conf.avail and etc/fonts/conf.d. Consider them?
;; TODO [2024-07-19 Fri] disabling this for now for no particular reason until I get back to fonts stuff
;; TODO [2024-07-22 Mon] using liberation fonts instead of ms-core now. Consider aliasing them to the equivalent microsoft fonts. See the fedora package for liberation fonts.
;; (define (add-fontconfig-config font-sans font-serif font-mono dirs)
;;   `(("fontconfig/fonts.conf"
;;      ,(apply mixed-text-file
;;              "fonts.conf"
;;              (list "<?xml version='1.0'?>" "\n"
;;                    "<!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>" "\n"
;;                    (sxml-serialize
;;                     `(fontconfig
;;                       ,@(map (lambda (dir)
;;                                `(dir ,dir))
;;                              dirs)
;;                       ,@(map (match-lambda
;;                                ((family . name)
;;                                 `(alias
;;                                   (family ,family)
;;                                   (prefer
;;                                    (family ,name)))))
;;                              `(("sans-serif" . ,(assoc-ref font-sans #:font/name))
;;                                ("serif" . ,(assoc-ref font-serif #:font/name))
;;                                ("monospace" . ,(assoc-ref font-mono #:font/name)))))))))))

;; (define (sxml-serialize tree)
;;   (call-with-output-string
;;     (lambda (port)
;;       (sxml->xml tree port)))) ; It'd be nice if this pretty-printed or if I added some code that did.

;; TODO These should really each be a function to parameterize the packages.
(define %fonts
  `((#:fonts/fira-code . ((#:font/name . "Fira Code")
                          (#:font/package . ,font-fira-code)))
    (#:fonts/gnu-unifont . ((#:font/name . "Unifont")
                            (#:font/package . ,font-gnu-unifont)))
    (#:fonts/google-noto . ((#:font/package . ,font-google-noto)))
    (#:fonts/google-noto-emoji . ((#:font/name . "Noto Emoji")
                                  (#:font/package . ,font-google-noto-emoji)))
    (#:fonts/google-noto-sans-cjk . ((#:font/package . ,font-google-noto-sans-cjk)))
    (#:fonts/google-noto-serif-cjk . ((#:font/package . ,font-google-noto-serif-cjk)))
    (#:fonts/iosevka-aile . ((#:font/name . "Iosevka Aile")
                             (#:font/package . ,font-iosevka-aile)))
    (#:fonts/iosevka-etoile . ((#:font/name . "Iosevka Etoile")
                               (#:font/package . ,font-iosevka-etoile)))
    (#:fonts/iosevka . ((#:font/name . "Iosevka")
                        (#:font/package . ,font-iosevka)))
    (#:fonts/liberation . ((#:font/package . ,font-liberation)))))

(define %date-formats
  '((#:date-format/simple . "%a %b %-d")))

(define %time-formats
  '((#:time-format/simple . "%-I:%M %p")))

(define* (xdg-user-directories #:key
                               stateless?
                               (dirs '((#:desktop . "$HOME/desktop")
                                       (#:documents . "$HOME/docs")
                                       (#:download . "$HOME/dl")
                                       (#:music . "$HOME/music")
                                       (#:pictures . "$HOME/pics")
                                       (#:publicshare . "$HOME/public")
                                       (#:templates . "$HOME/templates")
                                       (#:videos . "$HOME/vids"))))
  (list
   (simple-service
    'xdg-user-directories-add-packages
    home-profile-service-type
    (list xdg-utils
          xdg-user-dirs ; Copied from RDE. I don't think I need this (the only reason would be if programs ever call the `xdg-user-dir' shell command and this package isn't an input to theirs).
          desktop-file-utils)) ; Copied from RDE. I don't think I need this at all but it adds elisp code (a mode for editing .desktop files). I'll keep it just in case.

   (service
    home-xdg-user-directories-service-type
    (home-xdg-user-directories-configuration
     (desktop (assoc-ref dirs #:desktop))
     (documents (assoc-ref dirs #:documents))
     (download (assoc-ref dirs #:download))
     (music (assoc-ref dirs #:music))
     (pictures (assoc-ref dirs #:pictures))
     (publicshare (assoc-ref dirs #:publicshare))
     (templates (assoc-ref dirs #:templates))
     (videos (assoc-ref dirs #:videos))))

   (when stateless?
     (home-stateless-service
      'xdg-user-directories
      #:state
      (let ((normalize
             (lambda (dir)
               (as-> dir $
                     (let ((home-prefix "$HOME/"))
                       (if (string-prefix? home-prefix $)
                           (string-drop $ (string-length home-prefix))
                           $))
                     (string-append $ "/")))))
        (append
         (map (lambda (dir)
                `((#:path . ,(normalize dir))
                  (#:storage . #:storage/persist)))
              (list (assoc-ref dirs #:desktop)
                    (assoc-ref dirs #:documents)
                    (assoc-ref dirs #:music)
                    (assoc-ref dirs #:pictures)
                    (assoc-ref dirs #:publicshare)
                    (assoc-ref dirs #:templates)
                    (assoc-ref dirs #:videos)))
         `(((#:path . ,(normalize (assoc-ref dirs #:download)))
            (#:storage . #:storage/machine)))))))))

(define* (mesa #:key stateless?)
  (list
   (when stateless?
     (home-stateless-service
      'mesa
      #:state '(((#:path . ("mesa_shader_cache/" #:xdg-base "XDG_CACHE_HOME"))
                 (#:storage . #:storage/machine))
                ((#:path . ("mesa_shader_cache_db/" #:xdg-base "XDG_CACHE_HOME"))
                 (#:storage . #:storage/machine)))))))

(define (dbus-ipc)
  (list
   (service home-dbus-service-type)))

(define* (gdm #:key stateless?)
  (list
   (when stateless?
     (home-stateless-service
      'gdm
      #:state '(((#:path . ("gdm/" #:xdg-base "XDG_CACHE_HOME"))
                 (#:storage . #:storage/machine)
                 (#:mode . #o700)))))))

(define* (desktop-tty #:key
                      sessions
                      start-cmds)
  (list
   (simple-service
    'desktop-tty-default-session-start
    home-profile-service-type
    (list (default-session-start
            (match-let (((default-session _ ...) sessions))
              (assoc-ref start-cmds default-session)))))))

(define (default-session-start cmd)
  (package
    (inherit simple-package)
    (name "default-session-start")
    (source cmd)
    (arguments
     `(#:builder
       ,#~(begin
            (let ((bin (string-append #$output "/bin")))
              (mkdir #$output)
              (mkdir bin)
              (symlink #$source (string-append bin "/default-session-start"))))))))

(define* (gnome-desktop #:key stateless?)
  (list
   (simple-service
    'gnome-add-packages
    home-profile-service-type
    (list (gnome-start-package)
          (append-map package-propagated-inputs
                      (list gnome-meta-core-services
                            gnome-meta-core-shell
                            gnome-meta-core-utilities
                            gnome-essential-extras))))

   ;; (when stateless?
   ;;   (home-stateless-service
   ;;    'gnome
   ;;    #:state ...))
   ))

(define (gnome-start)
  (file-append (gnome-start-package) "/bin/" gnome-start-exe))

(define gnome-start-exe "gnome-start")

(define (gnome-start-package)
  (session-start
   gnome-start-exe
   (file-append gnome-session "/bin/gnome-session")
   #:log-file "gnome.log"
   #:pre-start (environment-variable-shell-definitions
                (list xdg-session-type-wayland-env-var))))

(define* (hyprland-wm #:key
                      pipewire?
                      portal-services
                      (main-mod #:ALT)
                      input
                      output
                      external-programs
                      extra-config)
  (append
   (list
    (simple-service
     'hyprland-add-packages
     home-profile-service-type
     (list (hyprland-start-package #:pipewire? pipewire?)
           hyprland))

    (simple-service
     'hyprland-conf
     home-xdg-configuration-files-service-type
     `(("hypr/hyprland.conf"
        ,(hyprlang-file
          "hyprland-conf"
          (append
           `((#:$mod ,main-mod)

             (#:bind #:$mod #:H #:workspace "-1")
             (#:bind #:$mod #:L #:workspace "+1")
             (#:bind #:$mod+Shift #:H #:movetoworkspace "-1")
             (#:bind #:$mod+Shift #:L #:movetoworkspace "+1")

             (#:bindm #:$mod #:mouse:272 #:movewindow)
             (#:bindm #:$mod+Shift #:mouse:272 #:resizewindow)

             (#:general ((#:gaps_out 55 5 5 320)))
             (#:animations ((#:enabled #f)))
             (#:xwayland ((#:force_zero_scaling #t)))
             (#:misc ((#:force_default_wallpaper 2)))

             (#:debug:disable_logs #f))

           (hyprland-general)
           (apply hyprland-input (alist->list input))
           ;; (apply hyprland-output (alist->list output))
           (apply hyprland-external-programs (alist->list external-programs))
           ;; (hyprland-app-windows)

           (or extra-config '())))))))

   (or portal-services '())))

(define* (hyprland-start #:key pipewire?)
  (file-append (hyprland-start-package #:pipewire? pipewire?)
               "/bin/" hyprland-start-exe))

(define hyprland-start-exe "hyprland-start")

(define* (hyprland-start-package #:key pipewire?)
  (session-start
   hyprland-start-exe
   (file-append hyprland "/bin/hyprland")
   #:log-file "hyprland.log"
   ;; #:pre-start (environment-variable-shell-definitions
   ;;              (cons* (xdg-current-desktop-env-var "Hyprland")
   ;;                     xdg-session-type-wayland-env-var
   ;;                     (wayland-env-vars pipewire?)))
   ;; TODO there's some issue in session-start, work around it
   #:pre-start (environment-variable-shell-definitions
                '(("ASDFASDFASDFFF" . "bob")))))

(define (hyprland-general)
  (append
   `(;; Windows
     (#:bind #:$mod+Shift #:C #:killactive)
     ;; (bindsym $mod+Shift+f fullscreen)
     (#:bind #:$mod+Shift #:Space #:togglefloating)
     ;; (bindsym $mod+Control+space focus mode_toggle)
     (#:bind #:$mod+Control #:H #:movefocus #:l)
     (#:bind #:$mod+Control #:J #:movefocus #:d)
     (#:bind #:$mod+Control #:K #:movefocus #:u)
     (#:bind #:$mod+Control #:L #:movefocus #:r)
     ;; (bindsym $mod+Shift+h move left)
     ;; (bindsym $mod+Shift+j move down)
     ;; (bindsym $mod+Shift+k move up)
     ;; (bindsym $mod+Shift+l move right)
     )

   ;; Workspaces
   (append
    (append-map (lambda (x)
                  (let ((n (number->string (modulo x 10))))
                    `((#:bind #:$mod ,n #:workspace ,x)
                      (#:bind #:$mod+Shift ,n #:movetoworkspace ,x))))
                (iota 10 1))
    ;; `((bindsym $mod+tab workspace back_and_forth))
    )

   `(;; Scratchpad
     ;; (bindsym $mod+Shift+minus move scratchpad)
     ;; (bindsym $mod+minus scratchpad show)

     ;; Layout
     ;; (bindsym $mod+Shift+b splith)
     ;; (bindsym $mod+Shift+v splitv)
     ;; (bindsym $mod+Shift+s layout stacking)
     ;; (bindsym $mod+Shift+w layout tabbed)
     ;; (bindsym $mod+Shift+e layout toggle split)

     ;; (bindsym $mod+Shift+a focus parent)

     ;; (bindsym $mod+Shift+r reload)
     (#:bind #:$mod+Control+Shift #:Q #:exit)


     ;; (default_border pixel)
     ;; (default_floating_border pixel)
     ;; (gaps inner 8)

     ;; (floating_modifier $mod normal)
     )))

(define* (hyprland-input #:key
                         inputs
                         default-kb-repeat-delay
                         default-kb-repeat-rate
                         default-tp-tap?)

  `((#:input ((#:follow_mouse 0)
              (#:float_switch_override_focus 0)
              (#:repeat_delay ,default-kb-repeat-delay)
              (#:repeat_rate ,default-kb-repeat-rate)
              (#:kb_layout "us")
              (#:kb_model "macbook78")
              (#:touchpad ((#:natural_scroll #t)
                           (#:tap-to-click ,default-tp-tap?))))))

  ;; (append
  ;;  `((focus_follows_mouse #f)
  ;;    (input type:keyboard
  ;;           ,(append
  ;;             (if default-kb-repeat-delay
  ;;                 `((repeat_delay ,default-kb-repeat-delay)) '())
  ;;             (if default-kb-repeat-rate
  ;;                 `((repeat_rate ,default-kb-repeat-rate)) '())))
  ;;    (input type:touchpad
  ;;           ((tap ,(if default-tp-tap?
  ;;                      'enabled 'disabled)))))

  ;;  (->> (or inputs '())
  ;;       (filter (lambda (input)
  ;;                 (assoc-ref input #:keyboard/layout)))
  ;;       (map (lambda (input)
  ;;              `(input ,(string-join (list (number->string
  ;;                                           (assoc-ref input #:keyboard/vendor-id))
  ;;                                          (number->string
  ;;                                           (assoc-ref input #:keyboard/product-id))
  ;;                                          (string-replace-substring
  ;;                                           (assoc-ref input #:keyboard/name) " " "_"))
  ;;                                    ":")
  ;;                ,(let ((kb-layout (assoc-ref input #:keyboard/layout)))
  ;;                   (append
  ;;                    `((xkb_layout ,(keyboard-layout-name kb-layout)))
  ;;                    (if-let ((variant (keyboard-layout-variant kb-layout)))
  ;;                            `((xkb_variant ,variant)) '())
  ;;                    (if-let ((model (keyboard-layout-model kb-layout)))
  ;;                            `((xkb_model ,model)) '())
  ;;                    (let ((options (keyboard-layout-options kb-layout)))
  ;;                      (if (null? options)
  ;;                          '()
  ;;                          `((xkb_options ,(string-join
  ;;                                           (keyboard-layout-options kb-layout) ","))))))))))))
  )

(define* (hyprland-output #:key
                          outputs
                          default-bg-image)
  (append
   `((output *
      ,(if default-bg-image
           `((bg ,default-bg-image fill)) '())))
   ;; (map (lambda (output)
   ;;        `((output ,(as-> (list (assoc-ref output #:display/make)
   ;;                               (assoc-ref output #:display/model)) $
   ;;                         (string-join $)
   ;;                         (quoted $)))
   ;;          ;; ...
   ;;          ))
   ;;      (or outputs '()))
   ))

;; TODO Temporary
(define (gexp-cmd cmd)
  #~(string-join '#$cmd))

(define* (hyprland-external-programs #:key
                                     idle-manager
                                     screen-locker
                                     notifier
                                     power-monitor
                                     app-launcher
                                     terminal
                                     backup-terminal
                                     emacs-interface)
  (let ((executions
         (map (lambda (cmd)
                (list #:exec-once (gexp-cmd cmd)))
              (list idle-manager
                    (assoc-ref notifier #:notifier/daemon)
                    power-monitor)))

        (bindings
         (append
          `((#:bind #:$mod #:Return #:exec ,(gexp-cmd terminal))
            ,@(if backup-terminal
                  `((#:bind #:$mod+Control+Shift #:Return #:exec ,(gexp-cmd backup-terminal)))
                  '())
            (#:bind #:$mod+Shift #:D #:exec ,(gexp-cmd app-launcher))
            (#:bind #:$mod+Control+Shift #:L #:exec ,(gexp-cmd screen-locker))
            (#:bind #:$mod+Shift #:Y #:exec ,(gexp-cmd (assoc-ref emacs-interface #:emacs/program)))
            (#:bind #:$mod #:Y #:exec ,(gexp-cmd (assoc-ref emacs-interface #:emacs/new-frame))))

          (hyprland-notifications notifier)
          (hyprland-backlights)
          (hyprland-volume)
          (hyprland-player))))

    (append executions bindings)))

(define (hyprland-notifications notifier)
  `((#:bind #:$mod #:M #:exec ,(gexp-cmd (assoc-ref notifier #:notifier/dismiss)))
    (#:bind #:$mod+Shift #:M #:exec ,(gexp-cmd (assoc-ref notifier #:notifier/restore)))
    (#:bind #:$mod+Control #:M #:exec ,(gexp-cmd (assoc-ref notifier #:notifier/menu)))))

(define (hyprland-backlights)
  (let ((backlights (brightnessctl-backlights)))
    `((#:bindl "" #:XF86MonBrightnessUp #:exec ,(gexp-cmd (assoc-ref backlights #:backlight/up)))
      (#:bindl "" #:XF86MonBrightnessDown #:exec ,(gexp-cmd (assoc-ref backlights #:backlight/down))))))

(define (hyprland-volume)
  (let ((volume (pipewire-volume))) ; TODO This should be dependent on whether media is pulseaudio or pipewire.
    `((#:bindl "" #:XF86AudioRaiseVolume #:exec ,(gexp-cmd (cmd-chain (assoc-ref volume #:volume/up))))
      (#:bindl "" #:XF86AudioLowerVolume #:exec ,(gexp-cmd (cmd-chain (assoc-ref volume #:volume/down))))
      (#:bindl "" #:XF86AudioMute #:exec ,(gexp-cmd (assoc-ref volume #:volume/mute-unmute)))
      (#:bindl "" #:XF86AudioMicMute #:exec ,(gexp-cmd (assoc-ref volume #:volume/mic-mute-unmute))))))

;; TODO I don't know how I want to handle "multi-commands" yet, just do this for the ones I know that are for now. However at least in this case I do like the idea of doing "&&" between them rather than, say, comma-separating them like Sway allows because in the general case if one of the commands fails it's probably best not to run any that would come after.
(define (cmd-chain cs)
  (->> cs
       (interpose (list "&&"))
       (apply append)))

(define (hyprland-player)
  (let ((player (playerctl-player)))
    `((#:bindl "" #:XF86AudioPlay #:exec ,(gexp-cmd (assoc-ref player #:player/play-pause)))
      (#:bindl "" #:XF86AudioPrev #:exec ,(gexp-cmd (assoc-ref player #:player/previous)))
      (#:bindl "" #:XF86AudioNext #:exec ,(gexp-cmd (assoc-ref player #:player/next))))))

(define* (hyprland-app-windows #:key (props (list emacs-modal-window-props
                                                  ;; ...
                                                  )))
  (map (lambda (p)
         (let ((commands (append (list (if (assoc-ref p #:window-prop/floating?)
                                           '((floating enable)) '())
                                       ;; ...
                                       ))))
           (if (not (null? commands))
               (append
                ;; Andrew Tropin says there's a bug in Sway that could affect this (https://github.com/swaywm/sway/issues/6950). I haven't experienced it but it's something to consider if I have issues.
                `(for_window ,(as-> (append
                                     (if-let ((title (assoc-ref p #:window-prop.criteria/title)))
                                             `(("title" . ,title)) '())
                                     ;; ...
                                     ) $
                                (map (match-lambda
                                       ((k . v)
                                        (string-append k "=" (quoted v))))
                                     $)
                                (string-join $)
                                (string-append "[" $ "]")))
                commands)
               '())))
       props))

(define* (sway-wm #:key
                  sway
                  pipewire?
                  doom?
                  portal-services
                  (xwayland? #t)
                  (sway-mod 'Mod1)
                  input
                  output
                  external-programs
                  extra-config)
  (append
   (list
    (simple-service
     'sway-add-packages
     home-profile-service-type
     ;; TODO [2024-06-16 Sun] RDE added `dconf' here because "It's needed to persist setting for gtk apps.", see https://github.com/abcdw/rde/commit/13c528ad6e2602284a77ab141391be0573778297
     ;; - does it belong here or in gtk-services?
     ;;   Possibly Andrew Tropin would have put it in his home-gtk3-service-type or feature-gtk3 if it weren't sway or wayland-specific
     (list (sway-start-package #:sway sway
                               #:pipewire? pipewire?)
           sway))

    (simple-service
     'sway-config
     home-xdg-configuration-files-service-type
     `(("sway/config"
        ,(apply
          mixed-text-file
          "sway-config"
          (serialize-sway-config
           (append
            `((xwayland ,(if xwayland? 'enable 'disable))
              ;; Some of these might not be necessary. To be safe I'm using a combination of those in RDE and suggested here:
              ;; - https://github.com/swaywm/sway/wiki#gtk-applications-take-20-seconds-to-start
              ;; - https://lists.sr.ht/~abcdw/rde-discuss/%3C87bku5ozzn.fsf%40bruun.xyz%3E
              ;; - https://github.com/emersion/xdg-desktop-portal-wlr#running
              (exec ,(file-append dbus "/bin/dbus-update-activation-environment")
                    DISPLAY WAYLAND_DISPLAY XDG_CURRENT_DESKTOP SWAYSOCK)
              (set $mod ,sway-mod))

            (sway-general)
            (apply sway-input (alist->list input))
            (apply sway-output (alist->list output))
            (apply sway-external-programs (alist->list external-programs))
            (sway-app-windows)
            (or extra-config '())))))))

    ;; (simple-service
    ;;  'sway-reload-config-on-change
    ;;  home-run-on-change-service-type
    ;;  `((,(string-append home-files-directory "/" xdg-configuration-files-directory "/sway/config")
    ;;     ,#~(system* #$(file-append sway "/bin/swaymsg") "reload"))))

    (when doom?
      (doom-service
       'sway
       #:packages '((package! i3wm-config-mode :pin "188e3978807ec39eba3cb69d973c0062af324215")))))

   (or portal-services '())))

(define* (sway-start #:key
                     sway
                     pipewire?)
  (file-append (sway-start-package #:sway sway
                                   #:pipewire? pipewire?)
               "/bin/" sway-start-exe))

(define sway-start-exe "sway-start")

(define* (sway-start-package #:key
                             sway
                             pipewire?)
  (session-start
   sway-start-exe
   (file-append sway "/bin/sway")
   #:log-file "sway.log"
   #:pre-start (environment-variable-shell-definitions
                (cons* (xdg-current-desktop-env-var "sway")
                       xdg-session-type-wayland-env-var ; Seems to be set even without this but relying on that might be a brittle, see https://www.reddit.com/r/swaywm/comments/skdt2b/comment/hvk8m8o
                       (wayland-env-vars pipewire?)))))

(define (sway-general)
  (append
   `(;; Windows
     (bindsym $mod+Shift+c kill)
     (bindsym $mod+Shift+f fullscreen)
     (bindsym $mod+Shift+space floating toggle)
     (bindsym $mod+Control+space focus mode_toggle)
     (bindsym $mod+h focus left)
     (bindsym $mod+j focus down)
     (bindsym $mod+k focus up)
     (bindsym $mod+l focus right)
     (bindsym $mod+Shift+h move left)
     (bindsym $mod+Shift+j move down)
     (bindsym $mod+Shift+k move up)
     (bindsym $mod+Shift+l move right))

   ;; Workspaces
   (append
    (append-map (lambda (x)
                  (let ((n (number->string (modulo x 10))))
                    `((bindsym ,(string-append "$mod+" n)
                       workspace number ,x)
                      (bindsym ,(string-append "$mod+Shift+" n)
                               move container to workspace number ,x))))
                (iota 10 1))
    `((bindsym $mod+tab workspace back_and_forth)))

   `(;; Scratchpad
     (bindsym $mod+Shift+minus move scratchpad)
     (bindsym $mod+minus scratchpad show)

     ;; Layout
     (bindsym $mod+Shift+b splith)
     (bindsym $mod+Shift+v splitv)
     (bindsym $mod+Shift+s layout stacking)
     (bindsym $mod+Shift+w layout tabbed)
     (bindsym $mod+Shift+e layout toggle split)

     (bindsym $mod+Shift+a focus parent)

     (bindsym $mod+Shift+r reload)
     (bindsym $mod+Control+Shift+q exit)


     (default_border pixel)
     (default_floating_border pixel)
     (gaps inner 8)

     (floating_modifier $mod normal))))

(define* (sway-input #:key
                     inputs
                     default-kb-repeat-delay
                     default-kb-repeat-rate
                     default-tp-tap?)
  (append
   `((focus_follows_mouse #f)
     (input type:keyboard
      ,(append
        (if default-kb-repeat-delay
            `((repeat_delay ,default-kb-repeat-delay)) '())
        (if default-kb-repeat-rate
            `((repeat_rate ,default-kb-repeat-rate)) '())))
     (input type:touchpad
            ((tap ,(if default-tp-tap?
                       'enabled 'disabled)))))

   (->> (or inputs '())
        (filter (lambda (input)
                  (assoc-ref input #:keyboard/layout)))
        (map (lambda (input)
               `(input ,(string-join (list (number->string
                                            (assoc-ref input #:keyboard/vendor-id))
                                           (number->string
                                            (assoc-ref input #:keyboard/product-id))
                                           (string-replace-substring
                                            (assoc-ref input #:keyboard/name) " " "_"))
                                     ":")
                 ,(let ((kb-layout (assoc-ref input #:keyboard/layout)))
                    (append
                     `((xkb_layout ,(keyboard-layout-name kb-layout)))
                     (if-let ((variant (keyboard-layout-variant kb-layout)))
                             `((xkb_variant ,variant)) '())
                     (if-let ((model (keyboard-layout-model kb-layout)))
                             `((xkb_model ,model)) '())
                     (let ((options (keyboard-layout-options kb-layout)))
                       (if (null? options)
                           '()
                           `((xkb_options ,(string-join
                                            (keyboard-layout-options kb-layout) ",")))))))))))))

(define* (sway-output #:key
                      outputs
                      default-bg-image)
  (append
   `((output *
      ,(if default-bg-image
           `((bg ,default-bg-image fill)) '())))
   ;; (map (lambda (output)
   ;;        `((output ,(as-> (list (assoc-ref output #:display/make)
   ;;                               (assoc-ref output #:display/model)) $
   ;;                         (string-join $)
   ;;                         (quoted $)))
   ;;          ;; ...
   ;;          ))
   ;;      (or outputs '()))
   ))

(define* (sway-external-programs #:key
                                 sway
                                 idle-manager
                                 screen-locker
                                 notifier
                                 power-monitor
                                 app-launcher
                                 terminal
                                 backup-terminal
                                 emacs-interface)
  (let ((executions
         (map (lambda (cmd)
                (cons "exec" cmd))
              (list idle-manager
                    (assoc-ref notifier #:notifier/daemon)
                    power-monitor)))

        (bindings
         (append
          `((bindsym $mod+Return exec ,@terminal)
            ,@(if backup-terminal
                  `((bindsym $mod+Control+Shift+Return exec ,@backup-terminal))
                  '())
            (bindsym $mod+Shift+d exec ,@app-launcher)
            (bindsym $mod+Control+Shift+l exec ,@screen-locker)
            (bindsym $mod+Shift+y exec ,@(assoc-ref emacs-interface #:emacs/program))
            (bindsym $mod+y exec ,@(assoc-ref emacs-interface #:emacs/new-frame)))

          (sway-notifications notifier)
          (sway-screenshot #:sway sway)
          (sway-backlights)
          (sway-volume)
          (sway-player))))

    (append executions bindings)))

(define (sway-notifications notifier)
  `((bindsym $mod+m exec ,@(assoc-ref notifier #:notifier/dismiss))
    (bindsym $mod+Shift+m exec ,@(assoc-ref notifier #:notifier/restore))
    (bindsym $mod+Control+m exec ,@(assoc-ref notifier #:notifier/menu))))

(define (sway-backlights)
  (let ((backlights (brightnessctl-backlights)))
    (bindsym-controls
     `(("XF86MonBrightnessUp" . ,(assoc-ref backlights #:backlight/up))
       ("XF86MonBrightnessDown" . ,(assoc-ref backlights #:backlight/down))))))

(define (sway-volume)
  (let ((volume (pipewire-volume))) ; TODO This should be dependent on whether media is pulseaudio or pipewire.
    (bindsym-controls
     `(("XF86AudioRaiseVolume" . ,(cmd-chain (assoc-ref volume #:volume/up)))
       ("XF86AudioLowerVolume" . ,(cmd-chain (assoc-ref volume #:volume/down)))
       ("XF86AudioMute" . ,(assoc-ref volume #:volume/mute-unmute))
       ("XF86AudioMicMute" . ,(assoc-ref volume #:volume/mic-mute-unmute))))))

;; TODO I don't know how I want to handle "multi-commands" yet, just do this for the ones I know that are for now. However at least in this case I do like the idea of doing "&&" between them rather than, say, comma-separating them like Sway allows because in the general case if one of the commands fails it's probably best not to run any that would come after.
(define (cmd-chain cs)
  (->> cs
       (interpose (list "&&"))
       (apply append)))

(define (sway-player)
  (let ((player (playerctl-player)))
    (bindsym-controls
     `(("XF86AudioPlay" . ,(assoc-ref player #:player/play-pause))
       ("XF86AudioPrev" . ,(assoc-ref player #:player/previous))
       ("XF86AudioNext" . ,(assoc-ref player #:player/next))))))

(define* (sway-screenshot #:key sway)
  `((bindsym $mod+grave exec ,(shot-output #:sway sway))
    (bindsym $mod+Control+grave exec ,swappy-clipboard)
    (bindsym $mod+Shift+grave exec ,(shot-window-or-selection #:sway sway))))


(define (bindsym-controls bs)
  (map (match-lambda
         ((key . cmd)
          (append
           (list "bindsym" "--locked" key "exec")
           cmd)))
       bs))

(define* (subject-output #:key sway)
  #~(format #f "~a -t get_outputs | ~a -r '.[] | select(.focused) | .name'"
            #$(file-append sway "/bin/swaymsg")
            #$(file-append jq "/bin/jq")))

(define* (subject-window-or-selection #:key sway)
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

(define* (shot-output #:key sway)
  (shot-script "output" #:output (subject-output #:sway sway)))

(define* (shot-window-or-selection #:key sway)
  (shot-script "window-or-selection" #:geom (subject-window-or-selection #:sway sway)))

(define swappy-clipboard
  (program-file
   "sway-swappy-clipboard"
   #~(system
      (format #f "~a | ~a -f -"
              #$(file-append wl-clipboard "/bin/wl-paste")
              #$(file-append swappy "/bin/swappy")))))

(define* (sway-app-windows #:key (props (list emacs-modal-window-props
                                              ;; ...
                                              )))
  (map (lambda (p)
         (let ((commands (append (list (if (assoc-ref p #:window-prop/floating?)
                                           '((floating enable)) '())
                                       ;; ...
                                       ))))
           (if (not (null? commands))
               (append
                ;; Andrew Tropin says there's a bug in Sway that could affect this (https://github.com/swaywm/sway/issues/6950). I haven't experienced it but it's something to consider if I have issues.
                `(for_window ,(as-> (append
                                     (if-let ((title (assoc-ref p #:window-prop.criteria/title)))
                                             `(("title" . ,title)) '())
                                     ;; ...
                                     ) $
                                (map (match-lambda
                                       ((k . v)
                                        (string-append k "=" (quoted v))))
                                     $)
                                (string-join $)
                                (string-append "[" $ "]")))
                commands)
               '())))
       props))

(define* (hyprlock-wm-piece #:key
                            colors
                            font
                            font-size
                            time-format
                            date-format
                            image
                            extra-config)
  (list
   (simple-service
    'hyprlock-add-packages
    home-profile-service-type
    (list hyprlock))

   (simple-service
    'hyprlock-conf
    home-xdg-configuration-files-service-type
    `(("hypr/hyprlock.conf"
       ,(hyprlang-file
         "hyprlock-conf"
         (let ((font (assoc-ref font #:font/name)))
           (append
            `((#:grace 5)
              (#:background ,(append
                              ;; TODO blur is enabled by default, yes?
                              ;; Any additional blur settings
                              (if image
                                  `((#:path ,image))
                                  '())))
              ;; Widgets
              (#:label ((#:font_family ,font)
                        (#:font_size ,(* font-size 3))
                        ;; TODO remove halign/valign everywhere if they're the default / unnecessary
                        (#:halign "center")
                        (#:valign "center")
                        (#:position 0 300)
                        ;; (#:color ,(hex->rgb (assoc-ref colors #:color/violet)))
                        (#:text ,(string-append "cmd[update:1000] echo \"$(date +\"" time-format "\")\""))))
              (#:label ((#:font_family ,font)
                        (#:font_size ,(* font-size 2))
                        (#:halign "center")
                        (#:valign "center")
                        (#:position 0 100)
                        ;; (#:color ,(hex->rgb (assoc-ref colors #:color/violet)))
                        (#:text ,(string-append "cmd[update:1000] echo \"$(date +\"" date-format "\")\""))))
              (#:input-field ((#:font_family ,font)
                              (#:font_size ,font-size)
                              (#:halign "center")
                              (#:valign "center")
                              (#:position 0 -100)
                              ;; (#:text_color ,(hex->rgb (assoc-ref colors #:color/fg)))
                              ;; (#:inner_color ,(string-append (hex->rgb (assoc-ref colors #:color/bg)) ", 0.5"))
                              ;; (#:check_color ,(hex->rgb (assoc-ref colors #:color/blue)))
                              ;; (#:fail_color ,(hex->rgb (assoc-ref colors #:color/red)))
                              )))
            (or extra-config '())))))))))

(define* (hyprlock-screen-locker)
  (let ((hyprlock-bin (file-append hyprlock "/bin/hyprlock")))
    `((#:screen-locker/lock . ,(list hyprlock-bin))
      (#:screen-locker/lock-immediately . ,(list hyprlock-bin
                                                 "--immediate")))))

(define* (swaylock-wm-piece #:key
                            colors
                            font
                            time-format
                            date-format
                            image
                            effects?
                            extra-config
                            swaylock
                            swaylock-effects)

  (list
   (simple-service
    'swaylock-add-packages
    home-profile-service-type
    (list (get-swaylock #:effects? effects?
                        #:swaylock swaylock
                        #:swaylock-effects swaylock-effects)))

   (simple-service
    'swaylock-config
    home-xdg-configuration-files-service-type
    `(("swaylock/config"
       ,(apply
         mixed-text-file
         "swaylock-config"
         (serialize-swaylock-config
          (let ((clr (lambda* (color #:optional alpha)
                       (as-> color $
                             (assoc-ref colors $)
                             (string-drop $ 1)
                             (string-append $ (or alpha "")))))
                (alpha "7f")) ; 50%
            (append
             `((daemonize)

               (font . ,(assoc-ref font #:font/name))

               (indicator-idle-visible)
               (indicator-caps-lock)
               (indicator-radius . "150")
               (indicator-thickness . "10")

               (key-hl-color . ,(clr #:color/green))
               (bs-hl-color . ,(clr #:color/yellow))
               (inside-color . ,(clr #:color/bg alpha))

               (ring-color . ,(clr #:color/bg))
               (ring-clear-color . ,(clr #:color/bg alpha))
               (ring-ver-color . ,(clr #:color/bg alpha))
               (ring-wrong-color . ,(clr #:color/bg alpha))

               (text-clear-color . ,(clr #:color/orange))
               (text-ver-color . ,(clr #:color/blue))
               (text-wrong-color . ,(clr #:color/red))
               (inside-clear-color . ,(clr #:color/orange))
               (inside-ver-color . ,(clr #:color/blue))
               (inside-wrong-color . ,(clr #:color/red))

               (text-color . ,(clr #:color/violet))
               (separator-color . "00000000")
               (line-uses-ring))

             (if image
                 (list #~(string-append "image=" #$image)) ; HACK `serialize-swaylock-config' doesn't allow gexp terms (it probably should). Ideally this line would be `(image . ,image)'. Maybe upstream a patch.
                 '())

             (if effects?
                 `((indicator)

                   (effect-blur . 7x5)

                   (clock)
                   (timestr . ,time-format)
                   (datestr . ,date-format))
                 '())

             (or extra-config '()))))))))))

(define* (swaylock-screen-locker #:key
                                 effects?
                                 swaylock
                                 swaylock-effects)
  (let ((swaylock-bin (file-append (get-swaylock #:effects? effects?
                                                 #:swaylock swaylock
                                                 #:swaylock-effects swaylock-effects)
                                   "/bin/swaylock")))
    `((#:screen-locker/lock . ,(append
                                (list swaylock-bin)
                                (if effects?
                                    (let ((grace-period "5"))
                                      (list (string-append "--grace=" grace-period)
                                            (string-append "--fade-in=" grace-period))) ; This is "pretty much broken" (I think just in terms of not being very pretty), see https://github.com/jirutka/swaylock-effects/commit/89daa14763.
                                    '())))
      (#:screen-locker/lock-immediately . ,(list swaylock-bin)))))

(define* (get-swaylock #:key
                       effects?
                       swaylock
                       swaylock-effects)
  (if effects?
      swaylock-effects
      swaylock))

(define* (hypridle-wm-piece #:key
                            session
                            screen-locker
                            (lock-timeout 600)
                            extra-config)
  (list
   (simple-service
    'hypridle-add-packages
    home-profile-service-type
    (list hypridle))

   (simple-service
    'hypridle-conf
    home-xdg-configuration-files-service-type
    `(("hypr/hypridle.conf"
       ,(hyprlang-file
         "hypridle-conf"
         (append
          `((#:listener ((#:timeout ,lock-timeout)
                         (#:on-timeout "echo 'Timed out'")
                         (#:on-resume "echo 'Welcome back'"))))
          (or extra-config '()))))))))

(define (hypridle-idle-manager)
  (list
   (file-append hypridle "/bin/hypridle")))

(define* (swayidle-wm-piece #:key
                            session
                            sway
                            screen-locker
                            (lock-timeout 600)
                            extra-config
                            swayidle)
  (list
   (simple-service
    'swayidle-add-packages
    home-profile-service-type
    (list swayidle))

   (simple-service
    'swayidle-config
    home-xdg-configuration-files-service-type
    `(("swayidle/config"
       ,(apply
         mixed-text-file
         "swayidle-config"
         (serialize-sway-config
          (append
           (if screen-locker
               (let ((lock-cmd-quoted #~(format #f "'~a'" (string-join '#$screen-locker))))
                 `(;(lock ,lock-cmd-quoted)
                                        ;(before-sleep ,lock-cmd-quoted)
                   (timeout ,lock-timeout ,lock-cmd-quoted)))
               '())
           (or extra-config '())))))))

   ;; (case session
   ;;   ((#:session/sway)
   ;;    (let* ((swaymsg (file-append sway "/bin/swaymsg"))
   ;;           (swaymsg-cmd (lambda (cmd)
   ;;                          #~(format #f "'~a \"~a\"'" #$swaymsg #$cmd)))
   ;;           (idle-timeout (+ lock-timeout 300)))
   ;;      (simple-service
   ;;       'swayidle-add-sway-power
   ;;       home-swayidle-service-type
   ;;       `((timeout ,idle-timeout ,(swaymsg-cmd "output * power off") resume ,(swaymsg-cmd "output * power on")))))))
   ))

(define* (swayidle-idle-manager #:key swayidle)
  (list
   (file-append swayidle "/bin/swayidle")
   "-w"))

(define* (mako-wm-piece #:key
                        colors
                        font
                        font-size)
  (list
   (simple-service
    'mako-add-packages
    home-profile-service-type
    (list mako))

   (simple-service
    'mako-config
    home-xdg-configuration-files-service-type
    `(("mako/config"
       ,(ini-file
         "mako-config"
         (let ((clr (lambda* (color #:optional alpha)
                      (as-> color $
                            (assoc-ref colors $)
                            (string-append $ (or alpha ""))
                            (string->symbol $)))))
           `((global ((font . ,(string->symbol (pango-font-description font font-size)))
                      (max-icon-size . 32)
                      (text-color . ,(clr #:color/fg))
                      (background-color . ,(clr #:color/bg "e6")) ; 90% alpha
                      (border-color . ,(clr #:color/accent))
                      (border-size . 2)
                      (border-radius . 8)))
             (urgency=low ((border-color . ,(clr #:color/green))))
             (urgency=high ((border-color . ,(clr #:color/red))))))
         #:ini-serialize
         (lambda (config)
           (ini-serialize config #:equal-string "="))))))))

(define (mako-notifier menu)
  (let ((makoctl (file-append mako "/bin/makoctl")))
    `((#:notifier/daemon . ,(list (file-append mako "/bin/mako")))
      (#:notifier/dismiss . ,(list makoctl
                                   "dismiss"))
      (#:notifier/restore . ,(list makoctl
                                   "restore"))
      (#:notifier/menu . ,(append
                           (list makoctl "menu")
                           menu
                           (list (quoted "Notification action")))))))

(define* (poweralertd-wm-piece)
  (list
   (simple-service
    'poweralertd-add-packages
    home-profile-service-type
    (list poweralertd))))

(define* (poweralertd-monitor #:key (ignore-types '("line power")))
  (append
   (list
    (file-append poweralertd "/bin/poweralertd")
    "-s")
   (append-map (lambda (t)
                 `("-i" ,(if (string-contains t " ")
                             (quoted t) t)))
               (or ignore-types '()))))

;; (define* (kanshi-wm-piece #:key
;;                           outputs
;;                           extra-config)
;;   (list
;;    (service
;;     home-kanshi-service-type
;;     (home-kanshi-configuration
;;      (config
;;       ;; Not done yet.
;;       (or extra-config '()))))))

(define* (brightnessctl-backlights #:key (step 10))
  (let ((cmd (lambda (op)
               (list (file-append brightnessctl "/bin/brightnessctl")
                     "--quiet" ; There's a big status report after every invocation that pollutes the logs.
                     "set"
                     (step-pct step op #:suffix)))))
    `((#:backlight/up . ,(cmd #:up))
      (#:backlight/down . ,(cmd #:down)))))

(define* (pulseaudio-volume #:key (step 5))
  (let* ((pactl (file-append pulseaudio "/bin/pactl"))
         (sink "@DEFAULT_SINK@")
         (source "@DEFAULT_SOURCE@")
         (unmute (list pactl "set-sink-mute" sink "false")))
    `((#:volume/up . ,(list unmute
                            (list pactl "set-sink-volume" sink (step-pct step #:up #:prefix))))
      (#:volume/down . ,(list unmute
                              (list pactl "set-sink-volume" sink (step-pct step #:down #:prefix))))
      (#:volume/mute-unmute . ,(list pactl "set-sink-mute" sink "toggle"))
      (#:volume/mic-mute-unmute . ,(list pactl "set-source-mute" source "toggle")))))

(define* (pipewire-volume #:key (step 5))
  (let* ((wpctl (file-append wireplumber "/bin/wpctl"))
         (sink "@DEFAULT_AUDIO_SINK@")
         (source "@DEFAULT_AUDIO_SOURCE@")
         (unmute (list wpctl "set-mute" sink "0")))
    `((#:volume/up . ,(list unmute
                            (list wpctl "set-volume" "--limit" "1" sink (step-pct step #:up #:suffix))))
      (#:volume/down . ,(list unmute
                              (list wpctl "set-volume" sink (step-pct step #:down #:suffix))))
      (#:volume/mute-unmute . ,(list wpctl "set-mute" sink "toggle"))
      (#:volume/mic-mute-unmute . ,(list wpctl "set-mute" source "toggle")))))

(define* (playerctl-player)
  (let ((cmd (lambda (action)
               (list (file-append playerctl "/bin/playerctl")
                     action))))
    `((#:player/play-pause . ,(cmd "play-pause"))
      (#:player/previous . ,(cmd "previous"))
      (#:player/next . ,(cmd "next")))))

(define (step-pct n op fix)
  (let ((percent (string-append (number->string n) "%"))
        (sign (case op
                ((#:up) "+")
                ((#:down) "-"))))
    (case fix
      ((#:prefix) (string-append sign percent))
      ((#:suffix) (string-append percent sign)))))

(define* (session-start exe
                        session-cmd
                        #:key
                        log-file
                        session-args
                        pre-start)
  (package
    (inherit simple-package)
    (name (string-append "session-" exe))
    (source (apply mixed-text-file exe
                   (append
                    (or (list pre-start) '())
                    (list
                     #~(string-join
                        (list
                         "exec" #$session-cmd #$@(or session-args '()) "$@"
                         (string-join
                          (list ">>"
                                (string-append
                                 "$" #$(assoc-ref log-home #:xdg-base) "/" #$(assoc-ref log-home #:path) "/" #$log-file)))
                         "2>&1"))))))
    (arguments
     `(#:builder
       ,#~(begin
            (let* ((bin (string-append #$output "/bin"))
                   (f (string-append bin "/" #$exe)))
              (mkdir #$output)
              (mkdir bin)
              (copy-file #$source f)
              (chmod f #o555)))))))

(define (xdg-current-desktop-env-var val)
  `("XDG_CURRENT_DESKTOP" . ,val)) ; "Normally" provided by a login manager.

(define xdg-session-type-wayland-env-var
  '("XDG_SESSION_TYPE" . "wayland")) ; "Normally" provided by a login manager.

;; I think these should really be set in packages / package program wrappers but this is easier and Andrew Tropin does it this way so I'll leave it as-is for now.
(define (wayland-env-vars pipewire?)
  ;; Copied from RDE: I don't know what all of these do. For some explanation see https://github.com/swaywm/sway/wiki/Running-programs-natively-under-Wayland and https://wiki.archlinux.org/title/Wayland#GUI_libraries.
  (append
   '(("SDL_VIDEODRIVER" . "wayland")
     ("CLUTTER_BACKEND" . "wayland")
     ("ELM_ENGINE" . "wayland_egl")
     ("ECORE_EVAS_ENGINE" . "wayland-egl")
     ("QT_QPA_PLATFORM" . "wayland-egl") ; Might not be necessary since I set `XDG_SESSION_TYPE=wayland', see https://github.com/swaywm/sway/wiki/Running-programs-natively-under-Wayland#qt5
     ("_JAVA_AWT_WM_NONREPARENTING" . "1"))
   (if pipewire?
       '(("RTC_USE_PIPEWIRE" . "true")) '()))) ; No idea what this is for. I thought it was equivalent to toggling on chrome://flags/#enable-webrtc-pipewire-capturer but it's not. The `rtc_use_pipewire=true' in the ungoogled-chromium configure flags is a separate issue too (it only builds Chromium with support for pipewire, it still has to be turned on in the settings or with a flag). [2024-07-14 Sun] update: possibly relevant?: https://github.com/emersion/xdg-desktop-portal-wlr/wiki/Screencast-Compatibility#webrtc-aka-firefoxchromium

(define* (flatpak-apps #:key
                       stateless?
                       portal-services)
  (append
   (list
    (simple-service
     'flatpak-add-packages
     home-profile-service-type
     (list flatpak))

    (simple-service
     'flatpak-exports
     home-environment-variables-service-type
     `(("XDG_DATA_DIRS" . "$XDG_DATA_DIRS${XDG_DATA_DIRS:+:}$XDG_DATA_HOME/flatpak/exports/share")))

    (when stateless?
      (home-stateless-service
       'flatpak
       #:state '(((#:path . ("flatpak/" #:xdg-base "XDG_DATA_HOME"))
                  (#:storage . #:storage/machine)
                  (#:mode . #o700))))))

   (or portal-services '())))

(define (portal-services/gtk)
  (list
   (simple-service
    'gtk-portal-add-packages
    home-profile-service-type
    (list xdg-desktop-portal
          xdg-desktop-portal-gtk))))

(define (portal-services/wlr)
  (list
   (simple-service
    'wlr-portal-add-packages
    home-profile-service-type
    (list xdg-desktop-portal
          xdg-desktop-portal-wlr))

   (simple-service
    'wlr-portal-config
    home-multi-xdg-configuration-files-service-type
    `(("xdg-desktop-portal-wlr/config"
       ,(ini-file
         "xdg-desktop-portal-wlr-config"
         `((screencast ((chooser_cmd . ,#~(string-append #$(file-append slurp "/bin/slurp")
                                                         " -f %o -or -c ff0000"))
                        (chooser_type . simple))))
         #:ini-serialize
         (lambda (config)
           (ini-serialize config #:equal-string "=")))))))) ; Donno if this is necessary (the examples don't have spaces around the equals), let's be safe.

(define (portal-services/hyprland)
  (list
   (simple-service
    'hyprland-portal-add-packages
    home-profile-service-type
    (list xdg-desktop-portal
          xdg-desktop-portal-hyprland/simple))

   ;; Example. We're not configuring anthing at the moment (also there's not really anthing to configure, 'max_fps' is the only option at time of writing).
   ;; (simple-service
   ;;  'hyprland-portal-config
   ;;  home-multi-xdg-configuration-files-service-type
   ;;  `(("xdg-desktop-portal-hyprland/config"
   ;;     ,(hyprlang-file
   ;;       "xdg-desktop-portal-hyprland-config"
   ;;       `((#:screencopy ((#:max_fps 120))))))))
   ))

(define* (portal-config-services #:key
                                 xdg-current-desktop
                                 preferred)
  (list
   (simple-service
    (symbol-append (string->symbol xdg-current-desktop)
                   '-portals-config)
    home-multi-xdg-configuration-files-service-type
    `((,(string-append "xdg-desktop-portal/" xdg-current-desktop "-portals.conf")
       ,(ini-file
         (string-append "xdg-desktop-portal-" xdg-current-desktop "-portals")
         `((preferred ((default . ,(string-join preferred ";")))))
         #:ini-serialize
         (lambda (config)
           (ini-serialize config #:equal-string "=")))))))) ; Donno if this is necessary (the examples don't have spaces around the equals), let's be safe.

(define* (pango-font-description font #:optional size)
  (string-join
   (append
    (list
     (assoc-ref font #:font/name))
    (if size
        (list (number->string size)) '()))))

(define (virtualization)
  (list
   (simple-service
    'virtualization-add-packages
    home-profile-service-type
    (list ;; qemu ; QEMU is kind of a fatty, maybe don't keep it installed all the time.
          ))))

(define* (doom-calendar-prog #:key
                             stateless?
                             org?)
  (append
   (list
    (doom-service
     'calendar
     #:modules '((#:app
                  calendar))))

   (if org?
       (doom-org-gcal #:stateless? stateless?) '())))

(define* (doom-org-gcal #:key stateless?)
  (append
   (let ((name 'org-gcal))
     (list
      (doom-service
       name
       #:modules '((#:app
                    calendar))
       #:config
       '((after! org-gcal
                 (setq org-gcal-client-id "446729771716-pp79934q99aro2h8v3iki1fejcodbdoo.apps.googleusercontent.com"
                       org-gcal-client-secret (-> (auth-source-search :host org-gcal-client-id) car (plist-get :secret) funcall)
                       org-gcal-recurring-events-mode 'nested))))

      (when stateless?
        (doom-stateless-service
         name
         #:state '(((#:path . ("org-gcal/token.gpg" #:doom-base #:cache))
                    (#:storage . #:storage/machine))
                   ((#:path . ("persist/org-gcal--sync-tokens" #:doom-base #:data))
                    (#:storage . #:storage/machine)))))))

   (doom-dash)))

;; TODO
;; - This comonent no longers depens on doom-user so don't rely on user-mail-address?
;; - nb: also it uses `org-directory'
;; '((after! org-gcal
;;           (setq ;; TODO `elisp-serialize' doesn't like the unquotes.
;;                 ;; org-gcal-fetch-file-alist (let ((cal-dir concat org-directory "/cal"))
;;                 ;;                             `((user-mail-address . ,(concat cal-dir "/" user-mail-address ".org"))
;;                 ;;                               ("addressbook%23contacts@group.v.calendar.google.com" . ,(concat cal-dir "/contacts.org"))
;;                 ;;                               ("en.usa%23holiday@group.v.calendar.google.com" . ,(concat cal-dir "/holidays.org"))))
;;                 )))

(define (aws-prog)
  (list
   (simple-service
    'aws-add-packages
    home-profile-service-type
    (list awscli))))

(define* (clojure-prog #:key
                       stateless?
                       doom?
                       doom-tree-sitter?
                       (jdk (list openjdk-lts "jdk")))
  (append
   (list
    (simple-service
     'clojure-add-packages
     home-profile-service-type
     (list clojure-tools
           jdk
           (@ (nongnu packages clojure) clj-kondo))) ; TODO Update with respect to nonphrenetic.

    (when stateless?
      (home-stateless-service
       'clojure
       #:state '(((#:path . ".m2/")
                  (#:storage . #:storage/machine))))))

   (if doom?
       (doom-clojure #:stateless? stateless?
                     #:tree-sitter? doom-tree-sitter?)
       '())))

(define* (doom-clojure #:key
                       stateless?
                       tree-sitter?)
  (append
   (let ((name 'clojure))
     (append
      (doom-ts-lang
       name
       #:tree-sitter? tree-sitter?
       #:config
       `((use-package! clojure-mode
                       :defer t
                       :init
                       (setq clojure-refactor-map-prefix (kbd "s-M r")) ; Has to be set before clojure-mode loads. ; REVIEW temporary binding
                       :config
                       (set-ligatures! 'clojure-mode :lambda "fn"))

         (use-package! cider
                       :defer t
                       :init
                       (-each '((cider-preferred-build-tool symbolp)
                                (cider-default-cljs-repl symbolp)
                                (cider-shadow-default-options stringp)
                                (cider-offer-to-open-cljs-app-in-browser booleanp)
                                (cider-clojure-cli-global-options stringp)
                                (cider-clojure-cli-global-options booleanp))
                              (-lambda ((sym f))
                                       (put sym 'safe-local-variable f)))
                       :config
                       (setq cider-repl-history-size ,very-big-history))

         ;; REVIEW make sure this is still working after nesting in `after!'
         (after! cider-repl
                 (add-hook! 'cider-repl-mode-hook
                            #'goto-address-prog-mode
                            #'highlight-numbers-mode
                            #'rainbow-delimiters-mode
                            #'yas-minor-mode-on
                            #'biome-sp-strict-h))

         (after! clj-refactor
                 (cljr-add-keybindings-with-prefix "s-M R")))) ; REVIEW temporary binding

      (if stateless?
          (list
           (doom-stateless-service
            'clojure
            #:state '(((#:path . ("cider-repl-history" #:doom-base #:cache))
                       (#:storage . #:storage/persist)))))
          '())))

   (doom-xml+csv)
   (doom-dash)))

(define* (node-prog #:key
                    stateless?
                    doom?
                    doom-tree-sitter?)
  (append
   (list
    (simple-service
     'node-add-packages
     home-profile-service-type
     (list node-lts))

    ;; (when stateless?
    ;;   (home-stateless-service
    ;;    'node
    ;;    #:state '(((#:path . ".npm/")
    ;;               (#:storage . #:storage/machine)))))
    )

   (if doom?
       (doom-javascript #:tree-sitter? doom-tree-sitter?) '())))

(define* (doom-org-prog #:key
                        (org-dir "org")
                        stateless?
                        evil?)
  (append
   (doom-org #:stateless? stateless?
             #:evil? evil?
             #:org-dir org-dir)

   (list
    (when stateless?
      (home-stateless-service
       'org
       #:state `(((#:path . ,(string-append org-dir "/"))
                  (#:storage . #:storage/persist))))))))

(define* (doom-org #:key
                   stateless?
                   evil?
                   (pretty? #t)
                   org-dir)
  (append
   (let ((name 'org))
     (list
      (doom-service
       name
       #:modules '((#:lang
                    (org +journal)))
       #:packages
       (if pretty?
           '((package! org-modern :pin "f619912e55b409a8a3ecb807a8c2a35faa0e482d")
             (package! org-modern-indent
                       :recipe (:host github
                                :repo "jdtsmith/org-modern-indent")
                       :pin "f2b859bc53107b2a1027b76dbf4aaebf14c03433")
             (package! org-appear :pin "32ee50f8fdfa449bbc235617549c1bccb503cb09"))
           '())

       #:config
       (append
        `((use-package! org
                        :defer t
                        :init
                        (setq org-directory ,(string-append "~/" org-dir))
                        :config
                        (setq org-log-done 'time
                              org-priority-lowest ?E
                              org-priority-default ?C
                              org-priority-faces
                              (-map (-lambda ((priority . color))
                                             (cons priority (doom-color color)))
                                    '((?A . red)
                                      (?B . orange)
                                      (?C . yellow)
                                      (?D . magenta)
                                      (?E . cyan)))))

          (after! ob
                  (setq org-babel-noweb-error-all-langs t)))

        (if evil?
            '((after! evil-org
                      (map! :map evil-org-mode-map
                            :nv "C-j" #'outline-forward-same-level
                            :nv "C-k" #'outline-backward-same-level)))
            '())

        (if pretty?
            '((after! org
                      (setq org-hide-emphasis-markers t
                            org-pretty-entities t
                            ;; org-ellipsis "…"
                            ))

              (use-package! org-appear
                            :hook (org-mode . org-appear-mode))

              (use-package! org-modern
                            :hook
                            (org-mode . org-modern-mode)
                            (org-agenda-finalize . org-modern-agenda)
                            :config
                            (setq org-modern-fold-stars '(("▶" . "▼") ("▷" . "▽") ("▹" . "▿") ("▸" . "▾")) ; HACK: remove a symbol pair that renders with a different line hight (at least in Fira Code).
                                  org-modern-hide-stars nil ; You still can't see them because `org-hide-leading-stars' is true but this way the indentation is retained (otherwise there's some extra and the amount grows each level down).
                                  org-modern-todo-faces
                                  (-map (-lambda ((kw . face))
                                                 `(,kw :inverse-video t :inherit ,face))
                                        org-todo-keyword-faces)
                                  org-modern-priority
                                  '((?A . "🔥") ; ❢
                                    (?B . "⚠") ; ⚑
                                    (?C . "●")
                                    (?D . "◆")
                                    (?E . "■"))
                                  org-modern-priority-faces
                                  (-map (-lambda ((priority . color))
                                                 `(,priority :foreground ,color))
                                        org-priority-faces))
                            (when (not line-spacing)
                              (setq org-modern-label-border 0.3))) ; Chosen arbitrarily. Looks good.

              (use-package! org-modern-indent
                            :hook (org-mode . org-modern-indent-mode)
                            :config
                            ;; Square brackets instead of rounded.
                            (setq org-modern-indent-begin (propertize "┌" 'face 'org-modern-indent-bracket-line)
                                  org-modern-indent-end (propertize "└" 'face 'org-modern-indent-bracket-line))
                            ;; TODO These should maybe be in my theme(s).
                            (custom-set-faces! '(org-modern-indent-bracket-line :inherit org-block-begin-line)
                                               '(org-block :background unspecified)))) ; Get rid of any special org block background, we have the block bracket to delineate them. The Doom themes base in particular sets a background.
            '())))

      (when stateless?
        (doom-stateless-service
         name
         #:state '(((#:path . ("org-clock-save.el" #:doom-base #:data))
                    (#:storage . #:storage/persist))
                   ((#:path . ("org/persist/" #:doom-base #:cache))
                    (#:storage . #:storage/machine)))))))

   (doom-dash)))

(define* (doom-dired-prog #:key
                          stateless?
                          icons?
                          (dirvish? #t)
                          desktop?
                          doom-ui?
                          emacs-handler)
  (let ((name 'dired))
    (append
     (list
      (doom-service
       name
       #:modules `((#:emacs
                    (dired ,@(if icons? '(+icons) '())
                           ,@(if dirvish? '(+dirvish) '())))))

      (when (and stateless? dirvish?)
        (doom-stateless-service
         name
         #:state '(((#:path . ("dirvish/" #:doom-base #:cache))
                    (#:storage . #:storage/machine))))))

     (if dirvish?
         (doom-transient) '())

     (if (and desktop? doom-ui?)
         (list
          (emacs-xdg-service name
                             "file"
                             (emacs-handler
                              "dired"
                              '((dired (car args))))
                             #:default-for '(inode/directory)))
         '()))))

(define* (direnv-prog #:key
                      doom?
                      shells)
  (append
   (list
    (simple-service
     'direnv-add-packages
     home-profile-service-type
     (list direnv))

    (simple-service
     'direnv-rc
     home-xdg-configuration-files-service-type
     `(("direnv/direnvrc" ,(local-file "direnvrc")))))

   (map (lambda (shell)
          (let ((direnv-hook
                 (lambda (shell-name)
                   (mixed-text-file
                    "direnv-hook"
                    (let ((direnv-bin (file-append direnv "/bin/direnv")))
                      #~(let ((quoted-subshell (lambda (command)
                                                 (string-append "\"$(" (string-join command) ")\""))))
                          (string-join
                           (list
                            "command" "-v" #$direnv-bin "> /dev/null"
                            "&&"
                            "eval" (quoted-subshell (list
                                                     #$direnv-bin "hook" #$shell-name))))))))))
            (case shell
              ((#:shell/bash) (simple-service
                               'direnv-bash-hook
                               home-bash-service-type
                               (home-bash-extension
                                (bashrc
                                 (list (direnv-hook "bash"))))))
              ((#:shell/zsh) (simple-service
                              'direnv-zsh-hook
                              home-zsh-service-type
                              (home-zsh-extension
                               (zshrc
                                (list (direnv-hook "zsh")))))))))
        shells)

   (list
    (when doom?
      (doom-service
       'direnv
       #:modules '((#:tools
                    direnv)))))))

(define direnv-whitelist-service-type
  (service-type
   (name 'direnv-whitelist)
   (extensions
    (list (service-extension home-xdg-configuration-files-service-type
                             (lambda (whitelist-entries)
                               `(("direnv/direnv.toml"
                                  ,(toml-file
                                    "direnv-config"
                                    (if (null? whitelist-entries)
                                        '()
                                        `((whitelist ((exact . ,(serialize-toml-array whitelist-entries)))))))))))))
   (compose concatenate)
   (extend (lambda (_ whitelist-entries)
             whitelist-entries))
   (default-value #f)
   (description "Whitelist for Direnv.")))

(define (ssh-prog)
  (list
   (service home-openssh-service-type)))

;; (define (nano-prog)
;;   (list
;;    (simple-service
;;     'nano-add-packages
;;     home-profile-service-type
;;     (list nano))

;;    (simple-service
;;     'nano-add-config
;;     home-xdg-configuration-files-service-type
;;     `(("nano/nanorc"
;;        ,%default-nanorc)))))

(define (doom-trash-prog)
  (list
   (doom-service
    'trash
    #:packages
    '((package! trashed :pin "52a52a363ce53855790e7a59aed6976eec18c9ea")))))

(define* (git-prog #:key
                   stateless?
                   doom?
                   email
                   sign-commits? ; TODO make this (sign-commits? #t) once I have my gpg stuff set up and I'm passing a gpg-sign-key.
                   gpg-sign-key
                   ;; git-send-email?
                   extra-config)
  (append
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
       (append
        `((user
           ((email . ,email)
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
          )
        (or extra-config '()))))))

   (if doom?
       (doom-git #:stateless? stateless?) '())))

(define* (doom-git #:key stateless?)
  (append
   (doom-version-control)

   (let ((name 'git))
     (list
      (doom-service
       name
       #:modules '((#:tools
                    (magit +forge)))
       #:config
       '((after! magit
                 (setq git-commit-style-convention-checks '(non-empty-second-line))
                 (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))))

      (when stateless?
        (doom-stateless-service
         name
         #:state '(((#:path . ("forge/forge-database.sqlite" #:doom-base #:data))
                    ;; TODO Not sure this should necessarily be :persist and not :machine, can't be bothered to figure it out right now so let's be safe.
                    (#:storage . #:storage/persist)))))))

   (doom-transient #:stateless? stateless?)))

(define (doom-version-control)
  (list
   (doom-service
    'version-control
    #:modules '((#:emacs
                 vc)
                (#:ui
                 (vc-gutter +pretty)))
    #:config
    '((use-package! diff-hl
                    :defer t
                    :init
                    (remove-hook! 'diff-hl-mode-hook #'diff-hl-flydiff-mode))))))

(define* (protonvpn-prog #:key stateless?)
  (list
   (simple-service
    'protonvpn-add-packages
    home-profile-service-type
    (list protonvpn-cli))

   (when stateless?
     (home-stateless-service
      'protonvpn
      #:state '(((#:path . ".pvpn-cli/")
                 (#:storage . #:storage/machine)))))))

;; (define (misc-progs)
;;   (list
;;    (simple-service
;;     'misc-progs-add-packages
;;     home-profile-service-type
;;     (list curl
;;           ;; lm-sensors
;;           unzip
;;           zip))))

(define* (pavucontrol-app #:key
                          stateless?
                          gtk-services)
  (append
   (list
    (simple-service
     'pavucontrol-add-packages
     home-profile-service-type
     (list pavucontrol))

    (when stateless?
      (home-stateless-service
       'pavucontrol
       #:ignore '(((#:path . ("pavucontrol.ini" #:xdg-base "XDG_CONFIG_HOME"))))))) ; Pavucontrol overwrites the current file (or a symlink) so we can't store this. Let's just ignore it.

   (or gtk-services '())))

(define* (font-manager-app #:key
                           stateless?
                           gtk-services)
  (append
   (list
    (simple-service
     'font-manager-add-packages
     home-profile-service-type
     (list fontmanager-no-googlefonts))

    (when stateless?
      (home-stateless-service
       'font-manager
       #:state '(((#:path . ("font-manager/" #:xdg-base "XDG_CACHE_HOME"))
                  (#:storage . #:storage/machine))
                 ((#:path . ("font-manager/" #:xdg-base "XDG_CONFIG_HOME"))
                  (#:storage . #:storage/persist))))))

   (or gtk-services '())))

;; Emacs already "has a menu" in the form of `completing-read', all we need is something to serve the role of application launcher.
(define (doom-menu-app)
  (list
   (doom-service
    'menu
    #:packages
    '((package! app-launcher
                :recipe (:host github
                         :repo "SebastienWae/app-launcher")
                :pin "d5015e394b0a666a8c7c4d4bdf786266e773b145")))))

(define (doom-menu emacs-handler)
  `((#:menu/menu . ,(list
                     (emacs-handler
                      "menu"
                      '((completing-read (concat (car args) ": ") (split-string input "\n") nil t))
                      #:modal? #t
                      #:modal-title "Menu"
                      #:minibuffer? #t
                      #:input? #t)))
    (#:menu/app-launcher . ,(list
                             (emacs-handler
                              "app-launcher"
                              '((app-launcher-run-app))
                              #:modal? #t
                              #:modal-title "App Launcher"
                              #:minibuffer? #t)))))

(define* (rofi-app #:key
                   stateless?
                   wayland?
                   colors
                   font
                   font-size)
  (list
   (simple-service
    'rofi-add-packages
    home-profile-service-type
    (list (get-rofi #:wayland? wayland?)))

   (simple-service
    'rofi-config
    home-xdg-configuration-files-service-type
    `(("rofi/config.rasi"
       ,(apply
         mixed-text-file
         "rofi-config.rasi"
         (serialize-rasi-config
          (append
           ;; Using Andrew Tropin's / RDE's confguration for now, revisit later.
           `((configuration
              ((modi . "run,ssh,drun")
               (drun-show-actions . #t)
               (show-icons . #t)
               (font . ,(pango-font-description font font-size))

               (kb-element-next . "")
               (kb-row-select . "Tab,Control+i")
               (kb-secondary-paste . "Control+y")
               (kb-remove-word-forward . "Alt+d")
               (kb-remove-word-back . "Control+w,Control+BackSpace")
               (kb-clear-line . "Control+slash")
               (kb-page-next . "Control+v")
               (kb-page-prev . "Alt+v"))))


           ;; Not done yet: configure Rofi to look pretty and use `colors' argument.
           ;; (list #~#$(string-append "@theme" " " (quoted "paper-float")))
           ;; (let ((clr (lambda* (color #:optional alpha)
           ;;              (as-> color $
           ;;                    (assoc-ref colors $)
           ;;                    (string-append $ (or alpha ""))))))
           ;;   `((*
           ;;      (;; (white . ,(clr #:color/fg))
           ;;       ;; (black . ,(clr #:color/bg))
           ;;       ;; (grey . ,(clr #:color/...))
           ;;       ;; (blue . ,(clr #:color/blue))
           ;;       (background . "#000000BF")))))
           ;; `((window
           ;;    ((transparency . "real"))))
           ))))))

   (when stateless?
     (home-stateless-service
      'rofi
      #:state '(((#:path . ("rofi3.druncache" #:xdg-base "XDG_CACHE_HOME"))
                 (#:storage . #:storage/machine)))))))

(define* (rofi-menu #:key wayland?)
  (let ((rofi-bin (file-append (get-rofi #:wayland? wayland?) "/bin/rofi")))
    `((#:menu/menu . ,(list rofi-bin "-dmenu" "-p"))
      (#:menu/app-launcher . ,(list rofi-bin "-show" "drun")))))

(define* (get-rofi #:key wayland?)
  (if wayland?
      rofi-wayland
      rofi))

(define* (gimp-app #:key
                   stateless?
                   gtk-services)
  (append
   (list
    (simple-service
     'gimp-add-packages
     home-profile-service-type
     (list gimp))

    (when stateless?
      (home-stateless-service
       'gimp
       #:state '(((#:path . ("gimp/" #:xdg-base "XDG_CACHE_HOME"))
                  (#:storage . #:storage/machine))
                 ((#:path . ("GIMP/" #:xdg-base "XDG_CONFIG_HOME"))
                  (#:storage . #:storage/machine))))))

   (or gtk-services '())))

(define* (alacritty-app #:key
                        colors
                        font
                        font-size)
  (list
   (simple-service
    'alacritty-add-packages
    home-profile-service-type
    (list alacritty))

   (simple-service
    'alacritty-config
    home-xdg-configuration-files-service-type
    `(("alacritty/alacritty.toml"
       ,(toml-file
         "alacritty-config.toml"
         (append
          `((window ((opacity . 0.75)))
            (window.padding ((x . 5)
                             (y . 5))))
          (if font
              `((font ((size . ,(- font-size 1))))
                (font.normal ,(append
                               `((family . ,(assoc-ref font #:font/name)))
                               (if-let ((weight (assoc-ref font #:font/weight)))
                                       `((style . ,(font-weight->alacritty-style weight))) '()))))
              '())
          (let ((clr (lambda (color)
                       (assoc-ref colors color))))
            `((colors.primary ((background . ,(clr #:color/bg))
                               (foreground . ,(clr #:color/fg))))
              (colors.normal ((black . ,(clr #:color/base0))
                              (white . ,(clr #:color/base8))
                              (red . ,(clr #:color/red))
                              (green . ,(clr #:color/green))
                              (yellow . ,(clr #:color/yellow))
                              (blue . ,(clr #:color/blue))
                              (magenta . ,(clr #:color/magenta))
                              (cyan . ,(clr #:color/cyan)))))))))))))

(define (font-weight->alacritty-style weight)
  (->> weight
       (string-delete #\-)
       string-downcase
       string-capitalize))

(define (alacritty-terminal)
  (list
   (file-append alacritty "/bin/alacritty")))

(define* (doom-vterm-app #:key setup-zsh?)
  (list
   (doom-service
    'vterm
    #:modules '((#:term
                 vterm)))

   (when setup-zsh?
     (simple-service
      'doom-vterm-zsh
      home-zsh-service-type
      (home-zsh-extension
       (zshrc
        (list (local-file "zsh-vterm"))))))))

(define (doom-vterm-terminal emacs-handler)
  (list
   (emacs-handler
    "vterm"
    '((+vterm/here nil)))))

(define* (ungoogled-chromium-app #:key
                                 stateless?
                                 toolkits
                                 wayland?
                                 pipewire?
                                 theme
                                 gtk-services
                                 qt-services
                                 (ungoogled-chromium ungoogled-chromium))
  (append
   (list
    (simple-service
     'ungoogled-chromium-add-packages
     home-profile-service-type
     ;; At the time of writing, ungoogled-chromium doesn't offer GTK 4 or QT 6 so just ignore the toolkits option.
     (list (chromium-wrapper ungoogled-chromium wayland? pipewire? theme)
           ublock-origin/chromium))

    (when stateless?
      (home-stateless-service
       'ungoogled-chromium
       #:state '(((#:path . ("chromium/" #:xdg-base "XDG_CACHE_HOME"))
                  (#:storage . #:storage/machine)
                  (#:mode . #o700))
                 ((#:path . ("chromium/" #:xdg-base "XDG_CONFIG_HOME"))
                  (#:storage . #:storage/machine)
                  (#:mode . #o700))
                 ((#:path . ".pki/nssdb/")
                  (#:storage . #:storage/machine)
                  (#:mode . #o700)
                  (#:parent-dir-perms . ((#:mode . #o700))))))))

   (or gtk-services '())
   (or qt-services '())))

(define* (chromium-wrapper package wayland? pipewire? theme
                           #:key gtk4?)
  ;; (let ((flags (append
  ;;               (if gtk4?
  ;;                   '((#:switches . '("--gtk-version=4"))) '())
  ;;               (if wayland?
  ;;                   '((#:switches . '("--ozone-platform-hint=auto"))) '())
  ;;               (if pipewire?
  ;;                   '((#:features . '("WebRTCPipeWireCapturer"))) '())
  ;;               ;; Currently Chromium doesn't detect the light/dark-ness of the GTK theme for the `prefers-color-scheme' media query: https://bugs.chromium.org/p/chromium/issues/detail?id=998903
  ;;               (if (equal? theme #:theme/dark)
  ;;                   '((#:switches . '("--force-dark-mode"))
  ;;                     (#:features . '("WebUIDarkMode")))
  ;;                   '()))))
  ;;   (as-> flags $
  ;;         (apply merge-with append $)
  ;;         (let ((switches-args
  ;;                (apply append (assoc-ref $ #:switches)))
  ;;               (features-arg
  ;;                (let ((features (assoc-ref $ #:features)))
  ;;                  (when features
  ;;                    (string-append "--enable-features="
  ;;                                   (string-join features ","))))))
  ;;           (append switches-args
  ;;                   (or (list features-arg) '())))
  ;;         (string-join $)
  ;;         ;; Not done yet. This is where I'd add the command line args to a package wrapper like e.g. `ungoogled-chromium/wayland' does.
  ;;         ))
  package)

(define* (icecat-app #:key
                     stateless?
                     gtk-services)
  (append
   (list
    (simple-service
     'icecat-add-packages
     home-profile-service-type
     (list icecat))

    (when stateless?
      (home-stateless-service
       'icecat
       #:state '(((#:path . ("mozilla/icecat/" #:xdg-base "XDG_CACHE_HOME"))
                  (#:storage . #:storage/machine)
                  (#:mode . #o700)
                  (#:parent-dir-perms . ((#:mode . #o700))))
                 ((#:path . ".mozilla/icecat")
                  (#:storage . #:storage/machine)
                  (#:mode . #o700)
                  (#:parent-dir-perms . ((#:mode . #o700))))
                 ((#:path . ".mozilla/extensions")
                  (#:storage . #:storage/machine)
                  (#:mode . #o700)
                  (#:parent-dir-perms . ((#:mode . #o700))))))))

   (or gtk-services '())))

(define* (nyxt-app #:key
                   stateless?
                   doom?)
  (list
   (simple-service
    'nyxt-add-packages
    home-profile-service-type
    (list nyxt
          gst-libav
          gst-plugins-base
          gst-plugins-good
          gst-plugins-bad
          gst-plugins-ugly))

   (when stateless?
     (home-stateless-service
      'nyxt
      #:state '(((#:path . ("nyxt/" #:xdg-base "XDG_CACHE_HOME"))
                 (#:storage . #:storage/machine))
                ((#:path . ("webkitgtk/" #:xdg-base "XDG_DATA_HOME"))
                 (#:storage . #:storage/machine)))))

   (when doom?
     (doom-service
      'nyxt
      #:modules '((#:lang
                   common-lisp))))))

(define* (tor-browser-app #:key stateless?)
  (list
   (simple-service
    'tor-browser-add-packages
    home-profile-service-type
    (list torbrowser))

   (when stateless?
     (home-stateless-service
      'tor-browser
      #:state '(((#:path . ("tor-browser/" #:xdg-base "XDG_DATA_HOME"))
                 (#:storage . #:storage/machine)))))))

(define* (vlc-app #:key
                  stateless?
                  qt-services)
  (append
   (list
    (simple-service
     'vlc-add-packages
     home-profile-service-type
     (list vlc))

    (when stateless?
      (home-stateless-service
       'vlc
       #:state '(((#:path . ("vlc/" #:xdg-base "XDG_CONFIG_HOME"))
                  (#:storage . #:storage/machine))
                 ((#:path . ("vlc/" #:xdg-base "XDG_DATA_HOME"))
                  (#:storage . #:storage/machine))))))

   (or qt-services '())))

(define* (gtk-services #:key
                       stateless?
                       theme
                       font
                       font-monospace
                       font-size)
  (list
   (simple-service
    'gtk-add-packages
    home-profile-service-type
    (list ;; arc-theme ; TODO Remove temporarily, it requires frequent rebuilds and I'm probably going to be changing it anyway. Also commented "gtk-theme" setting below.
          ;; I copied the following from Andrew Tropin's config. I'm not sure why I'd need or want all of them (perhaps as fallbacks and in the case of `gnome-themes-extra' as a fallback GTK 2 theme?). I might get rid of some of them later but for now I'll try having more than less.
          hicolor-icon-theme ; Andrew Tropin indicates (in abcdw/rde@3a8ea85f) that this is needed for network manager applet icons. [2024-07-20 Sat] update: deleted applets, do I still want this?
          adwaita-icon-theme
          ;; papirus-icon-theme ; TODO Remove for now, it massively increases guix build time. Why? Update: the situation may be better now: https://github.com/guix-mirror/guix/commit/aed385e18ec7b68a0bc1bb4b173aeadc9cd97245
          ;; gnome-themes-extra ; TODO Remove for now so I can avoid building its GTK 2 dependency.
          ))

   (simple-service
    'gtk-settings-and-dconf
    home-multi-xdg-configuration-files-service-type
    `(("gtk-3.0/settings.ini"
       ,(ini-file
         "gtk-3.0-settings.ini"
         `((Settings ((gtk-application-prefer-dark-theme . ,(equal? theme #:theme/dark)))))))
      ("gtk-4.0/settings.ini"
       ,(ini-file
         "gtk-4.0-settings.ini"
         `((Settings ((gtk-application-prefer-dark-theme . ,(equal? theme #:theme/dark)))))))
      ("dconf/user"
       ,(dconf-db
         "dconf-user"
         `((org/gnome/desktop/interface
            (;; (gtk-theme . ,(quoted-symbol
             ;;                (case theme
             ;;                  ((#:theme/light) "Arc")
             ;;                  ((#:theme/dark) "Arc-Dark"))))
             (font-name . ,(quoted-symbol
                            (pango-font-description font font-size)))
             (monospace-font-name . ,(quoted-symbol
                                      (pango-font-description font-monospace font-size))))))))))

   (when stateless?
     (home-stateless-service
      'gtk
      #:state '(((#:path . ("recently-used.xbel" #:xdg-base "XDG_DATA_HOME"))
                 (#:storage . #:storage/machine)))))))

(define (dconf-db name settings)
  (computed-file
   name
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules ((guix build utils) #:select (invoke)))
         (invoke
          #$(file-append dconf "/bin/dconf")
          "compile"
          #$output
          #$(file-union ; Target of `compile` must be a directory.
             "dconf-keyfiledir"
             `(("dconf-keyfile"
                ,(ini-file
                  "dconf-keyfile"
                  settings)))))))))

(define (quoted-symbol s)
  (-> s single-quoted string->symbol))

(define* (qt-services #:key
                      stateless?
                      wayland?)
  (list
   (simple-service
    'qt-add-packages
    home-profile-service-type
    (if wayland?
        ;; Don't install qtwayland 6 for now since Guix doesn't allow multiple versions of a package in a profile (see https://issues.guix.gnu.org/65508). This is fine for the time being since most QT apps, at least in Guix, use QT 5.
        (list ;; qtwayland
              qtwayland-5)
        '()))

   (when stateless?
     (home-stateless-service
      'qt
      #:state '(((#:path . ("QtProject.conf" #:xdg-base "XDG_CONFIG_HOME"))
                 (#:storage . #:storage/machine)))))))

(define* (work #:key
               stateless?
               (work-dir "work")
               projects)
  (append
   (if projects
       (append-map
        (match-lambda
          ((dir . opts)
           (let* ((dir* (string-append "/.persist/home/pharcosyle" "/" "work_for_now" "/" dir))
                  ;; (dir* (string-append (getenv "HOME") "/" work-dir "/" dir))
                  (proj-file (string-append dir* "/project.scm")))
             (if (file-exists? proj-file)
                 ;; TODO figure out the "add declarative" warning. Maybe rewrite to use `add-to-load-path'.
                 (apply (load proj-file) (->> opts
                                              (acons #:dir dir*)
                                              alist->list))
                 '()))))
        projects)
       '())

   (list
    (when stateless?
      (home-stateless-service
       'projects
       #:state `(((#:path . ,(string-append work-dir "/"))
                  (#:storage . #:storage/persist))))))))

(define (services-change-packages f svcs)
  (map (lambda (s)
         (and (service? s) ; Only here because I allow home components with #f not filtered out, see `create-he'.
              (service (f (service-kind s) home-profile-service-type)
                       (service-value s))))
       svcs))

(define services-only-packages
  (partial services-change-packages only-extensions))

(define services-sans-packages
  (partial services-change-packages without-extensions))

(define without-profile-extension
  (rpartial without-extensions home-profile-service-type))

(define home-multi-xdg-configuration-files-service-type
  (service-type
   (inherit home-xdg-configuration-files-service-type)
   (compose
    (lambda (exts)
      (as-> exts $
            (delete-duplicates $ (lambda (a b)
                                   (equal? (car a) (car b))))
            (apply append $))))))

(define log-home
  '((#:path . "log")
    (#:xdg-base . "XDG_STATE_HOME")))

(define (home-shepherd-service-log-file file)
  #~(string-append
     (getenv #$(assoc-ref log-home #:xdg-base))
     "/"
     #$(assoc-ref log-home #:path)
     "/"
     #$file))

(define very-big-history 1000000)

(define* (ini-file name config
                   #:key (ini-serialize ini-serialize))
  (apply mixed-text-file name (ini-serialize config)))

;; TOML is pretty close to (RDE's idea of) INI so just piggyback off of that until I have more sophisticated needs.
(define (toml-file name config)
  (ini-file name config))

;; TODO This should be part of `toml-file' eventually.
(define (serialize-toml-array elements)
  (as-> elements $
        (map quoted $)
        (string-join $ ", ")
        (string-append "[ " $ " ]")
        (string->symbol $)))

(define (serialize-hyprlang form)

  (define* (serialize-tree tree)
    (->> tree
         (map serialize-expr)
         (apply append)))

  (define* (serialize-expr expr
                           #:optional
                           (nesting-level 0))
    (append (list (aligner nesting-level))
            (let ((head (car expr))
                  (tail (cdr expr)))
              (append (list (serialize-term head))
                      (if (and (= (length tail) 1)
                               (tree? (car tail)))
                          (serialize-subtree (car tail) nesting-level)
                          (serialize-arguments tail))))
            (list "\n")))

  (define (serialize-subtree subtree nesting-level)
    (append (list " {" "\n")
            (->> subtree
                 (map (lambda (expr)
                        (serialize-expr expr (1+ nesting-level))))
                 (apply append))
            (list (aligner nesting-level)
                  "}")))

  (define (serialize-arguments arguments)
    (cons " = "
          (->> arguments
               (map serialize-term)
               (interpose ", "))))

  (define (serialize-term term)
    (match term
      (#t "true")
      (#f "false")
      ((? keyword? e) (-> e keyword->symbol symbol->string))
      ((? number? e) (number->string e))
      ((? string? e) e)
      (e e)))

  (define (aligner nesting-level)
    (apply string-append
           (map (const "    ") (iota nesting-level))))

  (define (expr? x)
    (list? x))

  (define (tree? x)
    (and (list? x) (every expr? x)))

  (serialize-tree form))

(define* (hyprlang-file name body)
  (apply mixed-text-file name (serialize-hyprlang body)))

(define* (system-features #:key
                          stateless?
                          stateless-storage-paths
                          login-shell
                          doom?
                          doom-flags
                          package-managers
                          nix-nixpkgs-config-settings
                          console?
                          kmonad?
                          kmonad
                          desktop?
                          pipewire?
                          pipewire
                          bluetooth?
                          login-manager
                          desktop-tty-start-cmds
                          sessions
                          hyprland-environment
                          sway-environment
                          virtualization?)
  (append
   (if stateless?
       (stateless #:storage-paths stateless-storage-paths) '())
   (if (member #:pm/guix (or package-managers '()))
       (guix-pm #:stateless? stateless?
                #:doom? doom?)
       '())
   (if (member #:pm/nix (or package-managers '()))
       (nix-pm #:stateless? stateless?
               #:doom? doom?
               #:doom-tree-sitter? (assoc-ref doom-flags #:doom/tree-sitter?)
               #:login-shell login-shell
               #:nixpkgs-config-settings nix-nixpkgs-config-settings)
       '())
   (if console?
       (if kmonad?
           (kmonad-tool #:doom? doom?
                        #:kmonad kmonad)
           '())
       '())
   (if desktop?
       (append
        (if pipewire?
            (pipewire-media #:stateless? stateless?
                            #:pipewire pipewire)
            '())
        (if bluetooth?
            (bluetooth) '())
        (dbus-ipc)
        (if login-manager
            (case login-manager
              ((#:lm/gdm) (gdm #:stateless? stateless?)))
            (desktop-tty #:sessions sessions
                         #:start-cmds desktop-tty-start-cmds))
        (if (member #:session/gnome (or sessions '()))
            (gnome-desktop #:stateless? stateless?) '())
        (if (member #:session/hyprland (or sessions '()))
            hyprland-environment '())
        (if (member #:session/sway (or sessions '()))
            sway-environment '()))
       '())
   (if virtualization?
       (virtualization) '())))

(define* (hyprland-environment #:key
                               pipewire?
                               doom?
                               input
                               output
                               portal-services
                               colors
                               wallpaper
                               font-variable-pitch
                               base-font-size
                               date-format
                               time-format
                               external-programs
                               (screen-locker (hyprlock-screen-locker)))
  (let ((session #:session/hyprland))
    (append
     (hyprland-wm #:pipewire? pipewire?
                  #:portal-services portal-services
                  #:input input
                  #:output output
                  #:external-programs
                  (append
                   `((#:idle-manager . ,(hypridle-idle-manager))
                     (#:screen-locker . ,(assoc-ref screen-locker #:screen-locker/lock-immediately)))
                   external-programs))
     (hyprlock-wm-piece #:colors colors
                        #:image wallpaper
                        #:font font-variable-pitch
                        #:font-size base-font-size
                        #:date-format date-format
                        #:time-format time-format)
     (hypridle-wm-piece #:session session
                        #:screen-locker (assoc-ref screen-locker #:screen-locker/lock))
     (mako-wm-piece #:colors colors
                    #:font font-variable-pitch
                    #:font-size base-font-size)
     (poweralertd-wm-piece))))

(define* (sway-environment #:key
                           sway
                           pipewire?
                           doom?
                           input
                           output
                           portal-services
                           colors
                           wallpaper
                           font-variable-pitch
                           base-font-size
                           date-format
                           time-format
                           external-programs
                           swaylock-effects?
                           swaylock
                           swaylock-effects
                           swayidle
                           (screen-locker (swaylock-screen-locker #:effects? swaylock-effects?
                                                                  #:swaylock swaylock
                                                                  #:swaylock-effects swaylock-effects)))
  (let ((session #:session/sway))
    (append
     (sway-wm #:sway sway
              #:pipewire? pipewire?
              #:doom? doom?
              #:portal-services portal-services
              #:input input
              #:output output
              #:external-programs
              (append
               `((#:sway . ,sway)
                 (#:idle-manager . ,(swayidle-idle-manager #:swayidle swayidle))
                 (#:screen-locker . ,(assoc-ref screen-locker #:screen-locker/lock-immediately)))
               external-programs))
     (swaylock-wm-piece #:colors colors
                        #:image wallpaper
                        #:font font-variable-pitch
                        #:date-format date-format
                        #:time-format time-format
                        #:effects? swaylock-effects?
                        #:swaylock swaylock
                        #:swaylock-effects swaylock-effects)
     (swayidle-wm-piece #:session session
                        #:sway sway
                        #:screen-locker (assoc-ref screen-locker #:screen-locker/lock)
                        #:swayidle swayidle)
     ;; TODO Omit dup with hyprland
     ;; (mako-wm-piece #:colors colors
     ;;                #:font font-variable-pitch
     ;;                #:font-size base-font-size)
     ;; (poweralertd-wm-piece)
     ;; (kanshi-wm-piece #:outputs outputs)
     )))

(define* (programs #:key
                   stateless?
                   interactive-shells
                   doom?
                   doom-flags
                   doom-ui?
                   email
                   desktop?
                   emacs-interface)
  (append
   (direnv-prog #:doom? doom?
                #:shells interactive-shells)
   ;; (protonvpn-prog #:stateless? stateless?)
   ;; (ssh-prog)
   (git-prog #:stateless? stateless?
             #:doom? doom?
             #:email email)
   (if doom?
       (append
        (if (assoc-ref doom-flags #:doom/calendar?)
            (doom-calendar-prog #:stateless? stateless?
                                #:org? (assoc-ref doom-flags #:doom/org?))
            '())
        (if (assoc-ref doom-flags #:doom/org?)
            (doom-org-prog #:stateless? stateless?
                           #:evil? (assoc-ref doom-flags #:doom/evil?))
            '())
        (doom-dired-prog #:stateless? stateless?
                         #:icons? (assoc-ref doom-flags #:doom/icons?)
                         #:desktop? desktop?
                         #:doom-ui? doom-ui?
                         #:emacs-handler (assoc-ref emacs-interface #:emacs/handler))
        (if doom-ui?
            (doom-trash-prog)
            '()))
       '()) ; Maybe have some alternatives for non-Doom here: Some file manager, trash-cli...
   ;; (misc-progs)
   ))

;; TODO Update with respect to nonphrenetic.
(use-modules ((nonphrenetic home) #:select (zoom-app google-chrome-app firefox-app)))

(define* (apps #:key
               stateless?
               pipewire?
               doom?
               doom-ui?
               theme
               colors
               font-variable-pitch
               font-mono
               base-font-size
               wayland?
               make-google-chrome)
  (let ((gtk (gtk-services #:stateless? stateless?
                           #:theme theme
                           #:font font-variable-pitch
                           #:font-monospace font-mono
                           #:font-size base-font-size))
        ;; (qt (qt-services #:stateless? stateless?
        ;;                  #:wayland? wayland?))
        )
    (append
     ;; (pavucontrol-app #:stateless? stateless?
     ;;                  #:gtk-services gtk)
     (font-manager-app #:stateless? stateless?
                       #:gtk-services gtk)
     (if doom-ui?
         (doom-menu-app)
         (rofi-app #:stateless? stateless?
                   #:wayland? wayland?
                   #:colors colors
                   #:font font-variable-pitch
                   #:font-size base-font-size))
     ;; (zoom-app #:stateless? stateless?
     ;;           #:qt-services qt)
     ;; (gimp-app #:stateless? stateless?
     ;;           #:gtk-services gtk)
     (alacritty-app #:colors colors
                    #:font font-mono
                    #:font-size base-font-size)
     (if doom-ui?
         (doom-vterm-app #:setup-zsh? #t
                         ;; TODO
                         ;; #:setup-zsh? (member #:shell/zsh interactive-shells)
                         )
         '())
     (google-chrome-app #:stateless? stateless?
                        #:toolkits '(#:chromium.toolkit/gtk4)
                        #:wayland? wayland?
                        #:pipewire? pipewire?
                        #:theme theme
                        #:gtk-services gtk
                        ;; #:qt-services qt
                        #:make-google-chrome make-google-chrome)
     ;; (firefox-app #:stateless? stateless?
     ;;              #:gtk-services gtk)
     ;; (nyxt-app #:stateless? stateless?
     ;;           #:doom? doom?)
     ;; (tor-browser-app #:stateless? stateless?)
     ;; (vlc-app #:stateless? stateless?
     ;;          #:qt-services qt)
     )))

;; TODO Update with respect to nonphrenetic.
(use-modules ((nonphrenetic home) #:select (%fonts) #:prefix nonphrenetic:))

(define* (he-entire #:key
                    stateless?
                    stateless-storage-paths
                    login-shell
                    (interactive-shells '(#:shell/zsh))
                    (wayland? #t)
                    emacs-pgtk
                    (emacs-interface (emacs-interface #:wayland? wayland?
                                                      #:emacs-pgtk emacs-pgtk))
                    doom?
                    doom-flags
                    doom-ui?
                    package-managers
                    nix-nixpkgs-config-settings
                    console?
                    kmonad?
                    kmonad
                    desktop?
                    media
                    media-pipewire
                    bluetooth?
                    (theme #:theme/dark)
                    (light-colors (assoc-ref %colors #:colors/nuclear-light))
                    (dark-colors (assoc-ref %colors #:colors/nuclear-dark))
                    (colors dark-colors)
                    (font-sans (assoc-ref nonphrenetic:%fonts #:fonts/apple-sf-pro))
                    (font-serif (assoc-ref nonphrenetic:%fonts #:fonts/apple-new-york))
                    (font-mono (assoc-ref %fonts #:fonts/fira-code))
                    (font-variable-pitch font-sans)
                    (base-font-size 10)
                    ;; TODO Temporarily remove until I figure out conflicting portal setups with multiple desktops.
                    ;; (portal-services ...)
                    login-manager
                    sessions
                    sessions-inputs
                    sessions-outputs
                    sessions-swaylock-effects?
                    sessions-swaylock
                    sessions-swaylock-effects
                    sessions-swayidle
                    virtualization?
                    email
                    projects
                    apps-make-google-chrome)
  (let ((pipewire? (equal? media #:media/pipewire)))
    (let ((he-base
           (lambda ()
             (append
              (let* ((wallpaper (assoc-ref %wallpapers #:wallpaper/alucard))
                     (input `((#:inputs . ,sessions-inputs)
                              (#:default-kb-repeat-delay . 200)
                              (#:default-kb-repeat-rate . 40)
                              (#:default-tp-tap? . #t)))
                     (output `((#:outputs . ,sessions-outputs)
                               (#:default-bg-image . ,wallpaper)))
                     (date-format (assoc-ref %date-formats #:date-format/simple))
                     (time-format (assoc-ref %time-formats #:time-format/simple))
                     (external-programs
                      (let ((menu (if doom-ui?
                                      (doom-menu (assoc-ref emacs-interface #:emacs/handler))
                                      (rofi-menu #:wayland? #t))))
                        `((#:notifier . ,(mako-notifier (assoc-ref menu #:menu/menu)))
                          (#:power-monitor . ,(poweralertd-monitor))
                          (#:app-launcher . ,(assoc-ref menu #:menu/app-launcher))
                          (#:terminal . ,(if doom-ui?
                                             (doom-vterm-terminal (assoc-ref emacs-interface #:emacs/handler))
                                             (alacritty-terminal)))
                          (#:backup-terminal . ,(if doom-ui?
                                                    (alacritty-terminal)
                                                    #f))
                          (#:emacs-interface . ,emacs-interface)))))
                (system-features #:stateless? stateless?
                                 #:stateless-storage-paths stateless-storage-paths
                                 #:login-shell login-shell
                                 #:doom? doom?
                                 #:doom-flags doom-flags
                                 #:package-managers package-managers
                                 #:nix-nixpkgs-config-settings nix-nixpkgs-config-settings
                                 #:console? console?
                                 #:kmonad? kmonad?
                                 #:kmonad kmonad
                                 #:desktop? desktop?
                                 #:pipewire? pipewire?
                                 #:pipewire media-pipewire
                                 #:bluetooth? bluetooth?
                                 #:login-manager login-manager
                                 #:desktop-tty-start-cmds `((#:session/gnome . ,(gnome-start))
                                                            (#:session/hyprland . ,(hyprland-start #:pipewire? pipewire?))
                                                            (#:session/sway . ,(sway-start #:sway sway
                                                                                           #:pipewire? pipewire?)))
                                 #:sessions sessions
                                 #:hyprland-environment (hyprland-environment #:pipewire? pipewire?
                                                                              #:doom? doom?
                                                                              #:input input
                                                                              #:output output
                                                                              #:portal-services (append
                                                                                                 (portal-config-services #:xdg-current-desktop "hyprland"
                                                                                                                         #:preferred '("hyprland" "gtk"))
                                                                                                 (portal-services/hyprland)
                                                                                                 (portal-services/gtk))
                                                                              #:colors colors
                                                                              #:wallpaper wallpaper
                                                                              #:font-variable-pitch font-variable-pitch
                                                                              #:base-font-size base-font-size
                                                                              #:date-format date-format
                                                                              #:time-format time-format
                                                                              #:external-programs external-programs)
                                 #:sway-environment (sway-environment #:sway sway
                                                                      #:pipewire? pipewire?
                                                                      #:doom? doom?
                                                                      #:input input
                                                                      #:output output
                                                                      #:portal-services (append
                                                                                         (portal-config-services #:xdg-current-desktop "sway"
                                                                                                                 #:preferred '("wlr" "gtk"))
                                                                                         (portal-services/wlr)
                                                                                         (portal-services/gtk))
                                                                      #:colors colors
                                                                      #:wallpaper wallpaper
                                                                      #:font-variable-pitch font-variable-pitch
                                                                      #:base-font-size base-font-size
                                                                      #:date-format date-format
                                                                      #:time-format time-format
                                                                      #:external-programs external-programs
                                                                      #:swaylock-effects? sessions-swaylock-effects?
                                                                      #:swaylock sessions-swaylock
                                                                      #:swaylock-effects sessions-swaylock-effects
                                                                      #:swayidle sessions-swayidle)
                                 #:virtualization? virtualization?))
              (shells #:stateless? stateless?
                      #:doom? doom?
                      #:doom-tree-sitter? (assoc-ref doom-flags #:doom/tree-sitter?)
                      #:login-shell login-shell
                      #:interactive-shells interactive-shells)
              (xdg-base-directories)
              (xdg-trash #:stateless? stateless?)
              (emacs-editor #:wayland? wayland?
                            #:emacs-pgtk emacs-pgtk)
              (if (or doom? doom-ui?)
                  (append
                   (doom #:stateless? stateless?
                         #:services (doom-core #:stateless? stateless?
                                               #:evil? (assoc-ref doom-flags #:doom/evil?)
                                               #:icons? (assoc-ref doom-flags #:doom/icons?)
                                               #:email email))
                   (if desktop?
                       (doom-desktop #:theme theme
                                     #:light-colors light-colors
                                     #:dark-colors dark-colors
                                     #:font font-mono
                                     #:font-variable-pitch font-variable-pitch
                                     #:font-size base-font-size)
                       '()))
                  '())
              (if desktop?
                  (append
                   (font-library #:stateless? stateless?
                                 #:font-sans font-sans
                                 #:font-serif font-serif
                                 #:font-mono font-mono
                                 #:extra-fonts (list (assoc-ref %fonts #:fonts/google-noto)
                                                     (assoc-ref %fonts #:fonts/liberation)
                                                     (assoc-ref nonphrenetic:%fonts #:fonts/apple-color-emoji)
                                                     (assoc-ref nonphrenetic:%fonts #:fonts/apple-sf-compact)
                                                     (assoc-ref nonphrenetic:%fonts #:fonts/apple-sf-arabic)
                                                     (assoc-ref %fonts #:fonts/google-noto-sans-cjk)
                                                     (assoc-ref %fonts #:fonts/google-noto-serif-cjk)
                                                     (assoc-ref nonphrenetic:%fonts #:fonts/apple-symbols)))
                   (xdg-user-directories #:stateless? stateless?)
                   (mesa #:stateless? stateless?)
                   (flatpak-apps #:stateless? stateless?
                                 ;; TODO Temporarily remove until I figure out conflicting portal setups with multiple desktops.
                                 ;; #:portal-services portal-services
                                 ))
                  '())
              (work #:stateless? stateless?
                    #:projects projects))))
          (he-programs
           (lambda ()
             (programs #:stateless? stateless?
                       #:interactive-shells interactive-shells
                       #:doom? doom?
                       #:doom-flags doom-flags
                       #:doom-ui? doom-ui?
                       #:email email
                       #:desktop? desktop?
                       #:emacs-interface emacs-interface)))
          (he-apps
           (lambda ()
             (apps #:stateless? stateless?
                   #:pipewire? pipewire?
                   #:doom? doom?
                   #:doom-ui? doom-ui?
                   #:theme theme
                   #:colors colors
                   #:font-variable-pitch font-variable-pitch
                   #:font-mono font-mono
                   #:base-font-size base-font-size
                   #:wayland? wayland?
                   #:make-google-chrome apps-make-google-chrome))))
      (append
       (he-base)
       (he-programs)
       (if desktop?
           (he-apps) '())))))
