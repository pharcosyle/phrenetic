(define-module (nonphrenetic home)
  #:use-module (gnu home services)
  #:use-module ((gnu packages browser-extensions) #:select (ublock-origin/chromium))
  #:use-module (gnu services)
  #:use-module ((nongnu packages chrome) #:select ((make-google-chrome-stable . make-google-chrome)))
  #:use-module ((nongnu packages databases) #:select (datomic-cli-tools))
  #:use-module ((nongnu packages fonts) #:select (font-apple-sf-pro font-apple-sf-compact font-apple-sf-mono font-apple-sf-arabic font-apple-new-york font-apple-color-emoji font-apple-symbols))
  #:use-module ((nongnu packages messaging) #:select (zoom))
  #:use-module ((nongnu packages mozilla) #:select (firefox))
  ;; #:use-module ((chromium chromium) #:select (chromium chromium+drm))
  #:use-module (phrenetic home)
  #:use-module (phrenetic utils)
  #:export (%nixpkgs-allow-unfree
            %fonts
            datomic-prog
            zoom-app
            ;; chromium-app
            google-chrome-app
            firefox-app))

(define %nixpkgs-allow-unfree
  '((allowUnfree . #t)))

;; TODO These should really each be a function to parameterize the packages.
(define %fonts
  `((#:fonts/apple-sf-pro . ((#:font/name . "SF Pro")
                             (#:font/package . ,font-apple-sf-pro)))
    (#:fonts/apple-sf-compact . ((#:font/name . "SF Compact")
                                 (#:font/package . ,font-apple-sf-compact)))
    (#:fonts/apple-sf-mono . ((#:font/name . "SF Mono")
                              (#:font/package . ,font-apple-sf-mono)))
    (#:fonts/apple-sf-arabic . ((#:font/name . "SF Arabic")
                                (#:font/package . ,font-apple-sf-arabic)))
    (#:fonts/apple-new-york . ((#:font/name . "New York")
                               (#:font/package . ,font-apple-new-york)))
    (#:fonts/apple-color-emoji . ((#:font/name . "Apple Color Emoji")
                                  (#:font/package . ,font-apple-color-emoji)))
    (#:fonts/apple-symbols . ((#:font/name . "Apple Symbols")
                              (#:font/package . ,font-apple-symbols)))))

(define (datomic-prog)
  (list
   (simple-service
    'datomic-add-packages
    home-profile-service-type
    (list datomic-cli-tools))))

(define* (zoom-app #:key
                   stateless?
                   qt-services)
  (append
   (list
    (simple-service
     'zoom-add-packages
     home-profile-service-type
     (list zoom))

    ;; (when stateless?
    ;;   (home-stateless-service
    ;;    'zoom
    ;;    #:state '(((#:path . ("zoom/" #:xdg-base "XDG_CONFIG_HOME"))
    ;;               (#:storage . #:storage/machine))
    ;;              ((#:path . ("zoom/" #:xdg-base "XDG_DATA_HOME"))
    ;;               (#:storage . #:storage/machine)))))
    )

   (or qt-services '())))

;; (define* (chromium-app #:key
;;                        stateless?
;;                        toolkits
;;                        wayland?
;;                        pipewire?
;;                        theme
;;                        gtk-services
;;                        qt-services
;;                        drm?
;;                        (chromium chromium)
;;                        (chromium+drm chromium+drm))
;;   (append
;;    (list
;;     (simple-service
;;      'chromium-add-packages
;;      home-profile-service-type
;;      ;; Chromium inherits from ungoogled-chromium and at the time of writing, ungoogled-chromium doesn't offer GTK 4 or QT 6 so just ignore the toolkits option.
;;      (list (chromium-wrapper (if drm?
;;                                  chromium+drm
;;                                  chromium)
;;                              wayland? pipewire? theme)
;;            ublock-origin/chromium))

;;     (when stateless?
;;       (home-stateless-service
;;        'chromium
;;        #:state '(((#:path . ("chromium/" #:xdg-base "XDG_CACHE_HOME"))
;;                   (#:storage . #:storage/machine)
;;                   (#:mode . #o700))
;;                  ((#:path . ("chromium/" #:xdg-base "XDG_CONFIG_HOME"))
;;                   (#:storage . #:storage/machine)
;;                   (#:mode . #o700))
;;                  ((#:path . ".pki/nssdb/")
;;                   (#:storage . #:storage/machine)
;;                   (#:mode . #o700)
;;                   (#:parent-dir-perms . ((#:mode . #o700))))))))

;;    (or gtk-services '())
;;    (or qt-services '())))

(define* (google-chrome-app #:key
                            stateless?
                            toolkits
                            wayland?
                            pipewire?
                            theme
                            gtk-services
                            qt-services
                            (make-google-chrome make-google-chrome)
                            (release-channel #:chrome.channel/stable))
  (append
   (list
    (simple-service
     'google-chrome-add-packages
     home-profile-service-type
     (list (chromium-wrapper (make-google-chrome
                              #:toolkits (map (lambda (toolkit)
                                                (case toolkit
                                                  ((#:chromium.toolkit/gtk3) 'gtk3)
                                                  ((#:chromium.toolkit/gtk4) 'gtk4)
                                                  ((#:chromium.toolkit/qt5) 'qt5)
                                                  ((#:chromium.toolkit/qt6) 'qt6)))
                                              toolkits))
                             wayland? pipewire? theme
                             #:gtk4? (member #:chromium.toolkit/gtk4 toolkits))))

    (when stateless?
      (let ((chrome-state-prefix (case release-channel
                                   ((#:chrome.channel/stable) "google-chrome")
                                   ((#:chrome.channel/beta) "google-chrome-beta")
                                   ((#:chrome.channel/unstable) "google-chrome-unstable"))))
        (home-stateless-service
         'google-chrome
         #:state `(((#:path . (,(string-append chrome-state-prefix "/") #:xdg-base "XDG_CACHE_HOME"))
                    (#:storage . #:storage/machine)
                    (#:mode . #o700))
                   ((#:path . (,(string-append chrome-state-prefix "/") #:xdg-base "XDG_CONFIG_HOME"))
                    (#:storage . #:storage/machine)
                    (#:mode . #o700))
                   ((#:path . ".pki/nssdb/")
                    (#:storage . #:storage/machine)
                    (#:mode . #o700)
                    (#:parent-dir-perms . ((#:mode . #o700)))))
         ;; It's weird that Chrome creates this, it's not even in one of the recognized locations (https://wiki.archlinux.org/title/XDG_MIME_Applications#mimeapps.list). Also the only time I've seen it not be empty is once when Gmail promted me to let it open emails and I hit accept. Whatever, lets just ignore it.
         #:ignore '(((#:path . ("mimeapps.list" #:xdg-base "XDG_DATA_HOME"))
                     (#:preds . ((#:empty-file)))))))))

   (or gtk-services '())
   (or qt-services '())))

(define* (firefox-app #:key
                      stateless?
                      gtk-services)
  (append
   (list
    (simple-service
     'firefox-add-packages
     home-profile-service-type
     (list firefox))

    (when stateless?
      (home-stateless-service
       'firefox
       #:state '(((#:path . ("mozilla/firefox/" #:xdg-base "XDG_CACHE_HOME"))
                  (#:storage . #:storage/machine)
                  (#:mode . #o700)
                  (#:parent-dir-perms . ((#:mode . #o700))))
                 ((#:path . ".mozilla/firefox")
                  (#:storage . #:storage/machine)
                  (#:mode . #o700)
                  (#:parent-dir-perms . ((#:mode . #o700))))
                 ((#:path . ".mozilla/extensions")
                  (#:storage . #:storage/machine)
                  (#:mode . #o700)
                  (#:parent-dir-perms . ((#:mode . #o700))))))))

   (or gtk-services '())))
