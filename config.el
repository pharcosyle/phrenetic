;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(use-package! dash)



(setq user-full-name "Krzysztof Baranowski"
      user-mail-address "pharcosyle@gmail.com")

;; Must be set before org loads (I think this is just a Doom, not org-mode, requirement). Keep this even though the value is the default so I can use it in the derived constants below.
(setq org-directory "~/org")

(defconst my--org-dir (concat org-directory "/"))
(defconst my--gcal-dir (concat my--org-dir "gcal/"))



;;;; Doom resets

(setq mac-right-option-modifier 'left)

(setq-default indent-tabs-mode t)

;; I'd like to have this on but in the Doom code it says it's more efficient not to.
;; (setq-default cursor-in-non-selected-windows t)



;;;; General

(setq scroll-margin 10
      save-interprogram-paste-before-kill t)

;; I like having line numbers on but hlissner says they're slow so I might want to disable them at some point. Keep in mind I use them to determine what lines are continuation lines so I might have to make the right fringe bigger if I do this and set visual-line-fringe-indicators.
;; (setq display-line-numbers-type nil)



;;;; Keybindings

(setq doom-leader-alt-key "s-;"
      doom-localleader-key "s-m"
      doom-localleader-alt-key "s-m")

(defun my--trans (&rest rest)
  (-each (-partition 2 rest)
    (-lambda ((to from))
      (define-key key-translation-map (kbd to) (kbd from)))))

(my--trans "C-h" "DEL"
           "s-H" "C-h" ; REVIEW I really don't like this binding

           "s-i" "<tab>"
           "s-I" "<backtab>"

           "s-h" "<left>"
           "s-j" "<down>"
           "s-k" "<up>"
           "s-l" "<right>")

(setq help-char (string-to-char "\C-_"))

(defalias 'original-yank-pop #'yank-pop)

;; REVIEW Some of these should be in `:after' (or their respective package) sections but I'm not totally certain where I want to put bindings yet and I'm lazy.
(map! "s-V" #'original-yank-pop

      "s-t" (lookup-key doom-leader-map (kbd "`"))
      "s-f" (cl-flet ((f (lookup-key doom-leader-map (kbd "s b"))))
              ;; `swiper' hangs initially when `visual-line-mode' is active. Plus Doom defaults to having `visual-line-mode' enabled in text-mode (and derived) buffers where it makes more sense to not search linewise.
              (cmd! (if visual-line-mode
                        (letf! ((#'swiper #'swiper-isearch))
                          (f))
                      (f))))
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
      "s-p" (lookup-key global-map (kbd "C-~"))

      "s-u" (lookup-key doom-leader-map (kbd "u"))
      "s-U" #'negative-argument         ; Trying this out.
      (:map universal-argument-map
       "s-u" #'universal-argument-more)

      (:after evil-easymotion
       "s-a" (lookup-key evilem-map (kbd "SPC")))

      "s-." #'repeat
      "s->" #'repeat-complex-command ; Try using this where simple emacs `repeat' (C-x z) fails but I donno if it'll prove useful.

      "s-J" #'evil-scroll-down
      "s-K" #'evil-scroll-up

      (:prefix "s-d"
       "h" #'git-gutter:popup-hunk
       "o" #'+macos/open-in-default-program
       "r" #'projectile-replace
       "t" #'tldr
       "s" #'org-save-all-org-buffers
       ;; "b" (cmd! (evil-local-mode 'toggle)
       ;;           (when evil-local-mode (evil-normal-state)))
       (:prefix "c"
        "f" #'org-gcal-fetch
        "s" #'org-gcal-sync
        "p" #'org-gcal-post-at-point)))



;;;; Packages

(defun my--sp-strict-h ()
  (add-hook! 'smartparens-enabled-hook :local
             #'turn-on-smartparens-strict-mode
             (lambda ()
               (map! :map smartparens-strict-mode-map
                     :i "DEL" #'sp-backward-delete-char))))

(add-hook! prog-mode #'my--sp-strict-h)

(after! avy
  (setq avy-single-candidate-jump t))

(setq-hook! emacs-lisp-mode indent-tabs-mode nil)

;; (after! clojure-mode
;;   (set-ligatures! 'clojure-mode :lambda "fn"))

(add-hook! cider-repl-mode
           #'goto-address-prog-mode
           #'highlight-numbers-mode
           #'rainbow-delimiters-mode
           #'yas-minor-mode-on
           #'my--sp-strict-h)

(after! cider
  (setq cider-repl-history-size 1000000
        cider-print-options '(("length" 100))))

(add-hook! clj-refactor-mode
  (cljr-add-keybindings-with-prefix "s-M R")) ; TODO temporary binding

(after! counsel
  (setq! counsel-yank-pop-separator "\n--------------------------------\n"))

(after! doom-modeline
  ;; (setq doom-modeline-checker-simple-format nil)
  (setq doom-modeline-persp-name t))

(after! emojify
  (setq emojify-display-style 'unicode))

(after! eshell
  (setq eshell-history-size nil))

(after! evil-org
  (map! :map evil-org-mode-map
        :nv "C-j" #'outline-forward-same-level
        :nv "C-k" #'outline-backward-same-level))

(after! evil-multiedit
  (setq evil-multiedit-follow-matches t))

(use-package! expand-region
  :defer t
  :init
  (map! :nv "s-e" #'er/expand-region
        :nv "s-E" #'er/contract-region)
  :config
  (setq expand-region-fast-keys-enabled nil) ; Unnecessary, my mapping is conventient enough and I don't want the repeat key to conflict with anything.
  ;; Copied from Doom config: ~/.emacs.d/modules/config/default/+emacs.el:12
  (defadvice! my--quit-expand-region-a (&rest _)
    "Properly abort an expand-region region."
    :before '(evil-escape doom/escape)
    (when (memq last-command '(er/expand-region er/contract-region))
      (er/contract-region 0))))

(remove-hook! (prog-mode text-mode conf-mode) #'highlight-indent-guides-mode)

(after! ivy
  (setq +ivy-buffer-preview t
        ivy-extra-directories nil
        ivy-count-format "(%d/%d) ")
  (dolist (i '(counsel-yank-pop
               counsel-evil-registers))
    (add-to-list 'ivy-height-alist `(,i . 10)))

  (map! :map ivy-minibuffer-map
        "s-J" #'ivy-scroll-up-command
        "s-K" #'ivy-scroll-down-command
        "C-r" #'ivy-reverse-i-search)) ; Doom overrides this, restore it.

(after! ivy-hydra
  (defhydra+ hydra-ivy ()
    ;; Doom overrides these imporant ivy hydra heads: ~/.emacs.d/modules/completion/ivy/autoload/hydras.el
    ("m" ivy-mark)
    ("u" ivy-unmark)
    ("t" ivy-toggle-marks)))

;; Not using lispy, remove the Doom module's hooks.
(remove-hook! (lisp-mode emacs-lisp-mode clojure-mode) #'lispy-mode)
(remove-hook! 'eval-expression-minibuffer-setup-hook #'doom-init-lispy-in-eval-expression-h)

(add-hook! prog-mode #'lispyville-mode)

(after! lispyville
  (lispyville-set-key-theme
   '(operators
     c-w
     c-u
     commentary))

  (map! :map lispyville-mode-map
        "s-C-j" #'lispyville-beginning-of-next-defun
        "s-C-k" #'lispyville-beginning-of-defun
        "s-C-," #'lispyville-end-of-defun
        "s-C-a" #'lispyville-drag-backward
        "s-C-g" #'lispyville-drag-forward
        "s-C-p" #'lispyville-prettify
        (:prefix "s-C-;"
         "R" #'lispyville-raise-list)))

(after! magit
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))

(after! org
  (setq org-agenda-files `(,my--org-dir
                           ,my--gcal-dir
                           ,(concat my--org-dir "projects"))
        org-log-done 'time
        org-priority-lowest ?E
        org-priority-default ?C
        org-priority-faces `((?A . ,(doom-color 'red))
                             (?B . ,(doom-color 'orange))
                             (?C . ,(doom-color 'blue))
                             (?D . ,(doom-color 'yellow))
                             (?E . ,(doom-color 'green)))))

(after! org-gcal
  (setq org-gcal-client-id "446729771716-pp79934q99aro2h8v3iki1fejcodbdoo.apps.googleusercontent.com"
        org-gcal-client-secret (-> (auth-source-search :host org-gcal-client-id) car (plist-get :secret) funcall)
        org-gcal-fetch-file-alist `(("pharcosyle@gmail.com" . ,(concat my--gcal-dir "pharcosyle@gmail.com.org"))
                                    ("addressbook%23contacts@group.v.calendar.google.com" . ,(concat my--gcal-dir "contacts.org"))
                                    ("en.usa%23holiday@group.v.calendar.google.com" . ,(concat my--gcal-dir "holidays.org")))
        org-gcal-recurring-events-mode 'nested))

(after! paren
  (setq! show-paren-delay 0))

(after! recentf
  (setq recentf-max-saved-items 500))

(after! smartparens
  (map! :map smartparens-mode-map
        "s-C-h" #'sp-backward-sexp
        "s-C-l" #'sp-forward-sexp
        "s-C-u" #'sp-backward-up-sexp
        "s-C-o" #'sp-up-sexp
        :gn "s-C-m" #'sp-backward-down-sexp ; Bind in normal mode explicitly to override the Doom mapping in ~/.emacs.d/modules/config/default/config.el:447
        "s-C-." #'sp-down-sexp
        "s-C-c" #'sp-splice-sexp
        "s-C-s" #'sp-splice-sexp-killing-backward
        "s-C-f" #'sp-splice-sexp-killing-forward
        "s-C-x" #'sp-backward-slurp-sexp
        "s-C-v" #'sp-forward-slurp-sexp
        "s-C-w" #'sp-backward-barf-sexp
        "s-C-r" #'sp-forward-barf-sexp
        (:prefix "s-C-;"
         "(" #'sp-wrap-round
         "[" #'sp-wrap-square
         "{" #'sp-wrap-curly
         "s" #'sp-split-sexp
         "j" #'sp-join-sexp
         "r" #'sp-raise-sexp
         "c" #'sp-convolute-sexp
         "w" #'sp-rewrap-sexp)))

(use-package! tldr
  :defer t
  :config
  (setq tldr-directory-path (concat doom-etc-dir "tldr/")))

(after! undo-fu
  (setq undo-fu-ignore-keyboard-quit t))

;; (after! undo-tree
;;   (setq undo-tree-visualizer-timestamps t))



;;;; UI

(setq doom-theme 'doom-pharcosyle-nuclear
      doom-font (font-spec :family "Source Code Pro" :size 12)
      rainbow-delimiters-max-face-count 8)

(after! evil
  (setq evil-default-cursor (lambda () (evil-set-cursor-color "#fdd94a"))
        evil-emacs-state-cursor (lambda () (evil-set-cursor-color "#ff9999"))))


;;; Screen

(add-to-list 'initial-frame-alist '(height . 57))
(add-to-list 'initial-frame-alist '(width . 192))
(set-frame-position (selected-frame) 80 0)

;; REVIEW Do this instead once I'm not working on a broken screen. I'm concerned the `fullboth' behavior I want is specific to `emacs-mac' which I won't be using much longer.
;; (add-to-list 'initial-frame-alist '(fullscreen . fullboth)) ; Make Emacs fill the screen without creating a new Mac space.



;;;; Projects

;;; Hyperdrive

(pushnew! safe-local-variable-values
          '(cider-preferred-build-tool . shadow-cljs)
          '(cider-default-cljs-repl . shadow)
          '(cider-shadow-default-options . ":app")
          '(cider-offer-to-open-cljs-app-in-browser . nil)
          '(cider-clojure-cli-global-options . "-A:dev")
          '(eval . (setenv "DATOMIC_APP_INFO_MAP" "{:app-name \"neutrino\"}"))
          '(eval . (setenv "DATOMIC_ENV_MAP" "{:env :dev}"))
          '(cider-clojure-cli-global-options . nil))

(prodigy-define-service
  :name "Amplify Mock"
  :command "amplify"
  :args '("mock")
  :cwd "~/Projects/Krush/hyperdrive/apps/singularity"
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "Datomic Access (exogenesis)"
  :command "bash"
  :args '("datomic" "client" "access" "exogenesis")
  :cwd "~/Projects/Krush/hyperdrive/ion/team"
  :kill-process-buffer-on-stop t)


;;; Massrealty

;; (pushnew! safe-local-variable-values
;;           '(ssh-deploy-root-remote . "/ssh:massrealty@35.196.144.73:/home/massrealty/deploy/homes/public_html/")
;;           '(ssh-deploy-automatically-detect-remote-changes . t))

;; (after! org-gcal
;;   (add-to-list 'org-gcal-fetch-file-alist `("krzysztof@massrealty.com" . ,(concat my--gcal-dir "krzysztof@massrealty.com.org")) 'append))
