;;; -*- lexical-binding: t; -*-

(use-package! dash)
(use-package! s)

(require 'params)

(setq user-full-name param-full-name
      user-mail-address param-email)

(defconst biome--org-dir (concat org-directory "~/org"))
(defconst biome--org-gcal-dir (concat biome--org-dir "/gcal"))

(defconst biome--very-big 1000000)

(defconst biome--theme 'doom-nuclear)

(let ((palette '(("fg" "#dee2f8") ("highlight" "#8496ff") ("comments" "#63677f") ("comment-delimiter" "#939abd") ("red" "#fb8578") ("orange" "#fdce5f") ("green" "#9fed9c") ("yellow" "#eddc91") ("blue" "#7db9fe") ("dark-blue" "#5f68de") ("magenta" "#e29bf7") ("violet" "#aeb9f3") ("cyan" "#75e0f9") ("gold" "#fdd94a") ("pink" "#ff9999"))))
(defun biome--color (color)
  (let ((k (-> color symbol-name (substring 1))))
    (cadr (assoc k palette))))
)

;; hlissner says this must be set before org loads. I'm not sure if this is a requirement of org-mode but the Doom org config certainly does a lot of gymnastics with this variable so I'll just set this exactly like the example config does.
(setq org-directory biome--org-dir)

(setq scroll-margin 10
      save-interprogram-paste-before-kill t)

;; I like having line numbers on but hlissner says they're slow so I might want to disable them at some point. Keep in mind I use them to determine what lines are continuation lines so I might have to make the right fringe bigger if I do this and set visual-line-fringe-indicators.
;; (setq display-line-numbers-type nil)

(setq-default indent-tabs-mode t)

;; I'd like to have this on but in the Doom code it says it's more efficient not to.
;; (setq-default cursor-in-non-selected-windows t)

;; REVIEW Might be useful if there end up being a lot more of these and they have a lot of similarities: https://www.gnu.org/software/emacs/manual/html_node/elisp/Extending-Rx.html
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

(setq help-char (string-to-char "\C-_"))

(defun biome--trans (&rest rest)
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
      "s-c" (if (featurep 'evil) #'evil-yank #'copy-region-as-kill)
      "s-v" #'yank
      "s-s" #'save-buffer
      "s-x" #'execute-extended-command
      ;; REVIEW I don't think I need this, any time I'm in visual mode I can use `evil-delete` ("d")
      ;; :v "s-x" #'kill-region
      "s-/" (cmd! (save-excursion (comment-line 1)))
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
      "s-U" #'negative-argument         ; Trying this out.
      (:map universal-argument-map
       "s-u" #'universal-argument-more)

      (:after evil-easymotion
       "s-a" (lookup-key evilem-map (kbd "SPC")))

      "s-." #'repeat

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
       ;; "b" (cmd! (evil-local-mode 'toggle)
       ;;           (when evil-local-mode (evil-normal-state)))
       (:prefix "c"
        "f" #'org-gcal-fetch
        "s" #'org-gcal-sync
        "p" #'org-gcal-post-at-point)))

;; (after! doom-themes
;;   (doom-themes-visual-bell-config))

(after! doom-modeline
  ;; (setq doom-modeline-checker-simple-format nil)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-persp-name t))

(use-package! highlight-indent-guides
  :defer t
  :init
  ;; I don't want indent guides on by default, remove all the Doom module's hooks.
  (remove-hook! '(prog-mode-hook
                  text-mode-hook
                  conf-mode-hook)
    #'highlight-indent-guides-mode))

(after! evil
  (defun biome--set-theme-cursor-color-symbols ()
    (put 'cursor 'evil-normal-color (biome--color :gold))
    (put 'cursor 'evil-emacs-color  (biome--color :pink)))

  (defadvice! biome-update-cursor-color-a (&rest _)
    :after #'+evil-update-cursor-color-h
    (when (memq biome--theme custom-enabled-themes)
      (biome--set-theme-cursor-color-symbols)))

  (map! :m [C-i] nil)) ; Remove Doom's binding for `evil-jump-forward'.

(after! evil-multiedit
  (setq evil-multiedit-follow-matches t))

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

(use-package! dired-x
  :defer t
  :init
  (remove-hook! 'dired-mode-hook #'dired-omit-mode)
  (add-hook! 'dired-mode-hook
    (defun require-dired-x-h ()
      (require 'dired-x))))

(after! eshell
  (setq eshell-history-size biome--very-big)) ; Setting this to `nil' to inherit envvar HISTSIZE is another option.

(after! magit
  (setq git-commit-style-convention-checks '(non-empty-second-line))
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))

(use-package! clojure-mode
  :defer t
  :init
  (setq clojure-refactor-map-prefix (kbd "s-M r")) ; Has to be set before clojure-mode loads. ; REVIEW temporary binding
  :config
  (set-ligatures! 'clojure-mode :lambda "fn")
  (custom-theme-set-faces! biome--theme
    `(clojure-character-face :foreground ,(doom-color 'violet) :weight bold)))

(after! cider
  (setq cider-repl-history-size biome--very-big
        cider-print-options '(("length" 100))))

;; REVIEW make sure this is still working after nesting in `after!'
(after! cider-repl
  (add-hook! 'cider-repl-mode-hook
             #'goto-address-prog-mode
             #'highlight-numbers-mode
             #'rainbow-delimiters-mode
             #'yas-minor-mode-on
             #'biome-sp-strict-h))

(after! clj-refactor
  (cljr-add-keybindings-with-prefix "s-M R")) ; REVIEW temporary binding

(after! elisp-mode
  (setq-hook! 'emacs-lisp-mode-hook indent-tabs-mode nil))

(after! org
  (setq org-agenda-files `(,biome--org-dir
                           ,biome--org-gcal-dir
                           ,(concat biome--org-dir "/projects"))
        org-log-done 'time
        org-priority-lowest ?E
        org-priority-default ?C
        org-priority-faces `((?A . ,(doom-color 'red))
                             (?B . ,(doom-color 'orange))
                             (?C . ,(doom-color 'blue))
                             (?D . ,(doom-color 'yellow))
                             (?E . ,(doom-color 'green)))))

(after! ob
  (setq org-babel-noweb-error-all-langs t))

(after! evil-org
  (map! :map evil-org-mode-map
        :nv "C-j" #'outline-forward-same-level
        :nv "C-k" #'outline-backward-same-level))

(after! scheme
  (setq-hook! 'scheme-mode-hook indent-tabs-mode nil))

(use-package! geiser
  :defer t
  :init
  (setq geiser-repl-current-project-function #'ignore)) ; Reset this back to its default (don't have separate REPLs for projects) because it doesn't play nice with the Guix store.

(after! org-gcal
  (setq org-gcal-client-id "446729771716-pp79934q99aro2h8v3iki1fejcodbdoo.apps.googleusercontent.com"
        org-gcal-client-secret (-> (auth-source-search :host org-gcal-client-id) car (plist-get :secret) funcall)
        org-gcal-fetch-file-alist `((user-mail-address . ,(concat biome--org-gcal-dir "/" user-mail-address ".org"))
                                    ("addressbook%23contacts@group.v.calendar.google.com" . ,(concat biome--org-gcal-dir "/contacts.org"))
                                    ("en.usa%23holiday@group.v.calendar.google.com" . ,(concat biome--org-gcal-dir "/holidays.org")))
        org-gcal-recurring-events-mode 'nested))

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
    :before '(evil-escape doom/escape)
    (when (memq last-command '(er/expand-region er/contract-region))
      (er/contract-region 0))))

(after! guix
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

;; TODO once I'm done packaging Doom figure out how to enable this
;; (use-package! guix-contributing)

(use-package! guix-packaging
  :defer t
  :init
  (setq guix-packaging--data-dir (concat doom-cache-dir "guix-packaging")))

;; REVIEW Trying out not having this so I can use avy dispatch commands.
;; (after! avy
;;   (setq avy-single-candidate-jump t))

(after! paren
  (setq! show-paren-delay 0))

(add-hook! 'prog-mode-hook #'biome-sp-strict-h)

(after! rainbow-delimiters
  (let ((default-max rainbow-delimiters-max-face-count))
    (defun biome-set-rainbow-delimiters-max-face-count ()
      (setq rainbow-delimiters-max-face-count
            (if (memq biome--theme custom-enabled-themes)
                8 default-max))))

  (biome-set-rainbow-delimiters-max-face-count)
  (add-hook! 'doom-load-theme-hook #'biome-set-rainbow-delimiters-max-face-count)

  (custom-theme-set-faces! biome--theme
    `(rainbow-delimiters-depth-1-face :foreground ,(doom-color 'fg))
    `(rainbow-delimiters-depth-2-face :foreground ,(doom-color 'magenta))
    `(rainbow-delimiters-depth-3-face :foreground ,(doom-color 'blue))
    `(rainbow-delimiters-depth-4-face :foreground ,(doom-color 'cyan))
    `(rainbow-delimiters-depth-5-face :foreground ,(doom-color 'green))
    `(rainbow-delimiters-depth-6-face :foreground ,(doom-color 'yellow))
    `(rainbow-delimiters-depth-7-face :foreground ,(doom-color 'orange))
    `(rainbow-delimiters-depth-8-face :foreground ,(doom-color 'red))))

(after! recentf
  (setq recentf-max-saved-items 500))

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
                     :i "DEL" #'sp-backward-delete-char))))

(after! transient
  (setq transient-history-limit biome--very-big))

(after! undo-fu
  (setq undo-fu-ignore-keyboard-quit t))

;; (after! undo-tree
;;   (setq undo-tree-visualizer-timestamps t))

(after! emojify
  (setq emojify-display-style 'unicode))

(define-key! :keymaps +default-minibuffer-maps
  "s-J" #'scroll-up-command
  "s-K" #'scroll-down-command)

(setq doom-theme biome--theme)

(custom-theme-set-faces! biome--theme
  `(font-lock-comment-delimiter-face :foreground ,(biome--color :comment-delimiter))
  `(font-lock-doc-face :foreground ,(doom-color 'cyan)))

(let ((font (font-spec :family param-font-name
                       :size param-font-size
                       :weight param-font-weight)))
  (when (find-font font)
    (setq doom-font font)))

(load "~/projects/phrenetic/meta" 'noerror 'nomessage)

(pushnew! safe-local-variable-values
          '(eval . (with-eval-after-load 'geiser-guile
                     (let ((root-dir
                            (file-name-directory
                             (locate-dominating-file default-directory ".dir-locals.el"))))
                       (make-local-variable 'geiser-guile-load-path)
                       (add-to-list 'geiser-guile-load-path root-dir)))))

(pushnew! safe-local-variable-values
          '(eval . (cl-flet ((enhance-imenu-lisp
                              (&rest keywords)
                              (dolist (keyword keywords)
                                (add-to-list
                                 'lisp-imenu-generic-expression
                                 (list (purecopy (concat (capitalize keyword)
                                                         (if (string= (substring-no-properties keyword -1) "s")
                                                             "es"
                                                           "s")))
                                       (purecopy (concat "^\\s-*("
                                                         (regexp-opt
                                                          (list (concat "define-" keyword))
                                                          t)
                                                         "\\s-+\\(" lisp-mode-symbol-regexp "\\)"))
                                       2)))))
                     ;; This adds the argument to the list of imenu known keywords.
                     (enhance-imenu-lisp
                      "bookmarklet-command"
                      "class"
                      "command"
                      "ffi-method"
                      "function"
                      "mode"
                      "parenscript"
                      "user-class"))))

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
  :cwd "~/projects/Krush/hyperdrive/apps/singularity"
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "Datomic Access (exogenesis)"
  :command "bash"
  :args '("datomic" "client" "access" "exogenesis")
  :cwd "~/projects/Krush/hyperdrive/ion/team"
  :kill-process-buffer-on-stop t)

;; (pushnew! safe-local-variable-values
;;           '(ssh-deploy-root-remote . "/ssh:massrealty@35.196.144.73:/home/massrealty/deploy/homes/public_html/")
;;           '(ssh-deploy-automatically-detect-remote-changes . t))

;; (after! org-gcal
;;   (add-to-list 'org-gcal-fetch-file-alist `("krzysztof@massrealty.com" . ,(concat biome--org-gcal-dir "/krzysztof@massrealty.com.org")) 'append))

(after! org
  (add-to-list 'org-agenda-files (concat biome--org-dir "/spring_cleaning") 'append))

;; I don't think I'll ever need this with Sway. Is it worth keeping this and having an "I'm not on Sway / some WM" conditional around it?
;; (add-to-list 'initial-frame-alist '(fullscreen . fullboth))

(map! "s-A" #'app-launcher-run-app)

(map! "s-&" (lambda (command)
              (interactive (list (read-shell-command "$ ")))
              (call-process-shell-command command nil 0 nil)))

(map! :leader
      "s-," (lookup-key doom-leader-map (kbd "<")))

(after! company
  (map! :map company-active-map
        "s-[" #'company-show-doc-buffer ; Currently opens Help, it would be better if I made it use Helpful.
        "s-]" #'company-show-location))

(after! evil-org
  (map! :map evil-org-mode-map
        (:prefix "g"
         :nv "{" #'evil-backward-paragraph
         :nv "}" #'evil-forward-paragraph)))

;; TODO Maybe do this if popping to the side is too annoying
;; (after! geiser-repl
;;   (setq geiser-repl-use-other-window nil))

;; Doesn't work yet: you have to switch to the buffer "manually" once before it starts being treated as real
(map! "s-d m" (cmd! (doom-set-buffer-real (current-buffer) t)))

;; `forward-char' in original definition is messing things up, do this for now
;; (defun +eshell/search-history ()
;;   (interactive)
;;   (consult-history))
(after! esh-mode
  (map! :map eshell-mode-map
        "C-s" #'consult-history))

;; Remove the :mode set by the Doom module. It's unnecessary (`guix-drv-mode.el' already does this, and for /nix/store drvs specifically) and it shadows `guix-derivation-mode'.
;; TODO should this go in the Guix or Nix config section?
(use-package! nix-drv-mode
  :defer t
  :init
  (setq auto-mode-alist (delete '("\\.drv\\'" . nix-drv-mode) auto-mode-alist)))

(after! vertico
  (setq vertico-count 20  ; Trying out, maybe too big.
        vertico-scroll-margin 7))

(use-package! battery
  :config
  (display-battery-mode 1))

(use-package! time
  :config
  (setq display-time-day-and-date t)
  ;; (setq display-time-24hr-format t)
  (display-time-mode 1))

;; If I keep this I can get rid of the "C-_" binding I have too. Update: maybe? Some places C-_ works and some s-h does (in vertico)? What about my key translation for C-? ? Straighten all this shit out.
(biome--trans "s-h" "C-h")

(map! "s-o" (lookup-key doom-leader-map (kbd "<")))
;; Get rid of binding for =s-r=?
(map! "s-r" nil)

;; Maybe "C-s-i" but then I'll have to change my sexp-movement command(s). Maybe keep this even if I do that, for consistency.
(biome--trans "C-S-i" "<backtab>")
;; Also messes with my sexp-movement bindings
;; (map! "C-s-j" #'evil-scroll-down
;;       "C-s-k" #'evil-scroll-up)

(after! evil
  (map! :map evil-motion-state-map
        ;; Maybe "C-s-o" but then I'll have to change my sexp-movement command(s). There are other bindings for jump-forward though, maybe I'll just use those.
        "C-S-o" #'evil-jump-forward))

;; Maybe `s-S' to save-and-tangle? If I don't get some sort of auto-tangling thing going
;; - maybe it could be "save and eval defun (C-M-x)" in lisp/programming modes?'
;; Maybe a hotkey that just jumps back and forth between my most recently focused browser window and eemacs?
;; Maybe bind `s-o' in `consult-buffer' to "close consult-buffer and open +vertico/switch-workspace-buffer"

;; (setq biome--phrenetic-dir "~/projects/phrenetic")

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
;;   (setq org-tanglesync-watch-files '("/home/pharcosyle/projects/phrenetic/phrenetic.org")))

(setq my-dotfiles-git-dir (concat "--git-dir=" (expand-file-name "~/home-state.git")))
;; (setq my-dotfiles-work-tree (concat "--work-tree=" (expand-file-name "~")))
(map! "s-b" (cmd!
             (pushnew! magit-git-global-arguments my-dotfiles-git-dir)
             (magit-status "~")))
(map! "s-B" (cmd!
             ;; (delq! my-dotfiles-git-dir magit-git-global-arguments)
             ;; Why is the above not working? Doing this for now:
             ;; - Update: because `delq!' tests for equality with `eq'
             (setq magit-git-global-arguments (remove my-dotfiles-git-dir magit-git-global-arguments))))

(map! (:map minibuffer-local-map
       "C-:" #'embark-act
       "C-c C-:" #'embark-export)
      (:leader
       "A" #'embark-dwim)
      ;; Trying out cycling (temp)
      "C-:" #'embark-act
      "s-q" #'embark-cycle)

(set-ligatures! '(html-mode nxml-mode web-mode)
  :font-ligatures '("<!--" "-->" "</>" "</" "/>" ; Fira Code
                    "://"))

(let ((oob-dir '"./ebullient/home/doom-private"))
(let ((oob-dir (concat "~/projects/phrenetic/" oob-dir))) ; TODO currently `meta-get-dir' returns a path relative to the phrenetic dir. That might change but regardless don't hardcode the path here eventually.
  (let* ((file "out-of-band"))
    (or (load (concat oob-dir "/" file) 'noerror 'nomessage)
        (load! file 'noerror))))
)
