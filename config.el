;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Krzysztof Baranowski"
      user-mail-address "pharcosyle@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode' ; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Source Code Variable" :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default.
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.





(setq mac-option-modifier 'control
      mac-control-modifier 'meta
      mac-command-modifier 'hyper)


(defun trans-key! (to from)
  (define-key key-translation-map (kbd to) (kbd from)))

;; (trans-key! "H-n" "ESC")
(define-key key-translation-map (kbd "H-n") [escape]) ; TODO this doesn't behave like ESC when quitting avy mode so maybe not other places either. Maybe do it better.

(trans-key! "H-k" "RET")

(trans-key! "M-h" "<left>")
(trans-key! "M-j" "<down>")
(trans-key! "M-k" "<up>")
(trans-key! "M-l" "<right>")

(trans-key! "H-j" "C-d")
(trans-key! "H-k" "C-u")

;; (trans-key! "H-h" "z H") ; oops cmd-h is mac hide
;; (trans-key! "H-l" "z L")

(after! avy
  (setq avy-all-windows t))

;; I probably want this but try without first. Add the appropriate `after!' or whatever.
;; (setq evil-multiedit-follow-matches t)

(add-to-list 'safe-local-variable-values '(cider-clojure-cli-global-options . nil))
(add-to-list 'safe-local-variable-values '(eval . (setenv "DATOMIC_APP_INFO_MAP" "{:app-name \"neutrino\"}")))
(add-to-list 'safe-local-variable-values '(eval . (setenv "DATOMIC_ENV_MAP" "{:env :dev}")))



(setq initial-frame-alist '((width . 196) (fullscreen . fullheight)))


;; (after! magit
;;   (setq magit-save-repository-buffers nil
;;         ;; git-commit-style-convention-checks nil))


;; The extra-font-locking code won't get run more than once, right?
(use-package! clojure-mode-extra-font-locking
  :after clojure-mode)
