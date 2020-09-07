;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(use-package! dash)


(setq user-full-name "Krzysztof Baranowski"
      user-mail-address "pharcosyle@gmail.com")

;; (setq org-directory "~/org/")           ; Must be set before org loads.



;;;; Doom resets

(setq-default indent-tabs-mode t
              word-wrap nil

              ;; I suspect these (and the default `visual-line-mode' for text-modes Doom does) will cause slowdowns in large files, I may want to remove them (selectively or altogether).
              truncate-lines nil
              truncate-partial-width-windows 50)

;; I'd like to have this on but in the doom code it says it's more efficient not to.
;; (setq-default cursor-in-non-selected-windows t)



(setq scroll-margin 10
      save-interprogram-paste-before-kill t)

;; I like having line numbers on but hlissner says they're slow so I might want to disable them at some point. Keep in mind I use them to determine what lines are continuation lines so I might have to make the right fringe bigger if I do this and set visual-line-fringe-indicators.
;; (setq display-line-numbers-type nil)



(setq doom-localleader-key "s-m"
      doom-localleader-alt-key "s-m")

(defun trans! (&rest rest)
  (-each (-partition 2 rest)
    (-lambda ((to from))
      (define-key key-translation-map (kbd to) (kbd from)))))

(trans! "s-n" "<escape>"

        "s-h" "<left>"
        "s-j" "<down>"
        "s-k" "<up>"
        "s-l" "<right>"

        "s-i" "RET"
        "s-o" "<tab>"
        "s-O" "<backtab>"

        "s-e" "C-x C-e"
        "s-E" "C-M-x"
        "s-." "C-x z"
        "s->" "C-x ESC ESC")            ; Try using this where simple emacs `repeat` fails but I doubt it'll prove useful.

(defalias 'original-yank-pop #'yank-pop)

(map! "s-V" #'original-yank-pop

      "s-t" (lookup-key doom-leader-map (kbd "`"))
      "s-u" (lookup-key doom-leader-map (kbd "u"))
      "s-f" (cl-flet ((f (lookup-key doom-leader-map (kbd "s b"))))
              ;; `swiper' hangs initially when `visual-line-mode' is active. Plus doom defaults to having `visual-line-mode' enabled in text-mode (and derived) buffers where it makes more sense to not search linewise.
              (cmd! (if visual-line-mode
                        (letf! ((#'swiper #'swiper-isearch))
                          (f))
                      (f))))
      "s-r" (lookup-key doom-leader-map (kbd "f r"))
      "s-w" (lookup-key doom-leader-map (kbd "b k"))
      "s-W" (lookup-key doom-leader-map (kbd "w d"))
      "s-M-w" (cmd! (kill-current-buffer) (+workspace/close-window-or-workspace))
      "s-g" (lookup-key doom-leader-map (kbd "g g"))
      "s-," (lookup-key doom-leader-map (kbd "w w"))
      "s-{" (lookup-key doom-leader-map (kbd "b p"))
      "s-}" (lookup-key doom-leader-map (kbd "b n"))
      (:after evil-easymotion
       "s-a" (lookup-key evilem-map (kbd "SPC")))

      "s-J" #'evil-scroll-down
      "s-K" #'evil-scroll-up

      ;; These should be in `:after' (or their respective package) sections but I'm not certain how I want to do them yet and I'm lazy.
      (:prefix "s-d"
       "d" #'git-gutter:popup-hunk
       "f" #'org-gcal-fetch
       "o" #'+macos/open-in-default-program
       "t" #'tldr))



(after! avy
  (setq avy-single-candidate-jump t))

(setq-hook! emacs-lisp-mode indent-tabs-mode nil)

(after! evil-org
  (map! :map evil-org-mode-map
        :nv "C-i" #'evil-jump-forward)) ; evil-org overrides this, restore it.

(add-hook! cider-repl-mode
           #'goto-address-prog-mode
           #'highlight-numbers-mode
           #'rainbow-delimiters-mode
           #'yas-minor-mode-on
           #'lispy-mode)

(after! cider
  (setq cider-repl-history-size 1000000
        cider-print-options '(("length" 100))))

(add-hook! clj-refactor-mode
  (cljr-add-keybindings-with-prefix "s-C-.")) ; TODO temporary binding

(setq clojure-refactor-map-prefix (kbd "s-C-,")) ; Has to be set before clojure-mode laods so don't put this in a hook. ; TODO temporary binding

(after! evil-multiedit
  (setq evil-multiedit-follow-matches t))

(remove-hook! (prog-mode text-mode conf-mode) #'highlight-indent-guides-mode)

(after! ivy
  (setq +ivy-buffer-preview t
        ivy-extra-directories nil)

  (map! :map ivy-minibuffer-map
        "s-J" #'ivy-scroll-up-command
        "s-K" #'ivy-scroll-down-command
        "<left>" (cmd! (if (and ivy--directory (= (minibuffer-prompt-end) (point)))
                           (ivy-backward-delete-char)
                         (left-char)))
        "<right>" (cmd! (if (ivy-alist-setting '((read-file-name-internal . t)))
                            (ivy-alt-done)
                          (right-char)))

        ;; Doom overrides these, restore them.
        "C-k" #'ivy-kill-line
        "C-r" #'ivy-reverse-i-search))

(after! lispy
  (lispy-set-key-theme '(lispy))

  ;; (map! :map lispy-mode-map-lispy
  ;;       "[" #'lispy-brackets)
  )

(after! lispyville
  (lispyville-set-key-theme
   '(c-u
     prettify
     text-objects
     commentary
     slurp/barf-cp
     additional-wrap))
  (setq lispyville-barf-stay-with-closing t)

  (map! :map lispyville-mode-map
        "s-C-j" #'lispyville-forward-sexp
        "s-C-k" #'lispyville-backward-sexp
        "s-C-h" #'lispyville-backward-up-list
        "s-C-l" #'lispyville-up-list
        "s-C-u" #'lispyville-beginning-of-next-defun
        "s-C-i" #'lispyville-beginning-of-defun
        "s-C-o" #'lispyville-end-of-defun))

(after! magit
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))

(after! org
  (setq org-agenda-files `(,org-directory "~/org/projects"  "~/org/gcal")
        org-priority-lowest ?D
        org-priority-faces `((?A . ,(doom-color 'red))
                             (?B . ,(doom-color 'orange))
                             (?C . ,(doom-color 'yellow))
                             (?D . ,(doom-color 'cyan)))))

(after! org-gcal
  (setq org-gcal-client-id "446729771716-pp79934q99aro2h8v3iki1fejcodbdoo.apps.googleusercontent.com"
        org-gcal-client-secret "UdMte0q2B3nMURYY0F1aqNYA"
        org-gcal-fetch-file-alist '(("pharcosyle@gmail.com" . "~/org/gcal/pharcosyle@gmail.com.org")
                                    ("addressbook%23contacts@group.v.calendar.google.com" . "~/org/gcal/contacts.org")
                                    ("en.usa%23holiday@group.v.calendar.google.com" . "~/org/gcal/holidays.org")
                                    ("krzysztof@massrealty.com" . "~/org/gcal/krzysztof@massrealty.com.org"))))

(after! paren
  (setq! show-paren-delay 0))

;; (after! undo-tree
;;   (setq undo-tree-visualizer-timestamps t))



;; Probably be more selective where it's enabled (with an :after and not global-... or aggressive-indent-excluded-modes). Even if I want it everywhere should I put it behind an :after just to defer loading?
;; (use-package! aggressive-indent
;;   :config
;;   (global-aggressive-indent-mode 1))

(use-package! tldr
  :defer t
  :config
  (setq tldr-directory-path (concat doom-etc-dir "tldr/")))



(setq doom-theme 'doom-pharcosyle-atomic
      doom-font (font-spec :family "Source Code Variable" :size 12)
      rainbow-delimiters-max-face-count 8)

(after! evil
  (setq evil-default-cursor (lambda () (evil-set-cursor-color "#fdd94a"))
        evil-emacs-state-cursor (lambda () (evil-set-cursor-color "#ff9999"))))

(custom-theme-set-faces! 'doom-pharcosyle-atomic
  '(font-lock-comment-face :foreground "#63677F")
  '(font-lock-comment-delimiter-face :foreground "#939abd")
  `(font-lock-doc-face :foreground ,(doom-color 'cyan))
  ;; `(line-number :inherit 'default :foreground ,(doom-color 'base5) :distant-foreground nil :weight normal :italic nil :underline nil :strike-through nil)
  `(line-number-current-line :inherit (hl-line default) :foreground "#AEB9F3" :distant-foreground nil :weight normal :italic nil :underline nil :strike-through nil)
  `(rainbow-delimiters-depth-1-face :foreground ,(doom-color 'fg))
  `(rainbow-delimiters-depth-2-face :foreground ,(doom-color 'magenta))
  `(rainbow-delimiters-depth-3-face :foreground ,(doom-color 'blue))
  `(rainbow-delimiters-depth-4-face :foreground ,(doom-color 'cyan))
  `(rainbow-delimiters-depth-5-face :foreground ,(doom-color 'green))
  `(rainbow-delimiters-depth-6-face :foreground ,(doom-color 'yellow))
  `(rainbow-delimiters-depth-7-face :foreground ,(doom-color 'orange))
  `(rainbow-delimiters-depth-8-face :foreground ,(doom-color 'red))
  `(clojure-interop-method-face :foreground ,(doom-color 'cyan))
  `(clojure-character-face :foreground ,(doom-color 'violet) :weight bold))



;; Can I move this line up without the "weird sizing things" my old config referrred to? Do I care?
;; - Update: Probably do this instead, it'll work on additional frames and presumably avoid "weird sizing things": https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-do-i-maximizefullscreen-emacs-on-startup
;; (toggle-frame-fullscreen)
(setq initial-frame-alist '((width . 194) (fullscreen . fullheight)))
(set-frame-position (selected-frame) 66 23)



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

(pushnew! safe-local-variable-values
          '(cider-preferred-build-tool . shadow-cljs)
          '(cider-default-cljs-repl . shadow)
          '(cider-shadow-default-options . ":app")
          '(cider-offer-to-open-cljs-app-in-browser . nil)
          '(cider-clojure-cli-global-options . "-A:dev")
          '(eval . (setenv "DATOMIC_APP_INFO_MAP" "{:app-name \"neutrino\"}"))
          '(eval . (setenv "DATOMIC_ENV_MAP" "{:env :dev}"))
          '(cider-clojure-cli-global-options . nil)

          '(ssh-deploy-root-remote . "/ssh:massrealty@35.196.144.73:/home/massrealty/deploy/homes/public_html/")
          '(ssh-deploy-automatically-detect-remote-changes . t))
