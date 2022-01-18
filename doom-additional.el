;; Doesn't work yet: you have to switch to the buffer "manually" once before it starts being treated as real
(map! "s-d m" (cmd! (doom-set-buffer-real (current-buffer) t)))

;; Temporary until I get semicolon working
(map! (:map minibuffer-local-map
       "C-:" #'embark-act
       "C-c C-:" #'embark-export)
      (:leader
       "A" #'embark-dwim)
      ;; Trying out cycling (temp)
      "C-:" #'embark-act
      "s-q" #'embark-cycle)

;; `forward-char' in original definition is messing things up, do this for now
;; (defun +eshell/search-history ()
;;   (interactive)
;;   (consult-history))
(after! esh-mode
  (map! :map eshell-mode-map
        "C-s" #'consult-history))




;; Probably move these to `after!' blocks
(set-ligatures! '(html-mode nxml-mode web-mode)
  :font-ligatures '("<!--" "-->" "</>" "</" "/>" ; Fira Code
                    "://"))




;;;; Keybinding ideas

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






;; Temporary copypasta from bloodhound, for convenience
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




;; Remove the :mode set by the Doom module. It's unnecessary (`guix-drv-mode.el' already does this, and for /nix/store drvs specifically) and it shadows `guix-derivation-mode'.
;; TODO should this go in the Guix or Nix config section?
(use-package! nix-drv-mode
  :defer t
  :init
  (setq auto-mode-alist (delete '("\\.drv\\'" . nix-drv-mode) auto-mode-alist)))



(after! vertico
  (setq vertico-count 20  ; Trying out, maybe too big.
        vertico-scroll-margin 7))
