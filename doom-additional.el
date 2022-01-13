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
      "s-q" #'embark-cycle
      ;; Override my guix binding (temp)
      (:leader
       :desc "Actions" "a" #'embark-act))

;; `forward-char' in original definition is messing things up, do this for now
;; (defun +eshell/search-history ()
;;   (interactive)
;;   (consult-history))
(after! esh-mode
  (map! :map eshell-mode-map
        "C-s" #'consult-history))




(setq +ligatures-all-modes-list
      '("www")
      +ligatures-prog-mode-list
      '(;; Fira Code (from https://github.com/tonsky/FiraCode/wiki/Emacs-instructions#using-ligatureel)
        "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
        ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
        "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
        "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
        "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
        "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
        "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
        "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
        "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
        "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"
        ;; Doom defaults (the ones not already present in Fira Code)
        "|||>" "<|||" "<==>" "||>" "::=" "=!=" "!!." ">->" "---" "<~>"
        "<||" "<-<" "_|_" "~=" "|}" "|]" "|-" "{|" "[|" "]#" ":>" ":<"
        ">:" "-|" "--" "<:" "#:" "#=" "#!" ".?" "?:" "?." "__" "(*"
        "*)" "://"))

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

;; Get rid of stuff in dotfiles without changing dotfiles for the moment
(define-key key-translation-map (kbd "s-i") nil)
(define-key key-translation-map (kbd "s-I") nil)

;; Maybe "C-s-i" but then I'll have to change my sexp-movement command(s). Maybe keep this even if I do that, for consistency.
(biome--trans "C-S-i" "S-TAB")
;; Also messes with my sexp-movement bindings
;; (map! "C-s-j" #'evil-scroll-down
;;       "C-s-k" #'evil-scroll-up)

(map! :m [C-i] nil) ; Remove Doom's binding for `evil-jump-forward'.
(after! evil
  (map! :map evil-motion-state-map
        ;; Maybe "C-s-o" but then I'll have to change my sexp-movement command(s). There are other bindings for jump-forward though, maybe I'll just use those.
        "C-S-o" #'evil-jump-forward))

;; Maybe `s-S' to save-and-tangle? If I don't get some sort of auto-tangling thing going
