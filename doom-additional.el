;; Doesn't work yet: you have to switch to the buffer "manually" once before it starts being treated as real
(map! "s-d m" (cmd! (doom-set-buffer-real (current-buffer) t)))

;; Get rid of binding for =s-r=?
(map! "s-o" (lookup-key doom-leader-map (kbd "<")))

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

;; If I keep this I can get rid of the "C-_" binding I have too
(biome--trans "s-h" "C-h")

;; `forward-char' in original definition is messing things up, do this for now
;; (defun +eshell/search-history ()
;;   (interactive)
;;   (consult-history))
(after! esh-mode
  (map! :map eshell-mode-map
        "C-s" #'consult-history))
