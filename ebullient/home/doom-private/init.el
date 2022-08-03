;;; -*- lexical-binding: t; -*-

(doom! :completion
       (company +childframe)
       (vertico +icons)

       :ui
       doom
       doom-dashboard
       (emoji +unicode)
       hl-todo
       hydra
       indent-guides
       (ligatures +extra)
       modeline
       nav-flash
       ophints
       (popup +defaults +all)
       tabs
       treemacs
       vc-gutter
       vi-tilde-fringe
       window-select
       workspaces

       :editor
       (evil +everywhere)
       file-templates
       fold
       format
       lispy
       multiple-cursors
       rotate-text
       snippets
       word-wrap

       :emacs
       (dired +icons)
       electric
       (ibuffer +icons)
       undo
       vc

       :term
       eshell
       vterm

       :checkers
       (syntax +childframe)

       :tools
       direnv ; TODO should be contributed by direnv component
       (eval +overlay)
       lookup
       (magit +forge)
       (pass +auth)
       pdf
       prodigy ; TODO dependency of hyperdrive. Keep it enabled by default too.
       rgb
       taskrunner
       tree-sitter
       ;; upload ; TODO dependency of massrealty/phucnet

       :lang
       clojure ; TODO contribute this with some Clojure module/compnent /if I end up having one/ (maybe along with the WIP clojure manifest)
       common-lisp ; TODO this should be contributed by Nyxt / my eventual nyxt project
       data
       emacs-lisp
       ;; graphql ; TODO dependency of krush/hyperdrive
       (json +tree-sitter)
       (javascript +tree-sitter)
       markdown
       (nix +tree-sitter) ; TODO this should be gated by Nix inclusion like system components do.
       (org +journal)
       (rest +jq)
       (scheme +guile) ; TODO Should be contributed by some Guix component (also the guix-related packages.el entries and stuff in config)
       (sh +tree-sitter)
       (web +tree-sitter)
       yaml ; TODO this should probably be specific to the krush/hyperdrive/afterburner(?) project

       :app
       calendar

       :config
       (default +bindings +smartparens))

;; (use-package-hook! ligature
;;   :pre-config
;;   (setq +ligatures-all-modes-list
;;         '("www")
;;         +ligatures-prog-mode-list
;;         '(;; Fira Code (from https://github.com/tonsky/FiraCode/wiki/Emacs-instructions#using-ligatureel)
;;           "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
;;           ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
;;           "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
;;           "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
;;           "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
;;           "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
;;           "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
;;           "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
;;           "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
;;           "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"
;;           ;; Doom defaults (just the ones not already present in Fira Code)
;;           "|||>" "<|||" "<==>" "||>" "::=" "=!=" "!!." ">->" "---" "<~>"
;;           "<||" "<-<" "_|_" "~=" "|}" "|]" "|-" "{|" "[|" "]#" ":>" ":<"
;;           ">:" "-|" "--" "<:" "#:" "#=" "#!" ".?" "?:" "?." "__" "(*"
;;           "*)" "://"))
;;   t)
