;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       company
       (ivy +icons)

       :ui
       doom
       doom-dashboard
       doom-quit
       (emoji +unicode)
       hl-todo
       hydra
       indent-guides
       modeline
       nav-flash
       ophints
       (popup +defaults +all)
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
       syntax

       :tools
       direnv
       (eval +overlay)
       lookup
       (magit +forge)
       (pass +auth)
       pdf
       prodigy
       rgb

       :os
       (:if IS-MAC macos)

       :lang
       clojure
       data
       emacs-lisp
       json
       javascript
       markdown
       nix
       org
       rest
       sh
       web
       yaml

       :app
       calendar

       :config
       (default +bindings +smartparens))



(use-package-hook! clojure-mode
  :post-init
  (setq clojure-refactor-map-prefix (kbd "s-M r"))) ; Has to be set before clojure-mode laods so don't put this in a hook. ; TODO temporary binding

(use-package-hook! lispyville
  :post-init
  (setq lispyville-key-theme nil))
