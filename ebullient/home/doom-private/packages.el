;; -*- no-byte-compile: t; -*-

(package! dash :pin "0ac1ecf6b56eb67bb81a3cf70f8d4354b5782341") ; TODO make this be introduced by my doom config (where I need it)?
(package! expand-region :pin "7e5bbe2763c12bae3e77fe0c49bcad05ff91dbfe")
(package! tldr :pin "d3fd2a809a266c005915026799121c78e8b358f0")
(package! trashed :pin "ddf5830730544435a068f2dc9ac75a81ea69df1d")

(package! guix :pin "c9aef52121b458297e70bb50f49f7276b4a8d759")
(package! build-farm :pin "5c268a3c235ace0d79ef1ec82c440120317e06f5") ; REVIEW trying out
(package! guix-packaging ; REVIEW trying out
  :recipe (:host github
           :repo "ryanprior/emacs-guix-packaging"
           :files (:defaults "snippets"))
  :pin "5bbd1f1a268b3dfd813a75125ca88cbf0bef6529")

(package! bluetooth :pin "3a3c3c04437518f1ce0e940adf4a5e020b9755c3") ; TODO just maybe make this dependent on my "bluetooth feature"

(package! symon :pin "8dd8b6df49b03cd7d31b85aedbe9dd08fb922335")

(package! org-tanglesync :pin "af83a73ae542d5cb3c9d433cbf2ce1d4f4259117")

(package! i3wm-config-mode :pin "3574d88241118ed6cc5a3022b6dde58d6f5af9dd") ; TODO maybe make this part of a Sway / Window Manager feature or introduced by my Dotfiles project

;; REVIEW Breaks stuff after I updated to emacs 29 latest:
;; Compiling EmacSQL SQLite binary ...
;; Code Review initialization: (error "No EmacSQL SQLite binary available, aborting")
(package! code-review :disable t)

(package! app-launcher
  :recipe (:host github
           :repo "SebastienWae/app-launcher")
  :pin "d5015e394b0a666a8c7c4d4bdf786266e773b145")

(package! 0x0 :pin "63cd5eccc85e527f28e1acc89502a53245000428")
