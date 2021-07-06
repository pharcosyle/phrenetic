;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! dash :pin "3bd52a45aa81a3aab0d02ece800042415669399a")
(package! expand-region :pin "530a593f5ee06280e26320967700a6292c5fef2e")
;; TODO this should probably be specific to the krush/hyperdrive/afterburner(?) project
;; (package! graphql-mode :pin "2371316a750b807de941184d49ca19d277ecadcd")
;; TODO was causing weird errors with company-capf in eshell, just start typing "guix search [...]" to reproduce
;; (package! guix :pin "c9aef52121b458297e70bb50f49f7276b4a8d759")
(package! tldr :pin "d59405bd72f3379417b9e73f06e8848b43cb021d")
(package! trashed :pin "23e782f78d9adf6b5479a01bfac90b2cfbf729fe")
(package! symon :pin "8dd8b6df49b03cd7d31b85aedbe9dd08fb922335")
