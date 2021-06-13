;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! dash :pin "aab346ed9d8f0f7ea033029c9688810353052e7e")
(package! expand-region :pin "4b8322774d9c1d8b64a0049d1dbbc1e7ce80c1a0")
;; TODO this should probably be specific to the krush/hyperdrive/afterburner(?) project
;; (package! graphql-mode :pin "2371316a750b807de941184d49ca19d277ecadcd")
;; TODO was causing weird errors with company-capf in eshell, just start typing "guix search [...]" to reproduce
;; (package! guix :pin "c9aef52121b458297e70bb50f49f7276b4a8d759")
(package! tldr :pin "d59405bd72f3379417b9e73f06e8848b43cb021d")
(package! trashed :pin "23e782f78d9adf6b5479a01bfac90b2cfbf729fe")
