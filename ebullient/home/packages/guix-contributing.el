(defvar guix-contributing-source-path "~/src/guix")

(with-eval-after-load 'yasnippet
  (add-to-list 'yas-snippet-dirs (concat guix-contributing-source-path "/etc/snippets")))

(load-file (concat guix-contributing-source-path "/etc/copyright.el"))

(when (and user-full-name
           user-mail-address)
  (setq copyright-names-regexp (format "%s <%s>" user-full-name user-mail-address)))

(provide 'guix-contributing)
