((nil
  (eval . (with-eval-after-load 'geiser-guile
            (let ((root-dir
                   (file-name-directory
                    (locate-dominating-file default-directory ".dir-locals.el"))))
              (make-local-variable 'geiser-guile-load-path)
              (add-to-list 'geiser-guile-load-path root-dir))))))
