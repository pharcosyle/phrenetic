(setq meta--dir-property "meta-dir")

(defun meta-get-dir ()
  (when-let ((entries (org-entry-get-with-inheritance meta--dir-property)))
    (thread-first entries
                  (split-string (org--property-get-separator meta--dir-property))
                  (string-join "/"))))

(defun meta-in-dir (sub-path)
  (concat (when-let ((dir (meta-get-dir)))
            (concat dir "/"))
          sub-path))

(defvar meta--gtf-running? nil)

;; Using `org-babel-tangle-collect-blocks' to implement this slows down tangling a small but noticeable amount. It also, unfortunately, performs expansion/evalution of header args, noweb references, etc. so when called in the context of an org-tangle it's almost guaranteed to infinitely recurse, hence the dynamically-bound guard variable (another way to do this would be with `unwind-protect').
(defun meta--get-tangled-files ()
  (when (not meta--gtf-running?)
    (let ((meta--gtf-running? 't))
      (mapcar #'car (org-babel-tangle-collect-blocks)))))

(defun meta-tangled-files->gitignore ()
  (string-join
   ;; HACK
   (mapcar (lambda (f)
             (string-remove-prefix "/home/pharcosyle/keep/phrenetic" f))
           (meta--get-tangled-files))
   "\n"))
