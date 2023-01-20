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
