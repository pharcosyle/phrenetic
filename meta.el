;;; -*- lexical-binding: t; -*-

(require 'dash)

(setq meta--base-dir ".")

(defun meta-get-dir ()
  (concat meta--base-dir
          (when-let ((dirs (org-entry-get (point) "meta-dir" 'inherit)))
            (-> dirs (split-string " /") (string-join "/")))))

(defun meta-in-dir (sub-path)
  (concat (meta-get-dir) "/" sub-path))
