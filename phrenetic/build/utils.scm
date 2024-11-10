(define-module (phrenetic build utils)
  #:use-module ((guix build syscalls) #:select (mkdtemp!))
  #:use-module ((guix build utils) #:select (delete-file-recursively))
  #:use-module (phrenetic common utils)
  #:export (no-follow-file-exists?
            call-with-temporary-directory))

(re-export-public-interface (phrenetic common utils))


(define (no-follow-file-exists? f)
  (false-if-exception (lstat f)))

;; Copied from (guix utils). Notably (gnu build linux-container) copies this function in like this too.
(define (call-with-temporary-directory proc)
  "Call PROC with a name of a temporary directory; close the directory and
delete it when leaving the dynamic extent of this call."
  (let* ((directory (or (getenv "TMPDIR") "/tmp"))
         (template  (string-append directory "/guix-directory.XXXXXX"))
         (tmp-dir   (mkdtemp! template)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (proc tmp-dir))
      (lambda ()
        (false-if-exception (delete-file-recursively tmp-dir))))))
