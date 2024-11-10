(define-module (phrenetic build stateless)
  #:use-module ((guix build utils) #:select (copy-recursively delete-file-recursively find-files mkdir-p store-file-name? symbolic-link?))
  #:use-module (phrenetic build utils)
  #:use-module ((ice-9 ftw) #:select (file-system-fold))
  #:use-module ((srfi srfi-1) #:select (any))
  #:use-module ((ice-9 match) #:select (match-lambda))
  #:export (activate
            tool))

(define (activate state
                  backup-base-dir
                  storage-paths)
  (define (run)
    (for-each process-state-entry state))

  (define (process-state-entry i)
    (let* ((path (assoc-ref i #:path))
           (directory-target? (string-suffix? "/" path))
           (target (if directory-target?
                       (string-drop-right path 1) path))
           (storage-dir (assoc-ref storage-paths (assoc-ref i #:storage))))
      (let ((parent-dir (dirname path))
            (parent-dir-perms (assoc-ref i #:parent-dir-perms)))
        (mkdirp-perms (string-append storage-dir parent-dir) parent-dir-perms)
        (mkdirp-perms parent-dir parent-dir-perms))
      (let ((state-file (string-append storage-dir target)))
        (cleanup-and-migrate target state-file storage-dir)
        (when directory-target?
          (mkdirp-perms state-file i))
        (when (or (no-follow-file-exists? state-file)
                  (assoc-ref i #:preemptively?))
          (symlink state-file target)))))

  (define (cleanup-and-migrate target state-file storage-dir)
    (let ((migrate-file
           (lambda (source dest)
             (copy-recursively source dest)
             (delete-file-recursively source)))
          (backup-file
           (lambda (file)
             (let* ((backup-dir (string-append
                                 storage-dir
                                 backup-base-dir
                                 "/stateless-backup-" (number->string timestamp)))
                    (backup-file (string-append
                                  backup-dir "/"
                                  (let* ((string-drop-prefix (lambda (s prefix)
                                                               (string-drop s (string-length prefix))))
                                         (without-storage-dir (lambda (file)
                                                                (string-drop-prefix file storage-dir)))
                                         (without-base-dir (lambda (file)
                                                             (string-drop-prefix file backup-base-dir))))
                                    ((compose without-base-dir
                                              without-storage-dir)
                                     file)))))
               (mkdir-p (dirname backup-file))
               (rename-file file backup-file)))))
      (when (no-follow-file-exists? target)
        (if ((symlink-to-storage? (map (match-lambda ((_ . dir) dir))
                                       storage-paths))
             target)
            (delete-file target)
            (begin
              (when (no-follow-file-exists? state-file)
                (if (nil? (find-files state-file)) ; TODO did I have `nil?` instead of `null?` for some reason or was it just an accident? Update: remember nil? and null? are different (empyt list and #f ar false vs just the empty list)
                    (delete-file-recursively state-file)
                    (backup-file state-file)))
              (migrate-file target state-file))))))

  (define (mkdirp-perms dir perms)
    (mkdir-p dir)
    (chown dir
           (let ((user (assoc-ref perms #:user)))
             (if user
                 (passwd:uid (getpwnam user)) -1))
           (let ((group (assoc-ref perms #:group)))
             (if group
                 (group:gid (getgrnam group)) -1)))
    (let ((mode (assoc-ref perms #:mode)))
      (when mode
        (chmod dir mode))))

  (define timestamp (current-time))

  (run))

;;;;;;;;;; TODO TEMPORRARY REMOVE ME
;; (define-module (phrenetic build stateless)
;;   #:use-module ((guix build utils) #:select (copy-recursively delete-file-recursively find-files mkdir-p store-file-name? symbolic-link?))
;;   #:use-module (phrenetic build utils)
;;   #:use-module ((ice-9 ftw) #:select (file-system-fold))
;;   #:use-module ((srfi srfi-1) #:select (any))
;;   #:export (activate
;;             tool))
;;;;;;;

(use-modules ;; ((guix build utils) #:select (find-files))
 ((srfi srfi-1) #:select (append-map every partition))
 ((srfi srfi-11) #:select (let-values))
 ((ice-9 ftw) #:select (scandir))
 ((ice-9 match) #:select (match match-lambda))
 ((ice-9 textual-ports) #:select (get-string-all)))


(define* (tool state
               ignore
               base-dir
               storage-paths)
  (let ((state* (normalize-entries state))
        (all-ignore (normalize-entries ignore)))
    (for-each
     (lambda (path)
       (display (string-append path "\n")))
     (case (run-mode)
       ((active) (diff-live state*
                            (filter (lambda (entry)
                                      (not (assoc-ref entry #:storage)))
                                    all-ignore)
                            base-dir
                            storage-paths))
       ((storage) (diff-storage (map (match-lambda ((_ . dir) dir))
                                     storage-paths)
                                state*
                                (filter (lambda (entry)
                                          (assoc-ref entry #:storage))
                                        all-ignore)
                                base-dir
                                storage-paths))))))

(define (run-mode)
  (string->symbol
   (or
    (false-if-exception (cadr (command-line)))
    ;; "active"
    "storage" ; TODO temporary for testing
    )))

(define (diff-live state
                   ignore
                   base-dir
                   storage-paths)
  (let-values (((ignore-files ignore-dirs) (files+dirs ignore)))
    (diff base-dir
          (append (state->known-symlinks state storage-paths)
                  (ignore->known ignore-files))
          (ignore->known ignore-dirs)
          #:omit-empty-dirs? #t)))

(define (normalize-entries entries)
  (map (lambda (entry)
         (let ((path (assoc-ref entry #:path)))
           (if (string-suffix? "/" path)
               (->> entry
                    (acons #:directory? #t)
                    (acons #:path (string-drop-right path 1)))
               entry)))
       entries))

;; TODO probably just do two filter statements instad of this.
(define (files+dirs entries)
  (partition (lambda (entry)
               (not (assoc-ref entry #:directory?)))
             entries))

(define (state->known-symlinks state storage-paths)
  (map (lambda (entry)
         (let ((path (assoc-ref entry #:path)))
           (cons path `((#:symlink-to ,(string-append
                                        (assoc-ref storage-paths (assoc-ref entry #:storage))
                                        path))))))
       state))

(define (ignore->known ignore)
  (map (lambda (entry)
         (cons (assoc-ref entry #:path)
               (or (assoc-ref entry #:preds)
                   '())))
       ignore))


;; (define (file-entries entries)
;;   (filter (lambda (entry)
;;                (not (assoc-ref entry #:directory?)))
;;              entries))

(define (storage-entries storage entries)
  (filter (lambda (entry)
            (equal? (assoc-ref entry #:storage) storage))
          entries))

(define (entries-with-storage-dir storage-dir entries)
  (map (lambda (entry)
         (acons #:path (string-append storage-dir (assoc-ref entry #:path)) entry))
       entries))

(define (diff-storage storages
                      state
                      ignore
                      base-dir
                      storage-paths)
  (let ((diff* ; TODO rename this
         (lambda (storage)
           (let ((storage-dir (assoc-ref storage-paths storage)))
             (let-values (((state-files state-dirs) (files+dirs state))
                          ((ignore-files ignore-dirs) (files+dirs ignore)))
               (diff (string-append storage-dir base-dir)
                     (append (->> state-files
                                  (storage-entries storage)
                                  (entries-with-storage-dir storage-dir)
                                  (map (lambda (entry)
                                         (cons (assoc-ref entry #:path) '()))))
                             (->> ignore-files
                                  (storage-entries storage)
                                  (entries-with-storage-dir storage-dir)
                                  ignore->known))
                     (append (->> state-dirs
                                  (storage-entries storage)
                                  (entries-with-storage-dir storage-dir)
                                  (map (lambda (entry)
                                         (cons (assoc-ref entry #:path) '()))))
                             (->> ignore-dirs
                                  (storage-entries storage)
                                  (entries-with-storage-dir storage-dir)
                                  ignore->known))))))))
    (append-map diff* storages)))

(define* (diff directory known-files known-dirs #:key omit-empty-dirs?)
  (as-> directory $
        (let* ((test (lambda (path known)
                       (let ((cand (assoc path known)))
                         (and cand
                              (every (match-lambda
                                       ((pred-name args ...)
                                        (apply (assoc-ref predicates pred-name) (cons path args))))
                                     (match cand ((_ . preds) preds))))))))
          (file-system-fold
           (let ((known-paths (map (match-lambda ((path . _) path))
                                   (append known-files known-dirs)))
                 (known-dir-paths (map (match-lambda ((path . _) path))
                                       known-dirs)))
             (fn (lambda (dir _ _*)
                   (and (not (member dir known-dir-paths))
                        (any (lambda (known-path)
                               (string-prefix? dir known-path))
                             known-paths)))))
           (fn (lambda (file _ res)
                 (if (test file known-files)
                     res
                     (cons file res))))
           (fn (lambda (_ _* res)
                 res))
           (fn (lambda (_ _* res)
                 res))
           (fn (lambda (dir _ res)
                 (if (or (test dir known-dirs)
                         (and omit-empty-dirs?
                              (null? (scandir-no-implied dir))))
                     res
                     (cons (string-append dir "/") res))))
           (lambda (path _ errno res) ; TODO use `fn' for this.
             (format (current-error-port) "~a: ~a~%" path (strerror errno))
             res)
           '()
           $))
        (sort $ string<?)))

(define (fn f)
  (lambda (path _ res)
    (let ((asdf (if (string-prefix? "//" path)
                    (string-drop path 1)
                    path)))
      (f asdf _ res))))

(define (scandir-no-implied dir)
  (scandir dir (negate dot-or-dot-dot?)))

(define (dot-or-dot-dot? file)
  (member file '("." "..")))

(define predicates
  `((#:symlink-to . ,(lambda (path to)
                       (and (symbolic-link? path)
                            (equal? (readlink path) to))))
    (#:symlink-to-store . ,(lambda (path)
                             (and (symbolic-link? path)
                                  (store-file-name? (readlink path)))))
    (#:empty-file . ,(lambda (path)
                       (zero? (-> path stat stat:size))))
    (#:empty-dir . ,(lambda (path)
                      (null? (scandir-no-implied path))))
    (#:file-content . ,(lambda (path content)
                         (let ((cont (false-if-exception
                                      (call-with-input-file path
                                        get-string-all))))
                           (and cont
                                (equal? cont content)))))))

(define test-state '(((#:path . "/etc/NetworkManager/system-connections/")
                      (#:storage . #:storage/machine))
                     ((#:path . "/var/lib/NetworkManager/")
                      (#:storage . #:storage/machine)
                      (#:mode . 448))
                     ((#:path . "/etc/machine-id")
                      (#:storage . #:storage/machine))
                     ((#:path . "/var/lib/upower/")
                      (#:storage . #:storage/machine))
                     ((#:path . "/root/.cache/nix/")
                      (#:storage . #:storage/machine))
                     ((#:path . "/etc/guix/signing-key.pub")
                      (#:storage . #:storage/machine)
                      (#:parent-dir-perms
                       (#:mode . 73)))
                     ((#:path . "/etc/guix/signing-key.sec")
                      (#:storage . #:storage/machine)
                      (#:parent-dir-perms
                       (#:mode . 73)))
                     ((#:path . "/root/.cache/guix/")
                      (#:storage . #:storage/machine))
                     ((#:path . "/var/lib/random-seed")
                      (#:storage . #:storage/machine))
                     ((#:path . "/var/db/nscd/")
                      (#:storage . #:storage/machine))
                     ((#:path . "/.lastweek")
                      (#:storage . #:storage/machine))))
(define test-ignore '(((#:path . "nix/")
                       (#:storage . #:storage/machine))
                      ((#:path . "/.machine/.swap")
                       (#:storage . #:storage/machine))
                      ((#:path . "/nix/store/"))
                      ((#:path . "/nix/var/nix/"))
                      ((#:path . "/nix/var/log/nix/"))
                      ((#:path . "/var/guix/"))
                      ((#:path . "/etc/guix/acl")
                       (#:preds
                        (#:symlink-to-store)))
                      ((#:path . "/etc/resolv.conf"))
                      ((#:path . "/etc/resolv.conf.bak"))
                      ((#:path . "/bin/sh")
                       (#:preds
                        (#:symlink-to-store)))
                      ((#:path . "/usr/bin/env")
                       (#:preds
                        (#:symlink-to-store)))
                      ((#:path . "/dev/"))
                      ((#:path . "/proc/"))
                      ((#:path . "/run/"))
                      ((#:path . "/sys/"))
                      ((#:path . "/tmp/"))
                      ((#:path . "/var/lock/"))
                      ((#:path . "/var/run/"))
                      ((#:path . "/etc/group"))
                      ((#:path . "/etc/passwd"))
                      ((#:path . "/etc/shadow"))
                      ((#:path . "/etc/.pwd.lock"))
                      ((#:path . "/etc/mtab")
                       (#:preds
                        (#:symlink-to "/proc/self/mounts")))
                      ((#:path . "/etc/ssl")
                       (#:preds
                        (#:symlink-to "/run/current-system/profile/etc/ssl")))
                      ((#:path . "/etc/static")
                       (#:preds
                        (#:symlink-to-store)))))
(define (test)
  (tool test-state
        test-ignore
        "/"
        `((#:storage/machine . "/.machine")
          (#:storage/persist . "/.persist"))))
;; (test)

;; TODO Just used in `activate` now
(define (symlink-to-storage? storage-dirs)
  (lambda (file)
    (and (symbolic-link? file)
         (any (lambda (storage-dir)
                (string-prefix? storage-dir (readlink file)))
              storage-dirs))))
