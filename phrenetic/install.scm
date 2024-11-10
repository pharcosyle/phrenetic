(define-module (phrenetic install)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((gnu packages cryptsetup) #:select (cryptsetup))
  #:use-module ((gnu packages disk) #:select (dosfstools parted))
  #:use-module ((gnu packages linux) #:select (btrfs-progs util-linux))
  #:use-module ((gnu packages package-management) #:select (guix))
  #:use-module (phrenetic utils)
  #:export (installer-package))

(define* (installer-package #:rest opts)
  (package
    (inherit simple-package)
    (name "installer")
    (source (apply installer-program opts))
    (arguments
     `(#:builder
       ,#~(begin
            (let ((bin (string-append #$output "/bin")))
              (mkdir #$output)
              (mkdir bin)
              (symlink #$source (string-append bin "/install"))))))))

(define* (installer-program #:key
                            os-name
                            efi?
                            boot-label
                            boot-target
                            root-label
                            disk-encryption?
                            luks-label
                            filesystem
                            mountables
                            btrfs-mount-flags
                            btrfs-mount-options)
  (program-file
   "installer"
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules ((guix build utils) #:select (invoke mkdir-p))
                      ((ice-9 match) #:select (match-lambda))
                      ((ice-9 readline) #:select (readline))
                      ((ice-9 string-fun) #:select (string-replace-substring)))

         (define* (cmd command #:rest args)
           (display (string-join (cons (basename command) args)))
           (newline)
           (apply invoke (cons command args)))

         (define* (read-user-input prompt #:key default)
           (let ((input (readline (string-append prompt
                                                 (if default
                                                     (string-append " (default " default ")") "")
                                                 ": "))))
             (if (string-null? input)
                 (or default (exit))
                 input)))

         (define (user-pause text)
           (readline (string-append text " Press enter to continue.")))

         (define (display-block-devices)
           (display "Block devices:")
           (newline)
           (invoke #$(file-append util-linux "/bin/lsblk"))
           (newline))

         (display-block-devices)
         (define device
           (read-user-input "Enter device (for exmple, /dev/sda)"))

         (let ((parted-bin #$(file-append parted "/sbin/parted"))
               (align "optimal")
               (boot-size "1GB"))
           (cmd parted-bin device "mklabel" "gpt")
           (for-each (match-lambda
                       ((label start end)
                        (cmd parted-bin "--align" align device "mkpart" label start end)))
                     `(("sky" "0%" ,boot-size)
                       ("earth" ,boot-size "100%")))
           (cmd parted-bin device "set" "1" (if #$efi? "esp" "bios_grub") "on")
           (for-each (lambda (n)
                       (cmd parted-bin device "align-check" align n))
                     '("1" "2")))
         (user-pause "Check that disks are aligned.")

         (display-block-devices)
         (define boot-partition
           (read-user-input (string-append "Boot partition")
                            #:default (string-append device "1")))
         (define root-partition
           (read-user-input (string-append "Root partition")
                            #:default (string-append device "2")))

         (cmd #$(file-append dosfstools "/sbin/mkfs.fat") "-F" "32" "-n" #$boot-label boot-partition)

         (define system-root
           (if #$disk-encryption?
               (let* ((crypt-name "deciphered-new")
                      (crypt-mapping (string-append "/dev/mapper/" crypt-name))
                      (cryptsetup-bin #$(file-append cryptsetup "/bin/cryptsetup")))
                 (cmd cryptsetup-bin "luksFormat" "--type" "luks2" "--pbkdf" "pbkdf2" "--label" #$luks-label root-partition)
                 (cmd cryptsetup-bin "open" root-partition crypt-name)
                 crypt-mapping)
               root-partition))

         (case #$filesystem
           ;; ((#:filesystem/ext4)
           ;;  (cmd #$(file-append e2fsprogs "/sbin/mkfs.ext4") "-L" #$root-label system-root))
           ((#:filesystem/btrfs)
            (cmd #$(file-append btrfs-progs "/bin/mkfs.btrfs") "--label" #$root-label system-root)))

         (define mount-dir
           (read-user-input (string-append "Mount directory")
                            #:default "/mnt"))

         (define mount-bin "/run/privileged/bin/mount")
         (define umount-bin "/run/privileged/bin/umount")

         (when (equal? #$filesystem #:filesystem/btrfs)
           (let ((btrfs-bin #$(file-append btrfs-progs "/bin/btrfs")))
             (cmd mount-bin system-root mount-dir)
             (for-each (match-lambda
                         ((subvol . _)
                          (cmd btrfs-bin "subvolume" "create" (string-append mount-dir "/" subvol))))
                       '#$mountables)
             (cmd umount-bin mount-dir)))

         (case #$filesystem
           ;; ((#:filesystem/ext4)
           ;;  (cmd mount-bin system-root mount-dir))
           ((#:filesystem/btrfs)
            (for-each
             (match-lambda
               ((subvol . mount-point)
                (let ((path-on-mount (string-append mount-dir mount-point)))
                  (mkdir-p path-on-mount)
                  (cmd mount-bin "-o"
                       (string-append
                        "subvol=" subvol
                        (let ((opts (append
                                     (or '#$btrfs-mount-options '())
                                     (map (lambda (flag)
                                            (string-replace-substring (symbol->string flag) "-" ""))
                                          (or '#$btrfs-mount-flags '())))))
                          (if (null? opts)
                              ""
                              (string-append "," (string-join opts ",")))))
                       system-root path-on-mount))))
             '#$mountables)))
         (let ((boot-target-on-mount (string-append mount-dir #$boot-target)))
           (mkdir-p boot-target-on-mount)
           (cmd mount-bin boot-partition boot-target-on-mount))

         (let ((source-dir "/etc/config-source")
               (configs-dir "phrenetic/configs"))
           (cmd #$(file-append guix "/bin/guix") "time-machine"
                (string-append "--channels=" source-dir "/" configs-dir "/" os-name "-channels-lock")
                "--"
                "system"
                (string-append "--load-path=" source-dir)
                "init"
                (string-append source-dir "/" configs-dir "/" #$os-name)
                mount-dir))

         (cmd umount-bin (string-append mount-dir #$boot-target))
         (case #$filesystem
           ;; ((#:filesystem/ext4)
           ;;  (cmd umount-bin mount-dir))
           ((#:filesystem/btrfs)
            (for-each (match-lambda
                        ((_ . mount-point)
                         (cmd umount-bin (string-append mount-dir mount-point))-point))
                      '#$mountables)))))))
