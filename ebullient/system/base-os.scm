(define-module (ebullient system base-os)
  #:use-module (ebullient system components)
  #:use-module (ebullient utils)
  #:export (base-os))

(define* (base-os #:key
                  host-name
                  timezone
                  locale
                  accounts
                  auto-login
                  disk-encryption?
                  luks-uuid
                  stateless?
                  nix?
                  console?
                  desktop?
                  pipewire?
                  bluetooth?
                  sessions
                  laptop?
                  macbook?
                  (kbl (if macbook?
                           macbook-kbl #f)))
  (-> barebones-os
      (host-info #:host-name host-name
                 #:timezone timezone
                 #:locale locale)
      grub-efi
      base-services
      nss
      (users #:who accounts
             #:bluetooth? bluetooth?)
      nonguix-substitutes
      linux-nonfree
      (as-> $
        (if nix?
            (nix $) $)
        (if console?
            (-> $
                (console-keyboard-layouts #:keyboard-layout kbl)
                hidpi-console-font)
            $)
        (if desktop?
            (-> $
                (desktop-services #:pipewire? pipewire?
                                  #:bluetooth? bluetooth?)
                (gdm #:auto-login (if disk-encryption?
                                      auto-login #f))
                (as-> $
                  (if (member #:session/gnome sessions)
                      (gnome-desktop $) $)
                  (if (member #:session/sway sessions)
                      (sway-wm $ #:laptop? laptop?) $)))
            $)
        (if macbook?
            (macbook-wireless $ #:bluetooth? bluetooth?) $)
        (let* ((persist-dir stateless-persist-dir)
               (subvols (btrfs-subvols #:stateless? stateless?
                                       #:persist-dir persist-dir
                                       #:nix? nix?)))
          (-> $
              (btrfs #:subvols subvols)
              (as-> $
                (if disk-encryption?
                    (disk-encryption $ #:source-uuid luks-uuid
                                     #:encrypted-mount-points (map cdr subvols))
                    $)
                (if stateless?
                    (stateless $ #:persist-dir persist-dir
                               #:bluetooth? bluetooth?)
                    $)))))))
