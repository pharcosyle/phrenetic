;;; desktop.el -*- lexical-binding: t; -*-

;; TODO rename efs to biome

(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun efs/exwm-init-hook ()
  (display-battery-mode 1)

  (setq display-time-day-and-date t)
  (display-time-mode 1)
  ;; TODO Also take a look at display-time-format and format-time-string

  (efs/run-in-background "nm-applet")
  ;; (efs/run-in-background "dunst")
  ;; (efs/run-in-background "pasystray")
  ;; (efs/run-in-background "blueman-applet")
  )

(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun efs/exwm-update-title ()
  (pcase exwm-class-name
    ("Chromium-browser" (exwm-workspace-rename-buffer (format "Chromium: %s" exwm-title)))))

(defun biome--shell-cmd (command)
  (start-process-shell-command command nil command))

(use-package exwm
  :config
  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'efs/exwm-update-title)

  ;; When EXWM starts up, do some extra confifuration
  (add-hook 'exwm-init-hook #'efs/exwm-init-hook)

  ;; NOTE: Uncomment the following two options if you want window buffers
  ;;       to be available on all workspaces!

  ;; Automatically move EXWM buffer to current workspace when selected
  ;; (setq exwm-layout-show-all-buffers t)

  ;; Display all EXWM buffers in every workspace buffer list
  ;; (setq exwm-workspace-show-all-buffers t)

  ;; NOTE: Uncomment this option if you want to detach the minibuffer!
  ;; Detach the minibuffer (show it with exwm-workspace-toggle-minibuffer)
  ;;(setq exwm-workspace-minibuffer-position 'top)

  (require 'exwm-randr)
  (exwm-randr-enable)
  (biome--shell-cmd  "xrandr --fb 2728x1800 --output eDP-1 --transform 1,0,-152,0,1,0,0,0,1")

  ;; TODO put this somewhere better
  (biome--shell-cmd  "xkbcomp -I$HOME/org/spring_cleaning/myxkb org/spring_cleaning/myxkb/current_setxkbmap_print $DISPLAY")

  ;; Load the system tray before exwm-init
  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 32) ; daviwil says explicity setting a system tray height can help prevent issues with icons not showing up.
  (exwm-systemtray-enable)

  ;; These keys should always pass through to Emacs
  ;; (setq exwm-input-prefix-keys
  ;;   '(?\C-x
  ;;     ?\C-u
  ;;     ?\C-h
  ;;     ?\M-x
  ;;     ?\M-`
  ;;     ?\M-&
  ;;     ?\M-:
  ;;     ?\C-\M-j  ;; Buffer list
  ;;     ?\C-\ ))  ;; Ctrl+Space
  (setq exwm-input-prefix-keys nil)

  ;; (setq exwm-input-simulation-keys
  ;;         '(([?\C-b] . [left])
  ;;           ([?\C-f] . [right])
  ;;           ([?\C-p] . [up])
  ;;           ([?\C-n] . [down])
  ;;           ([?\C-a] . [home])
  ;;           ([?\C-e] . [end])
  ;;           ([?\M-v] . [prior])
  ;;           ([?\C-v] . [next])
  ;;           ([?\C-d] . [delete])
  ;;           ([?\C-k] . [S-end delete])))
  (setq exwm-input-simulation-keys
        '(([?\s-c] . [C-c])))

  (define-key exwm-mode-map [?\s-\M-q] 'exwm-input-send-next-key) ; TODO see if I can reverse the order of super and meta

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(([?\s-q] . exwm-reset)

          ([?\s-Q] . exwm-input-release-keyboard)

          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ([?\s-t] . evil-switch-to-windows-last-buffer)

          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (biome--shell-command command)))))

  (exwm-input-set-key (kbd "s-A") 'counsel-linux-app)

  (exwm-enable))
