;;; desktop.el -*- lexical-binding: t; -*-

;; TODO rename efs to biome

(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun efs/exwm-init-hook ()
  (efs/run-in-background "nm-applet")
  (efs/run-in-background "pasystray")
  ;; (efs/run-in-background "blueman-applet") ; TODO I get an error when running this currently
  (efs/run-in-background (concat "dunst -config " (expand-file-name "~/org/spring_cleaning/exwm_stuff/my-dunstrc"))))

(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun efs/exwm-update-title ()
  (pcase exwm-class-name
    ("Chromium-browser" (exwm-workspace-rename-buffer (format "Chromium: %s" exwm-title))))) ; TODO get rid of preceding or trailing "Chromium"

(defun biome--shell-cmd (command)
  (start-process-shell-command command nil command))

(use-package! exwm
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

  ;; Load the system tray before exwm-init
  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 32) ; daviwil says explicity setting a system tray height can help prevent issues with icons not showing up.
  (exwm-systemtray-enable)

  ;; TODO might one or both of these fix chromium not being focused when I switch to it? What other effects might there be? Note that this doesn't seem to be problem on some other apps like gnome-terminal, perhaps there's I can set up an exwm local hook for chromium or something.
  ;; Window focus should follow the mouse pointer
  ;; (setq mouse-autoselect-window t
  ;;       focus-follows-mouse t)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
        '(?\C-x
          ;; ?\C-u
          ;; ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ;; ?\C-\M-j  ;; Buffer list
          ?\s-\ ; TODO I might want this in exwm global keys
          ?\s-, ; TODO I might want this in exwm global keys
          ))
  ;; (setq exwm-input-prefix-keys nil)

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

  ;; TODO might want to do some local simulation keys too

  ;; TODO maybe find a way to alias localleader key to the commands in exwm-mode-map under C-c?
  ;; TODO probably use `map!' instead of `define-key'
  ;; (define-key exwm-mode-map [?\s-\M-q] 'exwm-input-send-next-key) ; TODO see if I can reverse the order of super and meta

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  ;; TODO maybe setq! would work though?
  (setq exwm-input-global-keys
        `(([?\s-q] . exwm-reset)

          ([?\s-Q] . exwm-input-release-keyboard)

          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ([?\s-t] . evil-switch-to-windows-last-buffer)
          ;; ([?\s-\ ] . doom-leader-map)

          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (biome--shell-command command)))))

  ;; TODO why is exwm-input-set-key used like this, daviwil uses it in his dotfiles too. The docs say to only use it interactively.
  (exwm-input-set-key (kbd "s-A") 'counsel-linux-app)

  (exwm-enable))



(use-package! desktop-environment
  :after exwm
  :config
  (setq desktop-environment-brightness-get-command "light"
        desktop-environment-brightness-set-command "light %s"
        desktop-environment-brightness-get-regexp "^\\([0-9]+\\)"
        desktop-environment-brightness-normal-increment "-A 10"
        desktop-environment-brightness-normal-decrement "-U 10"
        desktop-environment-volume-get-command "pactl list sinks | grep '^[[:space:]]Volume:' | head -n 1 | tail -n 1 | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,'"
        desktop-environment-volume-set-command "pactl set-sink-volume @DEFAULT_SINK@ %s"
        desktop-environment-volume-toggle-command "pactl set-sink-mute @DEFAULT_SINK@ toggle"
        desktop-environment-volume-normal-increment "+5%"
        desktop-environment-volume-normal-decrement "-5%"
        desktop-environment-keyboard-backlight-normal-increment 25
        desktop-environment-keyboard-backlight-normal-decrement -25)
  ;; These are set into the exwm global keymap when the mode is enabled (this can be changed) so modify the map before doing so.
  (map! :map desktop-environment-mode-map
        "s-l" nil
        "<XF86KbdBrightnessUp>" #'desktop-environment-keyboard-backlight-increment
        "<XF86KbdBrightnessDown>" #'desktop-environment-keyboard-backlight-decrement
        "<XF86LaunchA>" (lookup-key desktop-environment-mode-map (kbd "<print>"))
        "S-<XF86LaunchA>" (lookup-key desktop-environment-mode-map (kbd "S-<print>")))
  (desktop-environment-mode))
