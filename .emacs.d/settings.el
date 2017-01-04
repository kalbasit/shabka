;; Load a theme
(load-theme 'zenburn :no-confirm)

;; Set the default directory to $HOME
(setq default-directory (f-full (getenv "HOME")))

;; Do not show the help on startup
(setq inhibit-startup-message t)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; Prefer utf8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Disable the menu bar, tool bar and scroll bar
(mapc
  (lambda (mode)
    (when (fboundp mode)
      (funcall mode -1)))
  '(menu-bar-mode tool-bar-mode scroll-bar-mode))

;; Change the autosave and backup location
;; TODO: Fix this, Wrong type argument: stringp, emacs-backup-dir
;(setq backup-directory-alist '((".*" . emacs-backup-dir)))
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup/")))

; TODO: Fix this, getting error "Wrong type argument: stringp, emacs-autosave-dir"
;(setq auto-save-file-name-transforms '((".*" emacs-autosave-dir t)))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/\\1" t)))

;; Enable flyspell
(flyspell-mode +1)

;; Auto clean whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Show keystrokes in minibuffer early
(setq echo-keystrokes 0.1)

;; Set default browser
(setq browse-url-browser-function 'browse-url-generic)
; (setq browse-url-generic-program
;       (cond
;         ((memq window-system '(mac ns)) "open")
;         (linux (executable-find "google-chrome"))
;         ))

;; Initial major mode is Emacs Lisp mode
(setq initial-major-mode 'emacs-lisp-mode)

;; Make default mode Emacs lisp mode
(setq default-major-mode 'emacs-lisp-mode)

;; Kill whole line (including \n)
(setq kill-whole-line t)

;; Indent with spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Do not pause on redisplay
(setq redisplay-dont-pause t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; Show column number in mode line
(column-number-mode 1)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)


;; Disable find-file-at-point
(setq ido-use-filename-at-point nil)

;; Javascript indent by 2
(setq js-indent-level 2)

;; Python indent with two spaces
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))
