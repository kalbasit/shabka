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

;; Always new-line-and-indent
(define-key global-map (kbd "RET") 'newline-and-indent)
