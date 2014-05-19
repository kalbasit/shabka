(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

;; Load a theme
(load-theme 'zenburn :no-confirm)

;; Initialize the code
(require 'git)
(require 'use-package)

(setq default-directory (f-full (getenv "HOME")))

(defun load-local (file)
  (load (f-expand file user-emacs-directory)))

(load-local "defuns")
(load-local "packages")
