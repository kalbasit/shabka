;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cask)
(cask-initialize)
(when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))
(require 'pallet)

;; Initialize the code
(require 's)
(require 'f)
(require 'ht)
(require 'git)
(require 'use-package)

;; Load a local file if it exists.
(defun load-local (file)
  (setq file_path (f-expand file user-emacs-directory))
  (when (f-exists? (concat file_path ".el"))
    (load file_path)))

;; Load our variables
(load-local "variables")

;; Load our functions
(load-local "defuns")

;; Load our settings
(load-local "settings")

;; Load our packages and their settings
(load-local "packages")

;; Load the bindings
(load-local "bindings")

;; Load the hooks
(load-local "hooks")
