(add-to-list 'load-path (getenv "CASK_PATH"))
(require 'cask)
(cask-initialize)
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

;; Load google-specific settings
(load-local "google-specific")
