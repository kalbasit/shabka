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

;; Load a local file
(defun load-local (file)
  (load (f-expand file user-emacs-directory)))

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
