;; Use the built-in `package.el' for managing packages
(require 'package)

;; Source packages from MELPA first
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))

(when (< emacs-major-version 27) (package-initialize))
(when (version< emacs-version "26.3")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
