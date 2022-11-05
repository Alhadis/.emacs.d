;; Locale
(set-language-environment "UTF-8")
(setenv "LANG" "en_AU.UTF-8")
(setenv "LC_COLLATE" "C")

;; Graphical displays
(when (display-graphic-p)
  ;; Enable Unicode support
  (setq locale-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (define-coding-system-alias 'UTF-8 'utf-8)

  ;; macOS: Fix search-paths when launched via symlink
  (when (and (eq (window-system) 'ns)
             (not (member "~/.files/bin/" exec-path)))
    (eval-when-compile (require 'subr-x))
    (dolist (var '("PATH" "MANPATH")) (setenv var
      (string-join (append
        (condition-case error
          (process-lines "/bin/sh" "-c" (format ". ~/.profile; printf '%%s\\n' \"$%s\" | tr : '\\n'" var))
          (message "Failed to list $%s: %s" var error) nil)
        (if (string= var "PATH")
          (seq-filter (lambda (x) (string-prefix-p invocation-directory x)) exec-path)
          (list (expand-file-name (concat invocation-directory "../Resources/man")))))
      path-separator)))
    (add-hook 'after-init-hook (lambda ()
      (setq exec-path (split-string (getenv "PATH") path-separator t))))))

;; No pointless distractions, please
(setq inhibit-startup-screen t)

;; Modifying this file, could you not
(setq custom-file "~/.emacs.d/custom.el")

;; Don't leave clutter around while editing
(setq auto-save-default nil
      create-lockfiles nil
      disabled-command-function nil)

;; Use a single directory for storing backup files
(setq backup-directory-alist `(("." . "~/.emacs.d/auto-save-list"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Keep a familiar workflow
(cua-mode t)
(setq help-window-select t
      backward-delete-char-untabify-method nil)

;; Configure indentation
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

(defun asmp ()
  "Return t if `major-mode' is any assembly-like language."
  (interactive)
  (not (null (derived-mode-p 'asm-mode
                             'iasm-mode
                             'nasm-mode
                             'masm-mode))))

(defun lispp ()
  "Return t if `major-mode' is any derivative of Lisp."
  (interactive)
  (not (null (derived-mode-p 'emacs-lisp-mode
                             'lisp-interaction-mode
                             'lisp-mode
                             'scheme-mode))))

(defun find-app (id)
  "Locate a macOS `*.app' bundle with the given ID."
  (unless (string= system-type "darwin")
    (error "macOS is required to use this function"))
  (when-let*
    ((app (car (process-lines "mdfind" (format "kMDItemCFBundleIdentifier == '%s'" id))))
     (exe (car (reverse (split-string app "/" t)))))
    (concat app "/Contents/MacOS/" (s-chop-suffixes '(".app") exe))))

(defun open-pdf (file &optional page-num)
  "Open a PDF document at the designated PAGE-NUM."
  (interactive "fOpen PDF: \nnPage number (default: 1): ")
  (setq file (expand-file-name file))
  (when (and (not (boundp 'page-num))
             (called-interactively-p 'any))
        (setq page-num 1))
  (start-process "open-pdf" nil
    (if (string= system-type "darwin")
        ;; XXX: open(1) discards fragment identifiers in URLs, so `file:///file.pdf#page=1' won't work.
        (or (find-app "com.google.Chrome")
            (find-app "com.apple.Safari")
            (find-app "org.mozilla.firefox"))
        "xdg-open") ;; TODO: Test this
    (format "file://%s#page=%s" file page-num)))

(defun configure-indent ()
  "Set buffer-local variables for indentation."
  (interactive)
  (unless (lispp)
    (setq default-tab-width 4
          indent-tabs-mode t
          indent-line-function 'insert-tab
          coffee-indent-tabs-mode t
          sh-basic-offset 4
          sh-indentation 4
          sh-use-smie nil
          tab-width 4
          nxml-child-indent 4)))

(add-hook 'text-mode-hook  'configure-indent)
(add-hook 'prog-mode-hook  'configure-indent)
(add-hook 'after-init-hook 'configure-indent)

;; Prevent tabs from creeping into Lisp code
(dolist (hook
  (list 'emacs-lisp-mode-hook
        'lisp-interaction-mode-hook
        'lisp-mode-hook
        'scheme-mode-hook))
  (add-hook hook
     (lambda ()
       (setq indent-tabs-mode nil)
       (setq tab-width 8)
       (add-hook 'before-save-hook
         (lambda ()
           (untabify (point-min) (point-max))
           (delete-trailing-whitespace))
         nil t))))

;; Prevent `*scratch*' buffer from being closed
(add-hook 'kill-buffer-query-functions
  (lambda () (not (eq (get-buffer "*scratch*") (current-buffer)))))

;; Load Git-related major modes
(add-to-list 'load-path "~/.emacs.d/git-modes")
(add-hook 'git-commit-mode-hook
  (lambda ()
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
    (setq indent-tabs-mode nil)
    (setq fill-column 72)))
(load "git-modes")
(load "git-commit")

;; Load everything else
(with-eval-after-load "outline" (require 'foldout nil t))
(unless (getenv "NO_PKG")
        (load "~/.emacs.d/packages"))
(load "~/.emacs.d/keybindings")
(load "~/.emacs.d/theme")
(load "~/.emacs.d/site" t)
