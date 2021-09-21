;; Source packages from MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 27) (package-initialize))
(when (version< emacs-version "26.3")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(defmacro use (name &rest body)
  "Attempt to load package NAME. If successful, evaluate BODY and return t."
  `(if (require ,name nil t)
       (progn ,@body t)
       (let ((list 'package-selected-packages))
            (unless (boundp list) (setq list ()))
            (add-to-list list ,name) nil)))

;; Begin loading packages
(use 'aggressive-indent
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))
(use 'cc-mode
  (add-hook 'cc-mode-hook (lambda ()
    (setq c-file-style "K&R")
    (setq c-basic-offset 4)
    (setq c-tab-always-indent t)
    (setq c-syntactic-indentation nil))))
(use 'cperl-mode
  (setq tab-width 8)
  (setq cperl-indent-level 8)
  (setq cperl-extra-newline-before-brace nil)
  (setq cperl-merge-trailing-else nil))
(use '0xc)
(use 'academic-phrases)
(use 'adoc-mode)
(use 'apache-mode)
(use 'bison-mode)
(use 'bnf-mode)
(use 'bnfc)
(use 'brainfuck-mode)
(use 'cmake-mode)
(use 'cperl-mode)
(use 'coffee-mode)
(use 'csv-mode)
(use 'cuda-mode)
(use 'cycle-quotes)
(use 'dashboard)
(use 'deadgrep)
(use 'dna-mode)
(use 'dockerfile-mode)
(use 'dotenv-mode)
(use 'dyalog-mode)
(use 'editorconfig)
(use 'enh-ruby-mode)
(use 'fic-mode)
(use 'form-feed
  (global-form-feed-mode))
(use 'forth-mode)
(use 'glsl-mode)
(use 'go-mode)
(use 'haskell-mode)
(use 'haskell-tab-indent)
(use 'iasm-mode)
(use 'ini-mode)
(use 'intel-hex-mode)
(use 'js2-mode)
(use 'less-css-mode)
(use 'lfe-mode)
(use 'markdown-mode)
(use 'masm-mode)
(use 'mocha)
(use 'js2-mode
  (add-hook 'js2-mode (lambda ()
    (setq js2-highlight-level 3)
    (setq js2-include-node-externs t)
    (setq js2-strict-trailing-comma-warning nil)
    (setq js2-strict-cond-assign-warning nil)
    (setq js2-strict-inconsistent-return-warning nil)
    (setq indent-line-function 'insert-tab)
    (setq indent-tabs-mode t)
    (setq tab-width 4)))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
  (dolist (patt '("\\.[cmsp]?js\\'"
                  "\\.es[0-9]?\\'"
                  "\\.eslintrc\\'"
                  "\\.jscript\\'"
                  "\\._?js[bmse]?\\'"
                  "\\.snap\\'"))
          (add-to-list 'auto-mode-alist (cons patt 'js2-mode)))
  (dolist (name '("chakra" "d8" "js" "node" "qjs" "rhino" "v8" "v8-shell"))
          (add-to-list 'interpreter-mode-alist (cons name 'js2-mode))))
(use 'move-text
  (global-set-key (kbd "C-<up>")   'move-text-up)
  (global-set-key (kbd "C-<down>") 'move-text-down))
(use 'multiple-cursors)
(use 'nasm-mode)
(use 'newlisp-mode)
(use 'ninja-mode)
(use 'nroff-mode)
(use 'plisp-mode
  (setq plisp-syntax-highlighting-p t))
(use 'pov-mode)
(use 'powershell)
(use 'realgud)
(use 'realgud-node-inspect)
(use 'rust-mode)
(use 's)
(use 'scad-mode)
(use 'sed-mode)
(use 'sgml-mode
  (define-key sgml-mode-map (kbd "tab") 'self-insert-command)
  (add-hook 'sgml-mode-hook (lambda ()
    (setq tab-width 4)
    (setq indent-tabs-mode t)
    (set (make-local-variable 'sgml-basic-offset) 4))))
(use 'shift-number)
(use 'slime)
(use 'sml-mode)
(use 'spice-mode)
(use 'ssh-config-mode)
(use 'toml-mode)
(use 'typescript-mode
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
  (dolist (name '("deno" "tsc" "ts-node"))
          (add-to-list 'interpreter-mode-alist (cons name 'typescript-mode))))
(use 'vimrc-mode)
(use 'wavefront-obj-mode)
(use 'yasnippet
  (add-to-list 'yas-snippet-dirs "~/Labs/YASR/snippets")
  (setq yas-indent-line "fixed")
  (yas-global-mode 1))
(use 'xterm-color)
(use 'x86-lookup
  (setq x86-lookup-browse-pdf-function #'open-pdf)
  (setq x86-lookup-pdf
    (locate-file "325462-sdm-vol-1-2abcd-3abcd.pdf"
      (list "~/Documents"      "~/Documents/eBooks"
            "~/Downloads"      "~/Downloads/eBooks"
            "~/.files/share"   "~/.files/share/doc"
            "~/.files/var/doc" "~/.files/var/share/doc"
            "~/.local/share"   "~/.local/share/doc"
            "/usr/local/share" "/usr/local/share/doc"
            "/usr/share"       "/usr/share/doc"
            "~/.emacs.d"))))
(use 'yaml-mode)
