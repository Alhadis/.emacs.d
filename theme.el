; -*- truncate-lines: t; -*-
;; Disable that nasty background applied to “bold” TTY text
(tty-suppress-bold-inverse-default-colors t)

;; Remove :inverse-video effect when resolving conflicts
(unless (getenv "DISPLAY")
  (progn (custom-set-faces
          '(smerge-refined-added   ((t (:inherit smerge-refined-change))))
          '(smerge-refined-removed ((t (:inherit smerge-refined-change)))))))

;; Modeline
(column-number-mode t)
(display-battery-mode t)
(show-paren-mode t)

;; Text colours
(custom-set-faces
  '(font-lock-function-name-face ((t (:weight bold))))
  '(match ((t (:background "brightcyan" :foreground "black")))))


;; Graphical displays
(when (getenv "DISPLAY")
  (fringe-mode '(0 . 0))
  (custom-set-variables
    '(blink-cursor-mode nil)
    '(cursor-type (quote (bar . 1)))
    '(global-display-line-numbers-mode t)
    '(scroll-bar-mode nil)
    '(tool-bar-mode nil))

  ; Frame parameters
  (add-to-list 'default-frame-alist '(background-mode . 'dark))
  (add-to-list 'default-frame-alist '(font            . "DejaVu Sans Mono-14"))
  (add-to-list 'default-frame-alist '(line-spacing    . 2))
  (add-to-list 'default-frame-alist '(width           . 130))
  (add-to-list 'default-frame-alist '(height          . 85))
  (add-to-list 'default-frame-alist '(alpha           . 90))
  (setq frame-title-format nil)

  ; macOS-specific polishes
  (when (eq (window-system) 'ns)
    (setq ns-use-proxy-icon nil
          ns-use-thin-smoothing t)
    (add-to-list 'default-frame-alist '(font . "Menlo-14"))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . 'dark)))

  ; Theme based on `tsdh-dark' theme
  (custom-set-faces
    '(custom-button                ((t (:foreground "#000000" :background "#888888" :box (:line-width 2 :style released-button) :bold))))
    '(default                      ((t (:background "#000000" :foreground "#FFFFFF"))))
    '(diff-added                   ((t (:inherit diff-changed :background "dark green"))))
    '(diff-changed                 ((t (:background "midnight blue"))))
    '(diff-indicator-added         ((t (:inherit diff-indicator-changed))))
    '(diff-indicator-changed       ((t (:weight bold))))
    '(diff-indicator-removed       ((t (:inherit diff-indicator-changed))))
    '(diff-removed                 ((t (:inherit diff-changed :background "dark red"))))
    '(dired-directory              ((t (:foreground "DodgerBlue" :weight bold))))
    '(error                        ((t (:foreground "red" :weight bold))))
    '(font-lock-builtin-face       ((t (:foreground "chartreuse2"))))
    '(font-lock-comment-face       ((t (:foreground "#00AA00"))))
    '(font-lock-constant-face      ((t (:foreground "dodger blue"))))
    '(font-lock-doc-face           ((t (:foreground "indian red"))))
    '(font-lock-function-name-face ((t (:foreground "spring green"))))
    '(font-lock-keyword-face       ((t (:foreground "light sea green" :weight bold))))
    '(font-lock-preprocessor-face  ((t (:foreground "cornflower blue"))))
    '(font-lock-string-face        ((t (:foreground "light salmon"))))
    '(font-lock-type-face          ((t (:foreground "medium purple"))))
    '(font-lock-variable-name-face ((t (:foreground "yellow green"))))
    '(font-lock-warning-face       ((t (:foreground "red" :weight bold))))
    '(header-line                  ((t (:inverse-video t :box (:line-width -1 :color "red" :style released-button)))))
    '(highlight                    ((t (:background "sea green"))))
    '(line-number                  ((t (:foreground "#666666"))))
    '(line-number-current-line     ((t (:foreground "#CCCCCC"))))
    '(menu                         ((t (:background "gray30" :foreground "gray70"))))
    '(minibuffer-prompt            ((t (:foreground "#4682b4" :weight bold))))
    '(mode-line                    ((t (:background "#444444"))))
    '(mode-line-inactive           ((t (:inherit mode-line :background "#222222" :foreground "#666666"))))
    '(region                       ((t (:background "#334466"))))
    '(scroll-bar                   ((t (:background "gray20" :foreground "dark turquoise"))))
    '(secondary-selection          ((t (:background "#333366" :foreground "#f6f3e8"))))
    '(show-paren-match             ((t (:background "DeepSkyBlue4"))))
    '(show-paren-mismatch          ((t (:background "dark magenta"))))
    '(widget-field                 ((t (:box (:line-width 2 :color "grey75" :style pressed-button)))))))
