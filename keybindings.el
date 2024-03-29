(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])
(global-unset-key [(control h)(control c)])
(global-unset-key (kbd "M-<"))
(global-unset-key (kbd "M->"))

(global-set-key (kbd "C-h C-f") 'describe-function)
(global-set-key (kbd "C-h C-c") 'describe-key-briefly)
(global-set-key (kbd "<backtab>") 'decrease-left-margin)
(global-set-key (kbd "<f5>") 'toggle-truncate-lines)
(global-set-key (kbd "<f7>") 'ispell-buffer)
(global-set-key (kbd "C-M-]") 'next-buffer)
(global-set-key (kbd "C-n") 'find-file)
(global-set-key (kbd "C-o") 'find-file-at-point)
(global-set-key (kbd "C-]") 'next-buffer)
(global-set-key (kbd "C-<prior>") 'beginning-of-buffer)
(global-set-key (kbd "C-<next>") 'end-of-buffer)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(define-key lisp-interaction-mode-map (kbd "C-e") 'eval-defun)
(add-hook 'dired-load-hook (lambda() (define-key dired-mode-map (kbd "C-o") nil)))


;; Custom commands

(global-set-key
 (kbd "C-w")
 (lambda () "Close the currently active buffer."
   (interactive)
   (if (minibufferp)
       (keyboard-quit)               ;; Quit minibuffer if focussed
     (if (and (< 1 (count-windows))  ;; Otherwise, kill current buffer
              (eq major-mode 'help-mode))
         (progn (kill-buffer) (delete-window))
       (kill-buffer)))))

(global-set-key
 (kbd "C-u")
 (lambda () "Erase text between point and start-of-line."
   (interactive)
   (kill-line 0)))

(global-set-key
 (kbd "C-g")
 (lambda () "Quit minibuffer if active; otherwise, call `goto-line'."
   (interactive)
   (if (minibufferp)
       (keyboard-quit)
     (call-interactively 'goto-line))))

(defun at-comment-p (&optional point)
  "Return non-nil if POINT is currently inside a comment."
  (nth 4 (syntax-ppss point)))

(global-set-key
 (kbd "<f1>")
 (lambda () "Look up documentation for the term at point."
   (interactive)
   (or (when (and (lispp) (not (at-comment-p)))
             (describe-symbol (symbol-at-point)) t)
       (when (and (asmp) (fboundp 'x86-lookup))
             (x86-lookup (thing-at-point 'word)) t)
       (call-interactively 'man-follow))))
