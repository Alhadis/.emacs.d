;; -*- lexical-binding: nil; no-byte-compile: t; no-native-compile: t; -*-
((nil .
   ((tab-width . 4)
    (indent-tabs-mode . t)
    (copyright-names-regexp . "John Gardner")
    (sentence-end-double-space . nil)))
 (c-mode .
   ((c-file-style . "BSD")
    (c-basic-offset . 4)))
 (emacs-lisp-mode .
   ((tab-width . 8)
    (indent-tabs-mode . nil)
    (byte-compile-warnings . (not free-vars unresolved))))
 (git-commit-mode .
   ((fill-column . 72)
    (indent-tabs-mode . nil)))
 ("elpa" . ((nil .
   ((warning-minimum-level . :error)
    (warning-suppress-types . ((lexical-binding))))))))
