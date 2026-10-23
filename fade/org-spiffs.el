;;;-*- lexical-binding: t; -*-
;;; init.el --- Base emacs config file
;;; Commentary:
;;; Copyright (c) 2018-2021 Brian O'Reilly <fade@deepsky.com>
;;; Code:

(require 'subr-x)

;;; taken from https://github.com/skx/dotfiles redefined to ameliorate
;;; the assignment to uninitialised variables. Also, #'goto-line is
;;; for interactive functions only. recast in terms of #'goto-char

(defun fade/org-global-prop (name)
  "Return the first Org keyword NAME without changing folding or point."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (when (re-search-forward
               (concat "^#\\+" (regexp-quote name) ":[ \t]*\\(.*\\)$") nil t)
          (string-trim (match-string-no-properties 1)))))))

(defun fade/org-mode-rename-buffer ()
  "Use this Org buffer's title as a unique buffer name."
  (let ((title (fade/org-global-prop "TITLE")))
    (when (and title (not (string-empty-p title)))
      (rename-buffer title t))))

;;; now in your init file, attach the renaming function to the 'org-mode-hook

(provide 'org-spiffs)

;;; org-spiffs.el ends here
