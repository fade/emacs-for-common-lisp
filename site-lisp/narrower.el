;;;-*- lexical-binding: t; -*-
;;; init.el --- Base emacs config file
;;; Commentary:
;; Copyright (c) 2018-2021 Brian O'Reilly <fade@deepsky.com>
;;; Code:

(defun narrow-to-regex ()
  "narrow the buffer visibility to the section between two regexes the user provides"
  (interactive)
  (let* ((beginRegex (read-regexp "begin pattern"))
         (endRegex (read-regexp "end pattern"))
         (beg)
         (end))
    (goto-char (point-min)) ;; go to the start of the buffer
    (if (re-search-forward beginRegex nil t nil)
        (setq beg (- (point) (length beginRegex))))
    (if (re-search-forward endRegex nil t nil)
        (setq end (point)))
    (if (and beg end (> end beg))
        (narrow-to-region beg end)
      (message "did not find both instances of the regex, %s %s, no narrow" beg end))))


;; narrower.el ends here
