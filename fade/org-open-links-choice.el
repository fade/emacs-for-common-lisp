;;;-*- lexical-binding: t; -*-
;;; init.el --- Base emacs config file
;;; Commentary: link handling utilities. Choose the browser, mainly.
;;; Copyright (c) 2018-2024 Brian O'Reilly <fade@deepsky.com>
;;; Code:

(require 'cl-lib)

;;; open link at point in browser of user's choice

(defun open-link-at-point-or-minibuffer-with-choice ()
  "Use `consult` to select a link at point, in the buffer, from `consult-omni` results, or prompt for a URL.
  Then choose to open it in macOS default browser or eww.
  If 'eww' is chosen, the link is opened in a window that occupies 80% of the frame height below the current one."
  (interactive)
  (let* ((url (or (thing-at-point 'url) ;; Check if there's a URL at point
                  (consult--read (thing-at-point--list 'url)) ;; Use consult to select a URL from the buffer
                  (consult-omni) ;; Use consult-omni results to select a link
                  (read-string "Enter URL: ")))) ;; Fall back to manual URL entry
    (if url
        (let ((choice (completing-read "Open link in: " '("System Browser" "eww"))))
          (cond
           ((string-equal choice "System Browser")
            ; potentially this could be a ladder of many system-types, therefore, #'cond
            (cond ((eq system-type 'darwin) (shell-command (concat "open " (shell-quote-argument url))))
                  (t (shell-command (concat "xdg-open " (shell-quote-argument url))))))
           ((string-equal choice "eww")
            ;; Calculate the height for the top window (20% of the frame height)
            (let ((window (selected-window))
                  (top-window-height (floor (* 0.2 (window-total-height)))))
              ;; Split window with 20% height on top and 80% height for eww on the bottom
              (select-window (split-window window top-window-height))
              (eww url)))))
      (message "No URL provided."))))


(defun timu-func-make-capture-frame ()
  "Create a new frame and run `org-capture'."
  (interactive)
  ;; Create a new frame for capture
  (make-frame '((name . "capture")
  		(top . 300)
  		(left . 700)
  		(width . 80)
  		(height . 25)))
  ;; Select the newly created frame
  (select-frame-by-name "capture")
  (delete-other-windows)
  ;; Use cl-letf to temporarily redefine switch-to-buffer-other-window
  (cl-letf (((symbol-function 'switch-to-buffer-other-window)
  	     (lambda (buf) (switch-to-buffer buf))))
    (org-capture)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

;; Org-Capture Function for Safari
(defun timu-func-url-safari-capture-to-org ()
  "Call `org-capture-string' on the current front most Safari window.
   Use `org-mac-link-safari-get-frontmost-url' to capture url from Safari.
   Triggered by a custom macOS Quick Action with a keyboard shortcut."
  (interactive)
  (org-capture-string (org-mac-link-safari-get-frontmost-url) "u")
  (ignore-errors)
  (org-capture-finalize))


(provide 'org-open-links-choice)
;;; org-open-links-choice.el ends here.
