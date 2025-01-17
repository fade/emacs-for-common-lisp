;;; All of the things that need to happen first and soon.

(defvar run-email nil)

(setq ad-redefinition-action 'accept)

;;; The following early init code is modeled heavily from alternateved@libera
;;; https://codeberg.org/alternateved

;;; Code:

;; Adjust garbage collection thresholds during startup, and thereafter
(setopt gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6)

(defun +gc-after-focus-change-fn ()
  "Run GC when frame loses focus."
  (run-with-idle-timer
   5 nil
   (lambda () (unless (frame-focus-state) (garbage-collect)))))

(defun +reset-init-values-h ()
  "Restore defalut values after init."
  (run-with-idle-timer
   1 nil
   (lambda ()
     (setopt gc-cons-threshold (* 20 1024 1024)
             gc-cons-percentage 0.1)
     (when (boundp 'after-focus-change-function)
       (add-function :after after-focus-change-function #'+gc-after-focus-change-fn)))))

(add-hook 'emacs-startup-hook '+reset-init-values-h)

;; Set the `file-name-handler' and `vc-handled-backends' to nil in order
;; optimize startup time. `file-name-handler-alist' is consulted on each
;; call to `require', `load', or various file/io functions.
(let ((old-file-name-handler-alist file-name-handler-alist))
  (setopt vc-handled-backends nil
          file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
            (lambda ()
              "Recover file name handlers."
              (setopt vc-handled-backends '(Git)
                      file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           old-file-name-handler-alist))))))

;;; Following from Lionyx <Rahul Juliato> in #systemcrafters
;;; Native Compile Settings
;; I use a <emacs-default-dir>/eln-cache-<machine-hostname>/ dir to store
;; my different machines' eln compiled code. This allows the same config
;; to be used on several machines without conflicts.
;; This is added in early-init.el to prevent unwanted recompilations.

(when (featurep 'native-compile)
  ;; Set the right directory to store the native compilation cache
  (let ((path (expand-file-name (concat "eln-cache-" (system-name) "/") user-emacs-directory)))
    (setq-default native-comp-eln-load-path       (list path)
		  native-compile-target-directory path)
    (startup-redirect-eln-cache path))

  (setq-default native-comp-async-report-warnings-errors nil  ;; Silence compiler warnings as they can be disruptive
		native-comp-jit-compilation              t    ;; Enable async native compilation
		package-native-compile                   t))  ;; Compile installed packages


;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq-default load-prefer-newer noninteractive)

;; Disable site-wide initializations
(setq site-run-file nil
      inhibit-default-init t)

;; Packages will be initialized later
(setopt package-enable-at-startup nil)

;; Silence compiler warnings and remove old versions of native-compiled files
(setopt native-compile-prune-cache t
        native-comp-async-report-warnings-errors 'silent)

;; Process performance tuning
(setopt process-adaptive-read-buffering nil
        read-process-output-max (* 4 1024 1024))

;; Unset a list of command line options that aren't relevant to this session
(unless (memq initial-window-system '(x pgtk))
  (setq command-line-x-option-alist nil))

;; In the beginning there was no fancy
(setopt fancy-startup-text nil
        fancy-about-text nil)

;; Set frame settings early
(setopt frame-inhibit-implied-resize t  ; do not resize the frame at this early stage.
        frame-resize-pixelwise t        ; fully cover the screen when maximized
        frame-title-format '("%b"))     ; a bit more meaningful frame title

;; Disable some things right away
(setopt inhibit-x-resources t
        inhibit-splash-screen t
        inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-buffer-menu t
        initial-scratch-message nil
        server-client-instructions nil ; disable instructions on how to close a frame
        ring-bell-function #'ignore    ; disable sound notification on error/keyboard quit
        visible-bell nil               ; disable visual notification on error/keyboard quit
        tooltip-mode nil               ; I have no need for tooltips
        use-dialog-box nil             ; prompts should go in the minibuffer, not in a GUI.
        use-short-answers t            ; never use `yes-or-no-p', prefer `y-or-n-p'
        warning-minimum-level :error)  ; I don't care about your warnings, just give me errors!

;; Disable additional graphical elements
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(menu-bar-mode -1)

;; Set initial frame parameters
(modify-all-frames-parameters
 `((horizontal-scroll-bars . nil)
   (vertical-scroll-bars . nil)
   (width . (text-pixels . 1200))
   (height . (text-pixels . 900))))

;;; early-init.el ends here
