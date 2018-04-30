;;; config.el --- Personal overrides and augmentation of Prelude configuration.
;;; Commentary:

;;; Code:

;; Swap option with command for more fluid use of Meta key.
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; Provide dynamic window numbers to use to jump to using M-<number>.
(window-numbering-mode t)

;; Simple move left or move right for the current buffer.
(global-set-key [(control <)] 'buf-move-left)
(global-set-key [(control >)] 'buf-move-right)

;; Avoid suspend (crashes on OS X, everytime!)
(global-set-key [(control z)] 'undo)

;; Use these to scroll the text but leave the cursor in place.
(global-set-key [(super shift down)] (lambda () (interactive (scroll-up 1))))
(global-set-key [(super shift up)] (lambda () (interactive (scroll-down 1))))

;; Use awesome symbol-aware replacement instead of default regular expression replace.
(global-set-key [(control meta %)] 'anzu-query-replace-at-cursor)

(setenv "GOPATH" (expand-file-name "~/work/go"))

;; Trigger mode based on shebang value
(add-to-list 'interpreter-mode-alist '("rails" . ruby-mode))

;; Use Docker File Mode for anything starting with Dockerfile
(add-to-list 'auto-mode-alist '("Dockerfile.*" . dockerfile-mode))

;; bury *scratch* buffer instead of kill it
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

(defvar python--pdb-breakpoint-string "import pdb; pdb.set_trace() ## DEBUG ##"
  "Python breakpoint string used by `python-insert-breakpoint'")

(defvar python--ipdb-breakpoint-string "import ipdb; ipdb.set_trace() ## DEBUG ##"
  "Python breakpoint string used by `python-insert-breakpoint'")

;; TODO: make this work with a C-u to differentiate ipdb vs. regular (or detect somehow?)
(defun python-insert-breakpoint ()
  "Inserts a python breakpoint using `pdb'"
  (interactive)
  (back-to-indentation)
  ;; this preserves the correct indentation in case the line above
  ;; point is a nested block
  (split-line)
  (insert python--pdb-breakpoint-string))

(setq ediff-split-window-function (if (> (frame-width) 150)
                                      'split-window-horizontally
                                    'split-window-vertically))

;; Save current window layout before ediff and restor after ediff completes
(defvar my-ediff-bwin-config nil "Window configuration before ediff.")
(defcustom my-ediff-bwin-reg ?b
  "*Register to be set up to hold `my-ediff-bwin-config'
    configuration.")

(defvar my-ediff-awin-config nil "Window configuration after ediff.")
(defcustom my-ediff-awin-reg ?e
  "*Register to be used to hold `my-ediff-awin-config' window
    configuration.")

(defun my-ediff-bsh ()
  "Function to be called before any buffers or window setup for
    ediff."
  (setq my-ediff-bwin-config (current-window-configuration))
  (when (characterp my-ediff-bwin-reg)
    (set-register my-ediff-bwin-reg
                  (list my-ediff-bwin-config (point-marker)))))

(defun my-ediff-ash ()
  "Function to be called after buffers and window setup for ediff."
  (setq my-ediff-awin-config (current-window-configuration))
  (when (characterp my-ediff-awin-reg)
    (set-register my-ediff-awin-reg
                  (list my-ediff-awin-config (point-marker)))))

(defun my-ediff-qh ()
  "Function to be called when ediff quits."
  (when my-ediff-bwin-config
    (set-window-configuration my-ediff-bwin-config)))

(add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash 'append)
(add-hook 'ediff-quit-hook 'my-ediff-qh)

(rvm-use-default)

;;; config.el ends here
