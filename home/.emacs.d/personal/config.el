;;; config.el --- Personal overrides and augmentation of Prelude configuration.
;;; Commentary:

;;; Code:

;; Swap option with command for more fluid use of Meta key.
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; Provide dynamic window numbers to use to jump to using M-<number>.
(window-numbering-mode t)

;; Avoid suspend (crashes on OS X, everytime!)
(global-set-key [(control z)] 'undo)

;; Use these to scroll the text but leave the cursor in place.
(global-set-key [(super shift down)] (lambda () (interactive (scroll-up 1))))
(global-set-key [(super shift up)] (lambda () (interactive (scroll-down 1))))

;; Use awesome symbol-aware replacement instead of default regular expression replace.
(global-set-key [(control meta %)] 'anzu-query-replace-at-cursor)

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

(rvm-use-default)

;;; config.el ends here
