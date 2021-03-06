;; global sets have the LOWEST priority (can be overriden by other maps)
(global-set-key (kbd "C-c d") 'spacemacs/duplicate-line-or-region)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-S-<up>") 'move-text-line-up)
(global-set-key (kbd "C-S-<down>") 'move-text-line-down)
(global-set-key (kbd "M-S-<up>") 'evil-scroll-line-up)
(global-set-key (kbd "C-x +") 'balance-windows-area)

;; override some of spacemacs' defaults
(define-key evil-emacs-state-map (kbd "C-z") 'undo)

;; use org-list-repair (from org-mode) to update numbers

(defun insert-timestamp ()
  (interactive)
  (insert (format-time-string "%c")))
