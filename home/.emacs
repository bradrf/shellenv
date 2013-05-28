(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(delete-selection-mode t)
 '(ns-alternate-modifier (quote super))
 '(ns-command-modifier (quote meta))
 '(ruby-indent-level 4)
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2e3436" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "apple" :family "Menlo")))))

(add-to-list 'load-path "~/.emacs.d/elisp")

; Use these to scroll the text but leave the cursor in place.
(global-set-key [(meta down)] (lambda () (interactive (scroll-up 1))))
(global-set-key [(meta up)] (lambda () (interactive (scroll-down 1))))

(setq scroll-preserve-screen-position t
      scroll-step 1
      scroll-conservatively 10000)

(iswitchb-mode t)
(global-set-key "\C-xb" 'iswitchb-buffer)
(global-set-key "\C-xB" 'iswitchb-buffer-other-window)
(global-set-key "\C-x\C-b" 'ibuffer)
(setq read-buffer-function 'iswitchb-read-buffer)
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; If the *scratch* buffer is killed, recreate it automatically
;; FROM: Morten Welind
;; http://www.geocrawler.com/archives/3/338/1994/6/0/1877802/
(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))
(defun kill-scratch-buffer ()
  ;; The next line is just in case someone calls this manually
  (set-buffer (get-buffer-create "*scratch*"))
  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))
  ;; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  ;; Since we killed it, don't let caller do that.
  nil)

;; Function indexing.
(require 'imenu)
(global-set-key "\C-f" 'imenu)
