(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-enabled-themes (quote (tango-dark)))
 '(delete-selection-mode t)
 '(fill-column 100)
 '(indent-tabs-mode nil)
 '(ispell-program-name "/usr/local/bin/aspell")
 '(js-indent-level 2)
 '(longlines-wrap-follows-window-size t)
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

(defun my-text-mode-hook ()
  ;(turn-on-auto-fill)
  ;(turn-on-filladapt-mode)
  (longlines-mode)
  (flyspell-mode))
(add-hook 'text-mode-hook 'my-text-mode-hook)

(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(defun my-coffee-mode-hook ()
  ;(whitespace-mode 1)
  (setq whitespace-action '(auto-cleanup))
  (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)))
(add-hook 'coffee-mode-hook 'my-coffee-mode-hook)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(require 'ruby-mode)
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))

(require 'flymake)
(require 'csharp-mode)

(require 'go-mode)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

; Allow remote use of sudo (e.g. /sudo:randomhost.your.domain:/path/to/file)
; (see http://www.gnu.org/software/tramp/#Multi_002dhops).
(require 'tramp)
(add-to-list 'tramp-default-proxies-alist
             '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '((regexp-quote (system-name)) nil nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From Steve Yegge's .emacs:

;;
;; Never understood why Emacs doesn't have this function.
;;
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;;
;; Never understood why Emacs doesn't have this function, either.
;;
(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t))))
