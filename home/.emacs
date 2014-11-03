(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bookmark-save-flag 1)
 '(coffee-tab-width 2)
 '(custom-enabled-themes (quote (tango-dark)))
 '(delete-selection-mode t)
 '(fill-column 100)
 '(grep-find-ignored-directories (quote ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules")))
 '(indent-tabs-mode nil)
 '(ispell-program-name "/usr/local/bin/aspell")
 '(js-indent-level 2)
 '(jshint-mode-node-program "/usr/local/bin/node")
 '(longlines-wrap-follows-window-size t)
 '(nginx-indent-level 2)
 '(ns-alternate-modifier (quote super))
 '(ns-command-modifier (quote meta))
 '(ruby-indent-level 2)
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "apple" :family "Menlo"))))
 '(whitespace-line ((t (:background "Red"))))
 '(whitespace-space ((t (:foreground "gray20")))))

;; use list-packages to choose those to install
;; use describe-package to show docs about package
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(cond
   ((string-equal system-type "darwin") ; OS X
    (global-set-key "\M-`" 'other-frame)))

;;TRANSPARENCY: (set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
(setq transparency-values '(90 75))
(set-frame-parameter (selected-frame) 'alpha transparency-values)
(add-to-list 'default-frame-alist '(alpha 90 75))
(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha transparency-values)))
(global-set-key (kbd "C-c t") 'toggle-transparency)

(global-set-key "\C-z" 'undo)

(add-to-list 'load-path "~/.emacs.d/elisp")

(global-set-key "\C-c\C-c" 'comment-region)

(require 'window-numbering)
(window-numbering-mode t)

(require 'buffer-move)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

; Use these to scroll the text but leave the cursor in place.
(global-set-key [(meta down)] (lambda () (interactive (scroll-up 1))))
(global-set-key [(meta up)] (lambda () (interactive (scroll-down 1))))

(setq scroll-preserve-screen-position t
      scroll-step 1
      scroll-conservatively 10000)

; Bind the standard control-minus and -plus (really, equals) to change font size of display.
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

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

(defun my-whitespace-hook ()
  (make-local-variable 'whitespace-line-column)
  (setq whitespace-line-column fill-column)
  (make-local-variable 'whitespace-style)
  (setq whitespace-style (quote (face tabs spaces trailing lines-tail space-before-tab
                                      indentation empty space-after-tab space-mark tab-mark)))
  (make-local-variable 'whitespace-action)
  (setq whitespace-action (quote (auto-cleanup)))
  (whitespace-mode))
(global-set-key "\C-cl" (lambda ()
                          (interactive)
                          (whitespace-toggle-options (quote (lines-tail)))))

(require 'flymake)
(require 'js)
(defun my-js-mode-hook ()
  (my-whitespace-hook))
(add-hook 'js-mode-hook 'my-js-mode-hook)
(defun my-jshint-mode-hook ()
  (flymake-mode t)
  (local-set-key "\C-c\C-v" 'flymake-display-err-menu-for-current-line))
(if (file-directory-p "/opt/work/src/jshint-mode")
    (progn
      (add-to-list 'load-path "/opt/work/src/jshint-mode")
      (require 'flymake-jshint)
      (add-hook 'js-mode-hook 'my-jshint-mode-hook)))
;; Use C-c C-v to see reason for errors

(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(defun my-coffee-mode-hook ()
  (my-whitespace-hook))
(add-hook 'coffee-mode-hook 'my-coffee-mode-hook)

(defun my-python-mode-hook ()
  (setq python-indent-offset 4)
  (my-whitespace-hook))
(add-hook 'python-mode-hook 'my-python-mode-hook)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; (require 'ruby-mode) => using enh-ruby-mode instead
(add-to-list 'auto-mode-alist '("Gemfile" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake" . enh-ruby-mode))
(defun my-ruby-mode-hook ()
  (my-whitespace-hook))
(add-hook 'enh-ruby-mode-hook 'my-ruby-mode-hook)
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

;(require 'csharp-mode)

;(require 'go-mode)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(add-to-list 'magic-mode-alist
             `(,(lambda ()
                  (and (string= (file-name-extension buffer-file-name) "h")
                       (re-search-forward "@class"
                                          magic-mode-regexp-match-limit t)))
               . objc-mode))

(require 'nginx-mode)
(add-to-list 'auto-mode-alist '("/etc/nginx/sites-available/.*" . nginx-mode))

(require 'tramp)
; To use sudo on the remote machine (see http://www.emacswiki.org/emacs/TrampMode#toc9):
;
;     C-x C-f /ssh:you@remotehost|sudo:remotehost:/path/to/file RET
;
; Or, as a different remote user:
;
;     C-x C-f /ssh:you@remotehost|sudo:them@remotehost:/path/to/file RET
;
; NOTE: The following is only necessary on older versions of emacs (< 24).
;
; Allow remote use of sudo (e.g. /sudo:randomhost.your.domain:/path/to/file)
; (see http://www.gnu.org/software/tramp/#Multi_002dhops).
; (add-to-list 'tramp-default-proxies-alist
;              '(nil "\\`root\\'" "/ssh:%h:"))
; (add-to-list 'tramp-default-proxies-alist
;              '((regexp-quote (system-name)) nil nil))

(defun read-file (filename)
  "Reads the data from a file and returns it as a string"
  (let ((real-filename (expand-file-name filename)))
    (with-temp-buffer
      (insert-file-contents real-filename)
      (buffer-string))))

(defun load-ssh-agent-env ()
  "Reads an SSH agent environment file and sets the necessary environment variables"
  (interactive)
  (let ((agent-env-fn (concat (file-name-as-directory (getenv "HOME"))
                              (file-name-as-directory ".ssh")
                              "agent_env.sh")))
    (if (file-readable-p agent-env-fn)
        (let* ((ssh-data (read-file agent-env-fn))
               (auth-sock (progn
                            (string-match "SSH_AUTH_SOCK=\\(.*?\\);" ssh-data)
                            (match-string 1 ssh-data)))
               (agent-pid (progn
                            (string-match "SSH_AGENT_PID=\\([0-9]*\\)?;" ssh-data)
                            (match-string 1 ssh-data)))
               )
          (setenv "SSH_AUTH_SOCK" auth-sock)
          (setenv "SSH_AGENT_PID" agent-pid)
          (list auth-sock agent-pid)
          (message (format "Using SSH agent %s via %s" agent-pid auth-sock)))
      (message (format "No SSH agent environment file found: " agent-env-fn)))))

(autoload 'dash-at-point "dash-at-point"
  "Search the word at point with Dash." t nil)
(global-set-key "\C-cd" 'dash-at-point)

(defun align-on-equals (begin end)
  "Align region on equal signs"
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=" 1 1 ))
(global-set-key "\C-x|" 'align-on-equals)

(require 'nginx-mode)
(add-to-list 'auto-mode-alist '(".*/nginx/sites-enabled/.*" . nginx-mode))
(add-to-list 'auto-mode-alist '(".*/nginx/files/.*\.conf" . nginx-mode))
(add-to-list 'auto-mode-alist '(".*/dev_nginx.*\.conf" . nginx-mode))

(if (file-directory-p "~/work/scala-mode2")
    (progn
      (add-to-list 'load-path "~/work/scala-mode2")
      (require 'scala-mode2)))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXECUTION

(load-ssh-agent-env)
(setq command-line-default-directory (concat (getenv "HOME") "/"))

(setq server-socket-dir "~/.emacs.d/sockets")
(if (not (file-exists-p (format "%s/%s" server-socket-dir "server")))
    (server-start))
