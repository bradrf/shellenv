(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bookmark-save-flag 1)
 '(coffee-tab-width 2)
 '(custom-enabled-themes (quote (tango-dark)))
 '(delete-selection-mode t)
 '(enh-ruby-deep-indent-paren nil)
 '(fci-rule-color "gray10")
 '(fill-column 100)
 '(font-use-system-font t)
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules")))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode t nil (ido))
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(menu-bar-mode nil)
 '(mouse-avoidance-mode (quote animate) nil (avoid))
 '(mouse-avoidance-nudge-dist 30)
 '(nginx-indent-level 2)
 '(ns-alternate-modifier (quote super))
 '(ns-command-modifier (quote meta))
 '(paragraph-start "\\f\\\\|[ \\t]*$\\\\|[ \\t]*[-+*] ")
 '(ruby-indent-level 2)
 '(save-place t nil (saveplace))
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-follow-symlinks t)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-sql-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-external-variable ((t (:foreground "magenta"))))
 '(whitespace-line ((t (:background "Red"))))
 '(whitespace-space ((t (:foreground "gray20")))))

;; help emacs figure out unknown interpreter files (i.e. based on shebang value)
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; todo: fix setting of font per os
;;       fix inc/dec font for whole window, not just buffer

(add-to-list 'load-path "~/.emacs.d/elisp")

;; use list-packages to choose those to install
;; use describe-package to show docs about package
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (eq system-type 'darwin)
  (global-set-key "\M-`" 'other-frame)
  ;; the following is a workaround to avoid long controlpaths for ssh via tramp
  (put 'temporary-file-directory 'standard-value '((file-name-as-directory "/tmp"))))
(when (eq system-type 'gnu/linux)
  (setq myfont "DejaVu Sans Mono-8")
  (set-default-font myfont)
  (set-face-attribute 'default t :font  myfont )
  (set-frame-font  myfont nil t))

;;TRANSPARENCY: (set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 95 95))
(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(95 95))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

;; enable fill-column-mode in all buffers
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

(global-set-key "\C-z" 'undo)

(global-set-key "\C-c\C-c" 'comment-region)

; better copy/paste abilities
(when (require 'easy-kill nil 'noerror)
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

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

;; obsolete in favor of icomplete-mode or ido-mode
;; (iswitchb-mode t)
;; (global-set-key "\C-xb" 'iswitchb-buffer)
;; (global-set-key "\C-xB" 'iswitchb-buffer-other-window)
;; (global-set-key "\C-x\C-b" 'ibuffer)
;; (setq read-buffer-function 'iswitchb-read-buffer)

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

;; move to top of list unless providing a starting number...
(defun renumber (&optional num)
  "Renumber the list items in the current paragraph, starting at point."
  (interactive "p")
  (setq num (or num 1))
  (let ((end (save-excursion
               (forward-paragraph)
               (point))))
    (while (re-search-forward "^[0-9]+" end t)
      (replace-match (number-to-string num))
      (setq num (1+ num)))))

;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Parsing.html
(defun insert-timestamp ()
  "Insert string for ISO 8601 timestamp (HTTP/JSON, e.g. 2015-07-21T08:11:42-0700)."
  (Interactive)
  (insert (format-time-string "%Y-%m-%dT%T%z")))
(defun insert-now ()
  "Insert string for the current time (e.g. 8:07 AM)."
  (interactive)
  (insert (format-time-string "%l:%M %p")))
(defun insert-today ()
  "Insert string for today (e.g. Tue, Jul 21, 2015)."
  (interactive)
  (insert (format-time-string "%a, %b %e, %Y")))

(defun my-text-mode-hook ()
  ;(turn-on-auto-fill)
  (turn-on-filladapt-mode)
  ;(longlines-mode) was replaced by...
  (visual-line-mode)
  (flyspell-mode))
(add-hook 'text-mode-hook 'my-text-mode-hook)

; Use describe-char (C-u C-x =) on an unknown Unicode character and use the #x<VALUE> in map below:
(defun tidy-replace-unicode-characters ()
  "Tidy up a buffer by replacing all special Unicode characters
   (smart quotes, etc.) with their more sane cousins"
  (interactive)
  (let ((unicode-map '(("[\u2018\|\u2019\|\u201A\|\uFFFD]" . "'")
                       ("[\u201c\|\u201d\|\u201e]" . "\"")
                       ("[\u2013\|\u2014]" . "-")
                       ("\u2026" . "...")
                       ("\u2014" . "-")
                       ("[\u2022|\u25a1|\u2606]" . "*")
                       ("\u00A9" . "(c)")
                       ("\u00AE" . "(r)")
                       ("\u2122" . "TM")
                       ("[\u02DC\|\u00A0]" . " "))))
    (save-excursion
      (cl-loop for (key . value) in unicode-map
            do
            (goto-char (point-min))
            (replace-regexp key value)))))

; Use this to find new chars to put in tidy list above ^^^
(defun find-next-unsafe-char (&optional coding-system)
  "Find the next character in the buffer that cannot be encoded by
coding-system. If coding-system is unspecified, default to the coding
system that would be used to save this buffer. With prefix argument,
prompt the user for a coding system."
  (interactive "Zcoding-system: ")
  (if (stringp coding-system) (setq coding-system (intern coding-system)))
  (if coding-system nil
    (setq coding-system
          (or save-buffer-coding-system buffer-file-coding-system)))
  (let ((found nil) (char nil) (csets nil) (safe nil))
    (setq safe (coding-system-get coding-system 'safe-chars))
    ;; some systems merely specify the charsets as ones they can encode:
    (setq csets (coding-system-get coding-system 'safe-charsets))
    (save-excursion
      ;;(message "zoom to <")
      (let ((end  (point-max))
            (here (point    ))
            (char  nil))
        (while (and (< here end) (not found))
          (setq char (char-after here))
          (if (or (eq safe t)
                  (< char ?\177)
                  (and safe  (aref safe char))
                  (and csets (memq (char-charset char) csets)))
              nil ;; safe char, noop
            (setq found (cons here char)))
          (setq here (1+ here))) ))
    (and found (goto-char (1+ (car found))))
    found))

(defun my-whitespace-hook ()
  (make-local-variable 'whitespace-line-column)
  (setq whitespace-line-column fill-column)
  (make-local-variable 'whitespace-style)
  (setq whitespace-style (quote (face tabs spaces trailing lines-tail space-before-tab
                                      indentation empty space-after-tab space-mark tab-mark)))
  (make-local-variable 'whitespace-action)
  (setq whitespace-action (quote (auto-cleanup)))
  (whitespace-mode)
  (whitespace-cleanup-mode))
(global-set-key "\C-cl" (lambda ()
                          (interactive)
                          (whitespace-toggle-options (quote (lines-tail)))))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(defun my-js-mode-hook ()
  (load-library "js2-refactor")
  (js2r-add-keybindings-with-prefix "C-c C-m") ; https://github.com/magnars/js2-refactor.el#refactorings
  (flycheck-mode t)
  (setq js2-highlight-level 3)
  (tern-mode t)
  (my-whitespace-hook))
(add-hook 'js2-mode-hook 'my-js-mode-hook)
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(defun my-coffee-mode-hook ()
  (my-whitespace-hook))
(add-hook 'coffee-mode-hook 'my-coffee-mode-hook)

(defun my-python-mode-hook ()
  (setq python-indent-offset 4)
  (my-whitespace-hook))
(add-hook 'python-mode-hook 'my-python-mode-hook)

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; (require 'ruby-mode) => using enh-ruby-mode instead
(add-to-list 'auto-mode-alist '("Gemfile" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb" . enh-ruby-mode))
(defun my-ruby-mode-hook ()
  (my-whitespace-hook))
(add-hook 'enh-ruby-mode-hook 'my-ruby-mode-hook)

; ruby repl to interact with ruby/rails code (quick jump to definition/docs...requires pry)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode-hook 'robe-mode)

; project management (indexing/caching of files in a project)
;(add-hook 'projectile-mode-hook 'projectile-rails-on)
;(add-hook 'ruby-mode-hook 'projectile-mode)
;(add-hook 'enh-ruby-mode-hook 'projectile-mode)

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

;; Amazing utility that can rename symbols for all known modes! Uses "multiple cursors" to edited
;; simultaneously.
(global-set-key (kbd "C-c m .") 'mc/mark-all-like-this-dwim) ; matches "smartly" using mode context
(global-set-key (kbd "C-c m *") 'mc/mark-all-symbols-like-this) ; matches everything in buffer

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

; (autoload 'dash-at-point "dash-at-point"
;   "Search the word at point with Dash." t nil)
; (global-set-key "\C-cd" 'dash-at-point)

(defun align-on-equals (begin end)
  "Align region on equal signs"
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=" 1 1 ))
(global-set-key "\C-x|" 'align-on-equals)

(require 'nginx-mode)
(add-to-list 'auto-mode-alist '(".*/nginx/sites-enabled/.*" . nginx-mode))
(add-to-list 'auto-mode-alist '(".*/nginx/files/.*\.conf" . nginx-mode))
(add-to-list 'auto-mode-alist '(".*/dev_nginx.*\.conf" . nginx-mode))

(add-to-list 'auto-mode-alist '("\\.scala" . scala-mode))

;(require 'web-mode) <= doesn't work for some reason, so we do this...
(autoload 'web-mode "web-mode" "web-mode" t nil)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; install notes:
;;   1. brew install gdbm
;;   2. gem install evernote_oauth gdbm ffi
;;   3. cd evernote-mode/ruby
;;   4. ruby setup.rb
(require 'evernote-mode)
(load "~/creds/evernote-config.el") ; keep devkey out of git! (setq evernote-developer-token "")
(setq evernote-mode-hook '(lambda () (progn (flyspell-mode))))
(setq enh-enclient-command (concat (getenv "HOME") "/.rvm/rubies/default/bin/enclient.rb"))

;; opens file in buffer on github
;; require not needed when installed via package (require 'browse-at-remote)

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))
(add-to-list 'auto-mode-alist '("\\.log" . display-ansi-colors))

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

(toggle-frame-maximized)

(rvm-use-default)
;; The following activates rvm automatically (to allow robe to use the correct ruby)
(require 'inf-ruby)
(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

(load-ssh-agent-env)
(setq command-line-default-directory (concat (getenv "HOME") "/"))

(setq server-socket-dir "~/.emacs.d/sockets")
(if (not (file-exists-p (format "%s/%s" server-socket-dir "server")))
    (server-start))
