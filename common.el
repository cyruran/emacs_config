;; Packages
(setq package-archives
      (quote (("gnu" . "http://elpa.gnu.org/packages/")
              ("melpa" . "https://melpa.org/packages/"))))

(package-initialize)

(defun set-exec-path-from-shell-PATH ()
  (interactive)
  (setenv "PATH" (s-chomp (shell-command-to-string "bash -c '. ~/.bashrc; echo $PATH'"))))

(defun copy-gdb-file-ref ()
  (interactive)
  (kill-new (format "%s:%s"
                    (file-name-nondirectory (buffer-file-name))
                    (line-number-at-pos))))

(global-unset-key "\C-o")
(global-unset-key "\C-z")

(global-set-key (kbd "C-s-b")
                (lambda ()
                  (interactive)
                  (bookmark-jump
                   (bookmark-completing-read "Jump to bookmark") 'switch-to-buffer-other-frame)))
(global-set-key (kbd "C-s-s")
                (lambda ()
                  (interactive)
                  (switch-to-buffer-other-frame "*scratch*")))

(global-set-key (kbd "<C-s-left>") #'tab-previous)
(global-set-key (kbd "<C-s-right>") #'tab-next)
(global-set-key (kbd "<C-s-down>") #'tab-new)
(global-set-key (kbd "<C-s-up>") #'tab-rename)
(global-set-key (kbd "C-s-q") #'tab-close)
(global-set-key [C-tab] (lambda ()
                          (interactive)
                          (tab-bar-select-tab (1+ (tab-bar--tab-index-recent 0)))))

;; From customize
(setq dired-dwim-target t)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq helm-always-two-windows t)
(setq helm-full-frame nil)
(setq helm-use-frame-when-more-than-two-windows nil)
(setq helm-use-undecorated-frame-option t)

(setq mouse-wheel-progressive-speed nil)
(setq parens-require-spaces nil)

(global-set-key (kbd "<XF86Favorites>") 'bookmark-jump)

(defun my-dired-select-this-file ()
  (interactive)
  (let ((full-filename (buffer-file-name)))
    (dired (if full-filename
               (file-name-directory full-filename)
             default-directory))
    (if full-filename
        (let ((filename (file-name-nondirectory full-filename)))
          (beginning-of-buffer)
          (search-forward-regexp (format "\\<%s\\>" filename))))))

(global-set-key (kbd "C-x d") 'my-dired-select-this-file)

(defun smart-quote (key key1)
  (if (region-active-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (goto-char start)
        (insert key)
        (goto-char (1+ end))
        (insert key1))
    (insert key key1)))

(defun my:magit-reset-hard (commit)
  (interactive (list (magit-read-branch-or-commit "Hard reset to" (concat "origin/" (magit-get-current-branch)))))
  (magit-reset-internal "--hard" commit))

(global-set-key (kbd "\"") 'self-insert-command);; (lambda () (interactive) (smart-quote "\"" "\""))
(global-set-key (kbd "M-\"") (lambda ()
                               (interactive)
                               (smart-quote "\"" "\"")
                               (backward-char)))

(global-set-key (kbd "M-<f1>") (lambda ()
                                 (interactive)
                                 (kill-buffer (buffer-name))
                                 (delete-frame)))

(setq magit-define-global-key-bindings nil)

(global-set-key (kbd "<f12> s") 'magit-status)
(global-set-key (kbd "<f12> c") 'magit-checkout)
(global-set-key (kbd "<f12> b") 'magit-branch-and-checkout)
(global-set-key (kbd "<f12> f") 'magit-fetch-all)
(global-set-key (kbd "<f12> F") 'magit-fetch-all-prune)
(global-set-key (kbd "<f12> l") 'magit-log-all)
(global-set-key (kbd "<f12> p") 'magit-pull)
(global-set-key (kbd "<f12> r") 'magit-rebase)
(global-set-key (kbd "<f12> R") 'my:magit-reset-hard)
(global-set-key (kbd "<f12> d") 'magit-branch-delete)

(defun git-status-or-vc-dir ()
  (interactive)
  (if (string= (vc-backend (buffer-file-name)) "Git")
      (magit-status)
    (call-interactively 'vc-dir)))

(global-set-key (kbd "C-x v d") 'git-status-or-vc-dir)

(setq tramp-default-method "ssh")
(advice-add #'tramp-sh-handle-vc-registered :override #'ignore)
(add-to-list 'backup-directory-alist
              (cons tramp-file-name-regexp "/tmp/"))

(defun my--plain-find-file-if-remote (orig-fun &rest args)
  (if (string= (file-remote-p default-directory 'method) "ssh")
      (progn
        (message "%S" args)
        (call-interactively #'find-file))
    (apply orig-fun args)))

(advice-add #'helm-find-files :around #'my--plain-find-file-if-remote)
;; (advice-mapc (lambda (advice _props) (advice-remove #'helm-find-files advice)) #'helm-find-files)

(defalias 'list-buffers 'ibuffer)
;; (defalias 'yes-or-no-p 'y-or-no-p)
(defun yes-or-no-p (&optional prompt)
  (interactive) (if prompt (y-or-n-p prompt) (call-interactively 'y-or-no-p)))

(global-set-key (kbd "M-<up>") (lambda () (interactive) (previous-line 5)))
(global-set-key (kbd "M-<down>") (lambda () (interactive) (next-line 5)))

;; Fix for putty
(global-set-key (kbd "ESC <up>") (lambda () (interactive) (previous-line 5)))
(global-set-key (kbd "ESC <down>") (lambda () (interactive) (next-line 5)))

(defun sudo-reopen ()
  (interactive)
  (aif (or (buffer-file-name)
           default-directory)
      (find-file (format "/sudo::%s" it))))

(defun plain-reopen ()
  (interactive)
  (aif (file-remote-p (or (buffer-file-name)
                          default-directory)
                      'localname)
      (find-file it)))

;; Appearance
(tool-bar-mode -1)
(menu-bar-mode -1)
;; (if (commandp 'scroll-bar-mode)
;;     (scroll-bar-mode -1))
(setq-default scroll-bar-width 12)
(set-window-scroll-bars (minibuffer-window) nil nil)
(delete-selection-mode 1)

(setq dired-listing-switches "-alh --group-directories-first")
(show-paren-mode 1)

(which-function-mode 1)

(set-default 'tags-case-fold-search nil)

(setq-default indent-tabs-mode nil)
(setq compilation-scroll-output 1)

(set-face-attribute 'default nil :height 120)

;; Helm
(progn
  (require 'helm)
  ;; (require 'helm-config)
  (helm-mode 1)

  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)

  (global-set-key (kbd "M-s M-i") 'helm-occur))

;; Perl settings
(defalias 'perl-mode 'cperl-mode)
(eval-after-load 'cperl-mode
  '(require 'perl-settings))

;; My hotkeys
(require 'my-functions)

;; Disable deselecting after killing
;; (defadvice kill-ring-save (after keep-transient-mark-active ())
;;   "Override the deactivation of the mark."
;;   (setq deactivate-mark nil))
;; (ad-activate 'kill-ring-save)

;; GUD
(eval-after-load 'gud-mode
  '(progn (gud-def gud-dump "x %e" "x" "Dump current expression")
          (global-set-key (kbd "<f6>") 'gud-next)
          (global-set-key (kbd "<f7>") 'gud-step)
          (global-set-key (kbd "<f5>") 'gud-dump)))

;; FSVN
(eval-after-load 'fsvn-mode
  '(require 'fsvn-custom nil t))

;; Complete
(eval-after-load 'prog-mode
  '(require 'company-settings nil t))

(global-set-key (kbd "M-\\") 'complete-symbol)

;; Projectile
(require 'projectile-settings nil t)

;; Custom helm
(eval-after-load 'helm-mode
  '(defun my-helm-upload-files (candidate)
     (my-upload
      (read-string "Enter hostname: ")
      (helm-marked-candidates)
      (read-string "Enter remote path: " "/root/kmoise"))))

;; (add-to-list 'load-path "~/.emacs.d/custom/")
(eval-after-load 'projectile-mode
  (require 'deploy nil t))

;; Async
(async-bytecomp-package-mode 1)
(setq async-bytecomp-allowed-packages '(all))

;; ps: .\ncat.exe -k -l 5656 | %{&"firefox" $_}
(defun browse-in-windows (url &rest args)
  (shell-command (format "echo %s | nc -q 0 10.0.2.2 5656" url)))

(eval-after-load 'org-mode
  '(require 'org-word-export))

(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'typescript-mode-hook 'subword-mode)

(defun replace-with-relative-path ()
  (interactive)
  (let ((fn (buffer-substring-no-properties (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (insert (file-relative-name fn))))

(require 'skeletons)
(setq vterm-keymap-exceptions '("C-c" "C-x" "C-h" "M-x" "M-o" "M-y" "C-b" "M-w" "C-b" "M-v" "C-v" "M-:"))
(add-hook 'vterm-mode-hook (lambda ()
                             (local-unset-key [f12])))

(setq tab-width 4)

(require 'dired)
(define-key dired-mode-map (kbd "<mouse-3>") #'dired-mouse-find-file)

(require 'vterm)

(define-key vterm-mode-map (kbd "C-c") 'vterm--self-insert)

(define-key vterm-mode-map (kbd "C-b C-b")             #'vterm-send-C-b)
(define-key vterm-mode-map (kbd "C-b C-u")             #'universal-argument)
(define-key vterm-mode-map (kbd "C-b C-l")             #'vterm-clear-scrollback)
(define-key vterm-mode-map (kbd "C-b C-r")             #'vterm-reset-cursor-point)
(define-key vterm-mode-map (kbd "C-b C-n")             #'vterm-next-prompt)
(define-key vterm-mode-map (kbd "C-b C-p")             #'vterm-previous-prompt)
(define-key vterm-mode-map (kbd "C-b C-t")             #'vterm-copy-mode)
(define-key vterm-mode-map (kbd "C-b C-y")             #'vterm-yank-primary)
(define-key vterm-mode-map (kbd "C-b C-c")             #'vterm-reset-cursor-point)
(define-key vterm-mode-map (kbd "C-b C-r")             (lambda ()
                                                         (interactive)
                                                         (vterm-copy-mode t)
                                                         (isearch-backward)))
(define-key vterm-mode-map [mouse-2]                   (lambda ()
                                                         (interactive)
                                                         (deactivate-mark)
                                                         (vterm-yank-primary)))
(define-key vterm-mode-map [C-M-mouse-1]                 (lambda (e)
                                                         (interactive "e")
                                                         (vterm-goto-char (nth 1 (cadr e)))))

(global-set-key (kbd "C-x C-l") (lambda ()
                              (interactive)
                              (list-buffers
                               nil ;; OTHER-WINDOW-P
                               "*VTerm Ibuffer*" ;; NAME
                               '((mode . vterm-mode)) ;; QUALIFIERS
                               nil ;; NOSELECT
                               nil ;; SHRINK
                               nil ;; FILTER-GROUPS
                               '((mark name)) ;; FORMATS
                               )))

(define-key vterm-copy-mode-map " " #'scroll-up-command)
(define-key vterm-copy-mode-map (kbd "<backspace>") #'scroll-down-command)

(defun incr-hex-color (arg)
  (interactive "p")
  (when (use-region-p)
    (aif (buffer-substring (region-beginning) (region-end))
         (when (string-match "\\`\\(..\\)\\(..\\)\\(..\\)\\'" it)
           (delete-active-region)
           (insert (string-join (mapcar (lambda (x) (format "%x" (+ (string-to-number x 16) arg)))
                                              (list (match-string 1 it)
                                                    (match-string 2 it)
                                                    (match-string 3 it)))))))))

(require 'komi-input)
(require 'vterm-prep)

(require 'my-work nil t)

(require 'vto)
(require 'run-term-commands)

;; (require 'mux)
(require 'neotree-patch)

(set-exec-path-from-shell-PATH)

(provide 'common)
