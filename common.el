;; Packages
(setq package-archives
      (quote (("gnu" . "http://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/")
          ;; ("marmalade" . "http://marmalade-repo.org/packages/")
          )))

(package-initialize)

(global-unset-key "\C-o")
(global-unset-key "\C-z")

(global-set-key (kbd "<XF86Favorites>") 'bookmark-jump)

(defun my-dired-select-this-file ()
  (interactive)
  (let ((full-filename (buffer-file-name)))
    (call-interactively 'dired)
    (if full-filename
      (let ((dir (file-name-directory full-filename))
            (filename (file-name-nondirectory full-filename)))
        (beginning-of-buffer)
        (search-forward-regexp (format "\\b%s\\b" filename))))))

(global-set-key (kbd "C-x d") 'my-dired-select-this-file)

(defun smart-quote (key key1)
  (if (region-active-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (goto-char start)
        (insert key)
        (goto-char (1+ end))
        (insert key1))
    (insert key)))

(defun my:magit-reset-hard (commit)
  (interactive (list (magit-read-branch-or-commit "Hard reset to" (concat "origin/" (magit-get-current-branch)))))
  (magit-reset-internal "--hard" commit))

(global-set-key (kbd "\"") (lambda () (interactive) (smart-quote "\"" "\"")))

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

(global-set-key (kbd "<f12> C") 'forge-checkout-pullreq)

(defun git-status-or-vc-dir ()
  (interactive)
  (if (string= (vc-backend (buffer-file-name)) "Git")
      (magit-status)
    (call-interactively 'vc-dir)))

(global-set-key (kbd "C-x v d") 'git-status-or-vc-dir)

(require 'tap-mode)

(setq tramp-default-method "ssh")

(defalias 'list-buffers 'ibuffer)
;; (defalias 'yes-or-no-p 'y-or-no-p)
(defun yes-or-no-p (&optional prompt)
  (interactive) (if prompt (y-or-n-p prompt) (call-interactively 'y-or-no-p)))

(global-set-key (kbd "M-<up>") (lambda () (interactive) (previous-line 5)))
(global-set-key (kbd "M-<down>") (lambda () (interactive) (next-line 5)))

(defun sudo-reopen ()
  (interactive)
  (find-file (format "/sudo::%s" (buffer-file-name))))

;; Appearance
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq dired-listing-switches "-lAXGh --group-directories-first")
(show-paren-mode 1)

(which-function-mode 1)

(set-default 'tags-case-fold-search nil)

(setq-default indent-tabs-mode nil)

;; (set-frame-font "Hack 13" nil t)
(set-face-attribute 'default nil :height 120)

;; (powerline-default-theme)

;; Org
(global-set-key (kbd "C-c l") 'org-store-link)

;; Pl-sense
;; (require 'plsense)
;; (setq plsense-popup-help-key "C-:")
;; (setq plsense-jump-to-definition-key "C->")

;; (plsense-config-default)

;; Ido
;; (ido-mode 1)
;; (require 'smex)
;; (global-set-key (kbd "M-x") 'smex)

;; Helm
(require 'helm)
(require 'helm-config)
(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "M-s M-i") 'helm-occur)

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
;; (gud-def gud-break-current-function (concat "b " (which-function)) "\C-B" "Break on current function")

;; FSVN
(eval-after-load 'fsvn-mode
  '(require 'fsvn-custom nil t))

;; Complete
(eval-after-load 'prog-mode
  '(require 'company-settings nil t))

;; Projectile
(require 'projectile-settings nil t)

;; (require 'term-settings nil t)

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

;; (helm-add-action-to-source "Upload files" 'my-helm-upload-files helm-source-projectile-files-list)

;; ps: .\ncat.exe -k -l 5656 | %{&"firefox" $_}
(defun browse-in-windows (url &rest args)
  (shell-command (format "echo %s | nc -q 0 10.0.2.2 5656" url)))

;; (require 'jcl-mode)
(eval-after-load 'org-mode
  '(require 'org-word-export))

(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'typescript-mode-hook 'subword-mode)

(defun replace-with-relative-path ()
  (interactive)
  (let ((fn (buffer-substring-no-properties (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (insert (file-relative-name fn))))

(setq lsp-keymap-prefix "s-m")
(require 'lsp-mode)
(require 'lsp-python-ms)
;; (add-hook 'python-mode-hook #'lsp-mode)
;; (add-hook 'python-mode-hook #'(lambda () (lsp-ui-mode 0)))
;; (add-hook 'python-mode-hook #'lsp)
(add-hook 'python-mode-hook #'highlight-indentation-mode)
(add-hook 'typescript-mode-hook #'lsp-mode)
(add-hook 'typescript-mode-hook #'(lambda () (lsp-ui-mode 0)))
(add-hook 'typescript-mode-hook #'lsp)

(add-hook 'c++-mode-hook #'lsp-mode)
(add-hook 'c++-mode-hook #'lsp)

;; (add-hook 'js-mode-hook #'lsp-mode)
;; (add-hook 'js-mode-hook #'lsp)

(require 'skeletons)

(require 'komi-input)

;; (require 'chords)

(provide 'common)
