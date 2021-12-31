(add-to-list 'load-path "~/emacs_config/")
(require 'common)

;; Customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" default)))
 '(dired-dwim-target t)
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(eww-search-prefix "https://m.ya.ru/?q=")
 '(font-use-system-font t)
 '(fringe-mode 10 nil (fringe))
 '(highlight-indent-guides-auto-enabled nil)
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(mode-line-format
   (quote
    ("%e" mode-line-modified mode-line-remote " " mode-line-buffer-identification " "
     ((which-func-mode
       ("" which-func-format " ")))
     ("%l:%c %p")
     " " mode-line-modes
     (vc-mode vc-mode)
     mode-line-end-spaces)))
 '(package-selected-packages
   (quote
    (helm-mt multi-term expand-region multiple-cursors solarized-theme ace-jump-buffer ace-jump-helm-line ace-jump-mode ace-window helm helm-company helm-make helm-projectile magit projectile s)))
 '(show-paren-mode t)
 '(term-bind-key-alist
   (quote
    (("M-<backspace>" . term-send-backward-kill-word)
     ("C-c" . term-interrupt-subjob)
     ("M-e" . term-send-esc)
     ("C-p" . previous-line)
     ("C-n" . next-line)
     ("C-s" . term-send-raw)
     ("C-r" . term-send-raw)
     ("C-m" . term-send-return)
     ("C-y" . term-paste)
     ("M-f" . term-send-forward-word)
     ("M-b" . term-send-backward-word)
     ("M-o" . term-send-backspace)
     ("M-p" . term-send-up)
     ("M-n" . term-send-down)
     ("M-M" . term-send-forward-kill-word)
     ("M-N" . term-send-backward-kill-word)
     ("<C-backspace>" . term-send-backward-kill-word)
     ("M-r" . term-send-reverse-search-history)
     ("M-d" . term-send-delete-word)
     ("M-," . term-send-raw)
     ("M-." . comint-dynamic-complete))))
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-very-old-color nil))
(put 'dired-find-alternate-file 'disabled nil)

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
