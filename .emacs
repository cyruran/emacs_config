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
 '(dired-dwim-target t)
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(eww-search-prefix "https://m.ya.ru/?q=")
 '(font-use-system-font t)
 '(fringe-mode 10 nil (fringe))
 '(highlight-indent-guides-auto-enabled nil)
 '(linum-format " %6d ")
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(minimap-mode nil)
 '(mode-icons-mode t)
 '(mode-line-format
   (quote
    ("%e" mode-line-modified mode-line-remote " " mode-line-buffer-identification " "
     ((which-func-mode
       ("" which-func-format " ")))
     ("%l:%c %p")
     " " mode-line-modes
     (vc-mode vc-mode)
     mode-line-end-spaces)))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-very-old-color nil))
(put 'dired-find-alternate-file 'disabled nil)

;; (require 'graphene)

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
