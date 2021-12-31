;; Perl

;; Indentation settings
(setq cperl-extra-newline-before-brace t
      cperl-brace-offset              -2
      cperl-merge-trailing-else        nil
      cperl-indent-level               4
      tab-width                        4
      default-tab-width                4
      indent-tabs-mode                 nil)

;; (setq perl-var-definition-regexp "^\\s-*\\(my\\|our\\|local\\)\\(\\s-+\\|\\s-*([^)]*\\)%s\\>")

(defun perltidy-region ()
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "perltidy" nil t))

(defun perltidy-buffer ()
  (interactive)
  (let ((position (point)))
    (shell-command-on-region (point-min) (point-max) "perltidy" nil t)
    (goto-char position)))

(defun my-perl-projectile-etags ()
  (interactive)
  (cd (projectile-project-root))
  (async-shell-command "etags --language=perl `find -type f -not -name '*.*' -not -path './.git/*' -not -path './.svn/*' -o -name '*.p[lm]'`"))

;; Perl keys
(eval-after-load 'cperl-mode
  '(progn
     (define-key cperl-mode-map (kbd "C-o f a") 'my-projectile-grep)
     
     (define-key cperl-mode-map (kbd "C-o e t") 'my-perl-projectile-etags)
     
     (define-key cperl-mode-map (kbd "C-o p t r") 'perltidy-region)
     (define-key cperl-mode-map (kbd "C-o p t b") 'perltidy-buffer)

     (define-key cperl-mode-map (kbd "<f9> r") 'perltidy-region)
     (define-key cperl-mode-map (kbd "<f9> b") 'perltidy-buffer)

     (define-key cperl-mode-map (kbd "M-<f5>") (lambda () (interactive) (cperl-mode)))))

(provide 'perl-settings)
