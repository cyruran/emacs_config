;; Projectile settings

;; Enable projectile with dired-mode only on local machine
(add-hook 'dired-mode-hook
          (lambda ()
            (if (file-remote-p dired-directory)
                nil
              (projectile-mode t))))

;; Enable projectile with fsvn-browse only on local machine
(eval-after-load 'fsvn-mode
  '(add-hook 'fsvn-browse-mode-prepared-hook
             (lambda ()
               (if (file-remote-p dired-directory)
                   nil
                 (projectile-mode t)))))

;; Enable caching and use native indexing method
(setq
 projectile-indexing-method 'native
 projectile-enable-caching t)

(eval-after-load 'projectile
  '(progn
     (defun my-projectile-grep (&optional regexp)
       (interactive)
       (if regexp
           (projectile-grep regexp)
         (call-interactively 'projectile-grep)))
     (defun my-projectile-build ()
       (interactive)
       (let* ((root (projectile-project-root))
                    (bf (format "%s/build_rule.sh" root)))
         (if (file-exists-p bf)
             (compile (format "%s %s" bf root))
           (message "%s %s" bf "not found"))))
     (define-key projectile-mode-map (kbd "C-o m") 'helm-make-projectile)
     (define-key projectile-mode-map (kbd "M-s M-u") 'helm-projectile-grep)
     (define-key projectile-mode-map (kbd "C-x p h") 'helm-projectile)
     (define-key projectile-mode-map (kbd "C-x p s g") 'projectile-grep)
     (define-key projectile-mode-map (kbd "C-x p i") 'projectile-invalidate-cache)
     (define-key projectile-mode-map (kbd "C-x p D") 'projectile-dired)
     (define-key projectile-mode-map (kbd "C-x p b") 'projectile-ibuffer)))

(defun my--remote-project-p (oldfun &rest r)
  (if (file-remote-p default-directory)
      nil
    (funcall oldfun r)))

(advice-add #'projectile-project-p :around #'my--remote-project-p)

(provide 'projectile-settings)
