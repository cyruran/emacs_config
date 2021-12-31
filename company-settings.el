;; Enable company-mode only for local filenames with prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              (when filename
                (if (file-remote-p filename)
                    nil
                  (progn (company-mode t)
                         (projectile-mode t)))))))

;; Enable lines numbering for prog-mode
(add-hook 'prog-mode-hook 'linum-mode)

(eval-after-load 'company-mode '(define-key company-mode-map (kbd "M-\\") 'helm-company))

(provide 'company-settings)
