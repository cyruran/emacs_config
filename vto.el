(define-derived-mode vto-mode org-mode "vto"
  "Major mode for vto"
  (vterm-prep-mode t)
  (setq org-startup-folded "content")
  (setq truncate-lines nil)
  (setq org-mouse-1-follows-link nil)
  (setq org-link-any-re: "\\'\\`"))

(define-key vto-mode-map (kbd "M-RET") #'mc/mark-next-like-this)

(defun vto-assign-vterm-buffer ()
  (interactive)
  (let ((base-buffer (pm-base-buffer)))
    (with-current-buffer base-buffer
      (setq-local vterm-prep-assosiated-buffer
                  (let ((ev (read-event)))
                    (and (mouse-event-p ev)
                         (window-buffer (caadr ev)))))
      (switch-to-buffer base-buffer))))

(defun vto-create-associated-term ()
  (interactive)
  (let* ((prep-buff (current-buffer))
         (vterm-buff (prog2
                         (split-window-below)
                         (vterm (format "*vterm %s*" (buffer-name)))
                       (if (boundp 'vto-local-cmd)
                           (vterm-send-string vto-local-cmd)))))
     (setq pop-up-windows t)
     (modify-syntax-entry ?. "_")
     (message default-directory)
     (with-current-buffer prep-buff
       (setq-local vterm-prep-assosiated-buffer vterm-buff))))

(defun vto-insert-sh ()
  (interactive)
  (insert "#+BEGIN_SRC bash\n\n#+END_SRC")
  (previous-line)
  (org-cycle))

(defun vto-insert-elisp ()
  (interactive)
  (insert "#+BEGIN_SRC emacs-lisp\n\n#+END_SRC")
  (previous-line)
  (org-cycle))

(add-to-list 'auto-mode-alist '("\\.vto\\.org" . vto-mode))

(provide 'vto)
