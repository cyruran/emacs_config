(define-minor-mode vterm-prep-mode ""
  :keymap `((,(kbd "C-c C-c") . vterm-prep-send)
            (,(kbd "C-c C-;") . ,(lambda ()
                                   (interactive)
                                   (comment-region (point-min) (point-max))
                                   (goto-char (point-max))
                                   (newline)))
            (,(kbd "<header-line> <mouse-3>") . ,(lambda ()
                                                  (interactive)
                                                  (let ((window (window-buffer
                                                                 (window-at (cadr (mouse-position))
                                                                            (cddr (mouse-position))
                                                                            (car (mouse-position))))))
                                                    (with-current-buffer window
                                                      (pop-to-buffer vterm-prep-assosiated-buffer nil t))))))
  :lighter "[prep]")


(defun vterm-prep-check-buffer-or-let-know ()
  (if (buffer-live-p (get-buffer vterm-prep-assosiated-buffer))
      t
   (progn
     (message "Terminal buffer lost" vterm-prep-assosiated-buffer)
     (setq header-line-format (propertize ">> DISCONNECTED" 'face '(:foreground "red")))
     nil)))

(defun vterm-prep-send ()
  (interactive)
  (when (vterm-prep-check-buffer-or-let-know)
      (let ((content (buffer-substring (point-min) (point-max))))
        (with-current-buffer vterm-prep-assosiated-buffer
          (vterm-insert content)
          (vterm-send-return))
        (if (vterm-prep-check-buffer-or-let-know)
            (pop-to-buffer vterm-prep-assosiated-buffer nil t)))))

(defun vterm-prep ()
  (interactive)
  (let ((vterm-buffer (current-buffer)))
    (switch-to-buffer-other-window (generate-new-buffer "*vterm-prep*"))
    (sh-mode)
    (vterm-prep-mode)
    (make-local-variable 'vterm-prep-assosiated-buffer)
    (setq vterm-prep-assosiated-buffer vterm-buffer)
    (setq header-line-format (propertize (format ">> %s" vterm-buffer) 'face '(:slant italic)))))

(provide 'vterm-prep)
