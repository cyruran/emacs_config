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

(defun vterm-prep (arg)
  (interactive "P")
  (when (eq major-mode 'vterm-mode)
    (let* ((vterm-buffer (current-buffer))
           (vterm-buffer-name (buffer-name vterm-buffer))
           (vterm-prep-buffer-name (format "*[%s]-prep*"
                                           vterm-buffer-name))
           (new-buff (lambda ()
                       (generate-new-buffer vterm-prep-buffer-name))))
      (message "%s" arg)
      (switch-to-buffer-other-window (if arg
                                         (funcall new-buff)
                                       (aif (get-buffer vterm-prep-buffer-name)
                                            it
                                            (funcall new-buff))))
      (sh-mode)
      (vterm-prep-mode)
      (make-local-variable 'vterm-prep-assosiated-buffer)
      (setq vterm-prep-assosiated-buffer vterm-buffer)
      (setq header-line-format (propertize (format ">> %s" vterm-buffer) 'face '(:slant italic))))))

(defun vterm-prep-send-to-prep (start end)
  (interactive "r")
  (let ((text (if (use-region-p)
                  (buffer-substring start end)
                (thing-at-point 'word))))
    (vterm-prep nil)
    (insert text "\n")))

(provide 'vterm-prep)
