(require 'company)

(define-minor-mode vterm-prep-mode ""
  :keymap (list
           (cons (kbd "C-c C-a") #'vterm-prep-send)
           (cons (kbd "C-c C-;") (lambda ()
                                   (interactive)
                                   (comment-region (point-min) (point-max))
                                   (goto-char (point-max))
                                   (newline)))
            (cons (kbd "<header-line> <mouse-3>")
                  (lambda ()
                    (interactive)
                    (let ((window (window-buffer
                                   (window-at (cadr (mouse-position))
                                              (cddr (mouse-position))
                                              (car (mouse-position))))))
                      (with-current-buffer window
                        (pop-to-buffer vterm-prep-assosiated-buffer nil t)))))
            (cons (kbd "M-\\") #'helm-company)
            (cons (kbd "C-c C-u") #'erase-buffer)
            (cons (kbd "C-c C-k") (lambda ()
                                    (interactive)
                                    (kill-buffer (current-buffer))))
            (cons (kbd "C-c C-c") (lambda (start end)
                                    (interactive "r")
                                    (vterm-prep-send
                                     (if (use-region-p)
                                         (buffer-substring start end)
                                       (thing-at-point 'line t))))))
  :lighter "[prep]")


(defun vterm-prep-check-buffer-or-let-know ()
  (if (buffer-live-p (get-buffer vterm-prep-assosiated-buffer))
      t
   (progn
     (message "Terminal buffer lost" vterm-prep-assosiated-buffer)
     (setq header-line-format (propertize ">> DISCONNECTED" 'face '(:foreground "red")))
     nil)))

(defun vterm-prep--get-cmdline ()
  (buffer-string))

(defun vterm-prep-send (&optional string)
  (interactive)
  (when (vterm-prep-check-buffer-or-let-know)
    (let ((content (or string
                       (vterm-prep--get-cmdline))))
      (if (and (stringp content)
               (not (string= content "")))
          (with-current-buffer vterm-prep-assosiated-buffer
            (vterm-insert content)
            (vterm-send-return))
        (if (vterm-prep-check-buffer-or-let-know)
            (pop-to-buffer vterm-prep-assosiated-buffer nil t))))))

(defun vterm-prep-compl-get-content ()
  (buffer-substring-no-properties (line-beginning-position) (point)))

(defun vterm-prep-compl-gather-completions (pref)
  (when (vterm-prep-check-buffer-or-let-know)
    (let ((content vterm-prep:compl-content)
          (inhibit-quit t))
      (with-current-buffer vterm-prep-assosiated-buffer
        (vterm-insert content)
        (with-local-quit
          (prog1
              (let* ((p (vterm--get-cursor-point))
                     (np p))
                (vterm-send "M-*")
                (let ((limit 120))
                  (while (and (eq p np) limit)
                    (sleep-for 0 500)
                    (setq np (vterm--get-cursor-point))
                    (setq limit (- limit 1))))
                (split-string (s-replace "\n" "" (concat pref (buffer-substring-no-properties p np)))))
            (vterm-send-C-u)))))))

(defun vterm-prep-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-simple-backend))
    (prefix (prog2
                (looking-back "\\(^\\| \\)\\(.*+\\)\\>")
                (match-string 2)
              (setq-local vterm-prep:compl-content (vterm-prep-compl-get-content))))
    (candidates (vterm-prep-compl-gather-completions arg))
    (meta (format "This value is named %s" arg))))

(defun vterm-prep--init ()
  (sh-mode)
  (vterm-prep-mode)
  (company-mode t)
  (make-local-variable 'vterm-prep-assosiated-buffer)
  (setq vterm-prep-assosiated-buffer vterm-buffer)
  (setq header-line-format (propertize (format ">> %s" vterm-buffer) 'face '(:slant italic)))
  (setq-local company-backends (cons #'vterm-prep-backend company-backends))
  (setq-local company-idle-delay nil))

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

      (let* ((buffer (get-buffer vterm-prep-buffer-name))
             (new (or arg (not buffer))))
        (switch-to-buffer-other-window (if new
                                           (generate-new-buffer vterm-prep-buffer-name)
                                         buffer))
        (if new
            (vterm-prep--init))
        (if (boundp 'vterm-prep--new-command-value)
            (insert vterm-prep--new-command-value))))))

(defun vterm-prep-send-to-prep (start end)
  (interactive "r")
  (let ((vterm-prep--new-command-value (if (use-region-p)
                                           (buffer-substring start end)
                                         (thing-at-point 'word))))
    (vterm-prep nil)))

(defun vterm-prep--new-command-entry ()
  (interactive)
  (let ((inhibit-read-only t))
    (beginning-of-buffer)
    (newline)
    (let ((start (point))
          (sep-start)
          (sep-end))
      (if (boundp 'vterm-prep--new-command-value)
          (insert vterm-prep--new-command-value))
      (newline)
      (let ((o (make-overlay start (point))))
        (overlay-put o 'face '(:extend t :background "#eee6ce"))
        (overlay-put o 'field t))
      (newline)
      (add-text-properties (- (point) 2) (point) '(read-only t))
      (goto-char start))))

(define-key vterm-mode-map (kbd "C-x C-e")
  (lambda ()
    (interactive)
    (let ((current-input (prog1
                             (buffer-substring (progn
                                                 (vterm-send-C-a)
                                                 (sleep-for 0 100)
                                                 (vterm--get-cursor-point))
                                               (progn
                                                 (vterm-send-C-e)
                                                 (sleep-for 0 100)
                                                 (vterm--get-cursor-point)))
                           (vterm-send-C-u))))
      (vterm-prep nil)
      (insert current-input))))

(provide 'vterm-prep)
