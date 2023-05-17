(require 'company)

(define-minor-mode vterm-prep-mode ""
  :keymap (list
           (cons (kbd "C-c C-a") #'vterm-prep-send-region-to-batch)
           (cons (kbd "C-c C-;") (lambda ()
                                   (interactive)
                                   (comment-region (point-min) (point-max))
                                   (goto-char (point-max))
                                   (newline)))
           (cons (kbd "C-M-<return>") (lambda ()
                                        (interactive)
                                        (call-interactively #'duplicate)
                                        (next-line)))
            (cons (kbd "<header-line> <mouse-3>")
                  (lambda ()
                    (interactive)
                    (let ((window (window-buffer
                                   (window-at (cadr (mouse-position))
                                              (cddr (mouse-position))
                                              (car (mouse-position))))))
                      (with-current-buffer window
                        (pop-to-buffer (vterm--get-associated-buffer) nil t)))))
            (cons (kbd "M-\\") #'helm-company)
            (cons (kbd "C-c C-u") #'erase-buffer)
            (cons (kbd "C-c C-k") (lambda ()
                                    (interactive)
                                    (kill-buffer (current-buffer))))
            (cons (kbd "C-c C-c") #'vterm-prep-send-region)
            (cons (kbd "C-c C-l") #'vterm-prep-send-region-local)
            (cons (kbd "C-c C-v") #'vterm-prep-pop-cmd))
  :lighter "[prep]")

(defun vterm-prep--construct-cmd (arg start end)
  (funcall (aif (unless arg
                  (awhen (gui-get-selection 'SECONDARY)
                         `(lambda (x) (s-replace "{}" ,it x))))
               it
             #'identity)
           (if (use-region-p)
               (buffer-substring start end)
             (thing-at-point 'line t))))

(defun vterm-prep-send-region (arg start end)
  (interactive "P\nr")
  (if (vterm--get-associated-buffer)
      (vterm-prep-send (vterm-prep--construct-cmd arg start end))
    (vterm-prep-send-region-local arg start end)))

(defun vterm-prep--show-local-buffer ()
  (let ((vterm-buffer-name "*vterm-local*"))
    (vterm--internal (lambda (buff &rest args)
                     (select-frame-set-input-focus
                      (window-frame (display-buffer-other-frame buff)))))))

(defun vterm-prep-send-region-local (arg start end)
  (interactive "P\nr")
  (let ((cmd (vterm-prep--construct-cmd arg start end)))
    (vterm-prep-send-to (vterm-prep--show-local-buffer) cmd)))

(defun vterm-prep-pop-cmd (arg start end)
  (interactive "P\nr")
  (pop-cmd (string-chop-newline (vterm-prep--construct-cmd arg start end))))

(defun vterm-prep-check-buffer-or-let-know ()
  (if (buffer-live-p (get-buffer (vterm--get-associated-buffer)))
      t
   (progn
     (message "Terminal buffer lost" (vterm--get-associated-buffer))
     (setq header-line-format (propertize ">> DISCONNECTED" 'face '(:foreground "red")))
     nil)))

(defun vterm-prep--get-cmdline ()
  (buffer-string))

(defun vterm-prep-send-to (buff str)
  (with-current-buffer buff
    (vterm-insert (s-chomp str))
    (vterm-send-return)))

(defun vterm-prep-send (&optional string)
  (interactive)
  (when (vterm-prep-check-buffer-or-let-know)
    (let ((content (or string
                       (vterm-prep--get-cmdline))))
      (if (and (stringp content)
               (not (string= content "")))
          (vterm-prep-send-to (vterm--get-associated-buffer) content)
        (if (vterm-prep-check-buffer-or-let-know)
            (pop-to-buffer (vterm--get-associated-buffer) nil t))))))

(defun vterm-prep-compl-get-content ()
  (buffer-substring-no-properties (line-beginning-position) (point)))

(defun vterm-prep-compl-gather-completions (pref)
  (when (vterm-prep-check-buffer-or-let-know)
    (let ((content vterm-prep:compl-content)
          (inhibit-quit t))
      (with-current-buffer (vterm--get-associated-buffer)
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

(defun vterm--get-associated-buffer ()
  (or (and (boundp 'vterm-prep-assosiated-buffer)
           vterm-prep-assosiated-buffer)
      (and (fboundp 'pm-base-buffer)
           (with-current-buffer (pm-base-buffer)
             (and (boundp 'vterm-prep-assosiated-buffer)
                  vterm-prep-assosiated-buffer)))))

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

(defun vterm-prep-run-more ()
  (let ((vterm-buff (current-buffer)))
    (aif (get-buffer (concat (buffer-name vterm-buff) "-batch"))
        (when (> (buffer-size it) 0)
          (with-current-buffer it
            (vterm-prep-send-to vterm-buff (buffer-string))
            (erase-buffer)))))
  (message "%S" (current-buffer)))

(defun vterm-prep-send-region-to-batch (arg start end)
  (interactive "P\nr")
  (let ((vterm-batch-buff (concat (or (vterm--get-associated-buffer)
                                      "*vterm-local*")
                                  "-batch"))
        (cmd (s-chomp (vterm-prep--construct-cmd arg start end))))
    (switch-to-buffer-other-frame vterm-batch-buff)
    (with-current-buffer vterm-batch-buff
      (insert cmd "\n"))))

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
