(define-minor-mode mux-mode ""
  :keymap '(("\C-x\C-f" . find-file))
  :lighter "[mux]"
  :after-hook (when (eq major-mode 'dired-mode)
                (local-set-key "C" 'mux-dired-copy-files)))

(defmacro with (expr &rest body)
  `(let ((it ,expr))
     ,@body))

(setq mux-prefix "/@cm")
(setq mux-regex (format "^\\(\\(%s\\):\\(\\([^:@]+\\)@\\)?\\([^:]+\\):\\)\\(.+\\)$" mux-prefix))

(defun mux-fn-parse (fn)
  (if (string-match mux-regex fn)
      (cl-map 'vector (lambda (x) (match-string x fn)) '(1 2 4 5 6))
    (vector nil nil nil nil fn)))

(defun mux-buffer-insert-file (filename)
  (condition-case nil
      (insert-file-contents filename t)
    (file-error
     (when (and file-exists
		        (not file-readable))
	   (kill-buffer (current-buffer))
       (signal 'file-error (list "Failed to insert file"
                                 filename))))))

(defun mux-create-file-buffer (filename read-fun writable)
  (let ((buf (generate-new-buffer (format "%s<%s>"
                                          (file-name-nondirectory filename)
                                          (file-remote-p filename)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (if (functionp read-fun)
            (funcall read-fun filename))
        (normal-mode t)
        (if (not writable)
            (view-mode t))
        (mux-clear-hooks)
        (mux-mode t)
        (current-buffer)))))

(defun mux-open-new-buffer (filename)
  (let ((file-exists (file-exists-p filename))
        (file-writable (file-writable-p filename))
        (file-readable (file-readable-p filename)))
    (cond ((and file-exists (not file-readable)) (signal 'file-error (list "File is not readable" filename)))
          ((file-directory-p filename) (with-current-buffer (dired-noselect filename)
                                         (mux-mode t)
                                         (current-buffer)))
          ((not file-exists) (if file-writable
                                 (mux-create-file-buffer filename (lambda (fn) (setq buffer-file-name fn)) t)
                               (signal 'file-error (list "Cannot create file" filename))))
          (t (mux-create-file-buffer filename #'mux-buffer-insert-file file-writable)))))

(defun mux--find-file-noselect (oldfun filename &rest args)
  (if (not (string-prefix-p mux-prefix filename))
      (apply oldfun filename args)
    (setq filename (expand-file-name filename))
    (let ((buf (get-file-buffer filename)))
      (if buf
          buf
        (mux-open-new-buffer filename)))))

(advice-add 'find-file-noselect :around #'mux--find-file-noselect)

(defun mux--magit-toplevel (oldfun &optional directory)
  (if (not (string-prefix-p mux-prefix (or directory default-directory)))
      (funcall oldfun directory)
    (unless directory
      (setq directory default-directory))
    (with-mux-cmd-output directory "cd %s && git rev-parse --show-toplevel" t
                         (if (zerop rc)
                             (concat (file-remote-p directory) output)
                           nil))))

(advice-remove 'magit-toplevel #'mux--magit-toplevel)

(setq mux-stderr "/tmp/.cm-mux-stderr")

(defmacro with-mux-fn-split (fn &rest body)
  (declare (indent 1))
  `(let ((c (mux-fn-parse ,fn)))
     (let ((prefix (aref c 0))
           (method (aref c 1))
           (username (aref c 2))
           (hostname (aref c 3))
           (path (aref c 4)))
       ,@body)))

(defun mux-conn-name (username hostname)
  (if username
      (concat hostname "@" username)
    hostname))

(defun mux-run-remote (remote-path cmd &optional inbuf)
  (with-mux-fn-split remote-path
    (let ((local-command (cond ((functionp cmd) (funcall cmd))
                               ((stringp cmd) (format cmd path)))))
      (if local-command
          (call-process "ssh" nil (list inbuf mux-stderr) nil
                        (mux-conn-name username hostname)
                        local-command)))))

(defun mux-get-cmd-output (remote-path cmd &optional gen-signal)
  (with-temp-buffer
    (let ((rc (mux-run-remote remote-path cmd (current-buffer))))
      (if (and (not (zerop rc)) gen-signal)
          (signal 'cmd-error (list "Command failed:"
                                   (with-temp-buffer
                                     (insert-file-contents mux-stderr)
                                     (buffer-string))))
        (end-of-buffer)
        (when (eq ?\n (preceding-char))
          (delete-char -1))
        (cons rc (buffer-string))))))

(defmacro with-mux-cmd-output (remote-path cmd gen-signal &rest body)
  (declare (indent 3))
  `(let* ((_ (mux-get-cmd-output ,remote-path ,cmd ,gen-signal))
          (rc (car _))
          (output (cdr _)))
     ,@body))

(defun mux-clear-hooks ()
  (setq-local kill-buffer-hook nil)
  (setq-local find-file-hook nil)
  (setq-local before-revert-hook nil))

(defun mux--expand-file-name (filename &optional default-directory-param)
  (when (and (not (string-prefix-p "/" filename))
             (not (string-prefix-p "~" filename)))
    (setq filename (concat
                    (if (string-suffix-p "/" default-directory-param)
                        default-directory-param
                      (concat default-directory-param))
                    filename)))
  (with-mux-fn-split
      filename
    (funcall
     (if prefix
         (lambda (x) (concat (or prefix "") x))
       #'identity)
     (let ((default-directory nil))
       (expand-file-name path)))))

(defun mux--insert-directory (dirname &rest args)
  (let ((default-directory "/tmp/"))
    (mux-clear-hooks)
    (mux-run-remote dirname "ls -alh --group-directories-first %s" t)
    (mux-mode t)))

(defun mux--write-region (start end filename &rest args)
  (if (stringp start)
      (mux-run-remote filename (lambda () (format "echo %s > %s" start path)))
    (with-mux-fn-split filename
      (let* ((command (format "tf=$(mktemp); cat > $tf; cat $tf > %s; rm $tf" path))
             (out-buff (with-current-buffer (get-buffer-create "*cm-mux*")
                         (insert (format "\n\n>>>> %s@%s: %s\n" username hostname command))
                         (current-buffer))))
        (call-process-region start end
                             "ssh" nil (list out-buff t) nil
                             (mux-conn-name username hostname)
                             command)))))

(defun mux--insert-file-contents (fn &optional visit beg end replace)
  (setq buffer-file-name fn)
  (mux-clear-hooks)
  (let ((content (cdr (mux-get-cmd-output (car args) "cat %s" t)))
        (p (point)))
    (when replace (erase-buffer))
    (insert content)
    (goto-char p)
    (list fn (string-bytes content))))

(defun mux--shell-command (command &optional output-buffer error-buffer)
  (let ((dir (or dired-directory
                 default-directory)))
    (switch-to-buffer-other-window "*CM Shell Command*")
    (erase-buffer)
    (mux-run-remote dir (concat "cd %s; " command) t)))

(defun mux--file-name-completion (file directory &optional pred)
  (with-mux-cmd-output (directory
                        (lambda ()
                          (format "cd %s; compgen -f %s | sed -n '1p;2q1'"
                                  path file))
                        nil)
    (if (zerop rc)
        (if (string= "" output)
            nil
          (if (string= output file)
              t
            output))
      file)))

(defun mux--file-name-all-completions (file directory)
  (split-string
   (cdr (mux-get-cmd-output directory
                            (lambda ()
                              (format "cd %s; compgen -f %s"
                                      path file))))))

(defun mux--executable-find (oldfun command &optional remote)
  (if (string-prefix-p mux-prefix default-directory)
      (let ((output (s-chomp
                     (with-output-to-string
                       (with-current-buffer standard-output
                         (with-mux-fn-split default-directory
                           (setq rc (call-process "ssh" nil t nil (mux-conn-name username hostname) "which" command))))))))
        (if (string= output "")
            nil
          output))
    (funcall oldfun command remote)))

(advice-add 'executable-find :around #'mux--executable-find)

(defun mux--process-file (program &optional infile buffer display &rest args)
  (cond ((eq buffer t)
         (setq buffer (current-buffer)))
        ((and (listp buffer)
              (eq (car buffer) t))
         (setq buffer (cons (current-buffer) (cdr buffer)))))
  (with-mux-fn-split default-directory
    (unless (null infile)
      (signal 'command-error "Cannot run ssh command with custom infile"))
    (setq new-args (list (mux-conn-name username hostname) (concat (format "cd %s && xargs -0 %s" path program))))
    (setq program "ssh")
    (with-temp-buffer
      (insert (string-join args "\0"))
      (let ((result (apply #'call-process-region (point-min) (point-max)
                           program nil buffer display new-args)))
        ;; (message "@-------: %S %S\nText Content:\n%s\n----Text End----\n"
        ;;          (list program new-args)
        ;;          result
        ;;          (buffer-string))
        result))))

;; FIXME
(defun mux--copy-file (file newname &optional ok-if-exists keep-time preserve-uid-gid preserve-perms)
  (if (and (not ok-if-exists)
           (file-exists-p newname))
      (signal 'file-already-exists (list "File already exists")))
  (let ((flags '("-i")))
    (cond (keep-time (add-to-list 'flags "-t"))
          (preserve-uid-git (add-to-list 'flags "-og"))
          (preserve-perms (add-to-list 'flags "-p")))
    (with-mux-fn-split file
      (message "!---- rsync %S"
             (append flags
                     (list (format "%s:%s" (mux-conn-name username hostname) path)
                           newname))))))

(defun mux-file-name-handler (operation &rest args)
  ;; (when (not (eq operation 'file-remote-p))
  ;;   (message "!---- %s %S" operation args))
  (let ((inhibit-file-name-operation operation)
        (inhibit-file-name-handlers '(mux-file-name-handler
                                      tramp-file-name-handler)))
    (cond
     ;; Apply operation to the bare path and concatenate with prefix
     ((memq operation '(directory-file-name
                        file-name-as-directory
                        file-name-directory))
      (with-mux-fn-split (car args)
        (concat prefix (funcall operation path))))

     ;; FIXME
     ;; Just call a further handler
     ((memq operation '(get-file-buffer
                        make-process
                        substitute-in-file-name))
      (apply operation args))

     ;; Just return the first parameter
     ((memq operation '(file-name-sans-versions))
      (car args))

     ;; Always return false/nil
     ((memq operation '(file-name-case-insensitive-p
                        file-newer-than-file-p))
      nil)
     (t
      ;; Pass params to function
      (aif (pcase operation
             ('expand-file-name #'mux--expand-file-name)
             ('file-name-completion #'mux--file-name-completion)
             ('file-name-all-completions #'mux--file-name-all-completions)
             ('write-region #'mux--write-region)
             ('insert-directory #'mux--insert-directory)
             ('insert-file-contents #'mux--insert-file-contents)
             ('shell-command #'mux--shell-command)
             ('process-file #'mux--process-file))
           (apply it args)
           ;; Some small utilities
           (pcase operation
             ('file-truename (if (s-starts-with-p "/" (car args))
                                 (car args)
                               (concat default-directory (car args))))
             ('file-remote-p (with-mux-fn-split (car args)
                               (pcase (cadr args)
                                 ('method method)
                                 ('user username)
                                 ('host hostname)
                                 ('localname path)
                                 ('nil prefix))))
             ('file-readable-p (zerop (mux-run-remote (car args) "[[ -r %s ]]")))
             ('file-exists-p (zerop (mux-run-remote (car args) "[[ -e %s ]]")))
             ('file-directory-p (zerop (mux-run-remote (car args) "[[ -d %s ]]")))
             ('file-accessible-directory-p (zerop (mux-run-remote (car args) "[[ -d %s ]]")))
             ('file-writable-p (zerop (mux-run-remote
                                       (car args)
                                       (lambda ()
                                         (format "[[ ( -e %s && -w %s ) || -w $(dirname %s) ]]"
                                                 path path path)))))

             ('make-directory (mux-run-remote (car args) (concat "mkdir "
                                                                 (if (cadr args) "-p " "")
                                                                 "%s")))
             ('file-name-nondirectory (with-mux-fn-split (car args)
                                        (funcall operation path)))
             ('unhandled-file-name-directory nil;; (with-mux-fn-split (car args)
                                             ;;                    path)
                                             )

             ('copy-file (apply 'mux--copy-file args))))))))

(add-to-list 'file-name-handler-alist (cons mux-regex #'mux-file-name-handler))

(defun disable-autosave--find-file-noselect-1 (buf filename &rest _)
  (if (string-prefix-p mux-prefix filename)
      (with-current-buffer buf
        (setq-local auto-save-default nil))))
(advice-add 'find-file-noselect-1 :before #'disable-autosave--find-file-noselect-1)

(defun mux--vterm--get-shell (oldfun)
  (if (string-prefix-p mux-prefix default-directory)
      (with-mux-fn-split default-directory
        (format "/usr/bin/ssh -t -o RemoteCommand='cd %s && bash -l' %s" path (mux-conn-name username hostname)))
    (funcall oldfun)))

(advice-add 'vterm--get-shell :around #'mux--vterm--get-shell)

(defun mux-dired-copy-files ()
  (interactive)
  (let ((fn-list (dired-get-marked-files))
        (default-directory "/tmp/"))
    (when fn-list
      (async-shell-command
       (read-string "Confirm cmd: "
                    (format "rsync -ai %s %s"
                            (mapconcat (lambda (x)
                                         (with-mux-fn-split x
                                           (concat (mux-conn-name username hostname)
                                                   ":" path))) fn-list " ")
                            (aif (dired-dwim-target-directories)
                                (read-string "Target: " (car it) nil (cdr it))
                              "")))))))
  
(provide 'mux)
