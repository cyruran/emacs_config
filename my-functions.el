;; My hotkeys
(global-set-key (kbd "M-RET") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-c d") 'ediff)

(global-set-key (kbd "C-x v e") 'vc-ediff)

(global-set-key (kbd "M-<mouse-4>") 'next-buffer)
(global-set-key (kbd "M-<mouse-5>") 'previous-buffer)

(global-set-key (kbd "C-x k") (lambda () (interactive) (kill-buffer (buffer-name))))

(global-set-key (kbd "M-s-<left>") 'windmove-left)
(global-set-key (kbd "M-s-<right>") 'windmove-right)
(global-set-key (kbd "M-s-<up>") 'windmove-up)
(global-set-key (kbd "M-s-<down>") 'windmove-down)

(global-set-key (kbd "M-s-j") 'windmove-left)
(global-set-key (kbd "M-s-;") 'windmove-right)
(global-set-key (kbd "M-s-l") 'windmove-up)
(global-set-key (kbd "M-s-k") 'windmove-down)

(global-set-key (kbd "C-o i") 'imenu)

(global-set-key (kbd "M-s-e") (lambda () (interactive) (eshell t)))
(global-set-key (kbd "M-s-s") (lambda () (interactive) (shell t)))

(global-set-key (kbd "M-s M-o") (lambda ()
                                  (interactive)
                                  (occur
                                   (if (use-region-p)
                                       (buffer-substring-no-properties (region-beginning) (region-end))
                                       (current-word)))))
(global-set-key (kbd "C-M-j") 'ace-jump-char-mode)
(global-set-key (kbd "C-M-S-j") 'ace-jump-helm-line)

(setq my-filename-parameter (lambda () (read-file-name "File name: " (file-name-directory (buffer-file-name)) (file-name-nondirectory (buffer-file-name)) nil nil nil)))

(defmacro alambda (args &rest body)
  `(labels ((self ,args ,@body))
     #'self))

(defmacro aif (x true &rest false)
  (declare (indent 2))
  `(let ((it ,x))
     (if it
         ,true
       ,@false)))

(defmacro awhen (x true)
  `(let ((it ,x))
     (when it
         ,true)))

(defmacro file-extension-match (filename &rest hash-args)
  `(let ((myHash (make-hash-table :test 'equal)))
     (aif (gethash (file-name-extension ,filename)
              (--reduce-from (progn (puthash (first it) (second it) myHash) myHash)
                             myHash
                             (list ,@hash-args)))
          it
          (gethash t myHash))))

(defun copy-filename (arg)
  "Copies local filename
With ARG copies remote filename"
  (interactive "P")
  (kill-new
   (funcall (if arg #'identity #'tramp-file-local-name)
            (if (string= major-mode "dired-mode")
                (aif (dired-get-marked-files)
                    (mapconcat 'identity it " ")
                  dired-directory)
              (buffer-file-name)))))

(eval-after-load 'cperl-mode
  '(defun my-perl-critic (cmdline)
     (interactive (list
                   (read-string "Command line: "
                                nil nil (format "perl ~/my-perlcritic.pl --severity 4 %s" (buffer-file-name)))))
     (let* ((full-filename (buffer-file-name))
            (filename (file-name-nondirectory full-filename))
            (dir (file-name-directory full-filename))
            (buf (get-buffer-create (format "*perlcritic <%s>*" filename)))
            lline)
       (shell-command cmdline buf)
       (with-current-buffer buf (compilation-mode)))))

(defun duplicate (arg)
  (interactive "p")
  (let* ((last (point))
         (selected (region-active-p))
         (beg (if selected (region-beginning) (line-beginning-position)))
         (end (if selected (region-end) (line-end-position)))
         (fragment (buffer-substring beg end)))
    (dotimes (if arg arg 1)
      (goto-char end)
      (newline)
      (insert fragment)
      (goto-char last))))

(defun find-functions-that-use (arg)
  (interactive "MSearch string: ")
  (let (matches result (initial-pos (point)))
    (while (search-forward arg nil t)
      (let ((func (which-function))
            (pos (point)))
        (push (cons func (1+ (count-lines 1 (progn (beginning-of-defun) (point))))) matches)
        (goto-char pos)))
    (goto-char initial-pos)
    
    (setq result (mapconcat 'identity (mapcar (lambda (pair)
                                                (format "%s:%s: %s" (buffer-name) (cdr pair) (car pair))) (delete-dups matches)) "\n"))

    (switch-to-buffer-other-window (get-buffer-create (format "*Occurences %s*" (buffer-name))))
    (insert result)
    (grep-mode)
    ))

;; (global-set-key (kbd "C-c C-d") 'duplicate)

(global-set-key (kbd "C-@") 'set-mark-command)
(global-set-key (kbd "M-SPC") 'er/expand-region)

(defun mark-line (line-feed)
  (interactive "P")
  (move-beginning-of-line nil)
  (set-mark-command nil)
  (if line-feed (progn (forward-line) (move-beginning-of-line nil)) (move-end-of-line nil))
  )
(global-set-key (kbd "C-S-l") 'mark-line)

(require 's)
(defun shell-command-on-file (filename command)
  "Run shell command on filename"
  (interactive (list (read-file-name "Enter file name: " (file-name-directory (buffer-file-name)) (file-name-nondirectory (buffer-file-name)) nil nil nil)
					 (read-string "Enter shell command: ")))
  (shell-command
   (if (s-index-of command "\%s")
       (format command filename)
     (concat command " " filename))))

(eval-after-load 'helm-mode
  '(progn (global-set-key (kbd "C-o p f") 'helm-projectile-find-file-dwim)
          (global-set-key (kbd "C-o p p") 'helm-projectile-switch-project)))

(global-set-key (kbd "<C-mouse-8>") 'backward-sexp)
(global-set-key (kbd "<C-mouse-9>") 'forward-sexp)

(defun make-current-file-executable ()
  (interactive)
  (awhen (buffer-file-name) (shell-command (format "chmod a+x %s" it))))

(defun rsync-file (filename hostname remote-file)
  (interactive (list (funcall my-filename-parameter)
                     (read-string "Hostname: ")
                     (read-string "Remote filename: ")))
  (shell-command (format "rsync --rsh=ssh %s %s:%s" filename hostname remote-file)))

(defun my-dired-backup ()
  (interactive)
  (dolist (filename (dired-get-marked-files))
    (copy-file filename (format "%s.bak%s" filename (replace-regexp-in-string
                                                     (rx (* (any " \t\n")) eos)
                                                     ""
                                                     (shell-command-to-string "date +%s"))))))

(defun my-transpose-selections ()
  (interactive)
  (when (secondary-selection-exist-p)
    (kill-region (region-beginning) (region-end))
    (insert (gui-get-selection 'SECONDARY))
    (let ((sec-start (overlay-start mouse-secondary-overlay))
          (sec-end (overlay-end mouse-secondary-overlay)))
      (delete-region sec-start sec-end)
      (goto-char sec-start)
      (yank))))

(defun vterm/save-bare-buffer-name (&optional bn)
  (let ((bn (or bn
                (if (boundp 'bare-buffer-name)
                    bare-buffer-name)
                (buffer-name))))
    (with-current-buffer
        (vterm (generate-new-buffer-name bn))
      (setq-local bare-buffer-name bn)
      (setq-local split-width-threshold nil))))

(defun my-vterm-toggle (arg)
  (interactive "P")
  (if (eq major-mode 'vterm-mode)
      (if arg
          (vterm/save-bare-buffer-name)
        (if (and (boundp 'my-vterm-toggle-prev) my-vterm-toggle-prev)
            (switch-to-buffer my-vterm-toggle-prev)
          (message "No associated buffer")))
    (let ((prev-buffer (current-buffer)))
      (let* ((sw_dir (expand-file-name
                      (or (unless arg (projectile-project-root))
                          dired-directory
                          (file-name-directory (buffer-file-name))
                          default-directory)))
             (buff-name (format "*vterm[%s]*" sw_dir)))
        (if (get-buffer buff-name)
            (switch-to-buffer buff-name)
          (progn
            (cd sw_dir)
            (vterm/save-bare-buffer-name buff-name)
            (make-local-variable 'my-vterm-toggle-prev)))
        (setq my-vterm-toggle-prev prev-buffer)))))

(defun my-vterm-detach ()
  (interactive)
  (when (and (boundp 'my-vterm-toggle-prev) my-vterm-toggle-prev)
    (progn
      (setq my-vterm-toggle-prev nil)
      (rename-buffer (concat (buffer-name) "[detached]") t))))

(defun my-dired-open-externally ()
  (interactive)
  (dolist (fn (dired-get-marked-files))
    (start-process (concat "open-filename " fn) nil "xdg-open" fn)))

(defun xdg-open-selection (start end)
  (interactive "r")
  (let ((word (if (use-region-p)
                  (buffer-substring start end)
                (thing-at-point 'word))))
    (start-process "" nil "xdg-open" word)))

(defun project-upload ()

(defun my-confirm-string-dwim (cmd)
  (read-string "Confirm: "
   (format cmd (aif (dired-dwim-target-directories)
                   (read-string "Dwim: " (car it) nil (cdr it))
                 ""))))

(setq my--shell-history '())

(defun my-run-shell-dwim ()
  (interactive)
  (shell-command
   (my-confirm-string-dwim
    (read-string "cmd: " nil 'my--shell-history))))

(global-set-key (kbd "M-<f3>") 'my-vterm-toggle)
(global-set-key (kbd "M-<f2>") (lambda (arg)
                                 (interactive "P")
                                 (let ((default-directory (expand-file-name "~")))
                                   (vterm arg))))

(provide 'my-functions)
