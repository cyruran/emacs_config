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

(defmacro aif (x true false)
  `(let ((it ,x))
     (if it
         ,true
       ,false)))

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

(defun copy-filename ()
  (interactive)
  (kill-new (buffer-file-name)))

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

(global-set-key (kbd "C-@") 'er/expand-region)

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

(provide 'my-functions)
