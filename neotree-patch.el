(require 'neotree)
(setq neo-mode-line-format "neotree")

(defun my-notf (func)
  `(lambda (&rest args)
    (not (apply ,func args))))

(defun my--neo-buffer--get-nodes (path)
  ;;(backtrace)
  ;;(message "$$$$$$$$$$")
  (let* ((local-path (if (file-remote-p path)
                   (file-remote-p path 'localname)
                 path))
         (nodes (neo-util--filter (lambda (x) (not (string-empty-p x)))
                                  (split-string (shell-command-to-string (concat "ls -1Ap --group-directories-first " local-path))
                                                "\n")))
         (dir-filter #'(lambda (x) (string= (substring x -1) "/")))
         (make-full-path (lambda (x) (expand-file-name (concat path "/" x)))))
    (cons (mapcar make-full-path (neo-util--filter dir-filter nodes))
          (mapcar make-full-path (neo-util--filter (my-notf dir-filter) nodes)))))

(advice-add #'neo-buffer--get-nodes :override #'my--neo-buffer--get-nodes)

(define-key neotree-mode-map (kbd "d") (neotree-make-executor
                                        :dir-fn 'neo-open-dired
                                        :file-fn (lambda (full-path &optional arg)
                                                   (neo-open-dired (file-name-directory full-path)))))
(defun run-async-shell-in-dir (full-path &optional arg)
  (setq default-directory full-path)
  (call-interactively #'async-shell-command))

(define-key neotree-mode-map (kbd "&") (neotree-make-executor
                                        :dir-fn 'run-async-shell-in-dir
                                        :file-fn (lambda (full-path &optional arg)
                                                   (run-async-shell-in-dir (file-name-directory full-path)))))

(defvar my--neotree-relative nil)

(defun my--neotree-copy-fn (full-path &optional arg)
  (kill-new
   (if arg
       (file-local-name full-path)
     (file-relative-name full-path my--neotree-relative))))

(defun my--neotree-set-relative (full-path &optional arg)
  (if arg
      (setq my--neotree-relative nil)
    (setq my--neotree-relative full-path)))

(defun neotree-open-here ()
  (interactive)
  (neotree-hide)
  (call-interactively 'neotree-dir))

(define-key neotree-mode-map (kbd "w") (neotree-make-executor
                                        :dir-fn 'my--neotree-copy-fn
                                        :file-fn 'my--neotree-copy-fn))

(define-key neotree-mode-map (kbd "M-r") (neotree-make-executor
                                          :dir-fn 'my--neotree-set-relative
                                          :file-fn (lambda (full-path &optional arg)
                                                     (my--neotree-set-relative (file-name-directory full-path)))))

(advice-add #'helm-occur :around (lambda (orig-fun &rest args)
                                   (if (not (and (neo-global--window-exists-p)
                                                 (eq (selected-frame)
                                                     (window-frame (neo-global--get-window)))))
                                       (apply orig-fun args)
                                     (save-selected-window
                                       (neotree-hide)
                                       (apply orig-fun args)
                                       (neotree-show)))))

;; (advice-mapc (lambda (advice _props) (advice-remove #'helm-occur advice)) #'helm-occur)

(provide 'neotree-patch)
