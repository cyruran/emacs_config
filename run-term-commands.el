(setq rtc--term-buff-name "*vterm rtc*")

(defun rtc-run ()
  (interactive)
  (with-current-buffer
      (let ((vterm-buffer-name rtc--term-buff-name))
        (vterm--internal (lambda (buff &rest args)
                           (select-frame-set-input-focus
                            (window-frame (display-buffer-other-frame buff))))))
    (vterm-send-string "bash -x ~/misc/rtc.sh\n")))

(global-set-key (kbd "M-<f9>") #'rtc-run)

(provide 'run-term-commands)
