(defvar vterm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-]")                 #'vterm--self-insert)
    (define-key map (kbd "M-<")                 #'vterm--self-insert)
    (define-key map (kbd "M->")                 #'vterm--self-insert)
    (define-key map [tab]                       #'vterm-send-tab)
    (define-key map (kbd "TAB")                 #'vterm-send-tab)
    (define-key map [backtab]                   #'vterm--self-insert)
    (define-key map [backspace]                 #'vterm-send-backspace)
    (define-key map (kbd "DEL")                 #'vterm-send-backspace)
    (define-key map [delete]                    #'vterm-send-delete)
    (define-key map [M-backspace]               #'vterm-send-meta-backspace)
    (define-key map (kbd "M-DEL")               #'vterm-send-meta-backspace)
    (define-key map [C-backspace]               #'vterm-send-meta-backspace)
    (define-key map [return]                    #'vterm-send-return)
    (define-key map (kbd "RET")                 #'vterm-send-return)
    (define-key map [C-left]                    #'vterm--self-insert)
    (define-key map [M-left]                    #'vterm--self-insert)
    (define-key map [C-right]                   #'vterm--self-insert)
    (define-key map [M-right]                   #'vterm--self-insert)
    (define-key map [C-up]                      #'vterm--self-insert)
    (define-key map [C-down]                    #'vterm--self-insert)
    (define-key map [M-up]                      #'vterm--self-insert)
    (define-key map [M-down]                    #'vterm--self-insert)
    (define-key map [left]                      #'vterm--self-insert)
    (define-key map [right]                     #'vterm--self-insert)
    (define-key map [up]                        #'vterm--self-insert)
    (define-key map [down]                      #'vterm--self-insert)
    (define-key map [prior]                     #'vterm--self-insert)
    (define-key map [S-prior]                   #'vterm--self-insert)
    (define-key map [next]                      #'vterm--self-insert)
    (define-key map [S-next]                    #'scroll-up-command)
    (define-key map [home]                      #'vterm--self-insert)
    (define-key map [end]                       #'vterm--self-insert)
    (define-key map [C-home]                    #'vterm--self-insert)
    (define-key map [C-end]                     #'vterm--self-insert)
    (define-key map [escape]                    #'vterm--self-insert)
    (define-key map [remap yank]                #'vterm-yank)
    (define-key map [remap xterm-paste]         #'vterm-xterm-paste)
    (define-key map [remap yank-pop]            #'vterm-yank-pop)
    (define-key map [remap mouse-yank-primary]  #'vterm-yank-primary)
    (define-key map (kbd "C-SPC")               #'vterm--self-insert)
    (define-key map (kbd "S-SPC")               #'vterm-send-space)
    (define-key map (kbd "C-_")                 #'vterm--self-insert)
    (define-key map [remap undo]                #'vterm-undo)
    (define-key map (kbd "M-.")                 #'vterm--self-insert)
    (define-key map (kbd "M-,")                 #'vterm--self-insert)
    (define-key map (kbd "C-b C-l")             #'vterm-clear-scrollback)
    (define-key map (kbd "C-l")                 #'vterm-clear)
    (define-key map (kbd "C-\\")                #'vterm--self-insert)
    (define-key map [remap self-insert-command] #'vterm--self-insert)
    (define-key map (kbd "C-b C-r")             #'vterm-reset-cursor-point)
    (define-key map (kbd "C-b C-n")             #'vterm-next-prompt)
    (define-key map (kbd "C-b C-p")             #'vterm-previous-prompt)
    (define-key map (kbd "C-b C-t")             #'vterm-copy-mode)
    (define-key map (kbd "C-b C-b")             #'vterm-send-C-b)
    map))

(require 'vterm)

(provide 'vterm.patch)