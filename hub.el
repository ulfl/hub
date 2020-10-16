;; Copyright (C) Ulf Leopold
;;
;; The below function works with Spacemacs / Ivy.

(defun hub ()
  "Run hub."
  (interactive)
  (let* ((commands (split-string (shell-command-to-string "hub -o") "\n"))
         (commands-alist (mapcar (lambda (str)
                                   (let ((ls (split-string str "|")))
                                     (cons (car ls) (cdr ls)))) commands))
         (action (lambda (res)
                   (spacemacs/shell-pop-eshell 1)
                   (eshell-return-to-prompt)
                   (insert (cadr res))
                   (eshell-send-input)))
         (tags-and-command (ivy-read "hub: " commands-alist :action action :caller 'hub)))))
