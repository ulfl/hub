;; Copyright (C) Ulf Leopold
;;
;; The below function works with Spacemacs / Ivy.

(defun hub ()
  "Run hub."
  (interactive)
  (let* ((commands (split-string (shell-command-to-string "hub -o") "\n"))
         (tags-and-command (ivy-read "hub: " commands))
         (cmd (replace-regexp-in-string "^[^|]*|" "" tags-and-command)))
    (spacemacs/shell-pop-eshell 1)
    (eshell-return-to-prompt)
    (insert cmd)
    (eshell-send-input)))
