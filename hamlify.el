(defun haml-ify ()
  "run html2haml on current buffer"
  (interactive)
  (setf filename buffer-file-name)
  (setf newfilename (concat
    (car (split-string filename "\\.")) ".html.haml"))
  (save-buffer)
  (shell-command (concat
    "/usr/bin/html2haml " filename " > " newfilename))
  (kill-buffer (current-buffer))
  (find-file newfilename))
