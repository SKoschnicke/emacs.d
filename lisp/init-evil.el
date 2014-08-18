(when (eval-when-compile (>= emacs-major-version 24))
  (require-package 'evil)
  (add-hook 'after-init-hook 'evil-mode))

(provide 'init-evil)
;;; init-evil.el ends here
