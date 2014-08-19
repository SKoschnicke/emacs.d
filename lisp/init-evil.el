(when (eval-when-compile (>= emacs-major-version 24))
  (require-package 'evil)
  (add-hook 'after-init-hook 'evil-mode))

(after-load 'evil
  (require-package 'evil-surround)
  (global-evil-surround-mode 1)
)

(provide 'init-evil)
;;; init-evil.el ends here
