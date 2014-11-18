(when (eval-when-compile (>= emacs-major-version 24))
  (require-package 'evil)
  (add-hook 'after-init-hook 'evil-mode))

(after-load 'evil
  (require-package 'evil-surround)
  (global-evil-surround-mode 1)
  ;(setq evil-want-C-u-scroll t) ;; bind C-u to scroll-up (as in vi)
  ; Use C-y for scrolling up (its C-u in vim but thats bound to universal argument in emacs)
  (define-key evil-normal-state-map "\C-y" (lambda ()
                    (interactive)
                    (previous-line nil)
                    (evil-scroll-line-up nil)
                    ))
)

(provide 'init-evil)
;;; init-evil.el ends here
