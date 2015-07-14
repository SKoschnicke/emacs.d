(when (eval-when-compile (>= emacs-major-version 24))
  (require-package 'evil-leader)
  (add-hook 'after-init-hook 'global-evil-leader-mode)
  (require-package 'evil)
  (add-hook 'after-init-hook 'evil-mode))

(require 'key-chord)
(key-chord-mode 1)

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
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

  ; jumping over buffer boundaries, like in vim
  (require-package 'evil-jumper)
  (global-evil-jumper-mode 1)
)

(after-load 'evil-leader
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key "s" 'save-buffer)
  (evil-leader/set-key "f" 'helm-swoop)
)

(provide 'init-evil)
;;; init-evil.el ends here
