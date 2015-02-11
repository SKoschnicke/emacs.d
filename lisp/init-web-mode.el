(require-package 'web-mode)

(defun my-setup-php ()
  ;; enable web mode
  (web-mode)

  ;; make these variables local
  (make-local-variable 'web-mode-code-indent-offset)
  (make-local-variable 'web-mode-markup-indent-offset)
  (make-local-variable 'web-mode-css-indent-offset)

  ;; set indentation, can set different indentation level for different code type
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

;  (flycheck-select-checker my-php)
;  (flycheck-mode t))

(add-to-list 'auto-mode-alist '("\\.php$" . my-setup-php))

(provide 'init-web-mode)
