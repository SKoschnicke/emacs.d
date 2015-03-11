;; Use C-f during file selection to switch to regular find-file
(require-package 'helm)
(require-package 'helm-cmd-t)
(require-package 'helm-projectile)

(setq helm-command-prefix-key "C-c h")

(require 'helm-config)
(require 'helm-files)
(require 'helm-grep)
(require 'helm-swoop)

(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h g") 'helm-do-grep)
(global-set-key (kbd "C-c h C-c w") 'helm-wikipedia-suggest)
(global-set-key (kbd "C-c h x") 'helm-register)
(global-set-key (kbd "C-c h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c t") 'helm-cmd-t)
(global-set-key (kbd "C-c g") 'helm-cmd-t-grep)
(global-set-key (kbd "C-c m") 'helm-mini)

(require 'helm-projectile)

(helm-mode 1)

(provide 'init-helm)
