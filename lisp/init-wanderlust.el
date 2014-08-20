(add-to-list 'package-archives
  '("e6h" . "http://www.e6h.org/packages/"))
(require-package 'wanderlust)

(autoload 'wl "wl" "Wanderlust" t)

(provide 'init-wanderlust)
