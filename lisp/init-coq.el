;; you need to install the 'proofgeneral' ubuntu package
(when (not *is-a-mac*)
  (load-file "/usr/share/emacs24/site-lisp/proofgeneral/generic/proof-site.el"))

(provide 'init-coq)
