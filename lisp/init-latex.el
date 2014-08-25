(require-package 'auctex)
(require-package 'cdlatex)
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
(setq ieeetran-class
      '("IEEEtran"
        "\\documentclass[11pt]{IEEEtran}"
        ("\\section{%s}" . "\\section*{%s}")
        ("\\subsection{%s}" . "\\subsection*{%s}")
        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
        ("\\paragraph{%s}" . "\\paragraph*{%s}")
        ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq article-class
      '("article"
        "\\documentclass[11pt]{article}"
        ("\\section{%s}" . "\\section*{%s}")
        ("\\subsection{%s}" . "\\subsection*{%s}")
        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
        ("\\paragraph{%s}" . "\\paragraph*{%s}")
        ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq beamer-class
  '("beamer"
"\\documentclass{beamer}
\\usepackage[german]{babel}
\\usepackage{listings}
\\usepackage{color}

\\definecolor{red}{rgb}{0.6,0,0} % for strings
\\definecolor{green}{rgb}{0.25,0.5,0.35} % comments
\\definecolor{purple}{rgb}{0.5,0,0.35} % keywords
\\definecolor{docblue}{rgb}{0.25,0.35,0.75} % doc

\\lstset{basicstyle=\\small\\ttfamily,
keywordstyle=\\color{purple},
stringstyle=\\color{red},
commentstyle=\\color{green},
morecomment=[s][\\color{docblue}]{/**}{*/},
numbers=left,
numberstyle=\\tiny\\color{gray},
stepnumber=1,
numbersep=10pt,
tabsize=2,
showspaces=false,
showstringspaces=false,
otherkeywords={define,include,\\#}}
\\usetheme{hsrm}
     [NO-DEFAULT-PACKAGES]
     [NO-PACKAGES]"
        ("\\section{%s}" . "\\section*{%s}")
        ("\\subsection{%s}" . "\\subsection*{%s}")
        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
        ("\\paragraph{%s}" . "\\paragraph*{%s}")
        ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
(add-to-list 'org-latex-classes ieeetran-class t)
(add-to-list 'org-latex-classes article-class t)
(add-to-list 'org-latex-classes beamer-class t)

(add-to-list 'org-latex-classes
  '("djcb-org-article"
"\\documentclass[11pt,a4paper]{article}
\\usepackage[T1]{fontenc}
\\usepackage{fontspec}
\\usepackage{graphicx}
\\usepackage{hyperref}
\\defaultfontfeatures{Mapping=tex-text}
\\setromanfont{Gentium}
\\setromanfont [BoldFont={Gentium Basic Bold},
                ItalicFont={Gentium Basic Italic}]{Gentium Basic}
\\setsansfont{Charis SIL}
\\setmonofont[Scale=0.8]{DejaVu Sans Mono}
\\usepackage{geometry}
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
            marginparsep=7pt, marginparwidth=.6in}
\\pagestyle{empty}
\\title{}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(require 'ox-beamer)

(setq org-latex-pdf-process
  '("xelatex -interaction nonstopmode -shell-escape %f"
     "xelatex -interaction nonstopmode -shell-escape %f")) ;; for multiple passes

(eval-after-load "org"
  '(progn
     ;; Change .pdf association directly within the alist
     (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")))

(setq org-export-latex-hyperref-format "\\ref{%s}")
(setq org-latex-listings t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org mode bibtex integration
(defun my-rtcite-export-handler (path desc format)
  (message "my-rtcite-export-handler is called : path = %s, desc = %s, format = %s" path desc format)
  (let* ((search (when (string-match "::#?\\(.+\\)\\'" path)
                   (match-string 1 path)))
         (path (substring path 0 (match-beginning 0))))
    (cond ((eq format 'latex)
           (if (or (not desc)
                   (equal 0 (search "rtcite:" desc)))
               (format "\\cite{%s}" search)
             (format "\\cite[%s]{%s}" desc search))))))


(org-add-link-type "rtcite"
                   'org-bibtex-open
                   'my-rtcite-export-handler)

(provide 'init-latex)
