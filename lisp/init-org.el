;; Always require org package because it is newer than the build in one
(require-package 'org)
(require-package 'org-jekyll)
(require-package 'org-fstree)
(when *is-a-mac*
  (require-package 'org-mac-link)
  (autoload 'org-mac-grab-link "org-mac-link" nil t)
  (require-package 'org-mac-iCal))

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

;; TODO: why do we have to load the exporter libs manually here?
(eval-after-load "org"
  '(require 'ox-md nil t))
(eval-after-load "org"
  '(require 'ox-ascii nil t))
(eval-after-load "org"
  '(require 'ox-odt nil t))
(eval-after-load "org"
  '(require 'ox-beamer nil t))
(eval-after-load "org"
  '(require 'ox-latex nil t))
(eval-after-load "org"
  '(require 'ox-asciidoc nil t))
(eval-after-load "org"
  '(require 'org-capture))

(setq org-export-backends (quote (ascii
                                  html
                                  beamer
                                  latex
                                  md
                                  odt
                                  asciidoc)))

(setq org-modules (quote (org-w3m org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail)))
;(setq org-modules (quote (org-w3m org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-habits)))

;; Various preferences
(setq org-log-done t
      org-completion-use-ido t
      org-edit-timestamp-down-means-later t
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-1d"
      org-agenda-span 14
      org-agenda-include-diary t
      org-agenda-window-setup 'current-window
      org-fast-tag-selection-single-key 'expert
      org-export-kill-product-buffer-when-displayed t
      org-pretty-entities t
      org-pretty-entities-include-sub-superscripts t
      org-agenda-log-mode-items (list 'clock 'state)
      org-agenda-start-with-log-mode t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-tags-column 80
      org-enforce-todo-dependencies t
      org-agenda-dim-blocked-tasks t)


; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)


(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(S)" "HOLD(h)" "|" "CANCELLED(c@/!)"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persistence-insinuate t)
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show clock sums as hours and minutes, not "n days" etc.
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

(after-load 'org-clock
  (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
  (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))


(require-package 'org-pomodoro)
(after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))


;; ;; Show iCal calendars in the org agenda
;; (when (and *is-a-mac* (require 'org-mac-iCal nil t))
;;   (setq org-agenda-include-diary t
;;         org-agenda-custom-commands
;;         '(("I" "Import diary from iCal" agenda ""
;;            ((org-agenda-mode-hook #'org-mac-iCal)))))

;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
;;             (lambda ()
;;               (goto-char (point-min))
;;               (save-excursion
;;                 (while (re-search-forward "^[a-z]" nil t)
;;                   (goto-char (match-beginning 0))
;;                   (insert "0:00-24:00 ")))
;;               (while (re-search-forward "^ [a-z]" nil t)
;;                 (goto-char (match-beginning 0))
;;                 (save-excursion
;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
;;                 (insert (match-string 0))))))

;; CUSTOM AGENDA
;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ;; ("h" "Habits" tags-todo "STYLE=\"habit\""
              ;;  ((org-agenda-overriding-header "Habits")
              ;;   (org-agenda-sorting-strategy
              ;;    '(todo-state-down effort-up category-keep))))
              (" " "Agenda"
               ((agenda "" ((org-agenda-span 2) (org-agenda-start-day "0d")))
                (tags-todo "-CANCELLED/!STARTED"
                           ((org-agenda-overriding-header "Started Tasks")
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header "Next Tasks")
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-CANCELLED+WAITING|HOLD/!"
                           ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                            (org-tags-match-list-sublevels nil))
                           nil)))
              ("O" "Overview" agenda ""
               ((org-agenda-span 14) (org-agenda-start-day "-1d")
                )))))

;; CUSTOM AGENDA END

(after-load 'org
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (when *is-a-mac*
    (define-key org-mode-map (kbd "M-h") nil))
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (when *is-a-mac*
    (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

(after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . t)
     (ledger . t)
     (ocaml . nil)
     (octave . t)
     (python . t)
     (ruby . t)
     (screen . nil)
     (sh . t)
     (sql . nil)
     (sqlite . t))))

; function to insert code block in org-mode
(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite" "javascript" "scala")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

; key binding for above
(after-load 'org
  (add-hook 'org-mode-hook '(lambda ()
                              ;; turn on flyspell-mode by default
                              (flyspell-mode 1)
                              ;; C-TAB for expanding (yasnippets)
                              (local-set-key (kbd "C-<tab>")
                                             'yas/expand-from-trigger-key)
                              ;; keybinding for editing source code blocks
                              (local-set-key (kbd "C-c s e")
                                             'org-edit-src-code)
                              ;; keybinding for inserting code blocks
                              (local-set-key (kbd "C-c s i")
                                             'org-insert-src-block)
                              )))

; enable syntax highlighting in soruce blocks
(setq org-src-fontify-natively t)

;;;;;;;;;;;;;;;;;;;;;;
;;; calendar and diary
(eval-after-load "calendar"
  '(european-calendar))
(setq diary-number-of-entries 5
      calendar-mark-diary-entries-flag t
      calendar-offset -1
      calendar-location-name "Kiel"
      calendar-latitude 54.33
      calendar-longitude 10.13
      calendar-time-display-form '(24-hours ":" minutes
                                            (if time-zone " (")
                                            time-zone
                                            (if time-zone ")"))
      calendar-holidays '((holiday-fixed 01 01 "Gesetzlicher Feiertag (Neujahr)")
                          (holiday-fixed 05 01 "Gesetzlicher Feiertag (Maifeiertag)")
                          (holiday-fixed 10 03 "Gesetzlicher Feiertag (Tag der Deutschen Einheit)")
                          (holiday-fixed 12 25 "Gesetzlicher Feiertag (1. Weihnachtstag)")
                          (holiday-fixed 12 26 "Gesetzlicher Feiertag (2. Weihnachtstag)")
                          (holiday-easter-etc -2 "Gesetzlicher Feiertag (Karfreitag)")
                          (holiday-easter-etc  1 "Gesetzlicher Feiertag (Ostermontag)")
                          (holiday-easter-etc 39 "Gesetzlicher Feiertag (Christi Himmelfahrt)")
                          (holiday-easter-etc 50 "Gesetzlicher Feiertag (Pfingstmontag)")))

(when (file-accessible-directory-p "~/SpiderOak Hive/org")
    (setq diary-file "~/SpiderOak Hive/org/diary"))

(after-load 'org
    (when (file-accessible-directory-p "~/SpiderOak Hive/org")
      (setq org-agenda-files (list "~/SpiderOak Hive/org"))
      (setq org-directory "~/SpiderOak Hive/org")
      (setq org-default-notes-file "~/SpiderOak Hive/org/refile.org")
      (setq org-mobile-directory "~/SpiderOak Hive/MobileOrg")
      (setq org-mobile-inbox-for-pull "~/SpiderOak Hive/org/from-mobile.org"))

    ;; I use C-c c to start capture mode
    (global-set-key (kbd "C-c c") 'org-capture)

    ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
    (setq org-capture-templates
          (quote (("t" "todo" entry (file "~/SpiderOak Hive/org/refile.org")
                   "* TODO %?\n%U\n%a\n")
                  ("r" "respond" entry (file "~/SpiderOak Hive/org/refile.org")
                   "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n")
                  ("n" "note" entry (file "~/SpiderOak Hive/org/refile.org")
                   "* %? :NOTE:\n%U\n%a\n")
                  ("j" "Journal" entry (file+datetree "~/SpiderOak Hive/org/diary.org")
                   "* %?\n%U\n")
                  ("m" "Meeting" entry (file "~/SpiderOak Hive/org/refile.org")
                   "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                  ("p" "Phone call" entry (file "~/SpiderOak Hive/org/refile.org")
                   "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                  ("h" "Habit" entry (file "~/SpiderOak Hive/org/refile.org")
                   "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))))

;(require 'org-publish)

;; (setq org-publish-project-alist
;;       '(("org-blog"; (name1)
;;          ;; Path to your org files.
;;          :base-directory "~/development/SKoschnicke.github.io/org"; (srcdir)
;;          :base-extension "org"; (extension)
;;          ;; Path to your Jekyll project.
;;          :publishing-directory "~/development/SKoschnicke.github.io/_posts"; (destination)
;;          :recursive t
;;          ;; this was for org-mode pre-version 8
;;          ;;:publishing-function org-publish-org-to-html
;;          ;; this is for org-mode version 8 and on
;;          :publishing-function org-html-publish-to-html
;;          :headline-levels 4
;;          :html-extension "html"
;;          :body-only t ;; Only export section between <body> </body> (body-only)
;;          :with-toc nil ;; no table of contents (breaks jekyll yaml frontmatter)
;;          :section-numbers nil ;; no section numbering
;;          )
;;         ("org-static-blog"; (name2)
;;          :base-directory "~/development/SKoschnicke.github.io/org/images"; (imgsrc)
;;          :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"; (imgext)
;;          :publishing-directory "~/development/SKoschnicke.github.io/assets"; (imgdest)
;;          :recursive t
;;          :publishing-function org-publish-attachment)

;;         ("blog" :components ("org-blog" "org-static-blog")); (combo)
;;         ))

    ;; I use C-c c to start capture mode
    (global-set-key (kbd "C-c c") 'org-capture)

    ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
    (setq org-capture-templates
          (quote (("t" "todo" entry (file "~/SpiderOak Hive/org/refile.org")
                   "* TODO %?\n%U\n%a\n")
                  ("r" "respond" entry (file "~/SpiderOak Hive/org/refile.org")
                   "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n")
                  ("n" "note" entry (file "~/SpiderOak Hive/org/refile.org")
                   "* %? :NOTE:\n%U\n%a\n")
                  ("j" "Journal" entry (file+datetree "~/SpiderOak Hive/org/diary.org")
                   "* %?\n%U\n")
                  ("m" "Meeting" entry (file "~/SpiderOak Hive/org/refile.org")
                   "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                  ("p" "Phone call" entry (file "~/SpiderOak Hive/org/refile.org")
                   "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                  ("h" "Habit" entry (file "~/SpiderOak Hive/org/refile.org")
                   "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

;(require 'org-publish)

;; (setq org-publish-project-alist
;;       '(("org-blog"; (name1)
;;          ;; Path to your org files.
;;          :base-directory "~/development/SKoschnicke.github.io/org"; (srcdir)
;;          :base-extension "org"; (extension)
;;          ;; Path to your Jekyll project.
;;          :publishing-directory "~/development/SKoschnicke.github.io/_posts"; (destination)
;;          :recursive t
;;          ;; this was for org-mode pre-version 8
;;          ;;:publishing-function org-publish-org-to-html
;;          ;; this is for org-mode version 8 and on
;;          :publishing-function org-html-publish-to-html
;;          :headline-levels 4
;;          :html-extension "html"
;;          :body-only t ;; Only export section between <body> </body> (body-only)
;;          :with-toc nil ;; no table of contents (breaks jekyll yaml frontmatter)
;;          :section-numbers nil ;; no section numbering
;;          )
;;         ("org-static-blog"; (name2)
;;          :base-directory "~/development/SKoschnicke.github.io/org/images"; (imgsrc)
;;          :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"; (imgext)
;;          :publishing-directory "~/development/SKoschnicke.github.io/assets"; (imgdest)

(provide 'init-org)
