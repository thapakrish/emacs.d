(require 'org)


;; To preserve source blocks' color while exporting
(setq org-latex-listings 'minted)
(require 'ox-latex)

(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-src-fontify-natively t)

;; Some keys
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done 'time)
(global-font-lock-mode 1)



;; For babel support
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (R . t)
   (ruby . t)
   (ditaa . t)
   (octave . t)
   (sqlite . t)
   (perl . t)
   ))

;; set org keywords
(setq org-todo-keywords
      '((sequence  "TODO(t)" "Active(a)" "NEXT(n)"
		  "Hold(h)"  "WAITING(w)" "Someday(s)" "Canceled(c)"
		  "DONE(d!)")))


(setf org-todo-keyword-faces
      '(("NEXT" . (:foreground "yellow" :background "red" :weight bold))
	("Active" . (:foreground "yellow" :background "red" :weight bold))           
	("TODO" . (:foreground "green" :background "black" :weight bold))
	("Canceled" . (:foreground "cyan" :background "steelblue" :weight bold))
	("Hold" . (:foreground "cyan" :background "blue" :weight bold))
	("WAITING" . (:foreground "yellow" :background "magenta2" :weight bold))
	("Someday" . (:foreground "yellow" :background "blue" :weight bold))
	("DONE" . (:foreground "yellow" :background "red"))))


;; org agenda files
(setq org-agenda-files (list "~/org/research.org"
			     "~/org/cpp.org"
			     "~/org/current.org"			     
			     "~/org/tdEmacs.org"
			     "~/org/tdLinux.org"
			     "~/org/readings.org"
     			     "~/org/class.org"
			     "~/org/personal.org"))


;; to include diary to agenda
(setq org-agenda-include-diary t)

;; org-todo-state-map
'(org-agenda-ndays 7)

;;show repetitive agenda entries only once
(setq org-agenda-repeating-timestamp-show-all nil)


;; org notes file
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; set capture templates


;;(setq org-directory "~/git/org")
;;(setq org-default-notes-file "~/git/org/refile.org")

;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)


;; source : http://doc.norang.ca/org-mode.html#Capture
;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/org/refile.org")
	       "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
	      ("n" "note" entry (file "~/org/refile.org")
	       "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
	      ("j" "Journal" entry (file+datetree "~/org/current.org")
	       "* %?\n%U\n" :clock-in t :clock-resume t)
	      ("w" "org-protocol" entry (file "~/org/refile.org")
	       "* TODO Review %c\n%U\n" :immediate-finish t)
	      ("m" "Meeting" entry (file "~/org/refile.org")
	       "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
	      ("h" "Habit" entry (file "~/org/refile.org")
	       "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))




;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)



;;(setq org-capture-templates
;;      '(("r" "Research" entry (file+headline "~/org/refile.org" "Tasks")
;;	 "* TODO %?\n  %i\n  %a")      
;;	("c" "Class" entry (file+headline "~/org/refile.org" "Tasks")
;;	 "* TODO %?\n  %i\n  %a")
;;	("e" "Emacs" entry (file+headline "~/org/refile.org" "Tasks")
;;	 "* TODO %?\n  %i\n  %a")
;;	("k" "Current" entry (file+headline "~/org/refile.org" "Tasks")
;;	 "* TODO %?\n  %i\n  %a")	
;;	("l" "Linux" entry (file+headline "~/org/refile.org" "Tasks")
;;	 "* TODO %?\n  %i\n  %a")))

;; where to refile
(setq org-refile-targets (quote ((nil :maxlevel . 9)
				 (org-agenda-files :maxlevel . 9))))

;; clock
(setq org-clock-into-drawer t)

(provide 'pkg-org)
