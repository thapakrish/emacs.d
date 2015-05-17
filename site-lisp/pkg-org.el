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
			     "~/org/tdEmacs.org"
			     "~/org/tdLinux.org"
			     "~/org/readings.org"
     			     "~/org/class.org"
			     "~/org/personal.org"))


;; to include diary to agenda
(setq org-agenda-include-diary t)

;; org-todo-state-map
'(org-agenda-ndays 7)

(provide 'pkg-org)
