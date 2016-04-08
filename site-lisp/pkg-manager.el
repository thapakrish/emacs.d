;;package.el for emacs
(require 'package)

;;melpa to get autoconflict on list-package
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ;; melpa has auto-complete mode
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Stable stable version from MELPA
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)


(provide 'pkg-manager)
