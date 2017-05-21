;;package.el for emacs
(require 'package)

;;melpa to get autoconflict on list-package
(setq package-archives '(("ELPA" . "https://tromey.com/elpa/")
			 ("gnu" . "https://elpa.gnu.org/packages/")
;;			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ;; melpa has auto-complete mode
			 ("melpa" . "https://melpa.milkbox.net/packages/")))

;; Stable stable version from MELPA
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)


(provide 'pkg-manager)
