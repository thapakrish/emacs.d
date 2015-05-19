;; Anything that can be automated should be automated.

;; Supress splash screen on start
(setq inhibit-startup-message t)


;;(when window-system ;; Could be useful sometimes
;;(speedbar 1))

;; Set path to dependencies
(setq site-lisp-dir
     (expand-file-name "site-lisp" user-emacs-directory))

(setq custom-dir
      (expand-file-name "custom" user-emacs-directory))

(setq research-dir
      (expand-file-name "research" user-emacs-directory))


;; Setup load paths
(add-to-list 'load-path site-lisp-dir)
(add-to-list 'load-path custom-dir)
(add-to-list 'load-path research-dir)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Packages Require ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'pkg-manager)
(require 'pkg-init)

(require 'pkg-org)
(require 'pkg-shell)

(require 'pkg-misc)

;;(require 'root-help)   ;; download it from CERN/SLAC + place it under research directory
(require 'phys-init)

(require 'pkg-latex)

;; move all the auto-generated files about custom-set variables
;; and ede-projects to custom/.custom.el

;; init.el ends here

