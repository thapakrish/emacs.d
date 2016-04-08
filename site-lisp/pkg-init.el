
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Packages Initialize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;initialize package.el
(package-initialize)



(require 'icicles)
;;(require 'magit)

(require 'auto-complete)
(require 'auto-complete-config) ;; default config for auto-complete
(ac-config-default)

(add-to-list 'ac-modes 'Org-mode) ;; for autocomplete to work in org mode
(ac-set-trigger-key "TAB")

;;tramp
(require 'tramp)
(setq tramp-default-method "scp")

;; for yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; search binary by gcc -xc++ -E -v -
(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)  
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-redhat-linux/4.1.2/include"))
;;  (add-to-list 'achead:include-directories '"/nfs/cern/root_v5.34.28_x64/include")

(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

;; fix iedit bug
(define-key global-map (kbd "C-c ;") 'iedit-mode)


;;install google c style package
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
;;


;; to import ical calendar to diary. Do it only once
;;(icalendar-import-file "~/Calendar/basic.ics"
;;		       "~/diary")


;;;;;;;;;;;;;;;;;;;;;;;;;;;  Helm configuration  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; got mostly from wiki-page.
(require 'helm)
(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode 1)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)



;; live grep in Helm
(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))
(helm-mode 1)


(require 'undo-tree)
(global-undo-tree-mode)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;  root configuration  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assuming ROOT is installed already

(require 'root-help)


        (defun root-c++-mode-hook ()
          "Hook for C++ mode - binding ROOT functions"
          (define-key c++-mode-map "\C-crc"  'root-class)
          (define-key c++-mode-map "\C-crh"  'root-header-skel)
          (define-key c++-mode-map "\C-crs"  'root-source-skel)
          (define-key c++-mode-map "\C-cri"  'root-include-header)
          (define-key c++-mode-map "\C-crm"  'root-main)
          (define-key c++-mode-map "\C-crl"  'root-insert-linkdef)
          (define-key c++-mode-map "\C-crp"  'root-insert-pragma)
          (define-key c++-mode-map "\C-crx"  'root-shell)
          (define-key c++-mode-map "\C-crg"  'root-send-line-to-root)
          (define-key c++-mode-map "\C-crr"  'root-send-region-to-root)
          (define-key c++-mode-map "\C-crb"  'root-send-buffer-to-root)
          (define-key c++-mode-map "\C-crf"  'root-execute-file))
        (add-hook 'c++-mode-hook 'root-c++-mode-hook)


;;(defun my-semantic-hook ()
;;  (imenu-add-to-menubar "TAGS"))
;;(add-hook 'semantic-init-hooks 'my-semantic-hook )

(semantic-mode 1)
(require 'semantic/ia)


(provide 'pkg-init)
