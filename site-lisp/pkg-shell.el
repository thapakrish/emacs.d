
;; Shell
;; For case insensitive tab completion 
(setq pcomplete-ignore-case t)

;; Cursor map to up and down arrow
;; Gives "Symbol's value as variable is void: comint-mode-map" error for now.
;; require 'commit fixed the issue
(require 'comint)
(define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
(define-key comint-mode-map (kbd "<down>") 'comint-next-input)




(defun my/tcsh-set-indent-functions ()
  (when (or (string-match ".*\\.alias" (buffer-file-name))
	    (string-match ".*csh$" (file-name-extension (buffer-file-name))))
    (require 'csh-mode) ; https://github.com/Tux/tcsh/blob/master/csh-mode.el
    (setq-local indent-line-function 'csh-indent-line)
    (setq-local indent-region-function 'csh-indent-region)))
(add-hook 'sh-set-shell-hook #'my/tcsh-set-indent-functions)
(provide 'pkg-shell)
