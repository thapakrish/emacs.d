(require 'cl)

(global-set-key (kbd "C-c o") 'occur)

;; y/n for yes/no
(fset 'yes-or-no-p 'y-or-n-p)


(add-hook 'c-mode-common-hook
	  (lambda()
	    (local-set-key (kbd "C-c <right>") 'hs-show-block)
	    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
	    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
	    (local-set-key (kbd "C-c <down>")  'hs-show-all)
	    (hs-minor-mode t)))

(defvar key-reminder
  '("C-h f to describe function"
    "M-\ to delete horizontal space"
    "C-x <left> to go to left buffer"
    "C-x z then z to repeat command"
    "M-x occur to find occurrence"
    "C-x c SPC to pop global mark"
    "C-x r l to list bookmarks"
    "C-x r m to create bookmarks"
    "C-x r-b to jump to bookmarks"    
    "C-h k to define key"
    "C-x C-t to swap lines"
    "C-cc to org capture"
    "C-h r to see emacs manual page"))


(defun kt-gentle-reminder ()
  "Display a random entry from `key-reminder'."
  (interactive)
  (unless (window-minibuffer-p)
    ;; pick a new random seed
    (random t)
    (message
     (concat "Use me: " (nth (random (length key-reminder)) key-reminder)))))


;; Tip of the day
;; From wiki
;;(defun totd ()
;;  (interactive)
;;  (random t) ;; seed with time-of-day
;;  (with-output-to-temp-buffer "*Tip of the day*"
;;    (let* ((commands (loop for s being the symbols
;;                           when (commandp s) collect s))
;;           (command (nth (random (length commands)) commands)))
;;      (princ
;;       (concat "Your tip for the day is:\n"
;;               "========================\n\n"
;;               (describe-function command)
;;               "\n\nInvoke with:\n\n"
;;               (with-temp-buffer
;;                 (where-is command t)
;;                 (buffer-string)))))))
;;
;;(add-hook 'after-init-hook 'totd)

(add-hook 'after-init-hook 'kt-gentle-reminder)  






;; use save-excursion?
(defun jump-quarter(n &optional buffer)
  "Move the line to nth quarter from point-min."
  (interactive (list (read-number "Type a quarter: "))
	       (list default (line-number-at-pos)))
  (goto-line (/ 
	      (line-number-at-pos (point-max)) n)))

(global-set-key (kbd "C-c j") 'jump-quarter)

;; kill ring
(global-set-key "\C-cy" 'browse-kill-ring)


(put 'dired-find-alternate-file 'disabled nil)

(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)


;; larger frame
(add-to-list 'default-frame-alist '(left . 0))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(width . 80))

(provide 'pkg-misc)
