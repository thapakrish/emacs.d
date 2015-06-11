;;semantic mode
(semantic-mode 1)

(require 'semantic/ia)
(require 'semantic/bovine/gcc)


(setq root-base-dir "/nfs/cern/root_v5.34.28_x64/include")
(setq plt-base-dir "/auto/nfs/home/kthapa/github/PLTOffline/include")

(semantic-add-system-include "root-base-dir" 'c++-mode)
(semantic-add-system-include "root-base-dir" 'c-mode)


(semantic-add-system-include "plt-base-dir" 'c++-mode)
(semantic-add-system-include "plt-base-dir" 'c-mode)

(defun my-c-mode-cedet-hook ()
  (add-to-list 'ac-sources 'ac-source-gtags)
  (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)
(add-hook 'c++-mode-common-hook 'my-c-mode-cedet-hook)


(defun my-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))
(add-hook 'c-mode-common-hook 'my-cedet-hook)
(add-hook 'c++-mode-common-hook 'my-cedet-hook)


(require 'ede)
(global-ede-mode 1)

;;create a project for a program
(ede-cpp-root-project "PLT"
                :name "PLT Offile Project"
                :file "~/github/PLTOffline/setup_cmsvol1.sh"
                :include-path '("/"
                                "/Common"
                                "/Interfaces"
                                "/Libs"
				"../interface"
                               )
                :system-include-path '("/nfs/cern/root_v5.34.28_x64/include"))


;;to atutomatically reparse open buffers in semantic
(global-semantic-idle-scheduler-mode 1)


(require 'function-args)
(fa-config-default)

(provide 'phys-init)
