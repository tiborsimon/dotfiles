;; my-projects.el
;;
;; Stuff related to maintaining and navigating around projects.
;; projectile, etc.

;;(use-package etags-select
;;  :ensure etags-select
;;  :init
;;  (setq etags-select-go-if-unambiguous t)
;;  )

(use-package org-mode
  :ensure org-mode
  :diminish org-mode
  :config
  (progn
      (after 'evil
        (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
      )
        (use-package org-mode
        :ensure org-mode
        :diminish org-mode
        :config
        (progn
            (after 'evil
                (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
            )
            )
        )
    )
  )

(provide 'my-projects)
