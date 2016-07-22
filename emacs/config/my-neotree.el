;; my-neotree.el
;;
;; Settings for Helm, an interactive narrowing and completion framework.

(use-package neotree
  :ensure neotree
  :config
  (progn
    (global-set-key [f2] 'neotree-toggle)))

(provide 'my-neotree)
