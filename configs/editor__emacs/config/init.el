;; Collecting packages in a separate directory.
(setq package-user-dir "~/.config/emacs/packages")

;; Add Melpa packages to Repos
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Load org package and the org-mode base config file.
(require 'org)
(org-babel-load-file "~/.config/emacs/conf.org")
