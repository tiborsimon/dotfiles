;; Add Melpa packages to Repos
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; load org package and our emacs-config.org file
(require 'org)
(org-babel-load-file "~/.emacs.d/conf.org")
