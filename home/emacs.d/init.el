;; Restarted on 2018-03-17

;; Configuring the load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Custom file location - https://stackoverflow.com/a/5058752/1565331
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Backup management - https://stackoverflow.com/a/151946/1565331
(setq backup-directory-alist `(("." . "~/.emacs.d/.backup")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)


;; Package management
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; Initializing use-package - https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Packages
(use-package evil
  :ensure t
  :init
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode)
  (define-key evil-normal-state-map (kbd ", w") 'evil-window-vsplit))


;; Basic cosmetic changes
(set-default 'cursor-type 'hbar)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode)

;; Parentheses
(show-paren-mode)
(electric-pair-mode)


(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
(setq smooth-scroll-margin 5)




;; Considerable IDO vs Helm discussion https://www.reddit.com/r/emacs/comments/3o36sc/what_do_you_prefer_ido_or_helm/
;; (setq x-select-enable-clipboard t)
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)


