(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(evil evil-leader company))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'evil)
; (require 'init-evil)
(evil-mode 1)

(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(setq evil-leader/in-all-states 1)
(evil-leader/set-key
    "e" 'find-file
    "b" 'switch-to-buffer
    "q" 'kill-buffer)
