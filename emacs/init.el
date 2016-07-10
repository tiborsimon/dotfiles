(add-to-list 'load-path (concat user-emacs-directory "config"))
(add-to-list 'load-path (concat user-emacs-directory "config/eyecandy"))
(add-to-list 'load-path (concat user-emacs-directory "config/languages"))
(add-to-list 'load-path (concat user-emacs-directory "elisp"))

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

(require 'use-package)


(require 'my-core)
(require 'my-evil)
(require 'my-eyecandy)

; ;; Essential settings.
; (setq inhibit-splash-screen t
;       inhibit-startup-message t
;       inhibit-startup-echo-area-message t)
; (menu-bar-mode -1)
; (tool-bar-mode -1)
; (when (boundp 'scroll-bar-mode)
;   (scroll-bar-mode -1))
; (show-paren-mode 1)
; (visual-line-mode 1)
; (setq require-final-newline t)

; ;; Smooth scrolling
; (setq scroll-margin 5
;       scroll-conservatively 9999
;       scroll-step 1)



; ;; EVIL MODE
; (require 'evil)
; ; (require 'init-evil)
; (evil-mode 1)
; (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
; (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
; (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
; (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
; (define-key evil-motion-state-map ";" 'evil-ex)


; (use-package evil-leader
;       :commands (evil-leader-mode)
;       :ensure evil-leader
;       :demand evil-leader
;       :init
;       (global-evil-leader-mode)
;       :config
;       (progn
; 	(evil-leader/set-leader "<SPC>")
; 	(setq evil-leader/in-all-states 1)
; 	(evil-leader/set-key "e" 'find-file)
; 	(evil-leader/set-key "b" 'switch-to-buffer)
; 	(evil-leader/set-key "w" 'save-buffer)
; 	(evil-leader/set-key "q" 'kill-buffer-and-window)
; 	(evil-leader/set-key "h" 'dired-jump)
; 	(evil-leader/set-key "v" 'split-window-right)
; 	(evil-leader/set-key "e" 'pp-eval-last-sexp)
; 	(evil-leader/set-key "," 'other-window)
; 	(evil-leader/set-key "b" 'ibuffer)
; 	(evil-leader/set-key "x" 'helm-M-x)
;         )
;       )


; (use-package evil-leader
;       :commands (evil-leader-mode)
;       :ensure key-chord
;       :demand key-chord
;       :config
;       (progn
; 	(key-chord-mode 1)
; 	(key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)
; 	(key-chord-define evil-visual-state-map  "jk" 'evil-normal-state)
;         )
;       )


; ;; POWERLINE
; (require 'powerline)
; ; (powerline-evil-vim-color-theme)
; (display-time-mode t)


; ; (load-theme 'aurora t)

; ;; Elisp navigation speedup
; (progn
;   (require 'elisp-slime-nav)
;   (defun my-lisp-hook ()
;     (elisp-slime-nav-mode)
;     (turn-on-eldoc-mode)
;     )
;   (add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
; )
; (evil-define-key 'normal emacs-lisp-mode-map (kbd "K")
;   'elisp-slime-nav-describe-elisp-thing-at-point)

; (require 'dired-x)
; (evil-define-key 'normal dired-mode-map "h" 'dired-up-directory)
; (evil-define-key 'normal dired-mode-map "l" 'dired-find-alternate-file)
; (evil-define-key 'normal dired-mode-map "o" 'dired-sort-toggle-or-edit)
; (evil-define-key 'normal dired-mode-map "v" 'dired-toggle-marks)
; (evil-define-key 'normal dired-mode-map "m" 'dired-mark)
; (evil-define-key 'normal dired-mode-map "u" 'dired-unmark)
; (evil-define-key 'normal dired-mode-map "U" 'dired-unmark-all-marks)
; (evil-define-key 'normal dired-mode-map "c" 'dired-create-directory)
; (evil-define-key 'normal dired-mode-map "n" 'evil-search-next)
; (evil-define-key 'normal dired-mode-map "N" 'evil-search-previous)
; (evil-define-key 'normal dired-mode-map "q" 'kill-this-buffer)
