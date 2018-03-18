;; Restarted on 2018-03-17

(load-theme 'tango-dark)

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

  (fset 'evil-visual-update-x-selection 'ignore)

  ;; Clipboard bypass support functions
  (defmacro without-evil-mode (&rest do-this)
    ;; Check if evil-mode is on, and disable it temporarily
    `(let ((evil-mode-is-on (evil-mode?)))
       (if evil-mode-is-on
           (disable-evil-mode))
       (ignore-errors
         ,@do-this)
       (if evil-mode-is-on
           (enable-evil-mode))))

  (defmacro evil-mode? ()
    "Checks if evil-mode is active. Uses Evil's state to check."
    `evil-state)

  (defmacro disable-evil-mode ()
    (evil-mode 0))

  (defmacro enable-evil-mode ()
    (evil-mode 1))

  ;; delete: char
  (evil-define-operator evil-destroy-char (beg end type register yank-handler)
    :motion evil-forward-char
    (evil-delete-char beg end type ?_))

  ;; delete: char (backwards)
  (evil-define-operator evil-destroy-backward-char (beg end type register yank-handler)
    :motion evil-forward-char
    (evil-delete-backward-char beg end type ?_))

  ;; delete: text object
  (evil-define-operator evil-destroy (beg end type register yank-handler)
    "Vim's 's' without clipboard."
    (evil-delete beg end type ?_ yank-handler))

  ;; delete: to end of line
  (evil-define-operator evil-destroy-line (beg end type register yank-handler)
    :motion nil
    :keep-visual t
    (interactive "<R><x>")
    (evil-delete-line beg end type ?_ yank-handler))

  ;; delete: whole line
  (evil-define-operator evil-destroy-whole-line (beg end type register yank-handler)
    :motion evil-line
    (interactive "<R><x>")
    (evil-delete-whole-line beg end type ?_ yank-handler))

  ;; change: text object
  (evil-define-operator evil-destroy-change (beg end type register yank-handler delete-func)
    (evil-change beg end type ?_ yank-handler delete-func))

  ;; paste: before
  (defun evil-destroy-paste-before ()
    (interactive)
    (without-evil-mode
       (delete-region (point) (mark))
       (evil-paste-before 1)))

  ;; paste: after
  (defun evil-destroy-paste-after ()
    (interactive)
    (without-evil-mode
       (delete-region (point) (mark))
       (evil-paste-after 1)))

  ;; paste: text object
  (evil-define-operator evil-destroy-replace (beg end type register yank-handler)
    (evil-destroy beg end type register yank-handler)
    (evil-paste-before 1 register))

  ;; Clipboard bypass key rebindings
  (define-key evil-normal-state-map "s" 'evil-destroy)
  (define-key evil-normal-state-map "S" 'evil-destroy-line)
  (define-key evil-visual-state-map "c" 'evil-destroy-change)
  (define-key evil-normal-state-map "x" 'evil-destroy-char)
  (define-key evil-normal-state-map "X" 'evil-destroy-whole-line)
  (define-key evil-normal-state-map "Y" 'evil-copy-to-end-of-line)
  (define-key evil-visual-state-map "p" 'evil-destroy-paste-before)
  (define-key evil-visual-state-map "P" 'evil-destroy-paste-after)

  ;; esc quits
  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
  In Delete Selection mode, if the mark is active, just deactivate it;
  then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  ;; Make escape quit everything, whenever possible.
  (define-key evil-normal-state-map (kbd "<escape>") 'keyboard-escape-quit)
  (define-key evil-visual-state-map (kbd "<escape>") 'keyboard-quit)
  (define-key minibuffer-local-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map (kbd "<escape>") 'minibuffer-keyboard-quit)

  ;; Define window switching
  (global-set-key (kbd "C-h") nil)
  (global-set-key (kbd "C-j") nil)
  (global-set-key (kbd "C-k") nil)
  (global-set-key (kbd "C-l") nil)
  (define-key evil-motion-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-motion-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-motion-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-motion-state-map (kbd "C-l") 'evil-window-right)

  ;; Define movement in insert mode
  (define-key evil-insert-state-map (kbd "C-h") 'evil-backward-char)
  (define-key evil-insert-state-map (kbd "C-j") 'evil-next-line)
  (define-key evil-insert-state-map (kbd "C-k") 'evil-previous-line)
  (define-key evil-insert-state-map (kbd "C-l") 'evil-forward-char)

  ;; Remap semicolon
  (define-key evil-motion-state-map ";" 'evil-ex)

  (use-package key-chord
    :ensure t
    :config
    (key-chord-mode 1)
    (key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)
    (key-chord-define evil-visual-state-map  "jk" 'evil-normal-state))

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "#"  'server-edit
      ","  'other-window
      "."  'mode-line-other-buffer
      ":"  'eval-expression
      "aa" 'align-regexp
      "a=" 'my-align-single-equals
      "b"  'helm-mini             ;; Switch to another buffer
      "B"  'magit-blame-toggle
      "c"  'comment-dwim
      "D"  'open-current-line-in-codebase-search
      "f"  'helm-imenu            ;; Jump to function in buffer
      "g"  'magit-status
      "h"  'fontify-and-browse    ;; HTML-ize the buffer and browse the result
      "l"  'whitespace-mode       ;; Show invisible characters
      "nn" 'air-narrow-dwim       ;; Narrow to region and enter normal mode
      "nw" 'widen
      "o"  'delete-other-windows  ;; C-w o
      "p"  'yank
      "q"  'kill-this-buffer
      "s"  'ag-project            ;; Ag search from project's root
      "S"  'delete-trailing-whitespace
      "t"  'gtags-reindex
      "T"  'gtags-find-tag
      "w"  'save-buffer
      "x"  'helm-M-x
      "y"  'yank-to-x-clipboard))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-indent-textobject
    :ensure t))

(use-package powerline
  :ensure t
  :config
  (defface my-pl-segment1-active
    '((t (:foreground "#000000" :background "#E1B61A")))
    "Powerline first segment active face.")
  (defface my-pl-segment1-inactive
    '((t (:foreground "#CEBFF3" :background "#3A2E58")))
    "Powerline first segment inactive face.")
  (defface my-pl-segment2-active
    '((t (:foreground "#F5E39F" :background "#8A7119")))
    "Powerline second segment active face.")
  (defface my-pl-segment2-inactive
    '((t (:foreground "#CEBFF3" :background "#3A2E58")))
    "Powerline second segment inactive face.")
  (defface my-pl-segment3-active
    '((t (:foreground "#CEBFF3" :background "#3A2E58")))
    "Powerline third segment active face.")
  (defface my-pl-segment3-inactive
    '((t (:foreground "#CEBFF3" :background "#3A2E58")))
    "Powerline third segment inactive face.")

  (defun my-powerline-default-theme ()
    "Set up my custom Powerline with Evil indicators."
    (setq-default mode-line-format
     '("%e"
        (:eval
          (let* ((active (powerline-selected-window-active))
            (seg1 (if active 'my-pl-segment1-active 'my-pl-segment1-inactive))
            (seg2 (if active 'my-pl-segment2-active 'my-pl-segment2-inactive))
            (seg3 (if active 'my-pl-segment3-active 'my-pl-segment3-inactive))
            (separator-left (intern (format "powerline-%s-%s"
                                            (powerline-current-separator)
                                            (car powerline-default-separator-dir))))
            (separator-right (intern (format "powerline-%s-%s"
                                             (powerline-current-separator)
                                             (cdr powerline-default-separator-dir))))
            (lhs (list
              (let ((evil-face (powerline-evil-face)))
                 (if evil-mode (powerline-raw (powerline-evil-tag) evil-face)))
                 (if evil-mode (funcall separator-left (powerline-evil-face) seg1))
                 (powerline-buffer-id seg1 'l)
                 (powerline-raw "[%*]" seg1 'l)
                 (when (and (boundp 'which-func-mode) which-func-mode)
                   (powerline-raw which-func-format seg1 'l))
                 (powerline-raw " " seg1)
                 (funcall separator-left seg1 seg2)
                 (when (boundp 'erc-modified-channels-object)
                   (powerline-raw erc-modified-channels-object seg2 'l))
                 (powerline-major-mode seg2 'l)
                 (powerline-process seg2)
                 (powerline-minor-modes seg2 'l)
                 (powerline-narrow seg2 'l)
                 (powerline-raw " " seg2)
                 (funcall separator-left seg2 seg3)
                 (powerline-vc seg3 'r)
                 (when (bound-and-true-p nyan-mode)
                   (powerline-raw (list (nyan-create)) seg3 'l))))
            (rhs (list
              (powerline-raw global-mode-string seg3 'r)
              (funcall separator-right seg3 seg2)
              (unless window-system
                (powerline-raw (char-to-string #xe0a1) seg2 'l))
              (powerline-raw "%4l" seg2 'l)
              (powerline-raw ":" seg2 'l)
              (powerline-raw "%3c" seg2 'r)
              (funcall separator-right seg2 seg1)
              (powerline-raw " " seg1)
              (powerline-raw "%6p" seg1 'r)
              (when powerline-display-hud
                (powerline-hud seg1 seg3)))))
            (concat (powerline-render lhs)
              (powerline-fill seg3 (powerline-width rhs))
              (powerline-render rhs)))))))
  (my-powerline-default-theme)
  (use-package powerline-evil
    :ensure t))

;; Essential settings.
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)
(electric-pair-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indicate-empty-lines t)
(setq-default indent-tabs-mode nil)

(setq visible-bell t)
(setq make-pointer-invisible t)
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)
(setq split-width-threshold nil)
(setq custom-safe-themes t)
(column-number-mode t)
(setq tab-width 2)
(setq tramp-default-method "ssh")

;; Basic cosmetic changes
(set-default 'cursor-type 'hbar)
(scroll-bar-mode -1)
(column-number-mode)
(blink-cursor-mode 0)




(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
(setq smooth-scroll-margin 3)

;; Considerable IDO vs Helm discussion https://www.reddit.com/r/emacs/comments/3o36sc/what_do_you_prefer_ido_or_helm/
;; (setq x-select-enable-clipboard t)
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
