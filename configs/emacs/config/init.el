;;;   ______ __  __          _____  _____
;;;  |  ____|  \/  |   /\   / ____|/ ____|
;;;  | |__  | \  / |  /  \ | |    | (___
;;;  |  __| | |\/| | / /\ \| |     \___ \
;;;  | |____| |  | |/ ____ \ |____ ____) |
;;;  |______|_|  |_/_/    \_\_____|_____/
;;;
;;; Restarted second time on 2018-03-17 then swotched to spacemacs.
;;; Restarted third time on 2019-01-07 after the spacemacs failure..
;;;

;;; ===========================================================================
;;; BASIC CONFIGS
;; Theme and font settings.
(load-theme 'tango-dark)
(set-face-attribute 'default nil :height 100)

;; Setting the default directory for my org file repo.
(setq default-directory "~/secrets/volume/org")

;; Custom file location - https://stackoverflow.com/a/5058752/1565331
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Backup management - https://stackoverflow.com/a/151946/1565331
;; NO BACKUP FILES! I am using git..
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)  ; stop creating .#lock files

;; Essential cosmetic settings.
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Window splitting algorithm - https://stackoverflow.com/a/23663761/1565331
(defun my-split-window-sensibly (&optional window)
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (with-selected-window window
               (split-window-below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it horizontally disregarding
             ;; the value of `split-width-threshold'.
             (let ((split-width-threshold 0))
               (when (window-splittable-p window t)
                 (with-selected-window window
                   (split-window-right))))))))
(setq split-window-preferred-function 'my-split-window-sensibly)


(show-paren-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indicate-empty-lines t)
(setq-default indent-tabs-mode nil)

(setq visible-bell nil)
(setq make-pointer-invisible t)
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)
(setq split-width-threshold nil)
(setq custom-safe-themes t)
(column-number-mode t)
(setq tab-width 2)

;; Basic cosmetic changes
(set-default 'cursor-type 'hbar)
(scroll-bar-mode -1)
(blink-cursor-mode 0)

;; Fixing scroll jumps - https://stackoverflow.com/a/4160949/1565331
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)


;;; ===========================================================================
;;; PACKAGE MANAGEMENT

;;; Bootstrapping straight.el es the package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; HOTFIX ORG START:: org install hotfix for straight.el
;;; https://github.com/raxod502/straight.el#installing-org-with-straightel
(require 'subr-x)
(straight-use-package 'git)

(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)
;;; HOTFIX ORG END


;;; ===========================================================================
;;; USE PACKAGE CONFIG
(straight-use-package 'use-package)

;;; ===========================================================================
;;; THEME SETTINGS
(use-package solarized-theme
  :straight t
  :config
  ;; Don't change the font for some headings and titles
  (setq solarized-use-variable-pitch nil)
  ;; Don't change size of org-mode headlines (but keep other size-changes)
  (setq solarized-scale-org-headlines nil)
  ;; Avoid all font-size changes
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  (load-theme 'solarized-dark)
  )

;;; ===========================================================================
;;; ORG MODE CONFIG
(use-package org
  :straight org-plus-contrib
  :config
  (require 'org-tempo)

  ;; Agenda settings
  (setq org-agenda-files (directory-files-recursively "~/secrets/volume/org/" "\.org$"))

  ;;; Clocktable custom columns
  (setq org-clock-display-default-range 'untilnow)
  (defun my-minutes-in-org-time (time)
    (let ((re  "\\(\\([0-9]+\\)d \\)?\\([0-9]+\\):\\([0-9]+\\)")
          (values '(2 3 4)))
      (save-match-data
        (catch 'exit
          (if (not (string-match re time))
              (throw 'exit 0.)))
        (let ((values (mapcar (lambda (num)
                                (string-to-number ;; convert to number
                                 (or (match-string num time) ;; the part of the regex that matches
                                     "0"))) ;; or zero in case no days exist, then match-string is nil
                              values)))
          (let ((days (nth 0 values))
                (hours (nth 1 values))
                (minutes (nth 2 values)))
            (+ (* 60
                  (+ (* 24 days)
                     hours))
               minutes))))))

  ;;; Capture templates
  (global-set-key (kbd "<f6>") 'org-capture)

  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  (setq org-default-notes-file "~/notes.org")

  (setq org-capture-templates
    '(("t" "Todo" entry (file+headline "~/secrets/volume/org/refile.org" "Tasks")
       "* TODO %^{title}\n  CREATED: %U\n   %?" :clock-in t :clock-resume t)
      ("j" "Journal" entry (file+olp+datetree "~/org/orgbook/journal.org")
       "* %<%Y-%m-%d %A>\n   %?" :clock-in t :clock-resume t)))

  ;;; ToDo related configuration
  (setq org-log-done "note"
        org-log-reschedule "note")

  ;;; Drawer settings
  (setq org-log-into-drawer t)
  (setq org-clock-into-drawer "CLOCKING")

  ;; Show the time in the powerline
  (display-time-mode t)
  (setq display-time-format "%m-%d %a %H:%M")

  ;; Turn off auto save mode
  (setq auto-save-default nil)

  ;; Set wides text width
  (setq-default fill-column 100)
  (add-hook 'org-mode-hook 'turn-on-auto-fill)

  ;;; Internal linking helper
  (defun my-copy-id-to-clipboard() "Copy the ID property value to killring,
     if no ID is there then create a new unique ID.
     This function works only in org-mode buffers.

     The purpose of this function is to easily construct id:-links to
     org-mode items. If its assigned to a key it saves you marking the
     text and copying to the killring."
         (interactive)
         (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
           (setq mytmpid (funcall 'org-id-get-create))
           (kill-new mytmpid)
           (message "Copied %s to killring (clipboard)" mytmpid)
           ))

  (global-set-key (kbd "<f5>") 'my-copy-id-to-clipboard)

  ;;; Source code config
  (org-babel-do-load-languages 'org-babel-load-languages
      '(
        (shell . t)
        (python . t)
        (C . t)
        (plantuml . t)
       ))

  ;;; Theme settings
  ;;; Hide emphasis markers
  (setq org-hide-emphasis-markers t)
  ;;; Substitute list items markup
  (font-lock-add-keywords 'org-mode
    '(("^ *\\([-]\\) "
     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;;; Set up variable headlines sizes
  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (:weight bold))))
   `(org-level-7 ((t (:weight bold))))
   `(org-level-6 ((t (:weight bold))))
   `(org-level-5 ((t (:weight bold))))
   `(org-level-4 ((t (:weight bold :height 1.05))))
   `(org-level-3 ((t (:weight bold :height 1.1))))
   `(org-level-2 ((t (:weight bold :height 1.15))))
   `(org-level-1 ((t (:weight bold :height 1.2))))
   `(org-document-title ((t (:weight bold :height 1.25 :underline nil)))))
)

(use-package org-bullets
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


;;; ===========================================================================
;;; EVIL MODE CONFIG

(use-package evil
  :straight t
  :config
  (evil-mode t)
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

  ;; Org mode heading shifting
  (define-key evil-motion-state-map (kbd "M-h") 'org-metaleft)
  (define-key evil-motion-state-map (kbd "M-j") 'org-metadown)
  (define-key evil-motion-state-map (kbd "M-k") 'org-metaup)
  (define-key evil-motion-state-map (kbd "M-l") 'org-metaright)

  ;; Define movement in insert mode
  (define-key evil-insert-state-map (kbd "C-h") 'evil-backward-char)
  (define-key evil-insert-state-map (kbd "C-j") 'evil-next-line)
  (define-key evil-insert-state-map (kbd "C-k") 'evil-previous-line)
  (define-key evil-insert-state-map (kbd "C-l") 'evil-forward-char)

  ;; Remap semicolon
  (define-key evil-motion-state-map ";" 'evil-ex)

  )

(use-package key-chord
  :straight t
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)
  (key-chord-define evil-visual-state-map  "jk" 'evil-normal-state)
  (key-chord-define evil-motion-state-map  "fj" 'recenter))

(use-package evil-leader
  :straight t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "<SPC>" 'recenter
    "."     'mode-line-other-buffer
    ";"     'helm-M-x
    "b"     'helm-mini             ;; Switch to another buffer
    "f"     'helm-imenu            ;; Jump to function in buffer
    "l"     'whitespace-mode       ;; Show invisible characters
    "o"     'delete-other-windows  ;; C-w o
    "pp"    'helm-projectile-find-file
    "q"     'kill-this-buffer
    "r"     'ranger-mode
    "S"     'delete-trailing-whitespace
    "w"     'save-buffer
    "y"     'yank-to-x-clipboard
    "m,"    'org-ctrl-c-ctrl-c
    "ma"    'org-agenda
    "mci"   'org-clock-in
    "mco"   'org-clock-out
    "mcc"   'org-clock-cancel
    "mcd"   'org-clock-display
    "mz"    'org-add-note
  ))


;;; ===========================================================================
;;; HELM CONFIG

(use-package helm
  :straight t
  :config
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line))

(use-package helm-descbinds
  :straight t)


;;; ===========================================================================
;;; PROJECTILE CONFIG

(use-package projectile
  :straight t)

(use-package helm-projectile
  :straight t)

(use-package smart-mode-line
  :straight t
  :config
  (sml/setup)
  (setq sml/theme 'dark)
  )
