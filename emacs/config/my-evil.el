;; my-evil.el
;;
;; Settings for ostracizing me from both the Emacs and Vim communities.
;; a.k.a. evil-mode

;; In order to work properly, we need to load evil-leader-mode before we load
;; evil-mode.
(use-package evil-leader
  :commands (evil-leader-mode global-evil-leader-mode)
  :ensure evil-leader
  :demand evil-leader
  :config
  (progn
    (evil-leader/set-leader "<SPC>")
    (setq evil-leader/in-all-states 1)
    (global-evil-leader-mode t)
    (evil-leader/set-key "e" 'find-file)
    (evil-leader/set-key "w" 'save-buffer)
    (evil-leader/set-key "q" 'delete-window)
    (evil-leader/set-key "h" 'dired-jump)
    (evil-leader/set-key "sh" 'split-window-vertically)
    (evil-leader/set-key "sv" 'split-window-horizontally)
    (evil-leader/set-key "," 'other-window)
    (evil-leader/set-key "b" 'ibuffer)
    (evil-leader/set-key "gg" 'my-google)
    (evil-leader/set-key "x" 'helm-M-x)))

;; Here's what we've all been waiting for.
;; Recreate Vim inside Emacs.
(use-package evil
  :ensure evil
  :config
  (progn

    (evil-mode 1)
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
    (define-key evil-motion-state-map ";" 'evil-ex)

    (use-package key-chord
      :ensure key-chord
      :demand key-chord
      :config
      (progn
        (key-chord-mode 1)
        (key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)
        (key-chord-define evil-visual-state-map  "jk" 'evil-normal-state)
        )
      )

    (use-package drag-stuff
      :ensure drag-stuff
      :demand drag-stuff
      :config
      (progn
        (drag-stuff-global-mode 1)
        (define-key drag-stuff-mode-map (kbd "M-k") 'drag-stuff-up)
        (define-key drag-stuff-mode-map (kbd "M-j") 'drag-stuff-down)
        (define-key evil-visual-state-map (kbd "M-l") 'my-shift-right-visual)
        (define-key evil-visual-state-map (kbd "M-h") 'my-shift-left-visual)
        )
      )

    (defun my-shift-left-visual ()
      (interactive)
      (indent-rigidly-left (region-beginning) (region-end))
      (evil-normal-state)
      (evil-visual-restore))

    (defun my-shift-right-visual ()
      (interactive)
      (indent-rigidly-right (region-beginning) (region-end))
      (evil-normal-state)
      (evil-visual-restore))


    ;; (setq evil-want-C-u-scroll t)
    ;; (setq evil-want-C-w-in-emacs-state t)
    ;; (setq evil-search-module        'isearch)
    ;; (setq evil-magic                'very-magic)
    ;; (setq evil-emacs-state-cursor   '("#dfaf8f" box))
    ;; (setq evil-normal-state-cursor  '("#f8f893" box))
    ;; (setq evil-insert-state-cursor  '("#f8f893" bar))
    ;; (setq evil-replace-state-cursor '("#cc9393" box))
    ;; (setq evil-want-fine-undo t)
    ;; (setq evil-want-change-word-to-end t)

    (use-package evil-nerd-commenter
      :ensure evil-nerd-commenter
      :config
      (progn
        (evil-leader/set-key "c" 'evilnc-comment-or-uncomment-lines)
        )
      )

    (use-package elisp-slime-nav
      :ensure elisp-slime-nav
      :config
      (progn
        (require 'elisp-slime-nav)
        (defun my-lisp-hook ()
          (elisp-slime-nav-mode)
          (turn-on-eldoc-mode)
          )
        (add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
        (evil-define-key 'normal emacs-lisp-mode-map (kbd "C-z") 'elisp-slime-nav-describe-elisp-thing-at-point)
        )
      )

    (use-package evil-matchit
      :ensure evil-matchit
      :commands evilmi-jump-items
      :init
      (progn
        (setq global-evil-matchit-mode t)
        (define-key evil-normal-state-map "%" 'evilmi-jump-items)))

    (use-package evil-surround
      :ensure evil-surround
      :config
      (progn
        (global-evil-surround-mode 1)))

    (use-package evil-multiedit
      :ensure evil-multiedit
      :config
      (progn
        ;; Highlights all matches of the selection in the buffer.
        (define-key evil-visual-state-map "R" 'evil-multiedit-match-all)
        ;; Match the word under cursor (i.e. make it an edit region). Consecutive presses will
        ;; incrementally add the next unmatched match.
        (define-key evil-normal-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
        ;; Match selected region.
        (define-key evil-visual-state-map (kbd "M-d") 'evil-multiedit-match-and-next)

        ;; Same as M-d but in reverse.
        (define-key evil-normal-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)
        (define-key evil-visual-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)

        ;; OPTIONAL: If you prefer to grab symbols rather than words, use
        ;; `evil-multiedit-match-symbol-and-next` (or prev).

        ;; Restore the last group of multiedit regions.
        (define-key evil-visual-state-map (kbd "C-M-D") 'evil-multiedit-restore)

        ;; RET will toggle the region under the cursor
        (define-key evil-multiedit-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

        ;; ...and in visual mode, RET will disable all fields outside the selected region
        (define-key evil-motion-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

        ;; For moving between edit regions
        (define-key evil-multiedit-state-map (kbd "C-n") 'evil-multiedit-next)
        (define-key evil-multiedit-state-map (kbd "C-p") 'evil-multiedit-prev)
        (define-key evil-multiedit-insert-state-map (kbd "C-n") 'evil-multiedit-next)
        (define-key evil-multiedit-insert-state-map (kbd "C-p") 'evil-multiedit-prev)

        ;; Ex command that allows you to invoke evil-multiedit with a regular expression, e.g.
        (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)
        )
      )

    ;; (evil-set-initial-state 'flycheck-error-list-mode 'normal)
    ;; (evil-set-initial-state 'git-commit-mode 'insert)
    ;; (evil-set-initial-state 'shell-mode 'emacs)
    ;; (evil-set-initial-state 'esup-mode 'emacs)
    ;; (evil-set-initial-state 'diff-mode 'emacs)
    ;; (evil-set-initial-state 'term-mode 'emacs)
    ;; (evil-set-initial-state 'multi-term-mode 'emacs)

    ;; (evil-define-text-object my-evil-next-match (count &optional beg end type)
    ;;   "Select next match."
    ;;   (evil-ex-search-previous 1)
    ;;   (evil-ex-search-next count)
    ;;   (list evil-ex-search-match-beg evil-ex-search-match-end))

    ;; (evil-define-text-object my-evil-previous-match (count &optional beg end type)
    ;;   "Select previous match."
    ;;   (evil-ex-search-next 1)
    ;;   (evil-ex-search-previous count)
    ;;   (list evil-ex-search-match-beg evil-ex-search-match-end))

    ;; (define-key minibuffer-local-map [escape] 'my-minibuffer-keyboard-quit)
    ;; (define-key minibuffer-local-ns-map [escape] 'my-minibuffer-keyboard-quit)
    ;; (define-key minibuffer-local-completion-map [escape] 'my-minibuffer-keyboard-quit)
    ;; (define-key minibuffer-local-must-match-map [escape] 'my-minibuffer-keyboard-quit)

    ;; (defun my-delete-trailing-whitespace-at-line ()
    ;;   "Delete trailing whitespace on the current line only."
    ;;   (interactive)
    ;;   (let ((begin (line-beginning-position))
    ;;         (end   (line-end-position)))
    ;;     (delete-trailing-whitespace begin end)))

    ;; (defvar my-last-insertion-end 0
    ;;   "The distance between point at the time of insert and beginning of line.
    ;; This tracks the saved value of the last insertion so we can figure out whether
    ;; to indent for it.")

    ;; (defvar my-last-insertion-distance 0
    ;;   "The distance between point at the time of insert and beginning of line.
    ;; This tracks the saved value of the last insertion so we can figure out whether
    ;; to indent for it.")

    ;; (defun my-sensible-to-indent-p ()
    ;;   "Determine whether it's sensible to indent the current line automagically.
    ;; Using the data stored from my-exit-insert-state, this function determines
    ;; whether or not it makes sense to indent the following line. The point of this
    ;; is to ensure we don't indent lines after the user has manually tabbed point to
    ;; the beginning of a line, but we do indent lines if there was already an
    ;; indentation from the last insert state.
    ;; A potential future improvement is to (rather than blindly indenting according
    ;; to mode, which is a potshot) indent intelligently to the saved state of point."
    ;;   (and (> my-last-insertion-distance 0)
    ;;        (my-current-line-is-empty)))

    ;; (evil-define-motion my-append-and-indent (count)
    ;;   "Moves to end of line, enters insert mode, and also indents the line."
    ;;   (evil-append-line count)
    ;;   (when (my-sensible-to-indent-p)
    ;;     (indent-according-to-mode)))

    ;; (defun my-save-insert-state-state ()
    ;;   "Save information about the state of insert state.
    ;; This does the following:
    ;; - Sets my-last-insertion-end to the character position at the end of the last
    ;; insert state.
    ;; - Sets my-last-insertion-line to the position at the beginning of the line from
    ;; the last insert state.
    ;; - Sets my-last-insertion-distance to the distance between those two points.
    ;; - Deletes trailing whitespace to the left of point.
    ;; The intent of this is to save the state of the insertion environment, so we can
    ;; make smart decisions based on it later."
    ;;   (interactive)
    ;;   (setq my-last-insertion-end
    ;;         (save-excursion
    ;;           (if (not (my-current-line-is-empty))
    ;;               (beginning-of-line-text))
    ;;           (point)))
    ;;   (setq my-last-insertion-line
    ;;         (save-excursion
    ;;           (goto-char my-last-insertion-end)
    ;;           (line-beginning-position)))
    ;;   (setq my-last-insertion-distance
    ;;         (- my-last-insertion-end my-last-insertion-line)))

    ;; (evil-define-motion my-ret-and-indent (count)
    ;;   "Move the cursor COUNT lines down.
    ;; If point is on a widget or a button, click on it.
    ;; In Insert state, insert a newline and indent."
    ;;   :type line
    ;;   (my-save-insert-state-state)
    ;;   (my-delete-trailing-whitespace-at-line)
    ;;   (evil-ret-gen count nil)
    ;;   (when (my-sensible-to-indent-p)
    ;;     (indent-according-to-mode)))

    ;; (defun my-what-line ()
    ;;   "Get the line, without printing the word 'line' before it."
    ;;   (1+ (count-lines 1 (point))))


    ;; (defun my-where-beginning-of-visual-line ()
    ;;   "Calculate the difference between the beginning
    ;; of the current visual line and point."
    ;;   (interactive)
    ;;   (let ((old-point (point))
    ;;         (bovl (save-excursion (beginning-of-visual-line)
    ;;                               (point))))
    ;;     (- old-point bovl)))


    ;; (defun my-current-line-is-empty ()
    ;;   "Returns t when the current line is empty or contains only whitespace."
    ;;   (interactive)
    ;;   (save-excursion
    ;;     (beginning-of-line)
    ;;     (looking-at "^\s*$")))


    ;; (defun my-electric-append-with-indent (count &optional vcount)
    ;;   "Indent the current line if it is empty.
    ;; Otherwise, just do a normal append-line."
    ;;   (interactive "p")
    ;;   (if (and (= (point) (line-beginning-position))
    ;;            (my-is-this-line-empty))
    ;;       (indent-according-to-mode))
    ;;   (evil-append-line count vcount))

    ;; (defun my-exit-insert-state ()
    ;;   "Function to be run when Evil exits insert state."
    ;;   (my-save-insert-state-state)
    ;;   (if (my-current-line-is-empty)
    ;;       (delete-horizontal-space t)))

    ;; (defun my-enter-insert-state ()
    ;;   "Function to be run when Evil enters insert state.
    ;; Loads indent data from my-sensible-to-indent-p and uses that to determine
    ;; whether to call indent-according-to-mode."
    ;;   (interactive)
    ;;   (if (my-sensible-to-indent-p)
    ;;       (indent-according-to-mode)))

    ;; ;; exiting insert mode -> delete trailing whitespace
    ;; (add-hook 'evil-insert-state-exit-hook 'my-exit-insert-state)
    ;; (add-hook 'evil-insert-state-entry-hook 'my-enter-insert-state)

    ;; (define-key evil-normal-state-map (kbd "RET") 'my-append-and-indent)
    ;; (define-key evil-normal-state-map (kbd "<S-return>") 'my-append-and-indent)
    ;; (define-key evil-normal-state-map (kbd "C-w }") 'evil-window-rotate-downwards)
    ;; (define-key evil-normal-state-map (kbd "C-w {") 'evil-window-rotate-upwards)

    ;; (define-key evil-insert-state-map (kbd "RET") 'my-ret-and-indent)
    ;; (define-key evil-insert-state-map (kbd "<S-backspace>")
    ;;   'backward-delete-char-untabify)
    ;; (define-key evil-insert-state-map (kbd "<S-return>")
    ;;   'electric-indent-just-newline)
    ;; (define-key evil-normal-state-map (kbd "<S-return>")
    ;;   'electric-indent-just-newline)

    ;; (define-key evil-normal-state-map (kbd "SPC a") 'ag)
    ; (define-key evil-normal-state-map (kbd "SPC SPC") 'helm-M-x)

    ;; (define-key evil-normal-state-map (kbd "C-q")   'universal-argument)

    ;; (define-key evil-normal-state-map (kbd "C-h")   'evil-window-left)
    ;; (define-key evil-normal-state-map (kbd "C-j")   'evil-window-down)
    ;; (define-key evil-normal-state-map (kbd "C-k")   'evil-window-up)
    ;; (define-key evil-normal-state-map (kbd "C-l")   'evil-window-right)
    ;; (define-key evil-normal-state-map (kbd "-") (kbd "dd"))

    ;; (define-key evil-normal-state-map "a"           'evil-append)
    ;; (define-key evil-normal-state-map "A"           'my-electric-append-with-indent)
    ;; (define-key evil-normal-state-map "$"           'my-smart-end)
    ;; (define-key evil-normal-state-map "0"           'my-smart-home)

    ;; (define-key evil-motion-state-map "h"           'evil-backward-char)
    ;; (define-key evil-motion-state-map "j"           'evil-next-visual-line)
    ;; (define-key evil-motion-state-map "k"           'evil-previous-visual-line)
    ;; (define-key evil-motion-state-map "l"           'evil-forward-char)
    ;; (define-key evil-motion-state-map "$"           'evil-end-of-line)
    ;; (define-key evil-motion-state-map "0"           'evil-beginning-of-line)

    ;; (define-key evil-normal-state-map "/"           'evil-search-forward)
    ;; (define-key evil-normal-state-map (kbd "SPC /") 'helm-swoop)
    ;; (define-key evil-motion-state-map "/"           'evil-search-forward)
    ;; (define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

    ;; (evil-ex-define-cmd "Q"  'evil-quit)
    ;; (evil-ex-define-cmd "Qa" 'evil-quit-all)
    ;; (evil-ex-define-cmd "QA" 'evil-quit-all)

    ;; (evil-define-key 'motion python-mode-map "]]" 'python-nav-forward-block)
    ;; (evil-define-key 'motion python-mode-map "][" 'python-nav-end-of-block)
    ;; (evil-define-key 'motion python-mode-map "[[" 'python-nav-backward-block)
    ;; (evil-define-key 'motion python-mode-map "[]" 'my-python-nav-backward-end-of-block)
    ;; (evil-define-key 'motion python-mode-map "[(" 'evil-previous-open-paren)
    ;; (evil-define-key 'motion python-mode-map "])" 'evil-next-close-paren)
    ;; (evil-define-key 'motion python-mode-map "[{" 'evil-previous-open-brace)
    ;; (evil-define-key 'motion python-mode-map "]}" 'evil-next-close-brace)

    )
  )

(provide 'my-evil)
