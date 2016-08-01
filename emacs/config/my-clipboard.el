(after 'evil
    (defun djoyner/evil-shift-left-visual ()
      (interactive)
      (evil-shift-left (region-beginning) (region-end))
      (evil-normal-state)
      (evil-visual-restore))

    (defun djoyner/evil-shift-right-visual ()
      (interactive)
      (evil-shift-right (region-beginning) (region-end))
      (evil-normal-state)
      (evil-visual-restore))

    ;;;; Support
    ;;;; http://stackoverflow.com/questions/26472216/how-to-copy-text-in-emacs-evil-mode-without-overwriting-the-clipboard

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
      "Disable evil-mode with visual cues."
      `(progn
         (evil-mode 0)
         (message "Evil mode disabled")))

    (defmacro enable-evil-mode ()
      "Enable evil-mode with visual cues."
      `(progn
         (evil-mode 1)
         (message "Evil mode enabled")))

    ;;;; Clipboard bypass

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
    (define-key evil-normal-state-map "c" 'evil-destroy-change)
    (define-key evil-normal-state-map "x" 'evil-destroy-char)
    (define-key evil-normal-state-map "X" 'evil-destroy-whole-line)
    (define-key evil-normal-state-map "Y" 'evil-copy-to-end-of-line)
    (define-key evil-visual-state-map "P" 'evil-destroy-paste-before)
    (define-key evil-visual-state-map "p" 'evil-destroy-paste-after)

    ;; http://emacs.stackexchange.com/questions/14940/emacs-doesnt-paste-in-evils-visual-mode-with-every-os-clipboard/15054#15054
    (fset 'evil-visual-update-x-selection 'ignore)
)

(provide 'my-clipboard)
