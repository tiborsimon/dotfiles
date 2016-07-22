;; my-move-text.el
;;
;; Settings for Helm, an interactive narrowing and completion framework.

(use-package move-text
  :ensure move-text
  :config
  (progn
    (defun move-text-internal (arg)
      (cond
       ((and mark-active transient-mark-mode)
        (if (> (point) (mark))
            (exchange-point-and-mark))
        (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
          (forward-line arg)
          (move-to-column column t)
          (set-mark (point))
          (insert text)
          (exchange-point-and-mark)
          (setq deactivate-mark nil)))
       (t
        (let ((column (current-column)))
          (beginning-of-line)
          (when (or (> arg 0) (not (bobp)))
            (forward-line)
            (when (or (< arg 0) (not (eobp)))
              (transpose-lines arg)
              (when (and (eval-when-compile
                           '(and (>= emacs-major-version 24)
                                 (>= emacs-minor-version 3)))
                         (< arg 0))
                (forward-line -1)))
            (forward-line -1))
          (move-to-column column t)))))

    (defun move-text-down (arg)
      "Move region (transient-mark-mode active) or current line
      arg lines down."
      (interactive "*p")
      (move-text-internal arg))

    (defun move-text-up (arg)
      "Move region (transient-mark-mode active) or current line
      arg lines up."
      (interactive "*p")
      (move-text-internal (- arg)))
)

(provide 'my-move-text)
