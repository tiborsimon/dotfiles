;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(require 'key-chord)
(key-chord-mode t)
(key-chord-define-global "jk" 'evil-normal-state)

;;============================================================================
;; ORG MODE

;; If you use `org' and don't want your org files in the default location
;; below, change `org-directory'. It must be set before org loads!
(use-package org
  :config
  (setq org-directory "~/private/notebook/")

  (require 'org-tempo)
  (setq org-log-into-drawer t)
  (setq org-clock-display-default-range 'untilnow)
  (setq org-clock-into-drawer "CLOCKING")

  (setq org-table-number-regexp
    "^\\([<>]?[-+^.0-9]*[0-9][-+^.0-9eEdDx()%:]*\\|\\(0[xX]\\)[0-9a-fA-F]+\\)$")

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

  ;; Set wides text width
  (setq-default fill-column 100)
  (add-hook 'org-mode-hook 'turn-on-auto-fill)

)

(map! :leader
      (:prefix ("m" . "<localleader>")
       :desc "Add note to header"
       "z" #'org-add-note))
(map! :leader
      (:prefix ("m" . "<localleader>")
       :desc "C-c C-c mapping"
       "," #'org-ctrl-c-ctrl-c))
(map! "<f2>" #'treemacs)
