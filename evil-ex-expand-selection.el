;;; evil-ex-expand-selection.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: August 27, 2023
;; Modified: August 27, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.2))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
;;-- end header


;;;###autoload (autoload 'evil-ex-expand-selection "editor/text-manipulation/autoload/evil-ex" nil t)
(evil-define-command evil-ex-expand-selection (&optional pattern case wholeline)
  "Ex function to create a visual selection by regexp match

PATTERN is a vim-style regexp. FLAGS is an optional string of characters. "
  (interactive "<p/>")
  (let ((start (point))
        (beg (line-beginning-position))
        (end (line-beginning-position))
        (regexp (when (car-safe pattern) (format "\\(^$\\)\\|\\(^.*?\\(%s\\)\\)" (car-safe pattern))))
        )
    (when regexp
      (save-excursion
        (evil-beginning-of-line)
        ;; Expand up
        (while (and (looking-at regexp) (not (bobp)))
          (unless (looking-at "^$") (setq beg (line-beginning-position)))
          (evil-previous-line)
          (evil-beginning-of-line))

        (goto-char end)
        (goto-char (line-beginning-position 2))

        ;; Expand down
        (while (and (looking-at regexp) (not (eobp)))
          (unless (looking-at "^$") (setq end (line-beginning-position)))
          (evil-next-line)
          (evil-beginning-of-line))

        )
      (unless (and (eq (line-beginning-position) beg)
                   (eq (line-beginning-position 1) end))
        (evil-visual-make-selection beg end 'line)
        (add-hook 'post-command-hook #'evil-ex-expand-ensure-visual-selection-h)
        )
      )
    )
  )


(defun evil-ex-expand-ensure-visual-selection-h ()
  " Auto-removing post-command hook to ensure visual selection in evil-ex"
  (evil-visual-restore)
  (remove-hook 'post-command-hook #'evil-ex-expand-ensure-visual-selection-h)
  )


(provide 'evil-ex-expand-selection)
;;; evil-ex-expand-selection.el ends here
