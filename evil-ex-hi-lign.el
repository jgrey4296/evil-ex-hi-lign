;;; evil-ex-hi-lign.el -*- lexical-binding: t; no-byte-compile: t; -*-
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
;;  Similar to the highlighting of evil-ex-substitute, but for aligning
;;
;;; Code:
;;-- end header
(require 'evil-ex)
(require 'evil-ex-gen-highlight)

(evil-ex-define-argument-type evil-ex-hi-lign-regexp
  " Like the highlighter for evil-ex-substitute, but for alignment "
  :runner (lambda (flag &optional arg)
            (evil-ex-gen-highlight-handler "\\(\\s-*\\)" flag arg))
  )

(evil-define-interactive-code "<al/>"
  "Ex Align argument"
  :ex-arg evil-ex-hi-lign-regexp
  (when evil-called-from-ex-p
    (evil-ex-gen-highlight-get-pattern-info evil-ex-argument t "\\(\\s-*\\)")
    )
  )


;;;###autoload (autoload 'evil-ex-hi-lighn "editor/text-manipulation/autoload/evil-ex-align" nil t)
(evil-define-command evil-ex-hi-lighn (beg end &optional pattern case wholeline)
  "Ex interface to `align-regexp'.

PATTERN is a vim-style regexp. FLAGS is an optional string of characters.
Supports the following flags:

g   Repeat alignment on all matches in each line"
  (interactive "<r><al/>")
  (when (car-safe pattern)
    (align-regexp beg end (car pattern) 1 1 nil)
    )
  )


;;;###autoload (autoload 'evil-ex-hi-lighn-expand "editor/text-manipulation/autoload/evil-ex-align" nil t)
(evil-define-command evil-ex-hi-lighn-expand (&optional pattern case wholeline)
  "Ex interface to `align-regexp', but auto-expands the selection for lines that match the regexp

PATTERN is a vim-style regexp. FLAGS is an optional string of characters.
Supports the following flags:

g   Repeat alignment on all matches in each line"
  (interactive "<al/>")
  (let ((start (point))
        (beg (line-beginning-position))
        (end (line-end-position 1))
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

        ;; Expand Down
        (while (and (looking-at regexp) (not (eobp)))
          (unless (looking-at "^$") (setq end (line-end-position)))
          (evil-next-line)
          (evil-beginning-of-line))

        ;; Run align
        (align-regexp beg end (car pattern) 1 1 nil)
        )
      )
    )
  )


(provide 'evil-ex-hi-lign)
;;; evil-ex-hi-lign.el ends here
