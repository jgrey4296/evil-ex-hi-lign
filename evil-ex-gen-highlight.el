;;; evil-ex-gen-highlight.el -*- lexical-binding: t; no-byte-compile: t; -*-
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
;;  Provides  an evil-ex argument type ("<p/>") which performs a similar highlighting as
;;  evil-ex-substitute, but for other operators.
;;
;;; Code:
;;-- end header
(require 'evil-ex)

(defvar evil-ex-gen-highlight-active t)

(evil-ex-define-argument-type gen-pattern
  " Like the highlighter for evil-ex-substitute, but for patterns"
  :runner (lambda (flag &optional arg)
            (evil-ex-gen-highlight-handler nil flag arg))
  )

(evil-define-interactive-code "<p/>"
  "Ex Pattern Argument with active highlighting"
  :ex-arg gen-pattern
  (when evil-called-from-ex-p
    (evil-ex-gen-highlight-get-pattern-info evil-ex-argument t))
  )

;;;###autoload
(defun evil-ex-gen-highlight-handler (prefix flag &optional arg)
  (with-current-buffer evil-ex-original-buffer
    (cond ((eq flag 'start)
           (evil-ex-make-hl 'evil-ex-gen-highlight-pattern
                            :face 'evil-ex-substitute-matches
                            :win (minibuffer-selected-window)
                            :update-hook #'evil-ex-pattern-update-ex-info
                            ;; :match-hook (and evil-ex-gen-highlight-active #'evil-ex-pattern-update-replacement))
                            )
           (setq flag 'update))
          ((eq flag 'stop)
           (evil-ex-delete-hl 'evil-ex-gen-highlight-pattern)))

    (when (and (eq flag 'update) (not (zerop (length arg))))
      (condition-case lossage
          (let* ((result (evil-ex-gen-highlight-get-pattern-info arg t prefix))
                 (pattern (pop result))
                 (range (or (evil-copy-range evil-ex-range)
                            (evil-range (line-beginning-position)
                                        (line-end-position)
                                        'line
                                        :expanded t)))
                 )
            ;; (setq evil-ex-substitute-current-replacement replacement)
            (evil-expand-range range)
            (evil-ex-hl-set-region 'evil-ex-gen-highlight-pattern
                                   (evil-range-beginning range)
                                   (evil-range-end range))
            (evil-ex-hl-change 'evil-ex-gen-highlight-pattern pattern))
        (end-of-file
         (evil-ex-pattern-update-ex-info nil "incomplete patternment spec"))
        (user-error
         (evil-ex-pattern-update-ex-info nil (format "%s" lossage)))))
    )
  )

;;;###autoload
(defun evil-ex-gen-highlight-get-pattern-info (string &optional implicit-r prefix)
  "return the pattern pattern info of command line string.
returns an ex-pattern (see `evil-ex-make-pattern')
if implicit-r is non-nil, then
the flag 'r' is assumed, i.e. in the case of an empty pattern the
last search pattern is used. "
  (let (pattern flags)
    (cond
     ((or (null string) (string-match-p "^[a-za-z]" string))
      ;; flags are everything that is not a white space
      (when (and string (string-match "[^[:space:]]+" string))
        (setq flags (match-string 0 string))))
     (t
      (let ((args (evil-delimited-arguments string 3)))
        (setq pattern (pop args)
              flags (pop args))
        ;; append implicit "r" flag if required
        (when (and implicit-r (not (memq ?r (append flags nil))))
          (setq flags (concat flags "r"))))))

    ;; if flags equals "&" add previous flags
    (if (and (not (zerop (length flags)))
             (= (aref flags 0) ?&))
        (setq flags (append (substring flags 1)
                            evil-ex-substitute-flags))
      (setq flags (append flags nil)))

    ;; if no pattern, use previous pattern, either search or
    ;; substitute pattern depending on `evil-ex-last-was-search' and
    ;; the r flag
    (when (zerop (length pattern))
      (setq pattern
            (if (eq evil-search-module 'evil-search)
                (if (and evil-ex-last-was-search (memq ?r flags))
                    (and evil-ex-search-pattern
                         (evil-ex-pattern-regex evil-ex-search-pattern))
                  )
              (if (eq case-fold-search t)
                  isearch-string
                (concat isearch-string "\\c")))
            flags (remq ?r flags)))
    ;; generate pattern
    (when pattern
      (setq pattern (evil-ex-gen-highlight-make-pattern pattern flags prefix)))
    (list pattern)))

(defun evil-ex-gen-highlight-make-pattern (regexp flags &optional prefix)
  "Create a PATTERN for patternment with FLAGS."
  (evil-ex-make-pattern (concat (or prefix "") regexp)
                        ;; case
                        (cond
                         ((memq ?i flags) 'insensitive)
                         ((memq ?I flags) 'sensitive)
                         ((not evil-ex-substitute-case)
                          evil-ex-search-case)
                         (t evil-ex-substitute-case))
                        ;; global
                        (or (and evil-ex-substitute-global
                                 (not (memq ?g flags)))
                            (and (not evil-ex-substitute-global)
                                 (memq ?g flags))))
  )

;;;###autoload (autoload 'evil-ex-gen-highlight-match-cmd)
(evil-define-command evil-ex-gen-highlight-match-cmd (pattern &rest args)
  "evil ex match, but with active highlighting"
  (interactive "<p/><!>")
  (evil-ex-match (format " |%s|" (car pattern)))
  )


(provide 'evil-ex-gen-highlight)
;;; evil-ex-gen-highlight.el ends here
