;;; pycompletemine.el -- Complete symbols at point using Pymacs.

;; Copyright (C) 2007  Skip Montanaro

;; Author:     Skip Montanaro
;; Maintainer: skip@pobox.com
;; Created:    Oct 2004
;; Keywords:   python pymacs emacs

;; This software is provided as-is, without express or implied warranty.
;; Permission to use, copy, modify, distribute or sell this software,
;; without fee, for any purpose and by any individual or organization, is
;; hereby granted, provided that the above copyright notice and this
;; paragraph appear in all copies.

;; Along with pycomplete.py this file allows programmers to complete Python
;; symbols within the current buffer.  See pycomplete.py for the Python side
;; of things and a short description of what to expect.

;;; Code
(require 'python) ;;FIXME: it seems that this package also works on python-mode.el
(require 'pymacs)

;;(pymacs-load "pycomplete")

(when (not (boundp 'py-dotted-expression-syntax-table))
  (setq py-dotted-expression-syntax-table
        (copy-syntax-table (if (boundp 'python-mode-syntax-table)
                               python-mode-syntax-table
                             py-mode-syntax-table)))
  (modify-syntax-entry ?_ "_" py-dotted-expression-syntax-table)
  (modify-syntax-entry ?. "_" py-dotted-expression-syntax-table))

;;; internal functions
(defun py-symbol-near-point ()
  "Return the first textual item to the nearest point."
  ;; alg stolen from etag.el
  (save-excursion
    (with-syntax-table py-dotted-expression-syntax-table
      (if (or (bobp) (not (memq (char-syntax (char-before)) '(?w ?_))))
          (while (not (looking-at "\\sw\\|\\s_\\|\\'"))
            (forward-char 1)))
      (while (looking-at "\\sw\\|\\s_")
        (forward-char 1))
      (if (re-search-backward "\\sw\\|\\s_" nil t)
          (progn (forward-char 1)
                 (buffer-substring (point)
                                   (progn (forward-sexp -1)
                                          (while (looking-at "\\s'")
                                            (forward-char 1))
                                          (point))))
        nil))))

(defun py-find-global-imports ()
  (save-excursion
    (let (first-class-or-def imports)
      (goto-char (point-min))
      (setq first-class-or-def
	    (re-search-forward "^ *\\(def\\|class\\) " nil t))
      (goto-char (point-min))
      (setq imports nil)
      (while (re-search-forward
	      "^[\t ]*\\(import \\|from \\([A-Za-z_\\.][A-Za-z_0-9\\.]*\\) import \\).*"
	      nil t)
	(setq imports (append imports
			      (list (buffer-substring
				     (match-beginning 0)
				     (match-end 0))))))
      imports)))

(defun py-complete ()
  (interactive)
  (let* ((pymacs-forget-mutability t)
         (symbol (py-symbol-near-point))
         (completions
          (pycomplete-pycomplete symbol
                                 (py-find-global-imports)
                                 default-directory)))
    (cond  ((null completions) ; no matching symbol
           (message "Can't find completion for \"%s\"" symbol)
           (ding))
          ((null (cdr completions)) ; sole completion
           (insert (car completions)))
          (t
           (message "Making completion list...")
           (with-output-to-temp-buffer "*PythonCompletions*"
             (display-completion-list completions))
           (message "Making completion list...%s" "done")))))

(defun py-complete-exec-region (begin end)
  "Exec selected lines in pymacs' global namespace. so that later
  completion have better inspection."
  (interactive "r")
  (pycomplete-exec-lines (buffer-substring-no-properties begin end)))

;; (define-key py-mode-map "\M-\C-i"  'py-complete)

(defconst pycomplete-version "$Revision: 100 $"
  "`pycomplete' version number.")

(defconst py-identifier
  "[A-Za-z_][A-Za-z_0-9]*"
  "Regular expression matching a python identifier.")


;; regular expressions regarding import statetment
;; based on Python Grammar
(defconst py-dotted-name-re
  (concat py-identifier "\\([.]" py-identifier "\\)*")
  "Regular expression matching a dotted_name production.")

(defconst py-dotted-as-name-re
  (concat py-dotted-name-re "\\(\\s +as\\s +" py-identifier "\\)*")
  "Regular expression matching a dotted_as_name production.")

(defconst py-dotted-as-names-re
  (concat py-dotted-as-name-re
          "\\(\\s *,\\s *"  py-dotted-as-name-re "\\)*")
  "Regular expression matching a dotted_as_names production.")

(defconst py-import-as-name-re
  (concat py-identifier "\\(\\s +as\\s +" py-identifier "\\)*" )
  "Regular expression matching a import_as_name production.")

(defconst py-import-as-names-re
  (concat py-import-as-name-re "\\(\\s *,\\s *" py-import-as-name-re "\\)*"
          "\\s *[,]?" )
  "Regular expression matching a import_as_names production.")

(defconst py-import-name-re
  (concat "^\\s *\\<import\\>\\s +" py-dotted-as-names-re)
  "Regular expression matching a import_name production.")

(defconst py-import-from-re
  (concat "^\\s *\\<from\\>\\s +" "\\([.]*" py-dotted-name-re "\\|[.]+\\)\\s +"
          "\\<import\\>\\s +" "\\([*]\\|(\\s *" py-import-as-names-re "[^)]*)"
          "\\|" py-import-as-names-re "\\)")
  "Regular expression matching a import_from production.")

(defconst py-imports-re
  (concat "\\("
          (mapconcat 'identity
                     (list py-import-name-re
                           py-import-from-re)
                     "\\|")
          "\\)")
  "Regular expression matching imports.")


;; for compatibility with python-mode 4.78
(unless (fboundp 'py-backslash-continuation-line-p)
  (defalias 'py-backslash-continuation-line-p 'py-backslash-continuation-preceding-line-p))


(defun blank-linep ()
  "check if current line is empty (only whitespaces and comments)"
  (save-excursion
    (beginning-of-line)
    (looking-at py-blank-or-comment-re)))


(defun char-before-blank ()
  "check if prev character is blank-type"
  (save-excursion
    (forward-char -1)
    (looking-at "[\n\t\r]")))


(defun pycomplete-version ()
  "Echo the current version of `pycomplete' in the minibuffer."
  (interactive)
  (message "Using `pycomplete' version %s" pycomplete-version)
  (py-keep-region-active))

(defun py-complete-python-dotexpr-begin nil
  (re-search-backward "[^a-zA-Z_0-9\\.]")
  (forward-char))

(defun py-complete-python-dotexpr-end nil
  (re-search-forward "[a-zA-Z_0-9\\.]*"))

(put 'python-dotexpr 'beginning-op 'py-complete-python-dotexpr-begin)
(put 'python-dotexpr 'end-op 'py-complete-python-dotexpr-end)

(defun py-complete-show (string &optional notip)
  (if (and (not notip)
           (require 'popup nil t))
      (progn
        (if (> (length string) 300)
            (setq string (substring string 0 300)))
        (if (and window-system (require 'popup-pos-tip nil t))
            (popup-pos-tip string)
          (popup-tip string)))
    (display-message-or-buffer string "*PythonHelp*")

    ;;FIXME: what if display-message-buffer not use buffer to show help?
    (with-current-buffer "*PythonHelp*"
      (setq buffer-read-only nil)
      (require 'woman)
      (woman-man-buffer))))


(defun py-complete-get-help (str1)
  (pycomplete-pyhelp str1 (py-find-global-imports) default-directory))

(defun py-complete-help (str1 &optional usetip)
  "get help on a python expression"
  (interactive "sHelp: \nP")
  (let ((help-string (py-complete-get-help str1)))
    ;; (if (and help-string (> (length help-string) 300))
    ;;     (with-output-to-temp-buffer "*Python Help*"
    ;;       (print help-string))
      (py-complete-show help-string (not usetip))))

(defun py-complete-help-thing-at-point nil
  (interactive)
  (require 'thingatpt)
  (save-excursion
    (backward-char 1)
    (let ((sym (thing-at-point 'python-dotexpr)))
      (if sym
          (py-complete-help sym 'usetip)))))

(set 'py-complete-current-signature nil)

(defun py-complete-signature (function)
  "get signature of a python function or method"
  (set 'py-complete-current-signature
       (pycomplete-pysignature function)))


(defun py-complete-signature-show nil
  (require 'thingatpt)
  (let ((sym (thing-at-point 'python-dotexpr)))
    (if sym
        (progn
          (py-complete-show (py-complete-signature sym) 'notip)))))


(defun py-complete-signature-expr nil
  (interactive)
  (require 'thingatpt)
  (let ((dotexpr (read-string "signature on: "
                              (thing-at-point 'python-dotexpr))))
    (if dotexpr
        (py-complete-show (py-complete-signature dotexpr) 'notip))))


(defun py-complete-electric-lparen nil
  "electricly insert '(', and try to get a signature for the stuff to the left"
  (interactive)
  (ignore-errors
    (py-complete-signature-show))
  (self-insert-command 1))


(defun py-complete-electric-comma nil
  "electricly insert ',', and redisplay latest signature"
  (interactive)
  (self-insert-command 1)
  (if py-complete-current-signature
      (py-complete-show (format "%s" py-complete-current-signature) 'notip)))


;;; user functions
(defvar py-complete-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<M-f1>")     'py-complete-help-thing-at-point)
    (define-key map "("                'py-complete-electric-lparen)
    (define-key map ","                'py-complete-electric-comma)
    (define-key map (kbd "C-c <M-f1>") 'py-complete-signature-expr)
    (define-key map (kbd "C-c <f1>")   'py-complete-help)
    (define-key map (kbd "C-c TAB")    'py-complete)
    map)
  "Keymap for py-complete minor mode."
  )

(define-minor-mode py-complete-mode
  "Minor mode for pycomplete.

Pymacs required.

Key bindings:
\\{{py-complete-mode-map}}"
  :group 'python
  :lighter " pyc"
  :keymap py-complete-mode-map
  (if py-complete-mode
      ;; turn on
      (progn
        (unless (functionp 'pycomplete-pycomplete)
          (pymacs-load "pycomplete"))
        (message "py-complete-mode enabled."))
    ;; turn off
    (message "py-complete-mode disabled.")))

     

(provide 'pycompletemine)
;;; pycompletemine.el ends here
