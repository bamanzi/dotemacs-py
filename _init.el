;;* python-related configuration

(setq dotemacs-py-dir (if load-file-name
                          (file-name-directory load-file-name)
                        default-directory))

(defconst pythonpath-envvar (getenv "PYTHONPATH")
  "Value of env-var PYTHONPATH when emacs started.")

(progn
  (add-to-list 'load-path (concat dotemacs-py-dir "elisp"))
  (add-to-list 'Info-default-directory-list (concat dotemacs-py-dir "info"))
  (setq Info-directory-list nil) ;; force re-init
  )

(defun setup-python2 ()
  (interactive)
  
  (setenv "PYTHONPATH" (concat pythonpath-envvar
                               path-separator        ; ':' on *nix
                               (concat dotemacs-py-dir "python-libs")))

  (setenv "PATH" (concat (getenv "PATH")
                         path-separator
                         (concat dotemacs-py-dir "bin")))
  
  (add-to-list 'exec-path (concat dotemacs-py-dir "bin") 'append)

  (setq python-shell-interpreter (if (executable-find "python2")
                                     "python2"
                                   "python"))
  (setq pymacs-python-command python-shell-interpreter))


(defun setup-python3 ()
  (interactive)
  (setenv "PYTHONPATH" (concat pythonpath-envvar
                               path-separator ; ':' on *unix
                               (concat dotemacs-py-dir "python3-libs")
                               path-separator
                               (concat dotemacs-py-dir "python-libs")))

  (setenv "PATH" (concat (getenv "PATH")
                         path-separator
                         (concat dotemacs-py-dir "bin-py3")))
  
  (add-to-list 'exec-path (concat dotemacs-py-dir "bin-py3") 'append)

  (setq python-shell-interpreter (if (executable-find "python3")
                                     "python3"
                                   "python"))
  (setq pymacs-python-command python-shell-interpreter))


(setup-python3)


;; ** python.el: use fganilla's python.el
;;emacs-24 already contains
(when (< emacs-major-version 24)
  (let ((python_el_e23 (concat (if load-file-name (file-name-directory load-file-name) default-directory)
                               "elisp/_emacs23/python.el")))
    (if (not (file-exists-p python_el_e23))
        (message "fgallina's python.el not found. you need to load it manually.")
      (message "loading fgallina's python.el for emacs-23")
      (load-file python_el_e23))))


(defun setup-ipython ()
  (interactive)
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

  (when (eq system-type 'windows-nt)
    (setq
     python-shell-interpreter "python.exe"
     python-install-dir (shell-command-to-string "python -c \"import sys; sys.stdout.write(sys.prefix)\"")
     python-shell-interpreter-args (concat "-i "
                                           (replace-regexp-in-string "/" "\\\\" python-install-dir)
                                           "\\Scripts\\ipython-script.py")))
  )

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Python
                     :key "M-x setup-ipython"
                     :description "Switch to use ipython as inferior python interpreter.")
     ))

;; ** get rid of cedet
(eval-after-load "python"
  `(progn
     (remove-hook 'python-mode-hook 'wisent-python-default-setup)
     (add-hook    'python-mode-hook 'python-kick-semantic-imenu 'append)
     ))
  
(defun python-kick-semantic-imenu ()
  "Surpress semantic's imenu-create-index-function for python-mode."
  (interactive)
  (if (fboundp 'setq-mode-local)
      (setq-mode-local python-mode imenu-create-index-function
                       (if (fboundp 'python-imenu-create-index)
                           ;; emacs <= 24.3 has no `python-imenu-create-index'
                           'python-imenu-create-index
                         'imenu-default-create-index-function)))
  ;;  (remove-hook 'python-mode-hook 'semantic-python-setup)
  (if (fboundp 'python-imenu-create-index)
      (setq imenu-create-index-function 'python-imenu-create-index)))


;; ** pymacs: interface between emacs lisp and python

(autoload 'pymacs-load  "pymacs"
  "Import the Python module named MODULE into Emacs." t)
(autoload 'pymacs-eval  "pymacs"
  "Compile TEXT as a Python expression, and return its value." t)
(autoload 'pymacs-autoload  "pymacs"
  "Pymacs's equivalent of the standard emacs facility `autoload'." t)

(eval-after-load "python"
  `(require 'pymacs)  ;;load `pymacs', for `pymacs-autoload'
  )

(autoload 'py-complete-mode "pycompletemine"
  "Minor mode for pycomplete." t)

;; ** code folding
(eval-after-load "python"
  `(progn
     (require 'hideshow)
     (let ((python-hideshow-exp
            '("^\\s-*\\(?:def\\|class\\|if\\|elif\\|else\\|for\\|try\\|except\\)\\>"
              nil
              "#" 
              (lambda (arg)
                (python-end-of-block)
                (skip-chars-backward " \t\n"))
              nil))
           (old-config (assoc 'python-mode hs-special-modes-alist)))
       (if old-config
           (setcdr old-config python-hideshow-exp)
         (add-to-list 'hs-special-modes-alist `(python-mode ,python-hideshow-exp))))
     ))

(defun python-mode-init-folding ()
  ;;outline
  (if (require 'qtmstr-outline nil t)
      ;;(setq outline-regexp "[[:space:]]*\\(?:\\(?:class\\|def\\)\\)\\_>")
      ;; `outline-regexp' configured by python.el supports folding for class/def/if/else/for...
      (qtmstr-outline-mode t)
    ;;hideshow
    (if (require 'hideshowvis nil t)
        (hideshowvis-enable)
      (hs-minor-mode t)))
  )

(eval-after-load "python"
  `(add-hook 'python-mode-hook 'python-mode-init-folding))


;; ** code completion
;; *** auto-complete front-end for GNU Emacs built-in completion
;;disadvantages:
;;   - inferior python process (`run-python') needed
;;   - you need to type code or to 'send region' to make module/object inspectable
;;advantages:
;;   - no other installation / configuration /setup needed
;;   

(defun python-ac-candidates--builtin-python-el (prefix)
  (let ((python-el (symbol-file major-mode)))
    (if (string-match "lisp/progmodes/python.el" python-el) ;;Emacs builtin python.el
        (if (functionp 'python-symbol-completions)          ;; ? - v23.2
            (python-symbol-completions prefix)
          (if (and (functionp 'python-shell-completion--get-completions) ;; v23.3 - ?
                   (python-shell-get-process)) ;; Note: `run-python' needed
              (python-shell-completion--get-completions (python-shell-get-process)
                                                       (buffer-substring-no-properties (line-beginning-position) (point))
                                                       prefix)
            (if (and (functionp 'python-shell-completion-get-completions) ;; ? - v24.3
                     (python-shell-get-process)) ;; Note: `run-python' needed
                (python-shell-completion-get-completions (python-shell-get-process)
                                                         (buffer-substring-no-properties (line-beginning-position) (point))
                                                         prefix)))))))

(eval-after-load "auto-complete"
  `(progn
    (ac-define-source python-builtin
      '( (candidates . (python-ac-candidates--builtin-python-el ac-prefix))
         (symbol . "py")
         (prefix . "[ \t\n['\",()]\\([^\t\n['\",()]+\\)\\=") ))

    (add-hook 'python-mode-hook
              #'(lambda ()
                  (add-to-list 'ac-sources 'ac-source-python-builtin 'append)))
    ))

;; *** pycompletemine from PDEE (https://github.com/pdee/pdee/ )
;; You need `pycompletemine.{el,py}' from PDEE and pymacs
;;advantages:
;;   + differ from `pycomplete', this one would work on both python-mode.el
;;     and GNU Emacs built-in python.el
;;   + doc info and signature for completions
;;disadvantages:
;;   - `pymacs' needed

(eval-after-load "pymacs"
  `(require 'pycompletemine nil t)
  )

(eval-after-load "pycompletemine"
  `(progn
     (pymacs-load "pycomplete")
     
     ;;add auto-complete frond-end     
     (when (and (featurep 'auto-complete)
                ;;need pycompletemine.el/pycomplete.py hacked by myself
                (fboundp 'pycomplete-get-all-completions-for-ac))
       
       (defun py-complete-get-help-short (prefix)
         (let ((result (py-complete-get-help prefix)))
           (if (> (length result) 300)
               (substring result 0 300)
             result)))
       
       (ac-define-source pycompletemine
         '((depends pycompletemine)  ;;FIXME: ok?
           (prefix .  "[ \t\n['\",()]\\([^\t\n['\",()]+\\)\\=")
           (candidates . (pycomplete-get-all-completions-for-ac ac-prefix))
           (symbol . "pyc")
           (document . py-complete-get-help-short)))
       
       (add-hook 'python-mode-hook
                 #'(lambda ()
                     (add-to-list 'ac-sources 'ac-source-pycompletemine))))
     ))

  
;; ** document lookup
;; *** pythonXXX.chm
(setq python-install-dir nil)
(defun python.chm (keyword)
   "Open python.chm (with keyhh.exe) and try to lookup current symbol."
   (interactive "sHelp on keyword: ")
   (unless python-install-dir
     (setq python-install-dir
           (shell-command-to-string "python -c \"import sys; sys.stdout.write(sys.prefix)\"")))
   (let ((files (directory-files (concat python-install-dir "\\doc") 'full "python.*.chm")))
     (when files
         (start-process "keyhh" nil "keyhh.exe"
                        "-Emacs"  ;;(concat "-" mode-name) ;; use mode name as ID
                        "-#klink" (format "'%s'" keyword)
                        (car files))
         (set-process-query-on-exit-flag (get-process "keyhh") nil))))

;; *** python.info
;; (eval-after-load "pydoc-info"
;;   `(progn
;;      (let ((py-info-dir (concat
;;                          (file-name-directory (locate-library "pydoc-info"))
;;                          "info")))
;;        (add-to-list 'Info-default-directory-list py-info-dir)
;;        (if Info-directory-list
;;            (add-to-list 'Info-directory-list py-info-dir)))
;;      ;;then use C-h S (`info-lookup-symbol') to lookup python doc
;;      ))

(eval-after-load "python"
  `(progn
     (require 'pydoc-info nil t) ))

(progn
  (defvar anything-c-source-info-python
    '((name . "Info index: python")
      (info-index . "python")))

  (defun anything-info-python ()
    (interactive)
    (anything
     :input (thing-at-point 'symbol)
     :prompt "Info about: "
     :candidate-number-limit 20
     :sources
     '( anything-c-source-info-python
        ;;         anything-c-source-info-elib
        ;;anything-c-source-info-cl
        )))

  (eval-after-load "python"
    `(progn
       (define-key python-mode-map (kbd "M-s <f1>") 'anything-info-python)
       ))
  )

;; *** pydoc command line
(autoload 'pydoc "pydoc"
  "Display pydoc information for NAME in a buffer named *pydoc*." t)
(autoload 'pydoc-at-point "pydoc"
  "Try to get help for thing at point." t)
(autoload 'pydoc-browse "pydoc"
  "Open a browser to pydoc." t)

(eval-after-load "python"
  `(progn
     (define-key python-mode-map (kbd "C-c <C-f1>") 'pydoc)
     ))

;;--- a simple version, stolen from http://stackoverflow.com/a/1068731
(defun pydoc- (&optional arg)
  (interactive (list
				(read-string "Call pydoc with arg: "
							 (with-syntax-table python-dotty-syntax-table
							   (current-word)))))
  (setq cmd (concat "pydoc " arg))
  (ad-activate-regexp "auto-compile-yes-or-no-p-always-yes")
  (shell-command cmd "*Pydoc Output*")
  (setq pydoc-buf (get-buffer "*Pydoc Output*"))
  ;;(switch-to-buffer-other-window pydoc-buf)
  (with-current-buffer pydoc-buf
    ;;(python-mode)
    (require 'woman)
    (woman-man-buffer)
    (help-mode))
  (ad-deactivate-regexp "auto-compile-yes-or-no-p-always-yes")
)

;;--- helm front-end
;; https://github.com/syohex/emacs-helm-pydoc
(autoload 'helm-pydoc "helm-pydoc"
  "Undocumented." t)


;; *** pylookup
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)

(setq pylookup-dir (file-name-directory (locate-library "pylookup")))
(setq pylookup-program (concat pylookup-dir "pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "pylookup.db"))

(when (eq system-type 'windows-nt)
  ;; (call-progress "pylookup.bat"...) would cause "file-error: spawning child process: exec format error"
  (setq pylookup-program "python.exe")

  (defun pylookup-exec-get-cache ()
    "Run a pylookup process and get a list of cache (db key)"

    (split-string
     (with-output-to-string
       (call-process pylookup-program nil standard-output nil
                     (if (eq system-type 'windows-nt) (locate-library "pylookup.py") "")
                     "-d" (expand-file-name pylookup-db-file)
                     "-c"))))

  (defun pylookup-exec-lookup (search-term)
    "Runs a pylookup process and returns a list of (term, url) pairs."

    (mapcar
     (lambda (x) (split-string x ";"))
     (split-string
      (with-output-to-string
        (apply 'call-process pylookup-program nil standard-output nil
               (if (eq system-type 'windows-nt) (locate-library "pylookup.py") "")                
               "-d" (expand-file-name pylookup-db-file)
               "-l" search-term
               "-f" "Emacs"
               pylookup-search-options))
      "\n" t)))
  )


;; ** run

;; *** doctest?

;; ** debug
;; *** pdb & ipdb (fgallina's python.el)

;; stolen from http://wenshanren.org/?p=351
(defun python-add-pdb-breakpoint ()
  "Add a pdb break point statement (pdb.set_trace())"
  (interactive)
  (newline-and-indent)
  (insert "import pdb; pdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import pdb; pdb.set_trace()"))

(defun python-add-ipdb-breakpoint ()
  "Add a ipdb break point statement (ipdb.set_trace())"
  (interactive)
  (newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))

(defun python-add-trepan-breakpoint ()
  "Add a trepan3k break point statement (trepan.api.debug())"
  (interactive)
  (newline-and-indent)
  (insert "import trepan.api; trepan.api.debug()")
  (highlight-lines-matching-regexp "^[ ]*import trepan.api; trepan.api.debug()"))

;; https://danielnouri.org/notes/2022/10/14/pdb-tracking-with-emacs-python-mode/
(defun python-comint-enable-pdbtracking ()
  "enable pdbtracking for pdb/pdbpp session started in eshell/shell."  
  (interactive)
  (unless (get-buffer-process (current-buffer))
    (user-error "Error: current buffer is not a comint? get-buffer-process() returns nil."))
  (add-hook 'comint-output-filter-functions
            'python-pdbtrack-comint-output-filter-function t))

(defun ipdb (cmdline)
  "Invoke `gud-pdb' with `ipdb' (https://pypi.python.org/pypi/ipdb) "
  (interactive
   (list (let ((gud-pdb-command-name "ipdb"))
           (require 'gud)
           (gud-query-cmdline 'pdb))))
  (pdb cmdline))

(defun trepan3k (cmdline)
  "Invoke `gud-pdb' with `trepan3k' (https://pypi.python.org/pypi/trepan3k) "
  (interactive
   (list (let ((gud-pdb-command-name "trepan3k"))
           (require 'gud)
           (gud-query-cmdline 'pdb))))
  (pdb cmdline))

;; *** pdbpp tracking on GUD
;; tested on emacs 26.1, 27.1 & 28.1
(eval-after-load "gud"
  `(progn
     ;; so we can leave the default value
     (make-variable-buffer-local 'gud-pdb-marker-regexp-start)
     (make-variable-buffer-local 'gud-pdb-marker-regexp)
     ))

(defun gud-pdb-enable-pdbpp-tracking ()
  (interactive)
  (unless (string-match "\\*gud-pdb.*" (buffer-name))
    (user-error "Error: you need to invoke this command (`gud-pdb-enable-pdbpp-tracking`) on a *PDB* buffer"))
  (setq gud-pdb-marker-regexp-start "^\\[[0-9]+\\] > "
        gud-pdb-marker-regexp (concat gud-pdb-marker-regexp-start
                                      "\\([[:graph:] \\]*\\)(\\([0-9]+\\))\\([a-zA-Z0-9_]*\\|\\?\\|"
                                      "<\\(?:module\\|listcomp\\|dictcomp\\|setcomp\\|genexpr\\|lambda\\|\\)>"
                                      "\\)()\\(->[^\n\r]*\\)?[\n\r]")
        gud-marker-filter 'gud-pdbpp-marker-filter))

(defun gud-pdbpp-marker-filter (string)
  (let ((output (ansi-color-filter-apply string)))
    (gud-pdb-marker-filter output)))

;; *** trepan3k trackping on GUD
;; trepan3k:
;;   (/home/pi/.emacs.d/packages/dotemacs-py/python-libs/pep8.py:1360 @788): <module>
;;   xx 1360     _main()
;;   (trepan3k)
;; or
;;   (/home/pi/.emacs.d/packages/dotemacs-py/python-libs/pep8.py:1331): _main
;;   -- 1331     options, args = process_options()
;;   (trepan3k) 

(defun gud-pdb-enable-trepan3k-tracking()
  (interactive)
  (unless (string-match "\\*gud-.*\\*" (buffer-name))
    (user-error "Error: you need to invoke this command (`gud-pdb-enable-trepan3k-tracking`) on a *PDB* buffer"))
  (setq gud-pdb-marker-regexp-start "^(/"
        gud-pdb-marker-regexp (concat "^(\\([[:graph:] \\]*\\):\\([0-9]+\\)\\(?: @[0-9]+\\)?): " ; (/file_path:lineno @n):
                                      "\\([a-zA-Z0-9_]*\\|\\?\\|" ; func_name
                                      "<\\(?:module\\|listcomp\\|dictcomp\\|setcomp\\|genexpr\\|lambda\\|\\)>" ; <module|xxx>
                                      "\\)\\(->[^\n\r]*\\)?[\n\r]")
        gud-marker-filter 'gud-pdbpp-marker-filter))

;; *** pdb, pydb, pydbgr (realgud)

;; TODO

;; ** python shell
(eval-after-load "python"
  `(progn     
     (define-key python-mode-map (kbd "<f9> M-`") 'run-python)
     (define-key python-mode-map (kbd "<f9> `")   'python-shell-switch-to-shell)
     
     (define-key python-mode-map (kbd "<f9> d")   'python-shell-send-defun)
     (define-key python-mode-map (kbd "<f9> SPC") 'python-shell-send-region)
     ))

;; make `python-shell-send-region' work for indented block
(eval-after-load "python"
  `(when (and (fboundp 'python-shell-send-region)
              (string= emacs-version "24.3")) ;; FIXME: any other version?

     (defun python-shell-send-region (start end)
       "Send the region delimited by START and END to inferior Python process."
       (interactive "r")
       (let* (
              (line-num (line-number-at-pos start))
              (selection (buffer-substring start end))
              (indented (string= (substring selection 0 2) "  "))
              (content (if indented
                           (concat
                            ;; When sending a region, add blank lines for non sent code so
                            ;; backtraces remain correct.
                            (make-string (- line-num 2) ?\n)
                            "if True:\n"
                            selection)
                         (concat (make-string (1- line-num) ?\n)
                                 selection))))
         (python-shell-send-string content  nil t)))
     ))

;; stolen from http://wenshanren.org/?p=351
(defun python-insert-interactive-stmt ()
  "Add a python shell (real-eval-print loop) into your progrem."
  (interactive)
  (newline-and-indent)
  (insert "import code; code.interact(local=vars())")
  (move-end-of-line 1)
  (if (get-buffer-process (current-buffer))
      (comint-send-input)))

;; *** python-x (more python-shell-send-xxx commands)
;; section: delimited by comments starting with "# ---";
;; fold: defined by "# {{{" and "# }}}"
(autoload 'python-shell-send-fold-or-section "python-x"
  "Send the section of code at point to the inferior Python process, up to the" t)

(autoload 'python-shell-send-line-and-step "python-x"
  "Send the current line (with any remaining continuations) to the inferior Python process," t)
(autoload 'python-shell-print-region-or-symbol "python-x"
  "Send the current region to the inferior Python process, if active; otherwise" t)

(eval-after-load "python"
  `(progn     
     (define-key python-mode-map (kbd "<f9> -")   'python-shell-send-fold-or-section)
     (define-key python-mode-map (kbd "<f9> {")   'python-shell-send-fold-or-section)
     
     (define-key python-mode-map (kbd "<f9> l")   'python-shell-send-line-and-step)
     (define-key python-mode-map (kbd "<f9> @")   'python-shell-print-region-or-symbol)

     (unless (fboundp 'python-info-encoding)
       ;; backported from emacs-25
       (defun python-info-encoding-from-cookie ()
         "Detect current buffer's encoding from its coding cookie.
Returns the encoding as a symbol."
         (let ((first-two-lines
                (save-excursion
                  (save-restriction
                    (widen)
                    (goto-char (point-min))
                    (forward-line 2)
                    (buffer-substring-no-properties
                     (point)
                     (point-min))))))
           (when (string-match (python-rx coding-cookie) first-two-lines)
             (intern (match-string-no-properties 1 first-two-lines)))))

       (defun python-info-encoding ()
         "Return encoding for file.
Try `python-info-encoding-from-cookie', if none is found then
default to utf-8."
         ;; If no encoding is defined, then it's safe to use UTF-8: Python 2
         ;; uses ASCII as default while Python 3 uses UTF-8.  This means that
         ;; in the worst case scenario python.el will make things work for
         ;; Python 2 files with unicode data and no encoding defined.
         (or (python-info-encoding-from-cookie)
             'utf-8))
       )))


  ;; *** python-cell
  (autoload 'python-cell-mode "python-cell"
    "Highlight MATLAB-like cells and navigate between them." t)

;; (add-hook 'python-mode-hook 'python-cell-mode)

(eval-after-load "python-cell"
  `(progn
     (define-key python-mode-map (kbd "<f9> c") 'python-shell-send-cell)

     ;; use a darker background color for CELL
     (setq python-cell-highlight-face 'fringe)
     ))

;; *** ipython
;;ipython.el needs python-mode.el, thus we use a lightweight solution
(defun ipython-in-term ()
  "Run ipython in `ansi-term'."
  (interactive)
  (ansi-term "ipython" "ipython"))

(defun bpython-in-term ()
  "Run bpython in `ansi-term'."
  (interactive)
  (ansi-term "bpython" "bpython"))


;; ** lint
;;TIPS: or you can M-x `python-check' (provided by python.el)

;; *** pep8
(autoload 'python-pep8  "python-pep8"
  "Run PEP8, and collect output in a buffer." t)
(autoload 'pep8 "python-pep8"
  "Run PEP8, and collect output in a buffer." t)

(defun pep8-simple ()
  (interactive)
  (let ((compile-command (concat "pep8 "
                                 (file-name-nondirectory (buffer-file-name))))
        (compilation-ask-about-save nil))
    (call-interactively 'compile)  
    ))

;; *** pylint
(autoload 'python-pylint "python-pylint"
  "Run PYLINT, and collect output in a buffer." t)
(autoload 'pylint  "python-pylint"
  "Run PYLINT, and collect output in a buffer." t)

;;my simple way
(defun pylint-simple ()
  (interactive)
  (let ((compile-command (concat "epylint "  ;; "pylint -rn -f parseable "
                                 (file-name-nondirectory (buffer-file-name))))
        (compilation-ask-about-save nil))
    (call-interactively 'compile)  
    ))

;; *** pyflakes
;;FIXME: when `flycheck' loaded, flymake might not work
(autoload 'flymake-python-pyflakes-load "flymake-python-pyflakes"
  "Undocumented." t)

(defalias 'pyflakes 'flymake-python-pyflakes-load)

(eval-after-load "flymake-python-pyflakes"
  `(progn
     (ad-disable-advice 'flymake-mode 'around 'flycheck-flymake-mode) ;;FIXME: not work?
     (ad-deactivate 'flymake-mode)
     ))

;; *** pychecker


;; ** code analyze (better support for code completion, go-to-definition)
;; *** anaconda-mode
;; https://github.com/proofit404/anaconda-mode

(eval-after-load "anaconda-mode"
  `(progn
     ;; anacond-mode >= 0.1.2
     (setq anaconda-mode-installation-directory
           (concat dotemacs-py-dir "python-libs"))
     
     ;; we don't use '0.1.7' subdir
     (defun anaconda-mode-server-directory ()
       anaconda-mode-installation-directory)

     ;; in case we've installed anaconda-mode <= 0.1.1
     (setq anaconda-mode-server-directory anaconda-mode-installation-directory)
     ))

(autoload 'anaconda-mode "anaconda-mode"
  "Code navigation, documentation lookup and completion for Python." t)

(defun anaconda-enable ()
  "Enable `anaconda-mode' in all future Python buffers."
  (interactive)
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'turn-on-anaconda-eldoc-mode) ;; what about `python-eldoc-at-point' from python.el?
  (if (require 'ac-anaconda nil t)
      (ac-anaconda-setup))

  ;; for current buffer
  (when (eq major-mode 'python-mode)
    (anaconda-mode 1)
    (turn-on-anaconda-eldoc-mode)
    (if (fboundp 'ac-anaconda-setup)
        (ac-anaconda-setup))))

(defun anaconda-disable ()
  "Disable `anaconda-mode' in all future Python buffers."
  (interactive)
  (remove-hook 'python-mode-hook 'anaconda-mode)
  (remove-hook 'python-mode-hook 'eldoc-mode)
  
  (when (eq major-mode 'python-mode)
    (anaconda-mode -1)
    (turn-off-anaconda-eldoc-mode)
    (if (boundp 'ac-source-anaconda)
        (setq ac-sources (remq 'ac-source-anaconda ac-sources))))
  )

(defun toggle-anaconda-mode ()
  (interactive)
  (require 'anaconda-mode)
  (if anaconda-mode
      (anaconda-disable)
    (anaconda-enable))
  (message "Anaconda-mode %s for current and future python buffers."
           (if anaconda-mode "enabled" "disabled")))


;; *** elpy
;; https://github.com/jorgenschaefer/elpy
;; powerful, but a little heavy (too much dependencies)

;; *** ropemacs (seems buggy)
(setq ropemacs-confirm-saving nil
      ropemacs-guess-project t
      ropemacs-separate-doc-buffer t)
(setq ropemacs-enable-autoimport t)
(setq ropemacs-autoimport-modules '("os" "shutil" "sys" "logging"
                                    "django.*"))

;;if you got this error: Key sequence C-x p n d starts with non-prefix key C-x p
;;you need this:
(setq ropemacs-global-prefix "C-c C-p")

(eval-after-load "pymacs"
  `(progn
     (if (fboundp 'pymacs-autoload) ;;pymacs>=0.25 needed
         (progn
           (pymacs-autoload 'rope-open-project "ropemacs" "rope-"
                            "Open a rope project." t)
           (pymacs-autoload 'ropemacs-mode "ropemacs" "rope-"
                            "Toggle ropemacs-mode." t))
       (progn
         (if nil
             (pymacs-load "ropemacs" "rope-"))))
     ))


;; ** virtualenv
(autoload 'pyvenv-activate "pyvenv"
  "Activate the virtual environment in DIRECTORY." t)

;; work with virtuanenvwrapper envs (all envs in one foldr)
(autoload 'pyvenv-workon "pyvenv"
  "Activate a virtual environment from $WORKON_HOME." t)


;; ** ein: ipython notebook

(setq ein:use-auto-complete t)
;; Or, to enable "superpack" (a little bit hacky improvements):
;; (setq ein:use-auto-complete-superpack t)

(if (fboundp 'cheatsheet-add)
    (progn
      (cheatsheet-add :group 'Python
                      :key "M-x ein:notebooklist-open"
                      :description "emacs-ipython-notebook: Open notebook list buffer.")
      ))

(defun bmz/enable-emacs-ipython-notebook ()
  (interactive)
  (if (not (executable-find "ipython"))
      (message "No ipython installed. Abort load ein.")
    (let ((ipython-version (shell-command-to-string "ipython --version")))
      (if (string< ipython-version "2.0")
          (progn
            ;; http://tkf.github.io/emacs-ipython-notebook/
            (add-to-list 'load-path (concat dotemacs-py-dir "elisp/ein"))
            (add-to-list 'load-path (concat dotemacs-py-dir "elisp/ein/ein-ipy1")))
        (progn
          ;; https://github.com/millejoh/emacs-ipython-notebook
          (add-to-list 'load-path (concat dotemacs-py-dir "elisp/ein"))
          (add-to-list 'load-path (concat dotemacs-py-dir "elisp/ein/ein-ipy3"))))
      (if (require 'ein-loaddefs nil t)
          (progn
            (when (fboundp 'cheatsheet-add)
                  (cheatsheet-add :group 'Python
                                  :key "M-x ein:notebooklist-open"
                                  :description "emacs-ipython-notebook: Open notebook list buffer.")
                  (cheatsheet-add :group 'Python
                                  :key "M-x ein:junk-new"
                                  :description "emacs-ipython-notebook: Open a notebook to try random thing.")
                  )
            (message "EIN loaded. Now you can start to use IPython Notebook with:")
            (message "  `ein:notebooklist-open' - Open notebook list buffer.")
            (message "  `ein:junk-new' - Open a notebook to try random thing.")))
      (message "Failed to load package `ein'"))))


;; ** misc
;; By default, Emacs inhibits (for `run-python') the loading of Python
;; modules from the current working directory, for security reasons.
;; To disable it:
;;(setq python-remove-cwd-from-path nil)

;; *** compilation (make traceback lines clickable)

(eval-after-load "compile"
  `(progn
     (unless (assoc 'python-tracebacks-and-caml compilation-error-regexp-alist-alist)
       ;; emacs >= 24.1 already has this (in compile.el)
       (add-to-list 'compilation-error-regexp-alist-alist
                    '(python-tracebacks-and-caml
                      "^[ \t]*File \\(\"?\\)\\([^,\" \n\t<>]+\\)\\1, lines? \\([0-9]+\\)-?\\([0-9]+\\)?\\(?:$\\|,\
\\(?: characters? \\([0-9]+\\)-?\\([0-9]+\\)?:\\)?\\([ \n]Warning\\(?: [0-9]+\\)?:\\)?\\)"
                      2 (3 . 4) (5 . 6) (7))))

     (add-to-list 'compilation-error-regexp-alist 'python-tracebacks-and-caml)
     ))

;; TODO: support pdb stacktrace & unittest error message
;; http://permalink.gmane.org/gmane.emacs.python-mode/370


;; *** highlight indentation
(autoload 'indent-guide-mode  "indent-guide"
  "Show vertical lines to guide indentation." t)

(autoload 'highlight-indentation-mode "highlight-indentation" nil t)
(autoload 'highlight-indentation-current-column-mode "highlight-indentation"
  "Hilight Indentation minor mode displays" t)

;; `indent-guide-mode' actually use a char as the guide line,
;; thus if you use term's copy method (such as putty's or tmux's),
;; maybe `highlight-indentation-mode' is better.
(defun highlight-indent-toggle ()
  (interactive)
  ;; (if (display-graphic-p)
  ;;     (indent-guide-mode)
  (highlight-indentation-current-column-mode)
  ;;  )
  )

(eval-after-load "python"
  `(add-hook 'python-mode-hook 'highlight-indent-toggle))


;; *** enhance the menu
(setq python-plus-menu
      '("Python+"
        ["Pydoc on symbol..." pydoc
         :help "Call `pydoc' command line utility."]
        ["Pydoc (helm)..." helm-pydoc
         :help "Call `pydoc' with helm interface."]
        ["Open module (via helm-pydoc)..." helm-pydoc
         :help "Open python module via `helm-pydoc'."]        
        ["Info page on symbol..." anything-info-python
         :help "Lookup document in file `python.info' (with `anything')."]
        "---"
        ["Highlight Indentation" highlight-indentation-current-column-mode
         :help "Highlight indentation with a vertical bar."
         :style toggle
         :selected (and (boundp 'highlight-indentation-current-column-mode)
                        (or highlight-indentation-mode highlight-indentation-current-column-mode))]
        ["Python Cell mode" python-cell-mode
         :help "Highlight MATLAB-like cells and navigate between them."
         :style toggle
         :selected (bound-and-true-p python-cell-mode)]
        ("Auto Complete"
         ["Toggle Auto-Complete mode" auto-complete-mode
          :help "Toggle auto-complete mode."
          :style toggle
          :selected (bound-and-true-p auto-complete-mode)]
         ["complete symbol at point" auto-complete
          :help "Start auto-completion at current point (use this if `ac-auto-start' set to nil)"]
         "---"
         ["ac-source-python-builtin" #'(lambda ()
                                         (interactive)
                                         (if (and (not (memq 'ac-source-python-builtin ac-sources))
                                                  (not (python-shell-get-process)))
                                             (call-interactively 'run-python))
                                         (ac-toggle-source 'ac-source-python-builtin))
          :help "use completion method in GNU Emacs' python.el (a running python shell required)"
          :style toggle
          :selected (memq 'ac-source-python-builtin ac-sources)]
         ["ac-source-pycomplete" #'(lambda()
                                     (interactive)
                                     (require 'pycompletemine)
                                     (unless (fboundp 'pycomplete-pycomplete)
                                       (pymacs-load "pycomplete"))
                                     (ac-toggle-source 'ac-source-pycompletemine))
          :help "use `pycompletemine' as completion back-end (`pymacs' needed)"
          :style toggle
          :selected (memq 'ac-source-pycompletemine ac-sources)]
         ["ac-source-anaconda" (ac-toggle-source 'ac-source-anaconda)
          :help "use `anaconda-mode' as completion back-end (`ac-anaconda' needed)"
          :style toggle
          :selected (memq 'ac-source-anaconda ac-sources)])
        ("pycomplete"
         ["load pymacs + pycomplete" (progn
                                       (pymacs-load "pycomplete")
                                       (require 'pycompletemine))
          :visible (not (fboundp 'pycomplete-pycomplete))]
         ["py-complete-mode"            py-complete-mode
          :help "Toogle py-complete-mode for keybindings in `py-complete-mode-map'."
          :style toggle
          :selected (bound-and-true-p py-complete-mode)]
         ["exec selected region in pymacs" py-complete-exec-region
          :help "Exec selected region in pymacs in order to gain better inspection/completion."
          :enable (fboundp 'pycomplete-exec-lines)]
         "---"
         ["complete symbol at point"    py-complete
          :enable (fboundp 'pycomplete-pycomplete)]
         ["help on symbol at point"     py-complete-help-thing-at-point
          :enable (fboundp 'pycomplete-pyhelp)]
         ["help on symbol..."           py-complete-help
          :enable (fboundp 'pycomplete-pyhelp)]
         ["show signature on symbol at point"  py-complete-signature-expr
          :enable (fboundp 'pycomplete-pysignature)]
         )
        ("debug"
         ["insert pdb breakpoint" python-add-pdb-breakpoint
          :help "Insert 'import pdb; pdb.set_trace()' at current line."]
         ["insert ipdb breakpoint" python-add-ipdb-breakpoint
          :help "Insert 'import ipdb; ipdb.set_trace()' at current line."]
         ["insert trepan3k breakpoint" python-add-trepan-breakpoint
          :help "Insert 'from trepan.api import debug; debug()' at current line."]
         "---"
         ["debug with pdb + gud.el" pdb
          :help "launch debugger with pdb + gud.el"]
         ["debug with ipdb + gud.el" ipdb
          :help "launch debugger with ipdb + gud.el"]
         ["debug with trepan3k + gud.el" trepan3k
          :help "launch debugger with trepan3k + gud.el"]         
         ["enable pdbtracking for comint" python-comint-enable-pdbtracking
          :help "enable pdbtracking for pdb/pdbpp session started in eshell/shell."]
         "---"
         ["insert 'code.interact()' here" python-insert-interactive-stmt
          :help "Add a python shell (real-eval-print loop) into your progrem."]
         )
        ;; project        
        "---"
        ["Activate virtual environment..." pyvenv-activate
         :help "Activate the virtual environment in DIRECTORY."
         :style toggle
         :selected (bound-and-true-p pyvenv-virtual-env)]
        ["Toggle anaconda-mode" toggle-anaconda-mode
         :help "Enable anaconda-mode for better support for navigation & completion"
         :style toggle
         :selected (bound-and-true-p anaconda-mode)]
        ["   Find definition" anaconda-mode-find-definitions
         :help "Find definitions for thing at point."
         :enable (bound-and-true-p anaconda-mode)]
        ["   Find assignments" anaconda-mode-find-assignments
         :help "Find assignments for thing at point."
         :enable (bound-and-true-p anaconda-mode)]
        ["   Find references" anaconda-mode-find-references
         :help "Find references for thing at point."
         :enable (bound-and-true-p anaconda-mode)]
        ["   Go back" anaconda-mode-go-back
         :help "Switch to buffer of most recent marker."
         :enable (bound-and-true-p anaconda-mode)]
        ["   Show document" anaconda-mode-show-doc
         :help "Show documentation for context at point."
         :enable (bound-and-true-p anaconda-mode)]
        ["   Complete symbol" anaconda-mode-complete
         :help "Complete symbol for thing at pont."
         :enable (bound-and-true-p andconda-mode)]
        ))

(eval-after-load "python"
  `(progn
     (easy-menu-define python-plus-menubar python-mode-map
       "Addtional commands for `python-mode'."
       python-plus-menu)
     ))
