;;* python-related configuration

(setq dotemacs-py-dir (if load-file-name
                          (file-name-directory load-file-name)
                        default-directory))

(progn
  (add-to-list 'load-path (concat dotemacs-py-dir "elisp"))
  (add-to-list 'Info-default-directory-list (concat dotemacs-py-dir "info"))
  (setq Info-directory-list nil)
  (setenv "PYTHONPATH"
          (concat dotemacs-py-dir "python-libs" path-separator
                  (getenv "PYTHONPATH")))
  (setenv "PATH"
          (concat dotemacs-py-dir "bin" path-separator
                  (getenv "PATH")))
  (add-to-list 'exec-path (concat dotemacs-py-dir "bin")))
 
               


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

;; ** get rid of cedet
(eval-after-load "python"
  `(progn
     (remove-hook 'python-mode-hook 'wisent-python-default-setup)     
     ))
  
(defun python-reset-imenu ()
  (interactive)
  (if (fboundp 'setq-mode-local)
      (setq-mode-local python-mode
                       imenu-create-index-function 'python-imenu-create-index))
;;  (remove-hook 'python-mode-hook 'semantic-python-setup)
  (setq imenu-create-index-function 'python-imenu-create-index))


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
  ;;hideshow
  (if (and (display-graphic-p)
           (require 'hideshowvis nil t))
      (progn
        (require 'hideshow-fringe nil t)
        (hideshowvis-enable))
    (hs-minor-mode t))

  ;;outline
  (setq outline-regexp "[[:space:]]*\\(?:\\(?:class\\|def\\)\\)\\_>")
  ;;   (if (fboundp 'qtmstr-outline-mode)
  ;;       (qtmstr-outline-mode t)
  ;;     (if (fboundp 'qtmstr-outline-mode-hook)
  ;;         (qtmstr-outline-mode-hook)))
  )

(eval-after-load "python"
  `(add-hook 'python-mode-hook 'python-mode-init-folding))


;; ** code completion
;; *** auto-complete front-end for GNU Emacs built-in completion
(defun python-symbol-completions-maybe (prefix)
  (let ((python-el (symbol-file major-mode)))
    (if (string-match "lisp/progmodes/python.el" python-el) ;;Emacs builtin python.el
        (python-symbol-completions prefix)
      nil) ;;otherwise, return nil
    ))

(eval-after-load "auto-complete"
  `(progn
    (ac-define-source python-builtin
      '( (candidates . (python-symbol-completions-maybe ac-prefix))
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
;;   - no 'send region' support, thus no completion for dynamic object

(eval-after-load "pymacs"
  `(require 'pycompletemine nil t)
  )

(eval-after-load "pycompletemine"
  `(progn
     ;;use auto-complete as frond-end
     (defun py-complete-get-help-short (prefix)
       (let ((result (py-complete-help prefix)))
         (if (> (length result) 300)
             (substring result 0 300)
           result)))
     
     (when (and (featurep 'auto-complete)
                ;;need pycompletemine.el/pycomplete.py hacked by myself
                (fboundp 'pycomplete-get-all-completions-for-ac))
       
       (defun py-complete-help-short (str1)
         (let ((result (if (fboundp 'py-complete-get-help)
                           (py-complete-get-help str1)
                         (py-complete-help str1))))
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
                     (add-to-list 'ac-sources 'ac-source-pycompletemine)))
       )))


  
;; ** document lookup
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
;;stolen from http://stackoverflow.com/a/1068731
(defun pydoc (&optional arg)
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

;; *** pdb, pydb, pydbgr (realgud)

;; ** python shell
;; *** python-cell
(autoload 'python-cell-mode "python-cell"
  "Highlight MATLAB-like cells and navigate between them." t)

(eval-after-load "python"
  `(progn
     (add-hook 'python-mode-hook 'python-cell-mode)

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

;; *** pychecker


;; ** ropemacs
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


;; ** misc
;; By default, Emacs inhibits (for `run-python') the loading of Python
;; modules from the current working directory, for security reasons.
;; To disable it:
;;(setq python-remove-cwd-from-path nil)

;; *** highlight-indentation
(autoload 'highlight-indentation-mode "highlight-indentation" nil t)
(eval-after-load "python"
  `(add-hook 'python-mode-hook 'highlight-indentation-mode))

;; *** virtualenv
(autoload 'virtualenv-activate  "virtualenv"
  "Activate the virtualenv located in DIR" t)

