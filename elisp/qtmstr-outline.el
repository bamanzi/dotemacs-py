;;; qtmstr-outline.el --- fringe indicator for outline-mode

;; Original author: quotemstr <twitter: quotemstr>
;; Original URL:    http://paste.lisp.org/display/86638 )
;; Current maintainer: Ba Manzi <bamanzi@gmail.com>

;;; Commmentary:

;; This package would add left-fringe indicator for outline(-minor)-mode.
;;
;; On terminal, left-margin would be used (because no left-fringe on term).
;; Note `linum-mode' would use left-margin also, thus they can't be turned
;; on together.
;;
;; Usage:
;;    step 1. M-x load-library RET qtmstr-outline RET
;;    step 2. M-x qtmstr-outline-mode

;;; Code:

(require 'foldout)

(eval-when-compile
  (require 'cl))

(defun qtmstr-outline-newline ()
  (interactive)
  (if (outline-on-heading-p)
      (outline-insert-heading)
    (newline-and-indent)))

(defun qtmstr-outline-demote ()
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (outline-demote)))

(defun qtmstr-outline-promote ()
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (outline-promote)))

(defun qtmstr-outline-up-heading ()
  (interactive)
  (outline-up-heading (if (outline-on-heading-p) 1 0)))

(defun qtmstr-outline-reveal-point ()
  "Ensure the point is visible"
  (save-excursion
    (when (ignore-errors (outline-back-to-heading t) t)
      (show-entry)
      (while (and (ignore-errors (outline-up-heading 1 t))
                  (outline-on-heading-p))
        (show-children)))))

(defun qtmstr-outline-toggle-children ()
  (interactive)
  (let ((saved-point (point)))
    (outline-back-to-heading)
    (outline-toggle-children)
    (when (<= saved-point (line-end-position))
      (goto-char saved-point))))

(defun qtmstr-outline-show-top ()
  (interactive)
  (widen)
  (save-excursion
    (goto-char (point-min))
    (show-all)
    (outline-next-heading)
    (beginning-of-line)
    (hide-sublevels (funcall outline-level)))

  (when (outline-invisible-p)
    (goto-char (previous-single-char-property-change
                (point) 'invisible nil (point-min)))))


(defun qtmstr-outline-post-command-hook ()
  (when (and (eq (get-char-property (point) 'invisible) 'outline)
             (not (bobp))
             (eq (get-char-property (1- (point)) 'invisible) 'outline)
             ;; Emacs called post-command-hook before adjusting the
             ;; point to be outside any "intangible" areas, including
             ;; invisible regions. If the last command was a movement,
             ;; we probably got moved into this area, and we'll be
             ;; moved out as soon as the post-command hooks finish.
             (not (and (symbolp this-command)
                       (eq (get this-command 'CUA) 'move)))
             (not disable-point-adjustment)
             (not global-disable-point-adjustment))

    (qtmstr-outline-reveal-point)))

(defun qtmstr-outline-define-keys_orig ()
  (interactive)
  (define-key outline-mode-map "\r" #'qtmstr-outline-newline)
  (define-key outline-mode-map [(tab)] #'qtmstr-outline-demote)
  (define-key outline-mode-map [(backtab)] #'qtmstr-outline-promote)

  (setq minor-mode-map-alist
        (assq-delete-all 'outline-minor-mode minor-mode-map-alist))

  (setq outline-minor-mode-map (make-sparse-keymap))
  (define-key outline-minor-mode-map [menu-bar] outline-minor-mode-menu-bar-map)
  (define-key outline-minor-mode-map [left-fringe mouse-1] #'qtmstr-outline-fringe-click)

  (mapc #'(lambda (ent)
            (define-key outline-minor-mode-map
              (vector '(control ?c) ?f (car ent))
              (cdr ent)))
        '((?>          . foldout-zoom-subtree)
          (?\r         . foldout-zoom-subtree)
          (?<          . foldout-exit-fold)
          (?a          . show-all)
          (?u          . qtmstr-outline-up-heading)
          (?s          . show-subtree)
          (?w          . qtmstr-outline-show-top)
          ((shift ?\t) . outline-previous-visible-heading)
          (?v          . outline-previous-visible-heading)
          (?f          . qtmstr-outline-toggle-children)
          (?\t         . outline-next-visible-heading)
          (backtab     . outline-previous-visible-heading)
          (left        . foldout-exit-fold)
          (right       . foldout-zoom-subtree)))

  (push (cons 'outline-minor-mode outline-minor-mode-map) minor-mode-map-alist))

(defun qtmstr-outline-define-keys ()
  (interactive)
  (define-key outline-mode-prefix-map (kbd "RET") #'qtmstr-outline-newline)
  (define-key outline-mode-prefix-map (kbd "C-u") #'qtmstr-outline-up-heading)
  (define-key outline-mode-prefix-map (kbd "TAB") #'qtmstr-outline-toggle-children)

  (define-key outline-mode-prefix-map (kbd "<M-right>") #'qtmstr-outline-demote)
  (define-key outline-mode-prefix-map (kbd "<M-left>")  #'qtmstr-outline-promote)
  (define-key outline-mode-prefix-map (kbd "<M-up>")    #'outline-move-subtree-up)
  (define-key outline-mode-prefix-map (kbd "<M-down>")  #'outline-move-subtree-down)  

  (define-key outline-mode-prefix-map (kbd "C-w")       #'qtmstr-outline-show-top)

  (define-key outline-minor-mode-map [left-fringe mouse-1] #'qtmstr-outline-fringe-click)
  (define-key outline-minor-mode-map [left-margin mouse-1] #'qtmstr-outline-fringe-click)
  (define-key outline-minor-mode-map [left-margin mouse-3] #'show-subtree)
  )

(eval-after-load "outline"
  '(progn
     (qtmstr-outline-define-keys)))

(defvar qtmstr-outline-major-modes
  '(espresso-mode))

(defun qtmstr-outline-find-file-hook ()
  (when (memq major-mode qtmstr-outline-major-modes)
    (outline-minor-mode 1)))

(add-hook 'find-file-hooks #'qtmstr-outline-find-file-hook)

(defun qtmstr-outline-on-header-click (e)
  (interactive "e")
  (let* ((position (nth 1 e))
         (window (nth 0 position))
         (pos (nth 1 position)))

    (with-selected-window window
      (goto-char pos)
      (qtmstr-outline-on-header-return)
      (goto-char pos))))

(defun qtmstr-outline-fringe-click (e)
  (interactive "e")
  (mouse-set-point e)
  (let* ((position (nth 1 e))
         (window (nth 0 position))
         (text-pos (nth 5 position)))

    (with-selected-window window
      (goto-char text-pos)
      (when (outline-on-heading-p)
        (qtmstr-outline-on-header-return))
      ))
  )

(defun qtmstr-outline-on-header-return ()
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (outline-toggle-children)))

(defvar qtmstr-outline-header-map
  (progn (let ((km (make-sparse-keymap)))
           (define-key km [mouse-1] #'qtmstr-outline-on-header-click)
;;           (define-key km "\r"      #'qtmstr-outline-on-header-return)
           (define-key km [mouse-3] #'show-subtree)
           (define-key km (kbd "C-c TAB") #'outline-toggle-children)
           km)))
(fset 'qtmstr-outline-header-map qtmstr-outline-header-map)

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'qtmstr-fringe-icon-expanded [0 0 0 126 126 0 0 0])
  (define-fringe-bitmap 'qtmstr-fringe-icon-collaped [0 24 24 126 126 24 24 0]))

(defvar qtmstr-outline-overlay-open
  (propertize " "
              'display '(if window-system
                          '(left-fringe qtmstr-fringe-icon-expanded)
                          '((margin left-margin) "-")))
  "String displayed before a header line when it is open")

(defvar qtmstr-outline-overlay-closed
  (propertize " "
              'display '(if window-system
                            '(left-fringe qtmstr-fringe-icon-collaped)
                          '((margin left-margin) "+")))
  "String displayed before a header line when it is closed")

(defun qtmstr-outline-fixup-overlay (o)
  "Set overlay style and behavior correctly depending on whether
its children are shown or hidden. Overlay's bounds must be
correct. Shoud not be called when buffer is narrowed."
  (assert (save-excursion (goto-char (overlay-start o))
                          (outline-on-heading-p t)))
  (let ((end (overlay-end o)))
    (cond ((outline-invisible-p end)
           (overlay-put o 'help-echo "mouse-1: open this outline node")
           (overlay-put o 'before-string qtmstr-outline-overlay-closed))
          (t
           (overlay-put o 'help-echo "mouse-1: close this outline node")
           (overlay-put o 'before-string qtmstr-outline-overlay-open)))))

(defface qtmstr-outline-header-face
  '((t :slant italic :underline t))
  "Face used for outline headings")

(defun qtmstr-outline-add-overlay-at-point ()
  "Assuming point is at the beginning of an outline heading,
add an overlay for this heading."
  (assert (outline-on-heading-p t))
  (let ((o (make-overlay (point) (point-at-eol) nil t)))
    (overlay-put o 'qtmstr-outline t)
    (overlay-put o 'evaporate t)
    (overlay-put o 'face 'qtmstr-outline-header-face)
    (overlay-put o 'mouse-face '(qtmstr-outline-header-face highlight))
    (overlay-put o 'pointer 'hand)
    (overlay-put o 'keymap 'qtmstr-outline-header-map)
    (overlay-put o 'follow-link t)
    (qtmstr-outline-fixup-overlay o)))

(defun qtmstr-outline-after-change (beg end len)
  (save-match-data
    (save-excursion
      (save-restriction
        (widen)
        (goto-char end)
        (end-of-line)
        (setq end (point))
        (goto-char beg)
        (forward-line 0)
        (while (< (point) end)
          (remove-overlays (point) (point-at-eol) 'qtmstr-outline t)
          (when (looking-at outline-regexp)
            (qtmstr-outline-add-overlay-at-point))
          (forward-line 1))))))

(defun qtmstr-outline-view-change ()
  (save-restriction
    (widen)
    (dolist (o (overlays-in (point-min) (point-max)))
      (when (overlay-get o 'qtmstr-outline)
        (qtmstr-outline-fixup-overlay o)))))

(defun qtmstr-outline-update-margin ()
  (if qtmstr-outline-mode
      (set-window-margins (selected-window) 2)
    (set-window-margins (selected-window) nil)))

(defun qtmstr-outline-add-overlays ()
  "Add overlays for outline headings"
  (interactive)
  (when (not window-system)
    (if (car (window-margins (selected-window)))
        (message "Warning: `qtmstr-outline' needs to use left-margin, but currenly it seems to be already used (`linum-mode' on?).\n
        Anyway `qtmstr-outline' will continue to turn on."))
    (add-hook 'window-configuration-change-hook
              'qtmstr-outline-update-margin nil 'local)
    (qtmstr-outline-update-margin))
  
  (add-hook 'after-change-functions #'qtmstr-outline-after-change t t)
  (add-hook 'post-command-hook #'qtmstr-outline-post-command-hook t t)
  (add-hook 'outline-view-change-hook #'qtmstr-outline-view-change t t)

  (let (overlay (re (concat "^\\(" outline-regexp ".*\\)$")))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (save-excursion
            (goto-char (match-beginning 0))
            (qtmstr-outline-add-overlay-at-point)))))))

(defun qtmstr-outline-remove-overlays ()
  (interactive)
  (remove-hook 'window-configuration-change-hook
               'qtmstr-outline-update-margin 'local)
  (qtmstr-outline-update-margin)

  (remove-hook 'after-change-functions #'qtmstr-outline-after-change t)
  (remove-hook 'post-command-hook #'qtmstr-outline-post-command-hook t)
  (save-restriction
    (widen)
    (remove-overlays nil nil 'qtmstr-outline t)))

(define-minor-mode qtmstr-outline-mode
  "Add left-fringe +/- icons and line overlays for outline-sections."
  nil
  " qo"
  :require 'outline
  :group 'outline
  (if qtmstr-outline-mode
      ;;turn on
      (progn
        (unless outline-minor-mode
          (outline-minor-mode t))
        (qtmstr-outline-add-overlays)
        ;;(unless (and (boundp 'hs-minor-mode) hs-minor-mode)
        ;; ;; avoid key conflictingve with hideshowvis
        ;; (local-set-key [left-fringe mouse-1] 'qtmstr-outline-fringe-click))
        )
    (qtmstr-outline-remove-overlays)))

(defun qtmstr-outline-mode-hook ()
  (if outline-minor-mode
      (progn
        (qtmstr-outline-add-overlays))

    (qtmstr-outline-remove-overlays)))

;;(add-hook 'outline-minor-mode-hook #'qtmstr-outline-mode-hook)

(provide 'qtmstr-outline)


;; Local Variables:
;; outline-regexp: ";;; "
;; coding: utf-8
;; End:

;;; qtmstr-outline ends here
