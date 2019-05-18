;; Emacs dyna-mode
;; $Id: dyna-mode.el 2481 2005-05-04 04:36:51Z jason $
;;
;; Installation:
;;
;; Put this dyna-mode.el file into a folder where Emacs finds it, e.g.,
;;    /usr/share/emacs/site-lisp/
;;
;; Alternatively, tell Emacs where to find this file by adding a line to
;; the .emacs file in your home directory:
;;    (add-to-list 'load-path "/directory/that/contains/this/file")
;;
;; Then, add the following lines to the .emacs file in your home directory:
;;
;;   (autoload 'dyna-mode "dyna-mode" "Major mode for editing Dyna programs." t)
;;   (add-hook 'dyna-mode-hook 'turn-on-font-lock)  ; if you want syntax highlighting
;;   (add-to-list 'auto-mode-alist '("\\.dyna[^.]*$" . dyna-mode))
;;
;; You can change the colors of the syntax highlighting with M-x customize-face <enter> <enter>
;; Then, in the customize-face buffer, search for the Font Lock and Dyna Style faces and
;; change them.

(require 'font-lock)
(require 'generic)

;; ----------------------------------------------------------------------
;; CUSTOMIZATION
;; ----------------------------------------------------------------------

(defgroup dyna nil
  "Major mode for editing Dyna programs"
  :group 'languages)

(defcustom dyna-indent-width 2
  "Level of indentation in Dyna buffers."
  :type 'integer
  :group 'dyna)

(defcustom dyna-mode-hook nil
  "Hook run when entering Dyna mode."
  :type 'hook
  :group 'dyna)

;; ----------------------------------------------------------------------
;; CREATE NEW MODE
;; ----------------------------------------------------------------------

;; keymap
(defvar dyna-mode-map nil)
(if dyna-mode-map
    ()       ; careful not to change it if it's already set up
  (setq dyna-mode-map (make-sparse-keymap))
  (define-key dyna-mode-map "\C-c\C-c" 'compile)
)

;; various options
(defun dyna-mode-setup ()    ; called by dyna-mode
  "Set up Dyna mode variables and functions."
  (use-local-map dyna-mode-map)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'dyna-indent-line)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "\\s<+ *")
  (show-paren-mode 1)

  ;; If there is no makefile in this directory, change the
  ;; initial compile command to just run dynac on this single
  ;; dyna file.
  ;; NOTE: If you don't like being prompted to edit the command,
  ;; set compilation-read-command to nil.

  (unless (or (file-exists-p "makefile")
              (file-exists-p "Makefile"))
    (set (make-local-variable 'compile-command)
         (concat "dynac "
                 (file-name-nondirectory buffer-file-name))))

  (run-hooks 'dyna-mode-hook)
)

;; a face we'll use
(make-face 'dyna-binarization-face)
(set-face-foreground 'dyna-binarization-face "gray")

;; define the new mode, with various syntax highlighting options
(define-generic-mode 'dyna-mode

  ;; comment symbol
  (list "%")

  ;; keywords to highlight
;  (list "bool" "string" "term" "int" "double" "if" "whenever" "true" "false")
  (list )

  ;; additional expressions to highlight
  '(

    ;; highlight text between ' and '
;    ("\\('[^']*'\\)"            1 'font-lock-string-face)

;;    ("[- \t]\\(query\\|item\\|retractable\\|iterable\\|operator\\|cname\\|retractable\\|pragma\\|structure\\|trainable\\|foreign\\|computed\\|gradient\\|union\\)"
;;     1 'font-lock-keyword-face)

    ;; highlight <tags>
    ;; ("\\(<[^>]*>\\)"         1 'dyna-style3-face)

    ;; highlight everything directly between two parens
    ;; (paren content that contains other brackets is NOT highlighted)
    ;; ("(\\([^()]+\\))"         1 'font-lock-type-face)

    ;; operators +, =, *
    ("\\(\\(log\\+\\|argm[ai][nx]\\|m[ai][nx]\\|[=:!\\|\\+\\-\\*]\\)?=\\)"         1 'font-lock-keyword-face)
    ("\\([\\+\\-\\*\\&]\\)"     1 'font-lock-type-face)
    ("\\(\\.\\)[ \t]*$"         1 'font-lock-type-face)
    ("\\(for\\|unless\\)"       1 'font-lock-keyword-face)
    ("^[ \t]*\\(:-\\)"          1 'font-lock-type-face)

    ;; ("\\(\\(^\\|=\\|*\\)[ \t]*[A-Za-z_]+[ \t]*\\)\\((\\|[+*-]\\)"         1 'font-lock-variable-name-face)
    ("[\[, (]\\([A-Z0-9_]['A-Za-z0-9_]*[ \t]*\\)"       1 'font-lock-variable-name-face)
    ("\\([a-z][A-Za-z0-9_]*[ \t]*\\)[ \t]*\\([,() ]\\)" 1 'font-lock-function-name-face)

    )

  ;; filenames that trigger this mode
  ;; (adds or replaces this mode's entry in auto-mode-alist)
  '("\\.dyna[^.]*$")

  ;; what to do when we start this mode up
  (list 'dyna-mode-setup)

  "Major mode for editing Dyna programs.

\\{dyna-mode-map}")


;; ----------------------------------------------------------------------
;; INDENTATION (used by the above)
;; ----------------------------------------------------------------------

(defun dyna-indent-line ()
  "Indent current line as Dyna code."
  (interactive)
  (let ((indent (dyna-calculate-indent))
        (pos (- (point-max) (point)))
        beg)
    (beginning-of-line)
    (setq beg (point))
    (dyna-front-of-line)
    (if (zerop (- indent (current-column)))
        nil
      (delete-region beg (point))
      (indent-to indent))
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))))

(defun dyna-calculate-indent ()
  "Compute dyna indentation level.
If the last non-empty-line ends with a period, return 0.
If the last non-empty-line ends with =, return DYNA-INDENT-WIDTH.
If there is an unmatched open parenthesis, return 1+ its position.
Otherwise, return the indentation of the previous line."
  (save-excursion
    (beginning-of-line)
    (if (or (save-excursion (dyna-previous-rule-end))
            (save-excursion (dyna-preprocessor-directive-p)))
        0
      (let ((open-paren-col (dyna-column-of-open-paren-in-rule)))
        (if (null open-paren-col)
            dyna-indent-width
          (1+ open-paren-col))))))

(defun dyna-front-of-line ()
  "Go the first non-whitespace character on the current line."
  (beginning-of-line)
  (skip-chars-forward " \t"))

(defun dyna-preprocessor-directive-p ()
  "Is this line a preprocessor directive?"
  (dyna-front-of-line)
  (looking-at "[#]"))

(defun dyna-previous-non-empty-line ()
  "Go to the previous non-{empty/comment} line."
  (if (not (zerop (forward-line -1)))
      nil
    (if (progn (dyna-front-of-line)
               (not (looking-at "\\s<\\|\\#\\|\n")))
        t
      (dyna-previous-non-empty-line))))

(defun dyna-end-of-clause ()
  "Go to the end of the current line, to the left of any comments."
  (beginning-of-line 1)
  (let ((eolpos (save-excursion (end-of-line) (point))))
    (if (re-search-forward comment-start-skip eolpos 'move)
        (goto-char (match-beginning 0)))
    (skip-chars-backward " \t")))

(defun dyna-previous-rule-end ()
  "Go to the end of the previous line, and check if it is looking at a
period or it is the beginning of the buffer."
  (if (not (dyna-previous-non-empty-line))
      t ;; beginning of buffer is equivalent to a rule end
    (dyna-end-of-clause)
    (save-excursion
      (forward-char -1)
      (looking-at "[.]"))))

(defun dyna-column-of-open-paren-in-rule ()
  "Return the position of the last unclosed parenthesis in the current
rule, or NIL if none exists."
  (save-excursion
    (let ((close-parens 0)
          (found-column nil))
      (while (and (null found-column)
                  (not (dyna-previous-rule-end)))
        ;; Look for parens.  If close parens, increase count.  If
        ;; open parens, decrease count.  But if count is already zero,
        ;; return (current-column).
        (let ((bol (save-excursion (beginning-of-line) (point))))
          (while (and (null found-column)
                      (re-search-backward "\\s(\\|\\s)" bol 'move))
            (if (looking-at "\\s)")
                (setq close-parens (1+ close-parens))
              (if (zerop close-parens)
                  (setq found-column (current-column))
                (setq close-parens (1- close-parens)))))))
      found-column)))

(provide 'dyna-mode)
