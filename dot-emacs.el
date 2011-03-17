;; Notes
;; =======
;;
;; Bookmarks 
;; ---------
;;  `C-x r m <somekey>` adds a bookmark, 
;;  `C-x r b <somekey>` visits that bookmark, 
;;  `C-x r l` lists bookmarks.
;;
;; Keyboard macros
;; ---------------
;;  1. start-kbd-macro
;;  2. end-kbd-macro
;;  3. name-last-kbd-macro
;; (fset 'latex-compile-and-open
;;    [?\M-x ?s ?h ?e ?l ?l ?  ?c ?o ?m ?  return ?p
;;     backspace ?l ?a ?t ?e ?x ?p ?d ?f backspace backspace
;;     backspace backspace backspace backspace backspace backspace
;;     ?p ?d ?f ?l ?a ?t ?e ?x ?  ?a ?c ?l ?- ?i ?j tab ?t tab return
;;     ?\C-x ?1 ?\C-x ?\C-f ?a ?c ?l ?- tab ?p ?d ?f return])

(defconst *emacs-root* (expand-file-name "~/emacs-support/"))

(defun add-path (p) 
  (add-to-list 'load-path (concat *emacs-root* p)))

(defun setup-paths ()
  (interactive)
  (add-path "")
  (add-path "site-lisp/pylint.el")
  (add-path "site-lisp")
  (add-path "site-lisp/scala-mode")
  (add-path "site-lisp/protobuf-mode.el"))

(setup-paths)

;; Common lisp
(require 'cl)
(require 'ido)
(require 'scala-mode-auto)
(require 'parenface)
(require 'dired+)
(require 'filecache)
(require 'protobuf-mode)
;;(require 'cython-mode)    ; we also have a simple cython-mode


;; maximize screen real estate
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)     ; hide menu-bar
(line-number-mode 1)   ; show line number near mode=line
;(linum-mode 0)         ; show line numbers on the side
(column-number-mode 1) ; show column number near mode-line

(defun my-window-placement ()
  (interactive)
  (if (window-system)
    (progn
      (set-frame-height (selected-frame) 82)
      (set-frame-width (selected-frame) 120)
      (set-frame-position (selected-frame) 420 0))))
(my-window-placement)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(current-language-environment "Latin-1")
 '(default-input-method "latin-1-prefix")
 '(global-font-lock-mode t nil (font-lock))
; '(hippie-expand-try-functions-list (quote (try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters (quote (("test-filters" ((or (filename . "perl") (mode . dired-mode)))) ("gnus" ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode)))) ("programming" ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode) (mode . java-mode) (mode . idl-mode) (mode . lisp-mode)))))))
 '(icomplete-mode nil nil (icomplete))
 '(inhibit-startup-screen t)
 '(mouse-wheel-mode t nil (mwheel))
 '(show-paren-mode t nil (paren))
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(visible-cursor nil)
 '(cursor-in-nonselected-windows nil)
)

(mouse-avoidance-mode)

(defun dark-colors ()
  (interactive)
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:stipple nil :background "black" :foreground "white" :inverse-video nil
                  :box nil :strike-through nil :overline nil :underline nil :slant normal
                  :weight normal :height 80 :width normal :foundry "bitstream"
                  :family "Bitstream Vera Sans Mono"))))
   '(bold ((t (:weight extra-bold))))
   '(comint-highlight-prompt ((t (:foreground "light blue"))))
   '(compilation-info ((((class color) (min-colors 16) (background light)) (:foreground "gray" :weight bold))))
   '(flymake-errline ((((class color)) (:underline "red"))))
   '(flymake-warnline ((((class color)) (:underline "yellow4"))))
   '(font-lock-builtin-face ((((class color) (min-colors 88) (background dark)) (:foreground "Purple2"))))
   '(font-lock-comment-face ((t (:foreground "red" :slant italic))))  ; red3
   '(font-lock-function-name-face ((t (:foreground "blue1"))))
   '(font-lock-keyword-face ((t (:foreground "orange"))))
   '(font-lock-string-face ((t (:foreground "forest green"))))  ; green4
   '(font-lock-type-face ((t (:foreground "blue"))))
   '(italic ((t (:foreground "Yellow1" :slant italic))))
   '(match ((((class color) (min-colors 88) (background light)) (:foreground "red"))))
   '(minibuffer-prompt ((t (:foreground "white"))))
   '(mode-line ((t (:background "blue" :foreground "white" :weight normal))))
   '(mode-line-inactive ((default (:inherit mode-line)) (nil (:background "grey" :foreground "blue"))))
   '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "purple"))))
  )
  (my-window-placement)
)

(dark-colors)

(defun light-colors ()
  (interactive)
  (custom-set-faces
   '(default ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :foundry "bitstream" :family "Bitstream Vera Sans Mono"))))
   '(mode-line ((t (:background "blue" :foreground "white" :weight normal))))
   '(mode-line-inactive ((default (:inherit mode-line)) (nil (:background "grey" :foreground "blue"))))
   '(minibuffer-prompt ((t (:foreground "black"))))
   '(font-lock-keyword-face ((t (:foreground "orange3"))))
   '(font-lock-function-name-face ((t (:foreground "royalblue"))))
   '(font-lock-type-face ((t (:foreground "royalblue"))))
  )
  (my-window-placement)
)

;; XXX: add this to my-python-config
;(font-lock-add-keywords
; 'python-mode '(("\\([\\+\\-]\\)?\\b\\([0-9]*\\.?[0-9]+\\)\\(e[0-9]+\\)?" . 'font-lock-constant-face)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar c-tab-always-indent nil)

(setq-default indent-tabs-mode nil)  ; No tabs! XXX: why does this need to be set with `setq-default` not `setq`

(setq tab-width 4                    ; XXX: might want to consider changing this back to 2...
      default-major-mode 'text-mode  ; Make text-mode the default mode for new buffers.
      ;; case-fold-search t            ; case-insensitive search
      case-fold-search nil           ; (!) if this is non-nil hippie-expand will be busted.
      ;; read-file-name-completion-ignore-case t
      ;; completion-ignore-case t
      cursor-in-non-selected-windows nil  ; Don't show a cursor in other windows
      mouse-yank-at-point t               ; mouse yank at point, not click!
)

;; Column width (used in longlines-mode)
(setq-default auto-fill-mode 1
              fill-column 80
)

;; Note: I think the smooth-scrolling cannot be on for these settings to take effect
; scroll-margin: line where scrolling should start;
; scroll-conservatively: how far the cursor is allowed to be center when scrolling starts
; scroll-preserve-screen-position: maintain screen position when you hit Page(Up|Down)
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
)

;; ido settings
(setq ido-case-fold  t                     ; be case-insensitive
      ido-enable-last-directory-history  t ; remember last used dirs
      ido-use-filename-at-point nil        ; don't use filename at point (annoying)
      ido-use-url-at-point nil             ; don't use url at point (annoying)
      ido-enable-flex-matching nil         ; don't try to be too smart
      ido-confirm-unique-completion t      ; wait for RET, even with unique completion
      confirm-nonexistent-file-or-buffer t
)

(ido-mode t)

;;(partial-completion-mode)

;; typed text replaces a selection, rather than append
(pending-delete-mode nil)

;; get rid of yes-or-no questions - y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

; What to do if visiting a symbolic link to a file under version control.
(setq vc-follow-symlinks t)


;; put semantic.cache files somewhere far away.
(setq semantic-load-turn-useful-things-on t
      semanticdb-default-save-directory "~/.emacs.d/semantic/"
)

;; put emacs backup files into their own directory
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/autosaves/"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun revert-buffer-and-refind-position ()
  (interactive)
  (let ((p (point-marker)))
    (revert-buffer t t t)
    (goto-char p)))

(defun my-keys ()
  ;; trying to define an alternate prefix keychord
  ;; (global-set-key (kbd "C-#")
  ;;                 (lookup-key
  ;;                  (lookup-key global-map (kbd "C-x"))
  ;;                  (kbd "r")))

  (global-unset-key [mouse-2])  ; no more accidental pastes from middle-click
  ;(global-set-key [mouse-2] '(lambda () (interactive) (message "Middle-click pasting disabled!")))

  ;; Copy-Cut-Paste from clipboard with Super-C Super-X Super-V
  (global-set-key (kbd "s-x") 'clipboard-kill-region)    ; cut
  (global-set-key (kbd "s-c") 'clipboard-kill-ring-save) ; copy
  (global-set-key (kbd "s-v") 'clipboard-yank)           ; paste

  (global-set-key [f12] 'revert-buffer-and-refind-position)
  (global-set-key "\C-x\C-k" 'kill-region)
  (global-set-key "\C-c\C-k" 'clipboard-kill-region)
  (global-set-key "\C-b" 'goto-matching-paren)


  (defun custom-kill-current-buffer ()
    (interactive)
    ;; get around the annoying "Active processes exist" query 
    (if (get-buffer-process (current-buffer))
        (process-kill-without-query (get-buffer-process (current-buffer))))
    ;; the usual behavior
    (kill-buffer (current-buffer)))

  (global-unset-key "\M-k")
  (global-set-key "\M-k" 'custom-kill-current-buffer)

  ;; set other ways to bring up M-x the "extended command"
  ;;(global-set-key "\C-x\C-m" 'execute-extended-command)
  ;;(global-set-key "\C-c\C-m" 'execute-extended-command)
  ;;(global-set-key "\C-cm"    'execute-extended-command)
  ;;(global-set-key "\C-xm"    'execute-extended-command)

  ;; weird function to indent to where the parser think things should go...
  (global-set-key [(control ?i)] 'my-indent-function)

  ;; F3 opens .emacs file
  (global-set-key [f3] '(lambda() (interactive) (set-buffer (find-file "~/.emacs"))))

  ;; hippie-expand M-/
  (global-unset-key [(meta ?/)])
  (global-set-key [(meta ?/)] 'hippie-expand)

  (global-unset-key [(control next)])
  (global-unset-key [(control ?z)])

  (global-unset-key [(control ?x) (control ?z)])

  (global-set-key [(kp-subtract)] 'kill-current-buffer)
  (global-set-key [(shift kp-subtract)] 'kill-current-buffer-and-window)
  (global-set-key [(control kp-subtract)] 'delete-window)
  ; (define-key help-mode-map [(kp-subtract)] 'kill-current-buffer)
  ; (define-key help-mode-map [(shift kp-subtract)] 'kill-current-buffer-and-window)
  ;; splitting and deleting windows
  (global-set-key [(control ?1)] 'delete-window)
  (global-set-key [(control ?2)] 'split-window-horizontally)
  (global-set-key [(control ?3)] 'split-window-vertically)
  (global-set-key [(control ?!)] 'delete-other-windows)
  ;; looking at diffs
  (global-set-key [(control ?4)] 'ediff-buffers)
  ;; toggle between interaction and non-interactive modes
  (global-set-key [(control ?\))] '(lambda() (interactive) (other-window 1)))
  (global-set-key [(control ?\()] '(lambda() (interactive) (other-window -1)))
  ;; ido...
  (global-set-key [(control ?7)] 'ido-switch-buffer)
  ;; cycle thru buffers
  (global-set-key [(control ?8)] 'ibuffer)
  (global-set-key [(control ?9)] 'bs-cycle-previous)
  (global-set-key [(control ?0)] 'bs-cycle-next)
  ;; reinstate the older space-completion for files
  (cond
   ((boundp 'minibuffer-local-filename-completion-map)
    (define-key minibuffer-local-filename-completion-map [(?\ )] 'minibuffer-complete))))

(my-keys)


(defun my-indent-function ()
  (interactive)
  (cond ((not mark-active)
         (indent-according-to-mode))
        (t
         (indent-region (point-marker) (mark-marker)))))

(defun scala-mode-setup ()
  (interactive)
  (add-hook 'scala-mode-hook
            '(lambda () (yas/minor-mode-on))))


(defun common-setup()
  (interactive)
  (setup-paths)

  ;; Append to python path just-in-case env is not initialize by ~/.bashrc
  (setenv "PYTHONPATH"
          (concat (getenv "PYTHONPATH") ".:~/projects:~/projects/python-extras"))


  (global-font-lock-mode t)
  (scala-mode-setup)
  ;;(load "ani-fcsh.el")
  ;;(setup-actionscript)
  ;;(load-library "perl-config") ;; extra support for perl coding
  (load-library "my-emisc")

  ;;(defalias 'perl-mode 'cperl-mode)
  ;;(setq cperl-mode-hook 'my-cperl-customizations)
  ;;(load-library "my-java-config")
  (load-library "my-python-config")

  (server-start)
  (set-mouse-color "black")
  (add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))
  (add-to-list 'auto-mode-alist '("\\.pyx$" . cython-mode))
)

(common-setup)

;; Simple cython-mode:
(define-derived-mode cython-mode python-mode "Cython"
  (font-lock-add-keywords
   nil
   `((,(concat "\\<\\(NULL"
               "\\|c\\(def\\|import\\|har\\|typedef\\)"
               "\\|e\\(num\\|xtern\\)"
               "\\|float\\|double\\|unsigned"
               "\\|in\\(clude\\|t\\|line\\)"
               "\\|object\\|public\\|struct\\|type\\|union\\|void"
               "\\)\\>")
      1 font-lock-keyword-face t))))


;(defvar *init*)
;(setq *init*
;      (cond ((eq window-system nil)        ;; Running in a terminal:
;             #'(lambda () (nox-setup)))
;            (t #'(lambda () (x-setup)))))  ;; Running under X
;(funcall *init*)


(setq dired-use-ls-dired nil)
(setq c-basic-offset 2)

(defun flip-to-last-buffer (&optional n)
  (interactive "p")
  (switch-to-buffer (car (cdr (buffer-list)))))

;; Enable some default-disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; set the webbrowser to use when we click on a link.
;(setq browse-url-browser-function '(("." . browse-url-firefox)))
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; ---------------------------------------------------------------------------------------
;; timv: not sure what these things do...

(setq find-file-compare-truenames t
      minibuffer-confirm-incomplete t
      minibuffer-max-depth nil)

(setq dired-no-confirm '(byte-compile chgrp chmod chown compress copy delete hardlink load move print
                                      shell symlink uncompress recursive-delete kill-file-buffer
                                      kill-dired-buffer patch create-top-dir revert-subdirs))

;; ---------------------------------------------------------------------------------------


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My LaTeX stuff
(defun latex-thing ()
  (interactive)
  (let ((pdf-file (concat (substring (buffer-file-name) 0 -4) ".pdf")))
    (if (file-exists-p pdf-file)
        (delete-file pdf-file))
    ;(tex-validate-buffer) ; check buffer for paragraphs containing mismatched $'s or braces.
    (shell-command (concat "pdflatex " (buffer-file-name)))
    ;; TODO 
    ;; * if "Fatal error occurred, no output PDF file produced!" don't open evince
    ;;   possible solutions might check if pdf-file was create via (find-file pdf-file)
    ;; * i don't like that i get an y/n question if envince is still running
    (shell-command (concat "evince " pdf-file " &"))
    ;(set-buffer (find-file pdf-file))   ; to open in emacs use this line
    (delete-other-windows)))

;(setq tex-command "pdftex")
;(set-variable (quote tex-dvi-view-command) "evince")
;(setq latex-run-command "pdflatex")

(add-hook 'latex-mode-hook
          '(lambda ()
             (local-unset-key "\C-c\C-c")
             (local-set-key "\C-c\C-c" 'latex-thing)
             (flyspell-mode t)
             (flyspell-buffer)
             (longlines-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fullscreen ()
  "make the emacs window fullscreen"
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(defun goto-matching-paren ()
  "If point is sitting on a parenthetic character, jump to its match."
  (interactive)
  (cond ((looking-at "\\s\(") (forward-list 1))
        ((progn
           (backward-char 1)
           (looking-at "\\s\)")) (forward-char 1) (backward-list 1))))

;; When turning on flyspell-mode, automatically check the entire buffer.
;(defadvice flyspell-mode (after advice-flyspell-check-buffer-on-start activate) (flyspell-buffer))

(defun change-indent (w)
  (interactive "nWidth: ")
  (set-variable 'c-basic-offset w))


;; Word count!
(defun word-count (&optional filename)
  "Returns the word count of the current buffer.  If `filename' is not nil, returns the word count of that file."
  (interactive)
  (save-some-buffers) ;; Make sure the current buffer is saved
  (let ((tempfile nil))
    (if (null filename)
        (progn
          (let ((buffer-file (buffer-file-name))
                (lcase-file (downcase (buffer-file-name))))
            (if (and (>= (length lcase-file) 4) (string= (substring lcase-file -4 nil) ".tex"))
                ;; This is a LaTeX document, so DeTeX it!
                (progn
                  (setq filename (make-temp-file "wordcount"))
                  (shell-command-to-string (concat "detex < " buffer-file " > " filename))
                  (setq tempfile t))
              (setq filename buffer-file)))))
    (let ((result (car (split-string (shell-command-to-string (concat "wc -w " filename)) " "))))
      (if tempfile
          (delete-file filename))
      (message (concat "Word Count: " result))
      )))

;(defun my-add-path (path-element)
;  "Add the specified path element to the Emacs PATH"
;  (interactive "DEnter directory to be added to path: ")
;  (if (file-directory-p path-element)
;      (setenv "PATH"
;              (concat (expand-file-name path-element)
;                      path-separator (getenv "PATH")))))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(font-lock-add-keywords nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):" 1 font-lock-warning-face t)))

;; CSS
;(setq cssm-indent-level 2)
;(setq cssm-newline-before-closing-bracket t)
;(setq cssm-indent-function #'cssm-c-style-indenter)
;(setq cssm-mirror-mode nil)

