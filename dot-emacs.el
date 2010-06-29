;; Common lisp
(require 'cl)

;; maximize screen real estate
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)     ; hide menu-bar
(line-number-mode 1)   ; show line number near mode=line
;(linum-mode 1)         ; show line numbers on the side

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(current-language-environment "Latin-1")
 '(default-input-method "latin-1-prefix")
 '(global-font-lock-mode t nil (font-lock))
 '(hippie-expand-try-functions-list (quote (try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters (quote (("test-filters" ((or (filename . "perl") (mode . dired-mode)))) ("gnus" ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode)))) ("programming" ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode) (mode . java-mode) (mode . idl-mode) (mode . lisp-mode)))))))
 '(icomplete-mode nil nil (icomplete))
 '(inhibit-startup-screen t)
 '(mouse-wheel-mode t nil (mwheel))
 '(show-paren-mode t nil (paren))
 '(transient-mark-mode t)
 '(truncate-lines t)
)


(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :foundry "bitstream" :family "Bitstream Vera Sans Mono"))))
 '(bold ((t (:weight extra-bold))))
 '(comint-highlight-prompt ((t (:foreground "light blue"))))
 '(compilation-info ((((class color) (min-colors 16) (background light)) (:foreground "gray" :weight bold))))
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow"))))
 '(font-lock-builtin-face ((((class color) (min-colors 88) (background dark)) (:foreground "Purple2"))))
 '(font-lock-comment-face ((t (:foreground "red3" :slant italic))))
 '(font-lock-function-name-face ((t (:foreground "blue"))))
 '(font-lock-keyword-face ((t (:foreground "orange"))))
 '(font-lock-string-face ((t (:foreground "green4"))))
 '(font-lock-type-face ((t (:foreground "blue"))))
 '(italic ((t (:foreground "Yellow1" :slant italic))))
 '(match ((((class color) (min-colors 88) (background light)) (:foreground "red"))))
 '(minibuffer-prompt ((t (:foreground "white"))))
 '(mode-line ((t (:background "blue" :foreground "white" :weight normal))))
 '(mode-line-inactive ((default (:inherit mode-line)) (nil (:background "grey" :foreground "blue"))))
 '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "purple"))))
 '(rst-level-1-face ((t (:background "darkgreen" :weight extra-bold))) t)
 '(rst-level-2-face ((t (:background "darkgreen"))) t)
 '(rst-level-3-face ((t (:background "darkgreen"))) t)
 '(rst-level-4-face ((t (:background "darkgreen"))) t)
)


(partial-completion-mode)

;; XXX: add this to my-python-config
;(font-lock-add-keywords
; 'python-mode '(("\\([\\+\\-]\\)?\\b\\([0-9]*\\.?[0-9]+\\)\\(e[0-9]+\\)?" . 'font-lock-constant-face)))


(defun my-window-placement ()
  (interactive)
  (if (window-system)
    (progn
      (set-frame-height (selected-frame) 82)
      (set-frame-width (selected-frame) 120)
      (set-frame-position (selected-frame) 420 0))))
(my-window-placement)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(column-number-mode 1) ; show column number near mode-line

(defvar c-tab-always-indent nil)

(setq-default indent-tabs-mode nil) ; No tabs! XXX: why does this need to be set with `setq-default` not `setq`

(setq
 tab-width 4                   ; XXX: might want to consider changing this back to 2...
 default-major-mode 'text-mode ; Make text-mode the default mode for new buffers.

; case-fold-search t            ; case-insensitive search
 case-fold-search nil           ; (!) if this is non-nil hippie-expand will be busted.
; read-file-name-completion-ignore-case t
; completion-ignore-case t
 cursor-in-non-selected-windows nil  ; Don't show a cursor in other windows
 mouse-yank-at-point t               ; mouse yank at point, not click!
)

(setq-default
 ;; Column width (used in longlines-mode)
 auto-fill-mode 1
 fill-column 100
)

;; Note: I think the smooth-scrolling cannot be on for these settings to take effect
; scroll-margin: line where scrolling should start;
; scroll-conservatively: how far the cursor is allowed to be center when scrolling starts
; scroll-preserve-screen-position: maintain screen position when you hit Page(Up|Down)
(setq
 scroll-margin 0                   
 scroll-conservatively 100000 
 scroll-preserve-screen-position 1 
)


(require 'ido)
(setq
  ido-case-fold  t                    ; be case-insensitive
  ido-enable-last-directory-history t ; remember last used dirs
  ido-use-filename-at-point nil       ; don't use filename at point (annoying)
  ido-use-url-at-point nil            ; don't use url at point (annoying)
  ido-enable-flex-matching nil        ; don't try to be too smart
  ido-confirm-unique-completion t     ; wait for RET, even with unique completion
  confirm-nonexistent-file-or-buffer t
)
(ido-mode t)



;; typed text replaces a selection, rather than append
(pending-delete-mode nil)

;; get rid of yes-or-no questions - y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

; What to do if visiting a symbolic link to a file under version control.
(setq vc-follow-symlinks t)

;; Emacs-related stuff under ~/emacs
(setq
 semantic-load-turn-useful-things-on t
 semanticdb-default-save-directory "~/.emacs.d/semantic/" ; put semantic.cache files somewhere far away.
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun revert-buffer-and-refind-position ()
  (interactive)
  (let ((p (point-marker)))
    (revert-buffer t t t)
    (goto-char p)))

(defun my-keys()
  ;; trying to define an alternate prefix keychord
  ;; (global-set-key (kbd "C-#")
  ;;                 (lookup-key
  ;;                  (lookup-key global-map (kbd "C-x"))
  ;;                  (kbd "r")))

  (global-unset-key [f5])
  (global-set-key [f5] 'goto-line)  ;; also M-g g
  (global-set-key [f12] 'revert-buffer-and-refind-position)
  (global-set-key "\C-x\C-k" 'kill-region)
  (global-set-key "\C-c\C-k" 'clipboard-kill-region)
  (global-set-key "\C-b" 'goto-matching-paren)

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

  ;(global-set-key [f5] 'mode-compile)

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


;; I either want to load up a heavyweight emacs if I am on my local machine
;;   and my slag heap o' elisp is available, or, if I am launching from
;;   a remote machine via terminal, I just want a useful subset
(defconst *home-emacs-support* (expand-file-name "~/emacs-support/"))
(defconst *emacs-root*
  (cond
;   ((file-directory-p *sendak-emacs-support*) *sendak-emacs-support*)
   ((file-directory-p *home-emacs-support*) *home-emacs-support*)
   (t nil)))

(defconst *full-elisp-available* (not (null *emacs-root*)))


(defun setup-paths ()
  (interactive)
  (cond
   (*full-elisp-available*
    (labels ((add-path (p) (add-to-list 'load-path (concat *emacs-root* p))))
      ;;(add-path "site-lisp/python")
      (add-path "site-lisp/pylint.el")
      ;(add-path "site-lisp/pymacs.el")
      (add-path "lisp")      ;; my elisp code
      (add-path "site-lisp") ;; stuff found elsewhere
      ;; org-mode stuff
      (add-path "site-lisp/org-mode")
      (add-path "site-lisp/org-mode-contrib/lisp")
      (add-path "site-lisp/org-mode-contrib/packages/org-export-freemind-0.1.0")
      (add-path "site-lisp/remember")
      ;;(add-path "site-lisp/slime-2.0")
      (add-path "site-lisp/scala-mode")
      (add-path "site-lisp/protobuf-mode.el")
      (add-path "site-lisp/nxml-mode-20041004")
      ;;(add-path "site-lisp/ruby")
      (add-path "site-lisp/color-theme-6.6.0")
      (add-path "site-lisp/cc-mode")
      (add-path "site-lisp/cedet-1.0pre4/common")
      ;;(add-path "site-lisp/ecb-2.32")
      (add-path "site-lisp/elib-1.0")
      (add-path "site-lisp/jde-2.3.5.1/lisp") ;; Java IDE support
      ))
   (t nil)))


(defun scala-mode-setup ()
  (interactive)
  (require 'scala-mode-auto)
  (add-hook 'scala-mode-hook
            '(lambda () (yas/minor-mode-on))))

;;(defun gnuplot-mode-setup()
;;  (interactive)
;;  (autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
;;  (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)
;;  (setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))
;;  (global-set-key [(f9)] 'gnuplot-make-buffer)
;;  )

;;(defun org-mode-setup ()
;;  (interactive)
;;  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;;  (global-set-key "\C-cl" 'org-store-link)
;;  (global-set-key "\C-ca" 'org-agenda)
;;  (setq org-agenda-include-diary t)
;;  (require 'remember)
;;  (setq remember-annotation-functions '(org-remember-annotation))
;;  (setq remember-handler-functions '(org-remember-handler))
;;  (add-hook 'remember-mode-hook 'org-remember-apply-template))


(defun common-setup()
  (interactive)
  (setup-paths)

  ;(require 'python-mode)
  (require 'parenface)
  (require 'dired+)
  (require 'filecache)
  ;(require 'smooth-scrolling)   ; this is a little annoying
  
  (global-font-lock-mode t)
  (cond
   (*full-elisp-available*
    (scala-mode-setup)
    ;;(org-mode-setup)
    ;;(load "ani-fcsh.el")
    ;;(setup-actionscript)
    ;;(load-library "perl-config") ;; extra support for perl coding
    (load-library "my-emisc")
    ;(autoload 'nxml-mode "nxml-mode" "Edit XML documents" t)
    ;(setq auto-mode-alist (cons '("\\.\\(xml\\|xsl\\|mxml\\|rng\\|xhtml\\)\\'" . nxml-mode) auto-mode-alist))
    )
   (t nil)))

(defun x-setup ()
  (interactive)
  (common-setup)
  (require 'cedet)
  (require 'yaml-mode)
  (require 'ecmascript-mode)
  (require 'protobuf-mode)
  ;;(require 'cython-mode)    ; we also have a simple cython-mode
  ;;(require 'ecb)
  ;;(defalias 'perl-mode 'cperl-mode)
  ; (setup-slime)  ;; uncomment for gnuserv
  ;(load-library "my-color-theme")
  ;(require 'color-theme-autoloads)
  ;(color-theme-initialize)
  ;;(setq cperl-mode-hook 'my-cperl-customizations)
  ;;(load-library "my-java-config")
  (load-library "my-python-config")

  ;; Load saved keyboard macros:
  (load-file (concat *emacs-root* "kbd-macros.el"))
  (server-start)
  (set-mouse-color "black")
  (add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.[aj]s$" . ecmascript-mode))
  (add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))
  (add-to-list 'auto-mode-alist '("\\.pyx$" . cython-mode))
;  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
)

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

(defun nox-setup ()
  (common-setup)
  (term-colors))

(defun term-colors ()
  (interactive)
  (custom-set-faces
    '(font-lock-string-face ((t (:foreground "darkgreen"))))
    '(font-lock-comment-face ((t (:foreground "darkred" :slant italic))))
    '(mode-line ((t (:background "blue" :foreground "white" :weight bold))))
    '(mode-line-inactive ((default (:inherit mode-line)) (nil (:background "lightblue" :foreground "white"))))))

(defun light-colors ()
  (interactive)
  (custom-set-faces
   '(default ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :foundry "bitstream" :family "Bitstream Vera Sans Mono"))))
   '(mode-line ((t (:background "blue" :foreground "white" :weight normal))))
   '(mode-line-inactive ((default (:inherit mode-line)) (nil (:background "grey" :foreground "blue"))))
   '(minibuffer-prompt ((t (:foreground "black"))))
   '(help-argument-name ((t (:foreground "orange"))))
   )
)


(defvar *init*)
(setq *init*
      (cond ((eq window-system nil)        ;; Running in a terminal:
             #'(lambda () (nox-setup)))
            (t #'(lambda () (x-setup)))))  ;; Running under X
(funcall *init*)

(setq dired-use-ls-dired nil)
(setq c-basic-offset 2)

(defun flip-to-last-buffer (&optional n)
  (interactive "p")
  (switch-to-buffer (car (cdr (buffer-list)))))

;; Load the default-dir.el package which installs fancy handling of
;; the initial contents in the minibuffer when reading file names.
(condition-case nil
    (require 'default-dir)
  (error nil))

;; Enable some default-disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;; ???: from nicholas lara's stuff...
;; Better middle mouse button interaction
;; (setq x-select-enable-clipboard t)
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
;; Default window navigation bindings
;; (windmove-default-keybindings)
;; Shell colors
;; (ansi-color-for-comint-mode-on)

(setq find-file-compare-truenames t
      minibuffer-confirm-incomplete t
      minibuffer-max-depth nil)

(setq browse-url-browser-function '(("." . browse-url-firefox)))

(setq dired-no-confirm '(byte-compile chgrp chmod chown compress copy delete hardlink load move print
                                      shell symlink uncompress recursive-delete kill-file-buffer
                                      kill-dired-buffer patch create-top-dir revert-subdirs))

(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/autosaves/"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)


;(autoload 'pymacs-apply "pymacs")
;(autoload 'pymacs-call "pymacs")
;(autoload 'pymacs-eval "pymacs" nil t)
;(autoload 'pymacs-exec "pymacs" nil t)
;(autoload 'pymacs-load "pymacs" nil t)
;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))


;; ;; TIM: IPython integration
;; (require 'ipython)
;; (require 'python-mode)
;; (require 'pymacs)
;;
;; (setq py-python-command-args '( "-colors" "Linux"))
;; (pymacs-load "ropemacs" "rope-")
;;
;; ;;(defadvice py-execute-buffer (around python-keep-focus activate)
;; ;;  "return focus to python code buffer"
;; ;;  (save-excursion ad-do-it))
;;
;; (defadvice py-execute-buffer (around python-keep-focus activate)
;;   "Thie advice to make focus python source code after execute command py-execute-buffer."
;;   (let ((remember-window (selected-window))
;;         (remember-point (point)))
;;     ad-do-it
;;     (select-window remember-window)
;;     (goto-char remember-point)))
;;
;; (defun rgr/python-execute()
;;   (interactive)
;;   (if mark-active
;;     (py-execute-string (buffer-substring-no-properties (region-beginning) (region-end)))
;;     (py-execute-buffer)))
;;
;; (global-set-key (kbd "C-c C-e") 'rgr/python-execute)
;;
;; (add-hook 'python-mode-hook
;;           '(lambda () (eldoc-mode 1)) t)
;;
;; ;; IPython code completion
;; (setq ipython-completion-command-string
;;       "print(';'.join(__IP.Completer.all_completions('%s')))\n")
;;
;; (provide 'python-programming)


;; Keyboard macros:
;;  1. start-kbd-macro
;;  2. end-kbd-macro
;;  3. name-last-kbd-macro
;; (fset 'latex-compile-and-open
;;    [?\M-x ?s ?h ?e ?l ?l ?  ?c ?o ?m ?  return ?p 
;;     backspace ?l ?a ?t ?e ?x ?p ?d ?f backspace backspace 
;;     backspace backspace backspace backspace backspace backspace 
;;     ?p ?d ?f ?l ?a ?t ?e ?x ?  ?a ?c ?l ?- ?i ?j tab ?t tab return
;;     ?\C-x ?1 ?\C-x ?\C-f ?a ?c ?l ?- tab ?p ?d ?f return])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My LaTeX stuff
(defun latex-thing ()
  (interactive)
  (let ((pdf-file (concat (substring (buffer-file-name) 0 -4) ".pdf")))
    (if (file-exists-p pdf-file)
        (delete-file pdf-file))
    ;(tex-validate-buffer) ; check buffer for paragraphs containing mismatched $'s or braces.
    (shell-command (concat "pdflatex " (buffer-file-name)))
    ;; !  ==> Fatal error occurred, no output PDF file produced!
    ;;(find-file pdf-file)
    (shell-command (concat "evince " pdf-file " &")))
  (delete-other-windows))

(add-hook 'latex-mode-hook
          '(lambda ()
             (flyspell-mode t)
             (flyspell-buffer)
             (longlines-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-kbd-macro-file ()
  (interactive)
  (set-buffer (find-file (concat *emacs-root* "kbd-macros.el"))))

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

