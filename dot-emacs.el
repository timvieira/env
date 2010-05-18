

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
 '(hippie-expand-try-functions-list (quote (try-expand-all-abbrevs try-expand-list try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill)))
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters (quote (("test-filters" ((or (filename . "perl") (mode . dired-mode)))) ("gnus" ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode)))) ("programming" ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode) (mode . java-mode) (mode . idl-mode) (mode . lisp-mode)))))))
 '(icomplete-mode nil nil (icomplete))
 '(inhibit-startup-screen t)
 '(mouse-wheel-mode t nil (mwheel))
 '(show-paren-mode t nil (paren))
 '(transient-mark-mode t)
 '(truncate-lines t))



(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :family "Bitstream Vera Sans Mono Roman"))))
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
 '(rst-level-4-face ((t (:background "darkgreen"))) t))

;; '(w3m-anchor-face ((((class color) (background light)) (:foreground "yellow"))))
;; '(w3m-arrived-anchor-face ((((class color) (background light)) (:foreground "yellow3"))))
;; '(w3m-header-line-location-content-face ((((class color) (background light)) (:box (:line-width 2 :color "grey75" :style released-button)))))
;; '(w3m-header-line-location-title-face ((((class color) (background light)) (:box (:line-width 2 :color "grey75" :style released-button))))))
;;
;; '(diredp-dir-priv ((t (:foreground "DarkRed"))))
;; '(diredp-exec-priv ((t (:foreground "green1"))))
;; '(diredp-file-name ((t (:foreground "LightBlue"))))
;; '(diredp-file-suffix ((t nil)))
;; '(diredp-flag-mark ((t (:foreground "Yellow"))))
;; '(diredp-flag-mark-line ((t (:foreground "yellow"))))
;; '(diredp-no-priv ((t nil)))
;; '(diredp-other-priv ((t nil)))
;; '(diredp-read-priv ((t nil)))
;; '(diredp-write-priv ((t (:foreground "green3"))))
;;
;; '(ediff-current-diff-A ((((class color) (min-colors 16)) (:background "grey50" :foreground "yellow"))))
;; '(ediff-current-diff-B ((((class color) (min-colors 16)) (:background "grey50" :foreground "yellow"))))
;; '(ediff-even-diff-A ((((class color) (min-colors 16)) (:foreground "yellow2"))))
;; '(ediff-even-diff-B ((((class color) (min-colors 16)) (:foreground "yellow2"))))
;; '(ediff-fine-diff-A ((((class color) (min-colors 16)) (:background "grey40" :foreground "red2"))))
;; '(ediff-fine-diff-B ((((class color) (min-colors 16)) (:background "grey40" :foreground "red2"))))
;; '(ediff-odd-diff-A ((((class color) (min-colors 16)) (:foreground "yellow2"))))
;; '(ediff-odd-diff-B ((((class color) (min-colors 16)) (:foreground "yellow2"))))



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
(setq scroll-step 1)   ; line-by-line scrolling

;; Column width (used in longlines-mode)
(setq auto-fill-mode 1)
(setq-default fill-column 100)

;; No tabs, always use spaces.
(setq-default tab-width 4)            ;; XXX: might want to consider changing this back to 2...
;(setq tab-width 4)
(setq-default indent-tabs-mode nil)


;;(setq-default case-fold-search nil) ; case-sensitive search
(setq-default case-fold-search t)  ; case-insensitive search

;; Make text-mode the default mode for new buffers.
(defvar default-major-mode 'text-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Common lisp
(require 'cl)

;; Emacs-related stuff under ~/emacs
(setq semantic-load-turn-useful-things-on t)
(setq semanticdb-default-save-directory "~/.emacs.d/semantic/")   ; put semantic.cache files somewhere far away.
(partial-completion-mode)



(setq browse-url-browser-function
      '(;; regex . browser-function
        ("." . browse-url-firefox)))

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

  ;; filesets definitions
  ;;(global-set-key [(control f6)] '(lambda() (interactive) (set-buffer (find-file (concat *emacs-root* "lisp/filesets-defs.el")))))
  ;;(global-set-key [f6] 'filesets-open)
  ;;(global-set-key [(meta f6)] 'filesets-close)

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
  ;(require 'picture) (global-set-key [(shift right)] 'picture-forward-column)
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
(defconst *sendak-emacs-support* (expand-file-name "~/emacs-support/"))
(defconst *home-emacs-support* (expand-file-name "~/emacs/"))
(defconst *emacs-root*
  (cond
   ((file-directory-p *sendak-emacs-support*) *sendak-emacs-support*)
   ((file-directory-p *home-emacs-support*) *home-emacs-support*)
   (t nil)))

(defconst *full-elisp-available* (not (null *emacs-root*)))


(defun setup-paths ()
  (interactive)
  (cond
   (*full-elisp-available*
    (labels ((add-path (p) (add-to-list 'load-path (concat *emacs-root* p))))
      (add-path "lisp")      ;; my elisp code
      (add-path "site-lisp") ;; stuff found elsewhere
      ;; org-mode stuff
      (add-path "site-lisp/org-mode")
      (add-path "site-lisp/org-mode-contrib/lisp")
      (add-path "site-lisp/org-mode-contrib/packages/org-export-freemind-0.1.0")
      (add-path "site-lisp/remember")
      ;;(add-path "site-lisp/slime-2.0")
      (add-path "site-lisp/scala-mode")
      (add-path "site-lisp/python-mode-1.0")
      (add-path "site-lisp/protobuf-mode.el")
      (add-path "site-lisp/nxml-mode-20041004")
      ;;(add-path "site-lisp/ruby")
      ;;(add-path "site-lisp/filesets")
      (add-path "site-lisp/color-theme-6.6.0")
      (add-path "site-lisp/cc-mode")
      (add-path "site-lisp/cedet-1.0pre4/common")
      ;;(add-path "site-lisp/ecb-2.32")
      (add-path "site-lisp/elib-1.0")
      (add-path "site-lisp/jde-2.3.5.1/lisp") ;; Java IDE support
      (add-path "site-lisp/pymacs.el")
      ;; TIM: IPython itegration (python-mode, pymacs, ipython, ropemacs)
      (add-path "site-lisp/ipython.el")
      (add-path "site-lisp/ropemacs-0.6")
      ))
   (t nil)))


(defun scala-mode-setup ()
  (interactive)
  (require 'scala-mode-auto)
  (load (concat *emacs-root* "site-lisp/sbt-compile.el"))
  (add-hook 'scala-mode-hook
            '(lambda () (yas/minor-mode-on))))

;;(defun filesets-setup()
;;  (load-library "filesets+") ;;
;;  (filesets-init)
;;  (load-library "filesets-defs")
;;  ; (require 'filesets-tellib)
;;  ; (require 'filesets-emacs)
;;  ; (filesets-install t)
;;  ; (filesets-support-ibuffer)
;;  ; (filesets-support-dired)
;;  )

(defun org-mode-setup ()
  (interactive)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (setq org-agenda-include-diary t)
  (require 'remember)
  (setq remember-annotation-functions '(org-remember-annotation))
  (setq remember-handler-functions '(org-remember-handler))
  (add-hook 'remember-mode-hook 'org-remember-apply-template))

;;(defun gnuplot-mode-setup()
;;  (interactive)
;;  (autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
;;  (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)
;;  (setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))
;;  (global-set-key [(f9)] 'gnuplot-make-buffer)
;;  )

(defun common-setup()
  (interactive)
  (setup-paths)
  (global-font-lock-mode t)
  ;; (setq-default abbrev-mode t)
  (cond
   (*full-elisp-available*
    (require 'smooth-scrolling)
    (require 'parenface)
    (require 'dired+)
    (require 'filecache)
    (require 'ido)
    (scala-mode-setup)
    (org-mode-setup)
    ;;(load "ani-fcsh.el")
    ;;(setup-actionscript)
    (ido-mode t)
    ;;(filesets-setup)
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
  ;;(require 'ecb)
  (x-colors)
  ;;(defalias 'perl-mode 'cperl-mode)
)

(defun x-colors ()
  (load-library "my-color-theme")
  (require 'color-theme-autoloads)
  (color-theme-initialize)
  ;;(setq cperl-mode-hook 'my-cperl-customizations)
  (load-library "my-python-config")
  ;;(load-library "my-java-config")
  ;; Load saved keyboard macros:
  (load-file (concat *emacs-root* "kbd-macros.el"))
  ; (setup-slime)  ;; uncomment for gnuserv
  (server-start)
  (set-mouse-color "black")
  ;; (read-abbrev-file (concat *emacs-root* "abbrev_defs.el"))
  ;; (setq save-abbrevs nil)
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
  (require 'ecmascript-mode)
  (add-to-list 'auto-mode-alist '("\\.[aj]s$" . ecmascript-mode))
  (require 'protobuf-mode)
  (add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))
  ;;(require 'cython-mode)
  ;; add the simple cython-mode
  (add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
)


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
  (custom-set-faces
    '(font-lock-string-face ((t (:foreground "lightgreen"))))
    '(font-lock-comment-face ((t (:foreground "lightblue2" :slant italic))))
    '(mode-line ((t (:background "blue" :foreground "white" :weight bold))))
    '(mode-line-inactive ((default (:inherit mode-line)) (nil (:background "lightblue" :foreground "white"))))))


(defvar *init*)
(setq *init*
      (cond ((eq window-system nil)        ;; Running in a terminal:
             #'(lambda () (nox-setup)))
            (t #'(lambda () (x-setup)))))  ;; Running under X
(funcall *init*)

(setq dired-use-ls-dired nil)
(setq c-basic-offset 2)
(defvar c-tab-always-indent nil)

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


;; Turns off audible bell
; (setq-default visible-bell t)

;; ???: from nicholas lara's stuff...
;; Better middle mouse button interaction
;; (setq x-select-enable-clipboard t)
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
;; Turn off bugging yes-or-no-p
;; (fset 'yes-or-no-p 'y-or-n-p)
;; Default window navigation bindings
;; (windmove-default-keybindings)
;; Shell colors
;; (ansi-color-for-comint-mode-on)

;; mouse avoidance: get that mouse out of my way!
;;(mouse-avoidance-mode "animate")



;;;;;;;
;;;
(setq
  find-file-compare-truenames t
  minibuffer-confirm-incomplete t
  minibuffer-max-depth nil)


;; pending-delete-mode causes typed text to replace a selection, rather than append
;; When Delete Selection mode is enabled, Transient Mark mode is also enabled and typed
;; text replaces the selection if the selection is active. Otherwise, typed text is just
;; inserted at point regardless of any selection
(pending-delete-mode 1)


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
;;    [?\M-x ?s ?h ?e ?l ?l ?  ?c ?o ?m ?  return ?p backspace ?l ?a ?t ?e ?x ?p ?d ?f backspace backspace backspace backspace backspace backspace backspace backspace ?p ?d ?f ?l ?a ?t ?e ?x ?  ?a ?c ?l ?- ?i ?j tab ?t tab return ?\C-x ?1 ?\C-x ?\C-f ?a ?c ?l ?- tab ?p ?d ?f return])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my LaTeX stuff
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


;; (defun reload-dot-emacs ()
;;   "Reload .emacs on the fly"
;;   (interactive)
;;   (if (bufferp (get-file-buffer ".emacs"))
;;       (save-buffer (get-buffer ".emacs")))
;;   (load-file "~/.emacs")
;;   (message ".emacs reloaded successfully"))

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


(defun change-java-indent (w)
  (interactive "nWidth: ")
  (set-variable 'c-basic-offset w))
