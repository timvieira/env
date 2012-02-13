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

;; speed-dial
(global-unset-key [f3])
(global-set-key [f3] '(lambda() (interactive) (set-buffer (find-file "~/.emacs"))))
(global-unset-key [f2])
(global-set-key [f2] '(lambda() (interactive) (set-buffer (find-file "~/Dropbox/todo/todo.org"))))
(global-unset-key [f4])
(global-set-key [f4] '(lambda() (interactive) (set-buffer (find-file "~/.bashrc"))))

; What to do if visiting a symbolic link to a file under version control.
(setq vc-follow-symlinks t)

(defun add-path (p)
  (add-to-list 'load-path (concat (expand-file-name "~/projects/env/emacs-support/") p)))

(add-path "")
(add-path "site-lisp/pylint.el")
(add-path "site-lisp")
(add-path "site-lisp/scala-mode")
(add-path "site-lisp/protobuf-mode.el")
(add-path "site-lisp/writegood-mode.el")
(add-path "site-lisp/org-7.8.03/lisp")

;; recent files list
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; Common lisp
(require 'cl)
(require 'ido)
(require 'parenface)
(require 'dired+)
(require 'filecache)
(require 'protobuf-mode)
(require 'writegood-mode)
;;(require 'cython-mode)    ; we also have a simple cython-mode

(require 'scala-mode-auto)
(add-hook 'scala-mode-hook
            '(lambda ()
               (scala-mode-feature-electric-mode)
               ))


;; maximize screen real estate
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)     ; hide menu-bar
(line-number-mode 1)   ; show line number near mode=line
(column-number-mode 1) ; show column number near mode-line

(defun my-window-placement ()
  (interactive)
  (if (window-system)
    ;; this function is a bit quirky; hence you'll see some seemingly redundant code below
    (progn
      ;; the following lines work *after* initialization
      (set-frame-height (selected-frame) 50)
      (set-frame-width (selected-frame) 120)
      ;; works during initialization
      (add-to-list 'default-frame-alist '(height . 50))
      (add-to-list 'default-frame-alist '(width . 120))
      (set-frame-position (selected-frame) 615 0))))

(custom-set-variables
 '(current-language-environment "Latin-1")
 '(default-input-method "latin-1-prefix")
 '(global-font-lock-mode t nil (font-lock))
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters (quote (("test-filters" ((or (filename . "perl") (mode . dired-mode)))) ("gnus" ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode)))) ("programming" ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode) (mode . java-mode) (mode . idl-mode) (mode . lisp-mode)))))))
 '(icomplete-mode nil nil (icomplete))
 '(inhibit-startup-screen t)
 '(mouse-wheel-mode t nil (mwheel))
 '(show-paren-mode t nil (paren))
 '(transient-mark-mode t)
;; '(hippie-expand-try-functions-list (quote (try-complete-file-name-partially
;;                                            try-complete-file-name
;;                                            try-expand-all-abbrevs
;;                                            try-expand-list
;;                                            try-expand-line
;;                                            try-expand-dabbrev
;;                                            try-expand-dabbrev-all-buffers
;;                                            try-expand-dabbrev-from-kill
;;                                            try-complete-lisp-symbol-partially
;;                                            try-complete-lisp-symbol)))
 '(truncate-lines t)
 '(visible-cursor nil)
 '(cursor-in-nonselected-windows nil)
)

(defun dark-colors ()
  (interactive)
  (custom-set-faces
   '(default ((t (:stipple nil :background "black" :foreground "white" :inverse-video nil
                  :box nil :strike-through nil :overline nil :underline nil :slant normal
                  :weight normal  :width normal :foundry "bitstream"
                  :family "Bitstream Vera Sans Mono"
                  ))))
   '(bold ((t (:weight extra-bold))))
   '(comint-highlight-prompt ((t (:foreground "light blue"))))
   '(compilation-info ((((class color) (min-colors 16) (background light)) (:foreground "gray" :weight bold))))
   '(flymake-errline ((((class color)) (:underline "red"))))
   '(flymake-warnline ((((class color)) (:underline "yellow4"))))
   '(font-lock-builtin-face ((((class color) (min-colors 88) (background dark)) (:foreground "Purple2"))))
   '(font-lock-comment-face ((t (:foreground "red" :slant italic))))
   '(font-lock-keyword-face ((t (:foreground "orange"))))
   '(font-lock-string-face ((t (:foreground "forest green"))))

   '(font-lock-function-name-face ((t (:foreground "blue"))))
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

(defun light-colors ()
  (interactive)
  (custom-set-faces

   '(default ((t (:stipple
                  nil :background "white" :foreground "black" :inverse-video
                  nil :box nil :strike-through nil :overline nil :underline
                  nil :slant normal :weight normal :width
                  normal :foundry "bitstream" :family "Bitstream Vera Sans Mono"))))

   '(mode-line ((t (:background "blue" :foreground "white" :weight normal))))
   '(mode-line-inactive ((default (:inherit mode-line)) (nil (:background "grey" :foreground "blue"))))
   '(minibuffer-prompt ((t (:foreground "black"))))
   '(font-lock-keyword-face ((t (:foreground "orange3"))))
   '(font-lock-function-name-face ((t (:foreground "royalblue"))))
   '(font-lock-type-face ((t (:foreground "royalblue"))))
  )
  (my-window-placement)
)

(dark-colors)
;(light-colors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(defvar c-tab-always-indent nil)
(setq c-tab-always-indent t)

(setq-default indent-tabs-mode nil)  ; No tabs! XXX: why does this need to be set with `setq-default` not `setq`

(setq tab-width 4)                    ; XXX: might want to consider changing this back to 2...

(setq default-major-mode 'text-mode  ; Make text-mode the default mode for new buffers.
      ;; case-fold-search t            ; case-insensitive search
      case-fold-search nil           ; (!) if this is non-nil hippie-expand will be busted.
      ;; read-file-name-completion-ignore-case t
      ;; completion-ignore-case t
      cursor-in-non-selected-windows nil  ; Don't show a cursor in other windows
      mouse-yank-at-point t               ; mouse yank at point, not click!
)

; require newline at end of file
;(setq require-final-newline t)

;; Column width (used in longlines-mode)
(setq-default auto-fill-mode 1
              fill-column 80)

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


;(defun revert-buffer-and-refind-position ()
;  (interactive)
;  (let ((p (point-marker)))
;    (revert-buffer t t t)
;    (goto-char p)))

(defun my-keys ()

  ;; disable things which often confuse me
  (global-unset-key [(control next)])
  (global-unset-key [(control ?z)])
  (global-unset-key [(control ?x) (control ?z)])
  (global-unset-key [mouse-2])  ; disable middle-click paste

  ;; Copy-Cut-Paste from clipboard
  (global-set-key (kbd "s-x") 'clipboard-kill-region)    ; cut
  (global-set-key (kbd "s-c") 'clipboard-kill-ring-save) ; copy
  (global-set-key (kbd "s-v") 'clipboard-yank)           ; paste

  (global-set-key "\C-b" 'goto-matching-paren)
  (global-set-key (kbd "s-q") '(lambda () (interactive) (whitespace-cleanup) (message "whitespace-cleanup")))

  (global-set-key [f7] 'flyspell-start)

  (defun custom-kill-current-buffer ()
    (interactive)
    ;; get around the annoying "Active processes exist" query
    (if (get-buffer-process (current-buffer))
        (process-kill-without-query (get-buffer-process (current-buffer))))
    ;; the usual behavior
    (kill-buffer (current-buffer)))

  (global-unset-key "\M-k")
  (global-set-key "\M-k" 'custom-kill-current-buffer)

  ;; hippie-expand M-/
  (global-unset-key [(meta ?/)])
  (global-set-key [(meta ?/)] 'hippie-expand)

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
;  (cond
;   ((boundp 'minibuffer-local-filename-completion-map)
;    (define-key minibuffer-local-filename-completion-map [(?\ )] 'minibuffer-complete)))
)

(my-keys)


(defun common-setup()
  (interactive)

  ;; Append to python path just-in-case env is not initialize by ~/.bashrc
  (setenv "PYTHONPATH"
          (concat (getenv "PYTHONPATH") ".:~/projects:~/projects/extras/python:~/projects/incubator"))

  (global-font-lock-mode t)
  (load-library "my-emisc")
  (load-library "my-python-config")

  (defun no-X-setup () nil)

  (defun X-setup ()
    (server-start)
    (set-mouse-color "black")
    (my-window-placement)
    (mouse-avoidance-mode)
  )

  (if (window-system) (X-setup) (no-X-setup))

  ;(add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))
  (add-to-list 'auto-mode-alist '("\\.pyx$" . cython-mode))

  (load-library "matlab")

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


(setq dired-use-ls-dired nil)
(setq c-basic-offset 2)

;(defun flip-to-last-buffer (&optional n)
;  (interactive "p")
;  (switch-to-buffer (car (cdr (buffer-list)))))

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

(defun flyspell-start ()
  (interactive)
  (flyspell-buffer)
  (flyspell-mode))

(defun run-pdflatex (file-name)
  (if (= 1 (shell-command (concat "pdflatex -halt-on-error " file-name)))
      (message "pdflatex failed")
    (progn
      (message "pdflatex succeeded")
      (delete-other-windows))))

(defun run-bibtex (f)
  (if (= 1 (shell-command (concat "bibtex " f)))
      (message "bibtex failed")
    (progn
      (message "bibtex succeeded")
      (delete-other-windows))))

;; Hanna's LaTeX Makefile template:
;;   echo "Running latex...."
;;   pdflatex -halt-on-error $(FILENAME).tex
;;   echo "Running bibtex...."
;;   bibtex $(FILENAME)
;;   echo "Rerunning latex...."
;;   pdflatex -halt-on-error $(FILENAME).tex
;;   pdflatex -halt-on-error $(FILENAME).tex  # run twice so that we get refs

(defun latex-thing ()
  (interactive)
  (let ((tex (buffer-file-name)))
    (let ((base (substring tex 0 -4)))  ; filename with out extension
      (let ((pdf (concat base ".pdf")))
        (let ((bib (concat base ".bib")))
          (if (file-exists-p pdf) (delete-file pdf))  ; delete old pdf
          (run-pdflatex tex)
          (if (file-exists-p bib)
              (progn
                (run-bibtex base)
                ;; rerun latex twice -- bibtex is weird like that
                (run-pdflatex tex)
                (run-pdflatex tex)))
   )))))

(defun latex-open-this-pdf ()
  (interactive)
  (let ((tex (buffer-file-name)))
    (let ((base (substring tex 0 -4)))  ; filename with out extension
      (let ((pdf (concat base ".pdf")))

        (if (= 1 (shell-command (concat "nohup evince " pdf " 2>/dev/null >/dev/null &")))
            (message "failed to open pdf")
          (progn
            (message "sucessfully opened pdf")
            (delete-other-windows)))

        ))))

(defun latex-setup ()
  (interactive)
  (local-unset-key "\C-c\C-c")
  (local-set-key "\C-c\C-c" 'latex-thing)

  (local-unset-key "\C-e\C-e")
  (local-set-key "\C-e\C-e" 'latex-open-this-pdf)

  (flyspell-start)
  ;(longlines-mode t)

  )

(add-hook 'latex-mode-hook
          'latex-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun change-indent (w)
  (interactive "nWidth: ")
  (set-variable 'c-basic-offset w))


;(defun open-shell ()
;  "open a gnome-terminal in cwd"
;  (interactive)
;  (shell-command (concat "gnome-terminal --working-directory " (shell-command-to-string "pwd") " &"))
;)

;(defun my-add-path (path-element)
;  "Add the specified path element to the Emacs PATH"
;  (interactive "DEnter directory to be added to path: ")
;  (if (file-directory-p path-element)
;      (setenv "PATH"
;              (concat (expand-file-name path-element)
;                      path-separator (getenv "PATH")))))

(defun sudo-edit ()
  (interactive)
  (let ((p (point)))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))
    (goto-char p)))

(font-lock-add-keywords nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):" 1 font-lock-warning-face t)))


(add-hook 'org-mode-hook
          '(lambda ()
             (org-indent-mode t)   ;; #+STARTUP: indent
             (org-babel-do-load-languages 'org-babel-load-languages '((python . t)))
             ;(flyspell-start)
             ;(longlines-mode t)
             ))


(defun ascii-fy ()
  (interactive)
  (replace-string " " " ")
  (replace-string "’" "'")
  (replace-string "“" "\"")
  (replace-string "”" "\"")
  (replace-string "—" "-")
  (replace-string "–" "-")
  (replace-string "ﬂ" "fl")
  (replace-string "ﬁ" "fi")
  (replace-string "•" "*")
  (replace-string "…" "...")
  (replace-string "à" "a")         ; lossy
  (replace-string "α" "\\alpha")   ; tex-fy
  (replace-string "→" "->"))


(defun get-shell ()
  "Spawn a gnome-terminal in CWD."
  (interactive)
  (shell-command "nohup gnome-terminal >& /dev/null &" )
  (delete-other-windows))
