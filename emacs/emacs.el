;; -*- coding: utf-8 -*-
;;
;; Notes
;; =====
;;
;; Bookmarks:
;;  `C-x r m <somekey>` adds a bookmark,
;;  `C-x r b <somekey>` visits that bookmark,
;;  `C-x r l` lists bookmarks.
;;
;; Keyboard macros:
;;  1. start-kbd-macro
;;  2. end-kbd-macro
;;  3. name-last-kbd-macro
;;     (fset 'latex-compile-and-open
;;        [?\M-x ?s ?h ?e ?l ?l ?  ?c ?o ?m ?  return ?p
;;         backspace ?l ?a ?t ?e ?x ?p ?d ?f backspace backspace
;;         backspace backspace backspace backspace backspace backspace
;;         ?p ?d ?f ?l ?a ?t ?e ?x ?  ?a ?c ?l ?- ?i ?j tab ?t tab return
;;         ?\C-x ?1 ?\C-x ?\C-f ?a ?c ?l ?- tab ?p ?d ?f return])
;;
;; Personal dictionary: ~/.aspell.en.pws or ~/projects/env/aspell.en.psw
;;
;; TODO
;; ====
;;
;;  - LEARN ctrl-alt-{k,f,b} to jump around parenthesized regions (these are
;;    alternatives to ctrl-alt-{left and right arrows})
;;

;; TODO: easily search google for word-at-point or phrase-in-region

;; speed-dial
(global-unset-key [f3])
(global-set-key [f3] '(lambda() (interactive) (set-buffer (find-file "~/.emacs.el"))))
(global-unset-key [f2])
(global-set-key [f2] '(lambda() (interactive) (set-buffer (find-file "~/Dropbox/todo/todo.org"))))
(global-unset-key [f4])
(global-set-key [f4] '(lambda() (interactive) (set-buffer (find-file "~/.bashrc"))))
(global-unset-key [f6])
(global-set-key [f6] '(lambda() (interactive) (set-buffer (find-file "~/projects/notes/thesis/NOTES.org"))))

; What to do if visiting a symbolic link to a file under version control.
(setq vc-follow-symlinks t)


;; Now install el-get at the very first
;(add-to-list 'load-path "~/.emacs.d/el-get/el-get")


;(unless (require 'el-get nil 'noerror)
;  (with-current-buffer
;      (url-retrieve-synchronously
;       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
;    (let (el-get-master-branch
;          ;; do not build recipes from emacswiki due to poor quality and
;          ;; documentation
;          el-get-install-skip-emacswiki-recipes)
;      (goto-char (point-max))
;      (eval-print-last-sexp)))
;  ;; build melpa packages for el-get
;  (el-get-install 'package)
;  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;                           ("melpa" . "http://melpa.org/packages/")))
;  (el-get-elpa-build-local-recipes))
;

;; highlight URLs in comments/strings
;(add-hook 'find-file-hooks 'goto-address-prog-mode)  ;; todo: remove? does this even work?

(defun add-path (p)
  (add-to-list 'load-path (concat (expand-file-name "~/projects/env/emacs/") p)))

(add-path "")
;(add-path "site-lisp/package.el")
(add-path "site-lisp")
;(add-path "site-lisp/writegood-mode.el")
;(add-path "site-lisp/haskell-mode")


;(load-file "site-lisp/emacs-grammarly.el")
;(global-set-key (kbd "C-c C-g") 'grammarly-save-region-and-run)


;; Note: Install packages early. In some cases late installation is buggy
;; (apparently the case with org-mode stuff -- must install before we load
;; customizations)
(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  
(unless package-archive-contents    ;; Refresh the packages descriptions
  (package-refresh-contents))

(setq package-load-list '(all))     ;; List of packages to load
;(unless (package-installed-p 'scala-mode2)
;  (package-refresh-contents) (package-install 'scala-mode2))
(unless (package-installed-p 'org)
  (package-refresh-contents) (package-install 'org))
;(unless (package-installed-p 'org-plus-contrib)
;  (package-refresh-contents) (package-install 'org-plus-contrib))
;(unless (package-installed-p 'color-theme)
;  (package-refresh-contents) (package-install 'color-theme))
(unless (package-installed-p 'color-theme-sanityinc-tomorrow)
  (package-refresh-contents) (package-install 'color-theme-sanityinc-tomorrow))
   
(unless (package-installed-p 'cython-mode)
  (package-refresh-contents) (package-install 'cython-mode))
(unless (package-installed-p 'flycheck-cython)
  (package-refresh-contents) (package-install 'flycheck-cython))
(unless (package-installed-p 'pylint)
  (package-refresh-contents) (package-install 'pylint))

(require 'cython-mode)
;(require 'flycheck-cython)
;(add-hook 'cython-mode-hook 'flycheck-mode)

(add-path "site-lisp/dyna-mode.el")
(autoload 'dyna-mode "dyna-mode" "Major mode for editing Dyna programs." t)
(add-hook 'dyna-mode-hook 'turn-on-font-lock)  ; if you want syntax highlighting
(add-to-list 'auto-mode-alist '("\\.dyna[^.]*$" . dyna-mode))


;; auctex
;(setq TeX-auto-save t)
;(setq TeX-parse-self t)
;(setq-default TeX-master nil)

;; TODO: configure anything.el
;;  - http://emacs-fu.blogspot.com/2011/09/finding-just-about-anything.html
;;  - http://www.emacswiki.org/emacs/Anything
;;  - http://metasandwich.com/2010/07/30/what-can-i-get-for-10-dolla-anything-el/

;(add-path "site-lisp/anything-config.el")
;(add-path "site-lisp/anything-match-plugin.el")
;(require 'anything-match-plugin)
;(require 'anything-config)


;;(defun load-rudel ()
;;  (interactive)
;;  ;; Do these in a shell in ~/src:
;;  ;; cvs -d:pserver:anonymous@cedet.cvs.sourceforge.net:/cvsroot/cedet login
;;  ;; cvs -z3 -d:pserver:anonymous@cedet.cvs.sourceforge.net:/cvsroot/cedet co -P cedet
;;  ;; cd cedet && make && cd ..
;;  ;; svn co https://rudel.svn.sourceforge.net/svnroot/rudel/rudel/trunk rudel
;;
;;  (add-path "site-lisp/rudel-0.2-4")
;;  (add-path "site-lisp/rudel-0.2-4/obby")
;;  (add-path "site-lisp/rudel-0.2-4/zeroconf")
;;
;;  (load-file "/home/timv/projects/env/emacs/site-lisp/rudel-0.2-4/rudel-loaddefs.el")
;;  ;  This will set Rudel up to be loaded on demand when one of the
;;  ;  commands `rudel-join-session', `rudel-host-session' or
;;  ;  `global-rudel-minor-mode' is invoked.
;;
;;  (require 'rudel-mode)
;;  (require 'rudel-obby)
;;  (global-rudel-minor-mode))

;; recent files list
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

; save minibuffer history
(require 'savehist)
(savehist-mode t)

;; Common lisp
;(require 'cl)
(require 'ido)
(require 'parenface)
(require 'dired+)
(require 'filecache)
;(require 'protobuf-mode)
;(require 'writegood-mode)
;(require 'zimpl-mode)

;; Highlight current line
;(global-hl-line-mode 1)

;; Blinking cursor
;(blink-cursor-mode 0)

;; TODO: evaluation period. Not sure how useful this is.
;(add-path "site-lisp/expand-region/")
;(require 'expand-region)  ; https://github.com/magnars/expand-region.el/
;(global-set-key (kbd "C-=") 'er/expand-region)  ; todo: add to keys section

;; maximize screen real estate
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)     ; hide menu-bar
(line-number-mode 1)   ; show line number near mode=line
(column-number-mode 1) ; show column number near mode-line

;; TODO: Can we fix the columns/rows discretization problem by adding a margin?
;(defun my-window-placement ()
;  (interactive)
;  (if (window-system)
;    (progn
;      (set-frame-position (selected-frame) 622 0)
;      ; rows instead of pixels => imperfect
;      (set-frame-size (selected-frame) 119 49))))


;(defun dark-colors ()
;  "Quicky change to custom dark color theme"
;  (interactive)
;  (custom-set-faces
;   '(default ((t (:stipple nil
;                  :background "black"
;                  :foreground "white"
;                  :inverse-video nil
;                  :box nil
;                  :strike-through nil
;                  :overline nil
;                  :underline nil
;                  :slant normal
;                  :height 100
;                  :foundry "bitstream"
;                  :family "Bitstream Vera Sans Mono"
;                  ))))
;   '(bold ((t (:weight extra-bold))))
;   '(comint-highlight-prompt ((t (:foreground "light blue"))))
;   '(compilation-info ((((class color) (min-colors 16) (background light)) (:foreground "gray" :weight bold))))
;   '(flymake-errline ((((class color)) (:underline "red"))))
;   '(flymake-warnline ((((class color)) (:underline "yellow4"))))
;   '(font-lock-builtin-face ((((class color) (min-colors 88) (background dark)) (:foreground "Purple2"))))
;   '(font-lock-comment-face ((t (:foreground "red" :slant italic))))
;   '(font-lock-keyword-face ((t (:foreground "orange"))))
;   '(font-lock-string-face ((t (:foreground "forest green"))))
;   '(font-lock-function-name-face ((t (:foreground "blue"))))
;   '(font-lock-type-face ((t (:foreground "blue"))))
;   '(font-lock-warning-face ((t (:foreground "LightGoldenrod"))))
;   '(italic ((t (:foreground "Yellow1" :slant italic))))
;   '(match ((((class color) (min-colors 88) (background light)) (:foreground "red"))))
;   '(minibuffer-prompt ((t (:foreground "white"))))
;   '(mode-line ((t (:background "blue" :foreground "white" :weight normal))))
;   '(mode-line-inactive ((default (:inherit mode-line)) (nil (:background "grey" :foreground "blue"))))
;   '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "purple"))))
;   '(org-link ((t (:foreground "cyan"))))
;   '(button ((t (:foreground "cyan"))))
;   '(rst-level-2-face ((t (:foreground "Purple2"))))
;  )
;)
;
;(defun light-colors ()
;  "Switch to a light color scheme."
;  (interactive)
;  (custom-set-faces
;   '(default ((t (:stipple nil
;                  :background "white"
;                  :foreground "black"
;                  :inverse-video nil
;                  :box nil
;                  :strike-through nil
;                  :overline nil
;                  :underline nil
;                  :slant normal
;                  :weight normal
;;                  :width  normal
;                  :foundry "bitstream"
;                  :family "Bitstream Vera Sans Mono"))))
;   '(mode-line ((t (:background "blue" :foreground "white" :weight normal))))
;   '(mode-line-inactive ((default (:inherit mode-line)) (nil (:background "grey" :foreground "blue"))))
;   '(minibuffer-prompt ((t (:foreground "black"))))
;   '(font-lock-keyword-face ((t (:foreground "orange3"))))
;   '(font-lock-function-name-face ((t (:foreground "royalblue"))))
;   '(font-lock-type-face ((t (:foreground "royalblue"))))
;  )
;)

(font-lock-add-keywords nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):" 1 '(:foreground "yellow") t)))

;(dark-colors)
;(light-colors)

;(add-hook 'window-setup-hook 'my-window-placement)
;(run-with-idle-timer 0.2 nil 'my-window-placement)  ; to avoid some issues, waits a half-second
;(my-window-placement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TODO: things I don't like about my tab situations
;;  - sometimes I screw up Makefiles
;;  - java wants to and indent width of 2, when I want 4..
;;
;(defvar c-tab-always-indent nil)
;(setq c-tab-always-indent t)
(setq-default indent-tabs-mode nil)  ; No tabs! XXX: why does this need to be set with `setq-default` not `setq`
(setq tab-width 4)

(setq c-basic-offset 2)  ;; timv: do I need this?

;; Column width (used in longlines-mode)
; [2018-05-11 Fri] Longlines-mode is obsolete according to emacs wiki - we're
; now supposed to use visual-line-mode)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(setq-default auto-fill-mode 1
              fill-column 80)

(setq default-major-mode 'text-mode  ; Make text-mode the default mode for new buffers.
      ;; case-fold-search t            ; case-insensitive search
      case-fold-search nil           ; (!) if this is non-nil hippie-expand will be busted.
      ;; read-file-name-completion-ignore-case t
      ;; completion-ignore-case t
      cursor-in-non-selected-windows nil  ; Don't show a cursor in other windows
      mouse-yank-at-point t               ; mouse yank at point, not click!
)

;; Note: I think the smooth-scrolling cannot be on for these settings to take effect
; scroll-margin: line where scrolling should start;
; scroll-conservatively: how far the cursor is allowed to be center when scrolling starts
; scroll-preserve-screen-position: maintain screen position when you hit Page(Up|Down)
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
)


;;; ido: "Interactively do" things (switch buffers, open files)
(require 'ido)
;(setq completion-ignored-extensions '(".aux" ".toc" ".tex~" "pyc"))
;(setq ido-ignore-extensions t)
;(setq ido-file-extensions-order '(".py" ".org" ".txt" ".tex" ".bib" ".scala" ".java"
;                                  ".el" ".xml" ".html" ".css" ".js"))
;(add-hook 'ido-setup-hook
;          (lambda ()
;            (define-key ido-completion-map [tab] 'ido-next-match)))

(setq ido-case-fold  t                     ; be case-insensitive
      ido-enable-last-directory-history  t ; remember last used dirs
      ido-use-filename-at-point nil        ; don't use filename at point (annoying)
      ido-use-url-at-point nil             ; don't use url at point (annoying)
      ido-enable-flex-matching nil         ; don't try to be too smart
      ido-confirm-unique-completion t      ; wait for RET, even with unique completion
      confirm-nonexistent-file-or-buffer t
)
(ido-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deal with annoying temporary files...

;; Put semantic.cache files somewhere far away.
(setq semantic-load-turn-useful-things-on t
      semanticdb-default-save-directory "~/.emacs.d/semantic/")

;; Put emacs backup files into their own directory
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/autosaves/"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove annoying dialogs:

;; get rid of yes-or-no questions - y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

;; Buffer `somebuffer' still has clients; kill it? (yes or no)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; get around the annoying "Active processes exist" query
(add-hook 'comint-exec-hook
          (lambda () (process-kill-without-query (get-buffer-process (current-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-keys ()

  ;; disable things which often confuse me
  (global-unset-key [(control next)])
  (global-unset-key [(control ?z)])              ;; background the window in terminal
  (global-unset-key [(control ?x) (control ?z)])
  (global-unset-key [mouse-2])                   ;; disable middle-click paste

  (global-set-key [(f5)] 'delete-other-windows)

  ;; Copy-Cut-Paste from clipboard
  (global-set-key (kbd "s-x") 'clipboard-kill-region)    ; cut
  (global-set-key (kbd "s-c") 'clipboard-kill-ring-save) ; copy
  (global-set-key (kbd "s-v") 'clipboard-yank)           ; paste

  (global-set-key (kbd "s-r") 'recentf-open-files)       ; recent files
  (global-set-key (kbd "s-a") 'anything)                 ; anything.el

  (global-set-key (kbd "\C-ca") 'anything)


  (global-set-key "\C-b" 'goto-matching-paren)
  (global-set-key (kbd "s-q") '(lambda () (interactive) (whitespace-cleanup) (message "whitespace-cleanup")))
  ;(global-set-key (kbd "s-q") '(lambda () (interactive) (message "whitespace-cleanup DISABLED!")))

  (global-set-key [f7] 'flyspell-start)

  ;; insert date stamp
  ;;
  ;; TODO: Jason has a nifty way of guessing the current file formatting (the
  ;; structure of date logged entries) and inserting a new one
  ;; automatically. Might also want to pay attention to any apparent sorting.
  ;;
  (global-set-key (kbd "\C-cd") 'insert-datestamp)

  ;; shortcut for killing buffers
  (global-unset-key "\M-k")
  (global-set-key "\M-k" 'kill-current-buffer)

  ;; context menu with contents of the yank ring
  (global-set-key "\C-cy" '(lambda () (interactive) (popup-menu 'yank-menu)))

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

  ;; use super+arrow keys to move between split windows
  ;(require 'windmove)
  ;(windmove-default-keybindings 'super)

  ;; lists functions, jump to begining of definition
  ;(global-set-key (kbd "M-i") 'ido-goto-symbol)
)

(my-keys)


(defun common-setup()
  (interactive)
  (global-font-lock-mode t)
  (load-library "my-emisc")
  (load-library "my-python")
  ;(load-library "my-haskell")
  (load-library "my-latex")
  (load-library "my-org")

  ; notably, no-X-setup does not start a server.
  (defun no-X-setup () nil)

  (defun X-setup ()
    (server-start)
    (set-mouse-color "black")
;    (mouse-avoidance-mode)
  )

  (add-to-list 'auto-mode-alist '("\\.tex$" . latex-setup))
  ;(load-library "matlab")

  ;(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
  ;(setq auto-mode-alist (cons '("\\.\\(md\\|markdown\\)" . markdown-mode) auto-mode-alist))

  (if (window-system) (X-setup) (no-X-setup))

)

(common-setup)

(setq dired-use-ls-dired nil)

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
(setq dired-no-confirm '(byte-compile chgrp chmod chown compress
                                      copy delete hardlink load
                                      move print shell symlink
                                      uncompress recursive-delete
                                      kill-file-buffer
                                      kill-dired-buffer patch
                                      create-top-dir
                                      revert-subdirs))
;; ---------------------------------------------------------------------------------------


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defun my-add-path (path-element)
;  "Add the specified path element to the Emacs PATH"
;  (interactive "DEnter directory to be added to path: ")
;  (if (file-directory-p path-element)
;      (setenv "PATH"
;              (concat (expand-file-name path-element)
;                      path-separator (getenv "PATH")))))

;; make the file executable if it is a script.
;(add-hook 'after-save-hook
;  'executable-make-buffer-file-executable-if-script-p)


;;------------------------------------------------------------------------------


;(setq org-latex-to-pdf-process '("pdflatex %f"))


(defun flyspell-ignore-tex ()
  "Tell flyspell to ignore latex commands when spell checking."
  (interactive)
  (set (make-variable-buffer-local 'ispell-parser) 'tex))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#4d4d4c" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#ffffff"))
 '(cursor-in-nonselected-windows nil)
 '(custom-safe-themes
   '("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default))
 '(fci-rule-color "#efefef")
 '(global-font-lock-mode t nil (font-lock))
 '(ibuffer-saved-filter-groups nil)
 '(icomplete-mode nil nil (icomplete))
 '(inhibit-startup-screen t)
 '(mouse-wheel-mode t nil (mwheel))
 '(package-selected-packages
   '(pylint flycheck-cython color-theme-sanityinc-tomorrow color-theme))
 '(show-paren-mode t nil (paren))
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(visible-cursor nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-highlight-prompt ((t (:foreground "light blue"))))
 '(compilation-info ((((class color) (min-colors 16) (background light)) (:foreground "gray" :weight bold))))
 '(flymake-error ((((class color)) (:underline "red"))))
 '(flymake-warning ((((class color)) (:underline "yellow4")))))

;; <INDENTING TEXT>
;;   Code copied from:  http://www.emacswiki.org/emacs/IndentingText

;; Shift the selected region right if distance is postive, left if
;; negative

; (defun shift-region (distance)
;   (let ((mark (mark)))
;     (save-excursion
;       (indent-rigidly (region-beginning) (region-end) distance)
;       (push-mark mark t t)
;       ;; Tell the command loop not to deactivate the mark
;       ;; for transient mark mode
;       (setq deactivate-mark nil))))
;
; (defun shift-right ()
;   (interactive)
;   (shift-region 1))
;
; (defun shift-left ()
;   (interactive)
;   (shift-region -1))
;
; ;; Bind (shift-right) and (shift-left) function to your favorite keys. I use
; ;; the following so that Ctrl-Shift-Right Arrow moves selected text one
; ;; column to the right, Ctrl-Shift-Left Arrow moves selected text one
; ;; column to the left:
;
; (global-set-key [C-S-right] 'shift-right)
; (global-set-key [C-S-left] 'shift-left)

;; </INDENTING TEXT>


; utf-8 everywhere (http://stackoverflow.com/questions/2901541/which-coding-system-should-i-use-in-emacs)
;(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)


; Note: We load theme late so that we tell emacs that it's "safe," which happens
; somewhere in custom-set-variables.
;(require 'color-theme)
;(load-theme 'sanityinc-tomorrow-eighties)

;(require 'langtool)
;(setq langtool-language-tool-jar "/home/timv/Downloads/LanguageTool-2.9/languagetool-commandline.jar"
;      langtool-mother-tongue "en"
;      langtool-disabled-rules '("WHITESPACE_RULE"
;                                "EN_UNPAIRED_BRACKETS"
;                                "COMMA_PARENTHESIS_WHITESPACE"
;                                "EN_QUOTES"))

(defun ansi-colors-show ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))


;; TODO: does this make hippie expand more usable?
;(delete 'try-expand-line hippie-expand-try-functions-list)
;(delete 'try-expand-list hippie-expand-try-functions-list)


;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev               ; any word that starts like this (in this buffer)
        try-expand-dabbrev-all-buffers   ; any word that starts like this (in any buffer)
        ;try-expand-dabbrev-from-kill
        try-expand-whole-kill            ; kill ring entry that starts like this one [added]
        try-expand-all-abbrevs           ; local abbrev table, then global, then other abbrev tables
        try-expand-list                  ; delimited list from elsewhere (in this buffer)
        try-expand-line                  ; line that starts just like this one (in this buffer)
        try-complete-file-name-partially ; partial filename completion (as far as unique) [added]
        try-complete-file-name           ; any filename that starts like this
        ))

(pending-delete-mode 1)  ;; crucial! typed text replaces a selection, rather than append


;(require 'color-theme)
(load-theme 'sanityinc-tomorrow-eighties)
