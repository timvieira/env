;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode configuration goes here...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; -- Display images in org mode
;; enable image mode first
(iimage-mode)
;; add the org file link format to the iimage mode regex
(add-to-list 'iimage-mode-image-regex-alist
             (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex "\\)\\]")  1))

;; I don't want to see blank lines in collapsed (contents) views. This setting
;; hides single blank lines and exposes the rest so I can clean them up.
(setq org-cycle-separator-lines 2)


;; Links to emails, web pages, and other files are sprinkled all over my org
;; files. The following setting control how org-mode handles opening the link.
;;
;; I like to keep links in the same window so that I don't end up with a ton of
;; frames in my window manager. I normally work in a full-screen window and
;; having links open in the same window just works better for
(setq org-link-frame-setup (quote ((vm . vm-visit-folder)
                                   (gnus . org-gnus-no-new-news)
                                   (file . find-file))))

(setq org-src-fontify-natively t)


(require 'org-latex)
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
             '("article"
               "\\documentclass{article}
               \\setlength\\parindent{0pt}   % no paragraph indentation
               \\newcommand{\\parents}[1]{\\textsc{Pa}(#1)}"
               ("\\section{%s}" . "\\section*{%s}")))


(defun org-init ()
  (interactive)
  (org-indent-mode t)   ;; alternative to "#+STARTUP: indent"

  (load "my-latex")

  ;; export to pdf
  (fset 'my-org-export-pdf
        [?\M-x ?o ?r ?g ?- ?e ?x ?p ?o ?r ?t return ?p])
  (local-unset-key "\C-c\C-c")
  (local-set-key "\C-c\C-c" 'my-org-export-pdf)

  (fset 'my-org-export-pdf
        [?\M-x ?o ?r ?g ?- ?e ?x ?p ?o ?r ?t return ?p])

  (local-unset-key "\C-c\C-c")
  (local-set-key "\C-c\C-c" 'my-org-export-pdf)

  (local-unset-key "\C-c\C-v")
  (local-set-key "\C-c\C-v" 'latex-open-this-pdf)

  (org-babel-do-load-languages
   (quote org-babel-load-languages)
   (quote ((emacs-lisp . t)
           (dot . t)
           (ditaa . t)
           (python . t)
           (gnuplot . t)
           (sh . t)
           (ledger . t)
           (org . t)
           (plantuml . t)
           (latex . t))))

  ;; Do not prompt to confirm evaluation
  (setq org-confirm-babel-evaluate nil)

  ;; highlight certain keywords
  (font-lock-add-keywords nil '(("\\<\\(FIX\\|TODO\\|todo\\|FIXME\\|CHECK\\|check\\|Check\\):" 1
                                 '(:foreground "red") t)))
  ;; highlight ascii hlines
  (font-lock-add-keywords nil '(("^\\([\\-\\=]+\\)$" 1 '(:foreground "yellow") t)))

  ;; highlight bullet stuff colon. E.g. "1. Something interesting: elaboration"
;  (font-lock-add-keywords nil '(("[\\-\\*] \\(.*?\\):" 1 '(:foreground "yellow") t)))

  (font-lock-add-keywords nil '(("[0-9\\.]+\\. \\(.*?\\):" 1 '(:foreground "yellow") t)))
;  (font-lock-add-keywords nil '(("- \\(.*?\\):" 1 '(:foreground "yellow") t)))
  (font-lock-add-keywords nil '(("[\-\\*] \\(.*?\\):" 1 '(:foreground "yellow") t)))

  ;(flyspell-start)
)

;(setq org-startup-with-inline-images t)
(add-hook 'org-mode-hook
          'org-init)