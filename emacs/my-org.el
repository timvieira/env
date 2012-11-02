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
;(setq org-cycle-separator-lines 2)
(setq org-cycle-separator-lines 1)


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

;(setq org-return-follows-link t)



(eval-after-load "org"
  '(progn
     ;; Change .pdf association directly within the alist
     (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")))


(require 'org-latex)
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
             '("article"
               "\\documentclass{article}
               \\setlength\\parindent{0pt}   % no paragraph indentation
               \\newcommand{\\parents}[1]{\\textsc{Pa}(#1)}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}"))
)


(require 'org-publish)
;(setq org-publish-project-alist
;      '(
;        ("org-notes"
;         :base-directory "~/projects/notes"
;         :base-extension "org"
;         :publishing-directory "~/projects/notes/export"
;         :recursive t
;         :publishing-function org-publish-org-to-pdf
;         :headline-levels 4             ; Just the default for this project.
;         :auto-preamble t
;         )
;      ))


(defun org-init ()
  (interactive)
  (org-indent-mode t)   ;; alternative to "#+STARTUP: indent"

  (load "my-latex")

  (local-unset-key (kbd "s-e"))
  ;; export to pdf
  (fset 'my-org-export-pdf
        [?\M-x ?o ?r ?g ?- ?e ?x ?p ?o ?r ?t return ?p])
  (local-set-key (kbd "s-e") 'my-org-export-pdf) ; 'org-publish-current-file)

  (local-unset-key (kbd "s-o"))
  (local-set-key (kbd "s-o") 'latex-open-this-pdf)

  (org-babel-do-load-languages
   (quote org-babel-load-languages)
   (quote ((emacs-lisp . t)
           (dot . t)
           (python . t)
           (gnuplot . t)
           (sh . t)
           (org . t)
           (latex . t))))

  ;; Do not prompt to confirm evaluation
  (setq org-confirm-babel-evaluate nil)

  ;; highlight certain keywords
  (font-lock-add-keywords nil '(("\\<\\(FIX\\|TODO\\|todo\\|FIXME\\|CHECK\\|check\\|Check\\|revise\\|REVISE\\):" 1
                                 '(:foreground "red") t)))

  ;; highlight ascii hlines
  (font-lock-add-keywords nil '(("^\\([\\-\\=]+\\)$" 1 '(:foreground "orange") t)))

  ;; highlight bullet stuff colon. E.g. "1. Something interesting: elaboration"
;  (font-lock-add-keywords nil '(("[\\-\\*] \\(.*?\\):" 1 '(:foreground "orange") t)))

  (font-lock-add-keywords nil '(("[0-9\\.]+\\. \\(.*?\\):" 1 '(:foreground "orange") t)))
;  (font-lock-add-keywords nil '(("- \\(.*?\\):" 1 '(:foreground "orange") t)))
  (font-lock-add-keywords nil '(("[\-\\*] \\(.*?\\):" 1 '(:foreground "orange") t)))

  ;(flyspell-start)
)

;(setq org-startup-with-inline-images t)
(add-hook 'org-mode-hook
          'org-init)
