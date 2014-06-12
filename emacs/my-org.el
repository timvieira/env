;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode configuration goes here...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; -- Display images in org mode
;; enable image mode first
;(iimage-mode)
;; add the org file link format to the iimage mode regex
;(add-to-list 'iimage-mode-image-regex-alist
;             (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex "\\)\\]")  1))

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

(setq org-return-follows-link t)


(eval-after-load "org"
  '(progn
     ;; Change .pdf association directly within the alist
     (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")))

(defun read-file (file)
  "Return a list of lines in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;(defvar org-latex-header)
;(setq org-latex-header
 ;     (read-file "/home/timv/projects/env/emacs/org-latex-header.tex"))

(require 'org-latex)
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))

(add-to-list 'org-export-latex-classes
             '("article"
;               (format "%s" org-latex-header)

"\\documentclass[12pt]{article}
\\input{/home/timv/projects/env/timv}
"
               ("\\section*{%s}" . "\\section*{%s}")
               ("\\subsection*{%s}" . "\\subsection*{%s}"))
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
;  (font-lock-add-keywords nil '(("[0-9\\.]+\\. \\(.*?\\):[ ]" 1 '(:foreground "orange") t)))
;  (font-lock-add-keywords nil '(("[0-9\\.]+\\. \\(.*?\\):$" 1 '(:foreground "orange") t)))

  ;; note: require a space to avoid false positive on "http:" and "file:"
;  (font-lock-add-keywords nil '(("[\-\\*] \\(.*?\\):[ ]" 1 '(:foreground "orange") t)))
;  (font-lock-add-keywords nil '(("[\-\\*] \\(.*?\\):$" 1 '(:foreground "orange") t)))

  ;(flyspell-start)
  (flyspell-ignore-tex))



;;------------------------------------------------------------------------------
;; Support for skid links
;;
;; USAGE:
;;
;; The link directive 'skid'
;;
;;  [[skid:author:"Jason Eisner"][Jason Eisner]]
;;
;; Programmatic
;;
;;  (skid-search "tags:related:discrete-backprop")
;;  (skid-search "machine learning")
;;
;; org-mode reference http://orgmode.org/org.html#Adding-hyperlink-types

(require 'org)

(org-add-link-type "skid" 'skid-search)

(defun skid-search-results (query)
  "skid tag search."
  (interactive)
  (switch-to-buffer (make-temp-name "Skid"))
  (insert (shell-command-to-string (concat "python -m skid search --format org --limit 0 --no-open --pager none " query " &")))
  (beginning-of-buffer)
  (org-mode))

(defun skid-search (query)
  "skid tag search."
  (interactive)
  (call-process-shell-command "/usr/local/bin/skid" nil 0 nil "search" "--top" query "2>/dev/null" "&")
  (delete-other-windows))

;;------------------------------------------------------------------------------

(defun notes (query)
  (shell-command (concat "source /home/timv/projects/env/bash/notes.bash && notes " query)))
; (notes "gumbel max")

(org-add-link-type "bash" 'async-shell-command)
(org-add-link-type "notes" 'notes)

;;------------------------------------------------------------------------------

;; org-mode links to dyna issue tracker
(org-add-link-type "dyna" '(lambda (x)
                             (browse-url (concat "https://github.com/nwf/dyna/issues/"
                                                 (substring x 1)))))




(setq org-startup-with-inline-images nil)
(add-hook 'org-mode-hook
          'org-init)
