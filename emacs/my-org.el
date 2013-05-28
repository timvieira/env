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

"\\documentclass{article}
\\setlength\\parindent{0pt}   % no paragraph indentation
\\usepackage{fullpage}
\\setlength{\\parskip}{.35cm plus4mm minus3mm}

\\usepackage{fancybox}
\\usepackage{pgf}
\\usepackage{tikz}
\\usetikzlibrary{arrows,automata}

\\usepackage{framed,color}
\\definecolor{shadecolor}{rgb}{1,0.8,0.3}

\\usepackage{multirow}
\\usepackage{url}
\\usepackage{graphicx}

\\usepackage{latexsym}
\\usepackage{amsfonts}
\\usepackage{amsmath}
\\usepackage{amsthm}
\\usepackage{amssymb}
\\usepackage{amsbsy}

\\usepackage{verbatim}
\\usepackage{xspace}
\\usepackage{url}
\\usepackage{algorithm2e}

\\usepackage{color}
\\usepackage{xcolor}
\\definecolor{darkgrey}{rgb}{0.2,0.2,0.2}
\\definecolor{grey}{rgb}{0.9,0.9,0.9}
\\definecolor{darkblue}{rgb}{0.0,0.0,0.5}
\\definecolor{darkpurple}{rgb}{0.4,0.0,0.4}
\\definecolor{darkred}{rgb}{0.5,0.0,0.0}
\\definecolor{darkorange}{rgb}{0.5,0.45,0.4}
\\definecolor{darkgreen}{rgb}{0.0,0.5,0.0}
\\definecolor{darkergreen}{rgb}{0.0,0.4,0.0}
\\definecolor{lightblue}{rgb}{0.8,0.8,1.0}
\\definecolor{lightgreen}{rgb}{0.8,1.0,0.8}
\\definecolor{lightred}{rgb}{1.0,0.8,0.8}
\\definecolor{lightyellow}{rgb}{1.0,1.0,0.8}
\\definecolor{lightorange}{rgb}{1.0,0.9,0.8}
\\definecolor{lightgrey}{rgb}{0.96,0.97,0.98}

\\newsavebox{\\savelisting}
\\newenvironment{timbox}
{\\begin{lrbox}{\\savelisting}
\\begin{minipage}{6.5in}
\\begin{flushleft}}
{\\end{flushleft}
\\end{minipage}
\\end{lrbox}
\\begin{center}
\\resizebox{\\columnwidth}{!}{\\setlength\\fboxsep{6pt}\\fbox{\\usebox{\\savelisting}}}
\\end{center}}

\\newcommand{\\Note}[3]{{\\textcolor{#2}{[\\textbf{#1:} #3]}}}
\\newcommand{\\todo}[1]{\\Note{TODO}{red}{#1}}
\\newcommand{\\timv}[1]{\\Note{timv}{magenta}{#1}}
\\newcommand{\\halt}{\\textsc{halt}\\xspace }
\\newcommand{\\astar}{A$^*$\\xspace}
\\newcommand{\\softmin}{{\\text{{softmin}}}}
\\newcommand{\\softmax}{{\\text{{softmax}}}}

\\newcommand{\\Expect}[2]{\\mathbb{E}_{#1}\\left[#2\\right]}

\\newcommand{\\loss}[1]{ \\mathcal{L}\\left( #1 \\right) }
\\newcommand{\\indicator}[1]{ \\textbf{1}\\left[ #1 \\right] }

\\newcommand{\\gradtheta}[1]{\\nabla_\\theta \\left[ #1 \\right] }
\\renewcommand{\\vec}[1]{\\boldsymbol{#1}}
\\renewcommand{\\|}{\\textrm{~}\\arrowvert\\textrm{~}}

\\newcommand{\\R}{\\ensuremath{\\mathbb{R}}}

\\DeclareMathOperator*{\\argmax}{arg\\,max}
\\DeclareMathOperator*{\\argmin}{arg\\,min}
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
;  (font-lock-add-keywords nil '(("[0-9\\.]+\\. \\(.*?\\):[ ]" 1 '(:foreground "orange") t)))
;  (font-lock-add-keywords nil '(("[0-9\\.]+\\. \\(.*?\\):$" 1 '(:foreground "orange") t)))

  ;; note: require a space to avoid false positive on "http:" and "file:"
;  (font-lock-add-keywords nil '(("[\-\\*] \\(.*?\\):[ ]" 1 '(:foreground "orange") t)))
;  (font-lock-add-keywords nil '(("[\-\\*] \\(.*?\\):$" 1 '(:foreground "orange") t)))

  ;(flyspell-start)
  (flyspell-ignore-tex)

)

(setq org-startup-with-inline-images nil)
(add-hook 'org-mode-hook
          'org-init)
