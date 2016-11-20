;; -*- coding: utf-8 -*-

;; NOTES:
;;
;;  - we hardcode the use of evince as pdf viewer
;;
;;  - assumes bibtex file has same prefix (e.g. mydoc.tex and mydoc.bib)
;;
;; INSTALLATION:
;;
;;  - put this code in your .emacs file or in it's own file (e.g. my-latex.el)
;;    and then add (load-library "my-latex") to your .emacs
;;
;;  - you should also include the following in your .emacs to associate with
;;    tex files
;;
;;       (add-to-list 'auto-mode-alist '("\\.tex$" . latex-setup))
;;

(defun flyspell-start ()
  "initialize flyspell-mode and check buffer."
  (interactive)
  (flyspell-buffer)
  (flyspell-mode))

;(defun run-pdflatex (file-name)
;  (if (= 1 (shell-command (concat "pdflatex -halt-on-error " file-name)))
;      (message "pdflatex failed")
;    (progn
;      ;(message "pdflatex succeeded")
;      (delete-other-windows))))

;; might need to edit configuration:
;; $ sudo emacs -nw /usr/share/texlive/texmf/web2c/texmf.cnf
;; change 'openout_any = p' to 'a' or 'r'
;(defun run-bibtex (f)
;  (if (= 1 (shell-command (concat "bibtex " f ".aux")))
;      (message "bibtex failed")
;    (progn
;      ;(message "bibtex succeeded")
;      (delete-other-windows))))

; todo: "make clean" first: sometimes deleting aux file fixes compile issues
;; (defun latex-thing ()
;;   (interactive)
;;   (let ((tex (buffer-file-name)))
;;     (let ((base (substring tex 0 -4)))  ; filename with out extension
;;       (let ((pdf (concat base ".pdf")))
;;         ;(let ((bib (car (directory-files "." nil ".*\\.bib"))))  ; todo: what if there's no bib
;;         (if (file-exists-p pdf) (delete-file pdf))  ; delete old pdf
;;         (run-pdflatex tex)
;;         ;(if (file-exists-p bib)
;;         ;    (progn
;;         (run-bibtex base)
;;         ;; run latex twice -- bibtex is weird like that
;;         (run-pdflatex tex)
;;         (run-pdflatex tex)))
;;    ))

(defun run-make-latex (f)
;  (if (= 1 (shell-command (concat "/home/timv/projects/env/bin/make-latex " f)))
    (if (= 1 (shell-command (concat "latexmk -pdf " f)))
      (message "make-latex failed")
    (progn
      (delete-other-windows))))

(defun latex-thing ()
  (interactive)
  (let ((tex (buffer-file-name)))
    (let ((base (substring tex 0 -4)))  ; filename with out extension
      (run-make-latex base)
   )))

(defun latex-open-this-pdf ()
  (interactive)
  (let ((tex (buffer-file-name)))
    (let ((base (substring tex 0 -4)))  ; filename with out extension
      (let ((pdf (concat base ".pdf")))
        (call-process-shell-command "nohup" nil 0 nil "evince" pdf "2>/dev/null" "&")
;        (shell-command (concat "nohup evince " pdf " 2>/dev/null >/dev/null &"))
        (delete-other-windows))
      )))

;(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(defun latex-setup ()
  (interactive)
  (latex-mode)

  (local-unset-key "\C-c\C-c")
  (local-set-key "\C-c\C-c" 'latex-thing)

  (local-unset-key "\C-c\C-v")
  (local-set-key "\C-c\C-v" 'latex-open-this-pdf)

  (flyspell-start)
  ;(longlines-mode t)
)
