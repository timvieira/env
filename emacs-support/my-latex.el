;; -*- coding: utf-8 -*-

(defun flyspell-start ()
  "initialize flyspell-mode and check buffer."
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
  (latex-mode)

  (local-unset-key "\C-c\C-c")
  (local-set-key "\C-c\C-c" 'latex-thing)

  (local-unset-key "\C-e\C-e")
  (local-set-key "\C-e\C-e" 'latex-open-this-pdf)

  (flyspell-start)
  ;(longlines-mode t)
)