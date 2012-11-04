(add-hook 'python-mode-hook
          (lambda ()
            (eldoc-mode 1)
            (setq outline-regexp "def\\|class ")))

(load-library "pylint")
(load-library "site-lisp/flymake-cursor")

(defcustom py-python-command "python"
  "*Shell command used to start Python interpreter."
  :type 'string
  :group 'python)

(defun my-python-documentation (w)
  "Launch PyDOC on the Word at Point"
  (interactive
   (list (let* ((word (thing-at-point 'word))
                (input (read-string
                        (format "pydoc entry%s: "
                                (if (not word) "" (format " (default %s)" word))))))
           (if (string= input "")
               (if (not word)
                   (error "No pydoc args given")
                 word)                  ; sinon word
             input))))			; sinon input
  (shell-command (concat py-python-command " -c \"from pydoc import help; help(\'" w "\')\"") "*PYDOCS*")
  (view-buffer-other-window "*PYDOCS*" t 'kill-buffer-and-window))

;;;; general init-cleanup and helper routines
(defun tim-flymake-create-temp-inplace (file-name prefix)
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((temp-name   (concat "/tmp/" (replace-regexp-in-string "/" "_" (file-name-sans-extension file-name))
                              "_" prefix
                              (and (file-name-extension file-name)
                                   (concat "." (file-name-extension file-name))))))
    (flymake-log 3 "create-temp-inplace: file=%s temp=%s" file-name temp-name)
    temp-name))


;; Configure flymake for python
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'tim-flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))


(defun my-python-hook ()
  (local-set-key [f7] 'flymake-mode)
  ;; highlight special comments
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIX\\|TODO\\|XXX\\|FIXME\\|HACK\\|REFACTOR\\):" 1 '(:foreground "orange") t)))
)

(add-hook 'python-mode-hook 'my-python-hook)

;; Set as a minor mode for python
(add-hook 'python-mode-hook '(lambda () (flymake-mode)))


; type "from debug import ip; ip()"
(fset 'python-ip
      [?f ?r ?o ?m ?  ?d ?e ?b ?u ?g ?  ?i ?m ?p ?o ?r ?t ?  ?i ?p ?\; ?  ?i ?p ?\( ?\)])
(local-set-key (kbd "s-b") 'python-ip)
