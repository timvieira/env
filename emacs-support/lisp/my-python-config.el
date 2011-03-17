(add-hook 'python-mode-hook
          (lambda ()
	    (eldoc-mode 1) 
	    (setq outline-regexp "def\\|class ")))

(load-library "pylint")

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
                 word)	                ; sinon word
	     input))))			; sinon input
  (shell-command (concat py-python-command " -c \"from pydoc import help; help(\'" w "\')\"") "*PYDOCS*")
  (view-buffer-other-window "*PYDOCS*" t 'kill-buffer-and-window))


(defun my-python-hook ()
  (local-set-key [f7] 'flymake-mode)

  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|XXX\\|FIXME\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t)))

)
(add-hook 'python-mode-hook 'my-python-hook)


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
                       'tim-flymake-create-temp-inplace)
                      )
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

;; Set as a minor mode for python
(add-hook 'python-mode-hook '(lambda () (flymake-mode)))


(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the
message in the minibuffer"
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
	  (let ((err (car (second elem))))
	    (message "%s" (fly-pyflake-determine-message err)))))))

(defun fly-pyflake-determine-message (err)
  "pyflake is flakey if it has compile problems, this adjusts the
message to display, so there is one ;)"
  (cond ((not (or (eq major-mode 'Python) (eq major-mode 'python-mode) t)))
	((null (flymake-ler-file err))
	 ;; normal message do your thing
	 (flymake-ler-text err))
	(t ;; could not compile err
	 (format "compile error, problem on line %s" (flymake-ler-line err)))))

(defadvice flymake-goto-next-error (after display-message activate compile)
  "Display the error in the mini-buffer rather than having to mouse over it"
  (show-fly-err-at-point))

(defadvice flymake-goto-prev-error (after display-message activate compile)
  "Display the error in the mini-buffer rather than having to mouse over it"
  (show-fly-err-at-point))

(defadvice flymake-mode (before post-command-stuff activate compile)
  "Add functionality to the post command hook so that if the
cursor is sitting on a flymake error the error information is
displayed in the minibuffer (rather than having to mouse over
it)"
  (set (make-local-variable 'post-command-hook)
       (cons 'show-fly-err-at-point post-command-hook))) 
