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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;flymake-ler(file line type text &optional full-file)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; This is a function which simplifies the insertion of debugging print
; statements in a python program.

(defun pyp (expr)
  "Insert a print statement as the next statement of the program.
   Include the name of the enclosing function or and accept from
   the minibuffer the expresion to be printed. The form of the
   statement to be entered and indented is:
      print 'function: expr =', expr"
  (interactive "sExpression to be printed: ")
  (save-excursion
    (py-beginning-of-def-or-class)
    (re-search-forward "[ ]*def[ ]+\\(\\w+\\)\\W")
    (setq pyp-funct (buffer-substring (match-beginning 1) (match-end 1))))
  (end-of-line)
  (newline)
  (insert "print '" pyp-funct ": " expr " =', " expr)
  (indent-for-tab-command))

;; The emacs function, ppy, will go to the end of the current line and
;; insert two new lines containing "print 'function: pretty printing
;; variable '" and "pp.pprint(variable)". The variable is picked up
;; from the minibuffer following the prompt.

(defun ppy (variable)
  "Insert statements following the next statements of the program which
   will pretty print the variable. Note that this is a variable and not
   an expression."
  (interactive "sVariable to be printed: ")
  (save-excursion
    (py-beginning-of-def-or-class)
    (re-search-forward "[ ]*def[ ]+\\(\\w+\\)\\W")
    (setq pyp-funct (buffer-substring (match-beginning 1) (match-end 1))))
  (end-of-line)
  (newline)
  (insert "print '" pyp-funct ": pretty printing " variable "'")
  (indent-for-tab-command)
  (newline)
  (insert "pp.pprint(" variable ")")
  (indent-for-tab-command)
  )

;; For the previous function, ppy, to generate working code it is
;; necessary to have the proper import statements at the top of the
;; module. To get them, go to a correct spot and execute the following
;; function.

(defun ppyi ()
  "Insert the code necessary to import pprint and create the necessary
   PrettyPrinter object at the current point."
  (interactive)
  (newline)
  (insert "import pprint")
  (indent-for-tab-command)
  (newline)
  (insert "pp = pprint.PrettyPrinter()")
  (indent-for-tab-command))

