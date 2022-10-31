;;(load-library "site-lisp/pylint.el")
;; (load-library "site-lisp/flymake-cursor")

(defcustom py-python-command "python"
  "*Shell command used to start Python interpreter."
  :type 'string
  :group 'python)

;(when (load "flymake" t)
;  (defun flymake-pylint-init ()
;    (let* ((temp-file (flymake-init-create-temp-buffer-copy
;                       'flymake-create-temp-inplace))
;           (local-file (file-relative-name
;                        temp-file
;                        (file-name-directory buffer-file-name))))
;      (list "epylint" (list local-file))))
;  (add-to-list 'flymake-allowed-file-name-masks
;               '("\\.py\\'" flymake-pylint-init)))


;; Configure flymake for Python
;???; (setq pylint "epylint")
;???; (when (load "flymake" t)
;???;   (defun flymake-pylint-init ()
;???;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;???;                        'flymake-create-temp-inplace))
;???;            (local-file (file-relative-name
;???;                         temp-file
;???;                         (file-name-directory buffer-file-name))))
;???;       (list (expand-file-name pylint "") (list local-file))))
;???;   (add-to-list 'flymake-allowed-file-name-masks
;???;                '("\\.py\\'" flymake-pylint-init)))


;; new ;; (defun my-python-documentation (w)
;; new ;;   "Launch PyDOC on the Word at Point"
;; new ;;   (interactive
;; new ;;    (list (let* ((word (thing-at-point 'word))
;; new ;;                 (input (read-string
;; new ;;                         (format "pydoc entry%s: "
;; new ;;                                 (if (not word) "" (format " (default %s)" word))))))
;; new ;;            (if (string= input "")
;; new ;;                (if (not word)
;; new ;;                    (error "No pydoc args given")
;; new ;;                  word)                  ; sinon word
;; new ;;              input))))			; sinon input
;; new ;;   (shell-command (concat py-python-command " -c \"from pydoc import help; help(\'" w "\')\"") "*PYDOCS*")
;; new ;;   (view-buffer-other-window "*PYDOCS*" t 'kill-buffer-and-window))

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

; Configure flymake for python
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

;(when (load "flymake" t)
;  (defun flymake-pylint-init ()
;    (let* ((temp-file (flymake-init-create-temp-buffer-copy
;                       'flymake-create-temp-inplace))
;           (local-file (file-relative-name
;                        temp-file
;                        (file-name-directory buffer-file-name))))
;      (list "epylint" (list local-file))))
;  (add-to-list 'flymake-allowed-file-name-masks
;               '("\\.py\\'" flymake-pylint-init)))

(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the message in the minibuffer"
  ;(require 'cl)
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
      (let ((err (car (second elem))))
        (message "%s" (flymake-ler-text err)))))))

;; To avoid having to mouse hover for the error message, these functions make
;; flymake error messages appear in the minibuffer
(add-hook 'post-command-hook 'show-fly-err-at-point)


(defun my-python-hook ()
  (local-set-key [f7] 'flymake-mode)

  (flymake-mode)

  ; type "from arsenal.debug import ip; ip()"
  ;(fset 'python-ip [?f ?r ?o ?m ?  ?a ?r ?s ?e ?n ?a ?l ?. ?d ?e ?b ?u ?g ?  ?i ?m ?p ?o ?r ?t ?  ?i ?p ?\; ?  ?i ?p ?\( ?\)])

  (fset 'python-ip [?f ?r ?o ?m ?  ?I ?P ?y ?t ?h ?o ?n?  ?i ?m ?p ?o ?r ?t ?  ?e ?m ?b ?e ?d ?\; ?  ?e ?m ?b ?e ?d ?\( ?\)])
  
  (local-set-key (kbd "\C-cb") 'python-ip)

  ;; Keymaps to navigate thru pylint errors
  (define-key python-mode-map "\C-cn" 'flymake-goto-next-error)
  (define-key python-mode-map "\C-cp" 'flymake-goto-prev-error)

  ;; highlight special comments
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIX\\|TODO\\|XXX\\|FIXME\\|HACK\\|REFACTOR\\):" 1 '(:foreground "orange") t)))
)


(add-hook 'python-mode-hook 'my-python-hook)
;(add-hook 'python-mode-hook '(lambda () (flymake-mode)))
;(add-hook 'python-mode-hook '(lambda () (eldoc-mode 1) (setq outline-regexp "def\\|class ")))
