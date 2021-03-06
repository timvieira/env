
;; Add the current dir for loading haskell-site-file.
(add-to-list 'load-path ".")

;; Always load via this. If you contribute you should run `make all`
;; to regenerate this.
(load "haskell-site-file")

(add-hook 'haskell-mode-hook 'haskell-hook)

;; Haskell main editing mode key bindings.
(defun haskell-hook ()
  ;; Use simple indentation.
  (turn-on-haskell-simple-indent)
  (define-key haskell-mode-map (kbd "<return>") 'haskell-simple-indent-newline-same-col)
  (define-key haskell-mode-map (kbd "C-<return>") 'haskell-simple-indent-newline-indent)

  ;; Load the current file (and make a session if not already made).
  (define-key haskell-mode-map [?\C-c ?\C-l] 'haskell-process-load-file)
  (define-key haskell-mode-map [f5] 'haskell-process-load-file)

  ;; Switch to the REPL.
  (define-key haskell-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch)
  ;; “Bring” the REPL, hiding all other windows apart from the source
  ;; and the REPL.
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)

  ;; Get the type and info of the symbol at point, print it in the
  ;; message buffer.
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)

  ;; Contextually do clever things on the space key, in particular:
  ;;   1. Complete imports, letting you choose the module name.
  ;;   2. Show the type of the symbol after the space.
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

  ;; Jump to the imports. Keep tapping to jump between import
  ;; groups. C-u f8 to jump back again.
  (define-key haskell-mode-map [f8] 'haskell-navigate-imports)

  ;; Jump to the definition of the current symbol.
  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

  ;; Save the current buffer and generate etags (a TAGS file) for the
  ;; whole project.
  (define-key haskell-mode-map (kbd "C-x C-s") 'haskell-mode-save-buffer)

;; TIMV: this is one of the most annoying things ever.
;  ;; Indent the below lines on columns after the current column.
;  (define-key haskell-mode-map (kbd "C-<right>")
;    (lambda ()
;      (interactive)
;      (haskell-move-nested 1)))
;  ;; Same as above but backwards.
;  (define-key haskell-mode-map (kbd "C-<left>")
;    (lambda ()
;      (interactive)
;      (haskell-move-nested -1)))
)
