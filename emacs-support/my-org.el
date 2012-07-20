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
(setq org-cycle-separator-lines 2)


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

;(setq org-startup-with-inline-images t)
(add-hook 'org-mode-hook
          '(lambda ()
             (org-indent-mode t)   ;; #+STARTUP: indent

             (org-babel-do-load-languages
              (quote org-babel-load-languages)
              (quote ((emacs-lisp . t)
                      (dot . t)
                      (ditaa . t)
                      (python . t)
                      (gnuplot . t)
                      (sh . t)
                      (ledger . t)
                      (org . t)
                      (plantuml . t)
                      (latex . t))))

             ; Do not prompt to confirm evaluation
             (setq org-confirm-babel-evaluate nil)

             ;(flyspell-start)
             ))