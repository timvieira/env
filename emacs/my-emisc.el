;; -*- coding: utf-8 -*-

(defun change-indent (w)
  (interactive "nWidth: ")
  (set-variable 'c-basic-offset w))

(defun sudo-edit ()
  (interactive)
  (let ((p (point)))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))
    (goto-char p)))

;; TODO: why does this need to be run multiple times sometimes?
(defun ascii-fy ()
  (interactive)
  (replace-string " " " ")
  (replace-string "“" "\"")
  (replace-string "’" "'")
  (replace-string "’" "'")
  (replace-string "‘" "'")
  (replace-string "‘" "'")
  (replace-string "‘" "'")
  (replace-string "“" "\"")
  (replace-string "”" "\"")
  (replace-string "—" "-")
  (replace-string "–" "-")
  (replace-string "ﬂ" "fl")
  (replace-string "ﬁ" "fi")
  (replace-string "•" "*")
  (replace-string "…" "...")
  (replace-string "à" "a")         ; lossy
  (replace-string "α" "\\alpha")   ; tex-fy
  (replace-string "→" "->"))

;; see open-shell:
(defun get-shell ()
  "Spawn a gnome-terminal in CWD."
  (interactive)
  (shell-command "nohup gnome-terminal >& /dev/null &" )
  (delete-other-windows))

; alternative to get-shell which doesn't get killed when emacs dies.
;(defun open-shell ()
;  "open a gnome-terminal in cwd"
;  (interactive)
;  (shell-command (concat "gnome-terminal --working-directory " (shell-command-to-string "pwd") " &"))
;)

(defun goto-matching-paren ()
  "If point is sitting on a parenthetic character, jump to its match."
  (interactive)
  (cond ((looking-at "\\s\(") (forward-list 1))
        ((progn
           (backward-char 1)
           (looking-at "\\s\)")) (forward-char 1) (backward-list 1))))

(defun fullscreen ()
  "make the emacs window fullscreen"
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

;; ------------------

(defun escape-squote(str)
  (string-match "'" str)
  (replace-match "\\\'" t t  str))

(defun describe-face-at-point ()
  "Describe face at point."
  (interactive)
  (describe-face (get-face-at-point)))

(defun customize-face-at ()
  "Customize face at point."
  (interactive)
  (customize-face (get-face-at-point)))

(defun get-face-at-point ()
  "Determine face at point using `get-char-property'.
If char at point has no face property, examine the text on the same line
as point as well."
  (let ((face (get-char-property (point) 'face)))
    (unless face
      (let ((start (point)))
    (while (null (or (setq face (get-char-property (point) 'face))
             (eolp)))
      (forward-char 1))))
    (unless face
      (let ((start (point)))
    (while (null (or (setq face (get-char-property (point) 'face))
             (bolp)))
      (forward-char -1))))
    (unless face
      (error "No face selected."))
    (if (listp face)
    (setq face (car face)))
    face))

(defun kill-whitespace-in-region()
  (interactive)
  (let* ((start (min (point) (mark)))
     (end (max (point) (mark)))
     (charcount (- end start)))
    (goto-char start)
    (while
    (> charcount 0)
      (cond ((member (char-after) (list ?\t ?\ ))
         (delete-char 1))
        (t (forward-char)))
      (setq  charcount (1- charcount))
      )))

(defun make-one-liner ()
  (interactive)
  (let* ((start (min (point) (mark)))
     (linecount (count-lines (point) (mark))))
    (goto-char start)
    (while
    (> linecount 0)
      (end-of-line)
      (delete-char 1)
      (setq linecount (1- linecount)))
    (beginning-of-line)))

(defun kill-superfluous-buffers()
  (interactive)
  (mapcar '(lambda(buffer)
       (let ((blength (length (buffer-name buffer)))
         (bstring (buffer-name buffer))
         )
         (if (and
          (string-equal (substring bstring 0 1) "*")
          (string-equal (substring bstring (- blength 1) blength) "*"))
         (kill-buffer buffer)))
       )
     (buffer-list)))

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun kill-current-buffer-and-window ()
  (interactive)
  (kill-buffer (current-buffer))
  (delete-window))

;; %S/%M/%H (sec/min/hour-mil) %l(hour) %p(am/pm) %d(day of month) %B (month)
; %Y %(year) %Z (time zone) %A (day name) %j (day of year) %U (week of year)
(defun insert-date ()
  (interactive)
  (insert (format-time-string "%A %B %d, %Y")))

(defun insert-time ()
  (interactive)
  (insert (format-time-string "%l:%M%p")))

;; http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_689.html
(defun insert-datestamp ()
  (interactive)
  (insert (format-time-string "[%Y-%m-%d %a]")))

(defun bracket-text (left right)
  (interactive "sLeft delim:\nsRight delim:")
  (delete-trailing-whitespace)
  (save-excursion
    (let* ((start (min (point) (mark)))
           (linecount (count-lines (point) (mark)))
           (ldelim (if left left "'"))
           (rdelim (if right right "'")))
      (goto-char start)
      (while
          (> linecount 0)
        (beginning-of-line)
        (re-search-forward "^[ \t]*")
        (replace-match ldelim)
        (end-of-line)
        (re-search-backward "[ \t]*$")
        (replace-match rdelim)
        (forward-line)
        (setq linecount (1- linecount))
        ))))

;; Word count!
(defun word-count (&optional filename)
  "Returns the word count of the current buffer.  If `filename' is not nil, returns the word count of that file."
  (interactive)
  (save-some-buffers) ;; Make sure the current buffer is saved
  (let ((tempfile nil))
    (if (null filename)
        (progn
          (let ((buffer-file (buffer-file-name))
                (lcase-file (downcase (buffer-file-name))))
            (if (and (>= (length lcase-file) 4) (string= (substring lcase-file -4 nil) ".tex"))
                ;; This is a LaTeX document, so DeTeX it!
                (progn
                  (setq filename (make-temp-file "wordcount"))
                  (shell-command-to-string (concat "detex < " buffer-file " > " filename))
                  (setq tempfile t))
              (setq filename buffer-file)))))
    (let ((result (car (split-string (shell-command-to-string (concat "wc -w " filename)) " "))))
      (if tempfile
          (delete-file filename))
      (message (concat "Word Count: " result))
      )))
