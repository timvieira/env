
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

(defun kill-Ms ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "")
      (replace-match ""))))

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

;; %S/%M/%H (sec/min/hour-mil) %l(hour) %p(am/pm) %d(day of month) %B (month) %Y (year) %Z (time zone) %A (day name) %j (day of year) %U (week of year)
(defun insert-date ()
  (interactive)
  (insert (format-time-string "%A %B %d, %Y")))

(defun insert-time ()
  (interactive)
  (insert (format-time-string "%l:%M%p")))

(defun insert-datestamp ()
  (interactive)
  (insert (format-time-string "[%D]")))

(defun journal-section-header ()
  (interactive)
  (insert "* ==== ")
  (org-time-stamp-inactive)
  (insert " ===="))

(defun journal-subsection-header ()
  (interactive)
  (insert "*** == ")
  (org-time-stamp-inactive t)
  (insert " =="))

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

