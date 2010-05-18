;; Support for running sbt in inferior mode.

(eval-when-compile (require 'cl))
(require 'tool-bar)
(require 'compile)

(defgroup sbt nil
  "Run SBT REPL as inferior of Emacs, parse error messages."
  :group 'tools
  :group 'processes)

(defconst sbt-copyright    "Copyright (C) 2008 Raymond Paul Racine")
(defconst sbt-copyright-2  "Portions Copyright (C) Free Software Foundation")
(defconst sbt-version      "0.02")
(defconst sbt-author-name  "Raymond Racine")
(defconst sbt-author-email "ray.racine@gamail.com")
(defconst sbt-legal-notice
  "This is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 2, or (at your option) any later version.  This is
distributed in the hope that it will be useful, but without any warranty;
without even the implied warranty of merchantability or fitness for a
particular purpose.  See the GNU General Public License for more details.  You
should have received a copy of the GNU General Public License along with Emacs;
see the file `COPYING'.  If not, write to the Free Software Foundation, Inc.,
59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.")

(defgroup sbt nil
  "Support for sbt build REPL."
  :group  'sbt
  :prefix "sbt-")

(defcustom sbt-program-name "sbt"
  "Program invoked by the `run-sbt' command."
  :type 'string
  :group 'sbt)

(defun sbt-build-buffer-name (mode)
 "*Scala Build Tool*")

(defun sbt-shell ()
  "Launch the sbt shell."
  (interactive)
  (winner-mode)
  (compile (concat "cd " (sbt-find-path-to-project) "; /home/timv/projects/bin/sbt compile") t)
  (pop-to-buffer (sbt-build-buffer-name nil))
  (end-of-buffer))

(defun sbt-clear ()
  "Clear (erase) the SBT buffer."
  (interactive)
  (with-current-buffer (sbt-build-buffer-name nil)
    (let ((inhibit-read-only t))
      (erase-buffer))))

;; (defun sbt-compilation-exit-message-function (status code msg)
;;   "Close the compilation window if there were no errors."
;;   ;; if compiling exits with status 0
;;   (when (and (eq status 'exit) (zerop code))
;;     ;; then bury the *compilation* buffer, so that C-x b doesn't go there
;;     (bury-buffer (sbt-build-buffer-name nil))
;;     ;;(kill-current-buffer-and-window))
;;     ;;(delete-other-windows)
;;     (delete-window)
;;     )
;;   ;; Always return the anticipated result of compilation-exit-message-function
;;   (cons msg code))
;; 
;; (customize-set-variable 'compilation-exit-message-function  'sbt-compilation-exit-message-function)


(setq compilation-finish-functions 'compile-autoclose)
(defun compile-autoclose (buffer string)
  (cond ((string-match "finished" string)
         (bury-buffer (sbt-build-buffer-name nil))
         (winner-undo)
         (message "Build successful."))
        (t                                                                    
         (message "Compilation exited abnormally: %s" string))))



(customize-set-variable 'scala-compile-error-regex  
                        '("^\\[error\\] \\([.a-zA-Z0-9/-]+[.]scala\\):\\([0-9]+\\):" 1 2 nil 2 nil))  
(customize-set-variable 'compilation-buffer-name-function 'sbt-build-buffer-name)  
(customize-set-variable 'compilation-error-regexp-alist (list scala-compile-error-regex))
(customize-set-variable 'compilation-mode-font-lock-keywords
                        '(("^\\[error\\] Error running compile:"
                           (0 compilation-error-face))
                          ("^\\[warn\\][^\n]*"
                           (0 compilation-warning-face))
                          ("^\\(\\[info\\]\\)\\([^\n]*\\)"
                           (0 compilation-info-face)
                           (1 compilation-line-face))
                          ("^\\[success\\][^\n]*"
                           (0 compilation-info-face))))

(customize-set-variable 'comint-prompt-read-only t)
(customize-set-variable 'compilation-buffer-name-function 
                        'sbt-build-buffer-name)
(customize-set-variable 'compilation-error-regexp-alist 
                        (list scala-compile-error-regex))
(set 'compilation-auto-jump-to-first-error t)

(ansi-color-for-comint-mode-on)

;; Locate the project root directory from the source buffer location.

(defun sbt-project-dir-p (path)
  "Does a project/build.properties exists in the given path."
  (file-exists-p (concat path "/project/build.properties")))

(defun sbt-at-root (path)
  "Determine if the given path is root."
  (equal path (sbt-parent-path path)))

(defun sbt-parent-path (path)
  "The parent path for the given path."
  (file-truename (concat path "/..")))

;; Search up the directory tree until directory with a "project" subdir 
;; is found with build.properties
(defun sbt-find-path-to-project ()
  "Move up the directory tree for the current buffer until root or a directory with a project/build.properities is found."
  (interactive)
  (let ((fn (buffer-file-name)))
    (let ((path (file-name-directory fn)))
      (while (and (not (sbt-project-dir-p path))
                  (not (sbt-at-root path)))
        (setf path (file-truename (sbt-parent-path path))))
      path)))
