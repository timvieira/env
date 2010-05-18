;;
(eval-when-compile (require 'cl))

(require 'filesets)

(defun filesets (&rest sets)
  (setq filesets-data '())
  (dolist (s sets filesets-data)
    (setq 
     filesets-data 
     (append filesets-data 
             (list (list (car s) 
                   (eval (cadr s))))))))

(defun under (root &rest paths)
  (let ((n))
    (dolist (p paths n)
      (setq n 
            (append n 
                    (list
                     (concat (eval root) p)))))))

(defun files (paths)
  (let ((n '(:files)))
    (dolist (p paths n)
      (setq n (append n (list (eval p)))))))


(defconst *emacs-config* (expand-file-name "~/adams-worldview/mananged-repos/projects/emacs-support/"))

(defconst *eclipse-ws* (expand-file-name "~/fs/eclipse-workspace/"))
(defconst *rexo* (concat *eclipse-ws* "rexo/"))
(defconst *rexa-common* (concat *eclipse-ws* "rexa-common-trunk/"))

(defconst *rexa-front* (concat *eclipse-ws* "rexa-front-trunk/"))
(defconst *rexa-front-java* (concat *rexa-front* "src/java/"))
(defconst *rexa-front-war* (concat *rexa-front* "src/war/"))
(defconst *rexa-front-gwt-src* (concat *rexa-front* "src/java/org/rexa/gwt/"))

    
(filesets
 '("emacs/java"
   (files 
    (under *emacs-config*
           "lisp/my-java-config.el")))

 '("home/rc"
   (files 
    (under "~/"
           ".bashrc")))

 '("gwt/xml"
   (files
    (under *rexa-front-gwt-src* 
           "paper/PaperHomepage.gwt.xml"
           "author/AuthorHomepage.gwt.xml"
           "grant/GrantHomepage.gwt.xml"
           "topic/TopicHomepage.gwt.xml")))
 
 '("org"
   (files
    (under "~/"
           "adams-worldview/managed-repos/projects/org-files/journal.org"
           "adams-worldview/managed-repos/projects/org-files/writing.org"
           "fs/eclipse-workspace/rexo/docs/public/org/schedule.org")))

 '("rexa/jsp"
   (files (list 
           (concat *rexa-front-war* "WEB-INF/jsp"))))

 '("gwt/paper"
   (files (list
           (concat *rexa-front-gwt-src* "paper/public/paperhomepage.html")
           (concat *rexa-front-war* "css/common.css")
           (concat *rexa-front-war* "www/org.rexa.gwt.paper.PaperHomepage/paperhomepage.html"))))
 
 '("gwt/welcome"
   (files (list
           (concat *rexa-front-gwt-src* "welcome/public/welcome.html")
           (concat *rexa-front-war* "css/common.css")
           (concat *rexa-front-war* "www/org.rexa.gwt.welcome.Welcome/welcome.html")) ))
 
 '("gwt/author"
   (files (list 
           (concat *rexa-front-gwt-src* "author/public/authorhomepage.html")
           (concat *rexa-front-war* "css/common.css")
           (concat *rexa-front-war* "www/org.rexa.gwt.author.AuthorHomepage/authorhomepage.html")) ))
 
 '("gwt/grant"
   (files (list
           (concat *rexa-front-gwt-src* "grant/public/granthomepage.html")
           (concat *rexa-front-war* "css/common.css")
           (concat *rexa-front-war* "www/org.rexa.gwt.grant.GrantHomepage/granthomepage.html")) ))
 
 '("gwt/search"
   (files (list
           (concat *rexa-front-gwt-src* "results/public/results.html")
           (concat *rexa-front-gwt-src* "results/public/PaperResultItem.html")
           (concat *rexa-front-war* "css/common.css")
           (concat *rexa-front-war* "www/org.rexa.gwt.results.Results/results.html")
           (concat *rexa-front-war* "www/org.rexa.gwt.results.Results/PaperResultItem.html"))
          ))
 
 
 '("gwt/topic"
   (files (list
           (concat *rexa-front-gwt-src* "topic/public/topichomepage.html")
           (concat *rexa-front-war* "css/common.css")
           (concat *rexa-front-war* "www/org.rexa.gwt.topic.TopicHomepage/topichomepage.html"))
          ))
 
 '("gwt/venue"
   (files (list
           (concat *rexa-front-gwt-src* "venue/public/venuehomepage.html")
           (concat *rexa-front-war* "css/common.css")
           (concat *rexa-front-war* "www/org.rexa.gwt.venue.VenueHomepage/venuehomepage.html"))
          ))
 
 '("rexa/spring/config"
   (files 
    (under (concat *rexo* "src/")
           "test/org/rexa/pipeline/spring-pipeline-acceptance-test-context.xml"
           "org/rexa/persistence/spring/spring-datasource-context.xml"
           "org/rexa/pipeline/spring-command-chain-context.xml"
           "org/rexo/coreference/graph/spring-coreference-graph-context.xml")
    ))
 
 '("projects/flash/fundamentals"
   (files (list
           "~/adams-worldview/projects/programming/flash-flex/fundamentals")
          ))
 )

(setq filesets-external-viewers
      '(
        ;; ("^.+\\..?html?$" browse-url
        ;; ((:ignore-on-open-all t)))
        ("^.+\\.pdf$" "xpdf"
         ((:ignore-on-open-all t)
          (:ignore-on-read-text t)
          (:constraint-flag "xpdf")))
        ("^.+\\.e?ps\\(.gz\\)?$" "ggv"
         ((:ignore-on-open-all t)
          (:ignore-on-read-text t)
          (:constraint-flag "ggv")))
        ("^.+\\.dvi$" "xdvi"
         ((:ignore-on-open-all t)
          (:ignore-on-read-text t)
          (:constraint-flag "xdvi")))
        ("^.+\\.doc$" "antiword"
         ((:capture-output t)
          (:ignore-on-read-text t)
          (:constraint-flag "antiword")))
        ("^.+\\.\\(tiff\\|xpm\\|gif\\|pgn\\)$" "gqview"
         ((:ignore-on-open-all t)
          (:ignore-on-read-text t)
          (:constraint-flag "gqview")))))

(provide 'filesets-defs)



