;; TIM |;;
;; TIM |(eval-when-compile (require 'cl))
;; TIM |
;; TIM |(require 'filesets)
;; TIM |
;; TIM |(defun filesets (&rest sets)
;; TIM |  (setq filesets-data '())
;; TIM |  (dolist (s sets filesets-data)
;; TIM |    (setq 
;; TIM |     filesets-data 
;; TIM |     (append filesets-data 
;; TIM |             (list (list (car s) 
;; TIM |                   (eval (cadr s))))))))
;; TIM |
;; TIM |(defun under (root &rest paths)
;; TIM |  (let ((n))
;; TIM |    (dolist (p paths n)
;; TIM |      (setq n 
;; TIM |            (append n 
;; TIM |                    (list
;; TIM |                     (concat (eval root) p)))))))
;; TIM |
;; TIM |(defun files (paths)
;; TIM |  (let ((n '(:files)))
;; TIM |    (dolist (p paths n)
;; TIM |      (setq n (append n (list (eval p)))))))
;; TIM |
;; TIM |
;; TIM |(defconst *emacs-config* (expand-file-name "~/adams-worldview/mananged-repos/projects/emacs-support/"))
;; TIM |
;; TIM |(defconst *eclipse-ws* (expand-file-name "~/fs/eclipse-workspace/"))
;; TIM |(defconst *rexo* (concat *eclipse-ws* "rexo/"))
;; TIM |(defconst *rexa-common* (concat *eclipse-ws* "rexa-common-trunk/"))
;; TIM |
;; TIM |(defconst *rexa-front* (concat *eclipse-ws* "rexa-front-trunk/"))
;; TIM |(defconst *rexa-front-java* (concat *rexa-front* "src/java/"))
;; TIM |(defconst *rexa-front-war* (concat *rexa-front* "src/war/"))
;; TIM |(defconst *rexa-front-gwt-src* (concat *rexa-front* "src/java/org/rexa/gwt/"))
;; TIM |
;; TIM |    
;; TIM |(filesets
;; TIM | '("emacs/java"
;; TIM |   (files 
;; TIM |    (under *emacs-config*
;; TIM |           "lisp/my-java-config.el")))
;; TIM |
;; TIM | '("home/rc"
;; TIM |   (files 
;; TIM |    (under "~/"
;; TIM |           ".bashrc")))
;; TIM |
;; TIM | '("gwt/xml"
;; TIM |   (files
;; TIM |    (under *rexa-front-gwt-src* 
;; TIM |           "paper/PaperHomepage.gwt.xml"
;; TIM |           "author/AuthorHomepage.gwt.xml"
;; TIM |           "grant/GrantHomepage.gwt.xml"
;; TIM |           "topic/TopicHomepage.gwt.xml")))
;; TIM | 
;; TIM | '("org"
;; TIM |   (files
;; TIM |    (under "~/"
;; TIM |           "adams-worldview/managed-repos/projects/org-files/journal.org"
;; TIM |           "adams-worldview/managed-repos/projects/org-files/writing.org"
;; TIM |           "fs/eclipse-workspace/rexo/docs/public/org/schedule.org")))
;; TIM |
;; TIM | '("rexa/jsp"
;; TIM |   (files (list 
;; TIM |           (concat *rexa-front-war* "WEB-INF/jsp"))))
;; TIM |
;; TIM | '("gwt/paper"
;; TIM |   (files (list
;; TIM |           (concat *rexa-front-gwt-src* "paper/public/paperhomepage.html")
;; TIM |           (concat *rexa-front-war* "css/common.css")
;; TIM |           (concat *rexa-front-war* "www/org.rexa.gwt.paper.PaperHomepage/paperhomepage.html"))))
;; TIM | 
;; TIM | '("gwt/welcome"
;; TIM |   (files (list
;; TIM |           (concat *rexa-front-gwt-src* "welcome/public/welcome.html")
;; TIM |           (concat *rexa-front-war* "css/common.css")
;; TIM |           (concat *rexa-front-war* "www/org.rexa.gwt.welcome.Welcome/welcome.html")) ))
;; TIM | 
;; TIM | '("gwt/author"
;; TIM |   (files (list 
;; TIM |           (concat *rexa-front-gwt-src* "author/public/authorhomepage.html")
;; TIM |           (concat *rexa-front-war* "css/common.css")
;; TIM |           (concat *rexa-front-war* "www/org.rexa.gwt.author.AuthorHomepage/authorhomepage.html")) ))
;; TIM | 
;; TIM | '("gwt/grant"
;; TIM |   (files (list
;; TIM |           (concat *rexa-front-gwt-src* "grant/public/granthomepage.html")
;; TIM |           (concat *rexa-front-war* "css/common.css")
;; TIM |           (concat *rexa-front-war* "www/org.rexa.gwt.grant.GrantHomepage/granthomepage.html")) ))
;; TIM | 
;; TIM | '("gwt/search"
;; TIM |   (files (list
;; TIM |           (concat *rexa-front-gwt-src* "results/public/results.html")
;; TIM |           (concat *rexa-front-gwt-src* "results/public/PaperResultItem.html")
;; TIM |           (concat *rexa-front-war* "css/common.css")
;; TIM |           (concat *rexa-front-war* "www/org.rexa.gwt.results.Results/results.html")
;; TIM |           (concat *rexa-front-war* "www/org.rexa.gwt.results.Results/PaperResultItem.html"))
;; TIM |          ))
;; TIM | 
;; TIM | 
;; TIM | '("gwt/topic"
;; TIM |   (files (list
;; TIM |           (concat *rexa-front-gwt-src* "topic/public/topichomepage.html")
;; TIM |           (concat *rexa-front-war* "css/common.css")
;; TIM |           (concat *rexa-front-war* "www/org.rexa.gwt.topic.TopicHomepage/topichomepage.html"))
;; TIM |          ))
;; TIM | 
;; TIM | '("gwt/venue"
;; TIM |   (files (list
;; TIM |           (concat *rexa-front-gwt-src* "venue/public/venuehomepage.html")
;; TIM |           (concat *rexa-front-war* "css/common.css")
;; TIM |           (concat *rexa-front-war* "www/org.rexa.gwt.venue.VenueHomepage/venuehomepage.html"))
;; TIM |          ))
;; TIM | 
;; TIM | '("rexa/spring/config"
;; TIM |   (files 
;; TIM |    (under (concat *rexo* "src/")
;; TIM |           "test/org/rexa/pipeline/spring-pipeline-acceptance-test-context.xml"
;; TIM |           "org/rexa/persistence/spring/spring-datasource-context.xml"
;; TIM |           "org/rexa/pipeline/spring-command-chain-context.xml"
;; TIM |           "org/rexo/coreference/graph/spring-coreference-graph-context.xml")
;; TIM |    ))
;; TIM | 
;; TIM | '("projects/flash/fundamentals"
;; TIM |   (files (list
;; TIM |           "~/adams-worldview/projects/programming/flash-flex/fundamentals")
;; TIM |          ))
;; TIM | )
;; TIM |
;; TIM |(setq filesets-external-viewers
;; TIM |      '(
;; TIM |        ;; ("^.+\\..?html?$" browse-url
;; TIM |        ;; ((:ignore-on-open-all t)))
;; TIM |        ("^.+\\.pdf$" "xpdf"
;; TIM |         ((:ignore-on-open-all t)
;; TIM |          (:ignore-on-read-text t)
;; TIM |          (:constraint-flag "xpdf")))
;; TIM |        ("^.+\\.e?ps\\(.gz\\)?$" "ggv"
;; TIM |         ((:ignore-on-open-all t)
;; TIM |          (:ignore-on-read-text t)
;; TIM |          (:constraint-flag "ggv")))
;; TIM |        ("^.+\\.dvi$" "xdvi"
;; TIM |         ((:ignore-on-open-all t)
;; TIM |          (:ignore-on-read-text t)
;; TIM |          (:constraint-flag "xdvi")))
;; TIM |        ("^.+\\.doc$" "antiword"
;; TIM |         ((:capture-output t)
;; TIM |          (:ignore-on-read-text t)
;; TIM |          (:constraint-flag "antiword")))
;; TIM |        ("^.+\\.\\(tiff\\|xpm\\|gif\\|pgn\\)$" "gqview"
;; TIM |         ((:ignore-on-open-all t)
;; TIM |          (:ignore-on-read-text t)
;; TIM |          (:constraint-flag "gqview")))))
;; TIM |
;; TIM |(provide 'filesets-defs)
;; TIM |


