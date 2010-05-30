;; TIM |;; Keyboard bindings
;; TIM |
;; TIM |(global-set-key "\C-x\C-m" 'execute-extended-command)
;; TIM |(global-set-key "\C-c\C-m" 'execute-extended-command)
;; TIM |(global-set-key [(tab)] 'my-indent-function)
;; TIM |(global-set-key [(control i)] 'indent-region)
;; TIM |(global-set-key [f3] '(lambda() (interactive) (set-buffer (find-file "~/.emacs"))))
;; TIM |(global-set-key [f6] 'flip-to-last-buffer)
;; TIM |
;; TIM |(global-unset-key [(meta ?/)])
;; TIM |(global-set-key [(meta ?/)] 'hippie-expand)
;; TIM |
;; TIM |;; (global-unset-key [(meta ?\ )])
;; TIM |(global-unset-key [(control ?z)])
;; TIM |
;; TIM |
;; TIM |;; ;; Script run, create, open
;; TIM |;; ;; (global-set-key '[\M-a) \M- space)] 'snippet-insert-snippet)
;; TIM |
;; TIM |(global-set-key [(control ?c) (control ?s) (control ?\ )] 'snippet-run-perl-script)
;; TIM |(global-set-key [(control ?c) (control ?s) (control ?j)] 'snippet-edit-snippet)
;; TIM |;; (global-set-key [(control ?c) (control ?\ )] 'snippet-run-perl-script)
;; TIM |;; (global-set-key [(meta ?s) (meta ?s) (meta ?\ )] 'snippet-create-script-from-region)
;; TIM |;; (global-set-key [(control ?c) (control ?c) (control ?c) (control ?\ )] 'snippet-edit-snippet)
;; TIM |;; (global-set-key [(control ?x) ?n ?a] 'snippet-create-snippet-from-region)
;; TIM |;; (global-set-key [(control ?x) ?n ?s] 'snippet-create-script-from-region)
;; TIM |;; (global-set-key [(control ?x) ?n ?\"] 'snippet-make-temp-script)
;; TIM |;; (global-set-key [(control ?x) ?n ?\'] 'snippet-run-temp-perl-script)
;; TIM |
;; TIM |(global-set-key [(control ?,)] 'bs-cycle-previous)
;; TIM |(global-set-key [(control ?.)] 'bs-cycle-next)
;; TIM |(global-set-key [(meta f1)] 'kill-whitespace-in-region)
;; TIM |(global-set-key [f5] 'mode-compile)   	  
;; TIM |
;; TIM |(global-set-key "\C-c\C-t" 'insert-time)
;; TIM |(global-set-key "\C-c\C-d" 'insert-date)
;; TIM |
;; TIM |;;;;;;;;;;;;;;;;;;;;;;;;
;; TIM |
;; TIM |;; Numeric keypad.
;; TIM |
;; TIM |(global-set-key [(kp-subtract)] 'kill-current-buffer)
;; TIM |(global-set-key [(shift kp-subtract)] 'kill-current-buffer-and-window)
;; TIM |(global-set-key [(control kp-subtract)] 'delete-window)
;; TIM |(define-key help-mode-map [(kp-subtract)] 'kill-current-buffer)
;; TIM |(define-key help-mode-map [(shift kp-subtract)] 'kill-current-buffer-and-window)
;; TIM |;; (define-key list-mode-map [(kp-subtract)] 'kill-current-buffer)
;; TIM |;; (define-key list-mode-map [(shift kp-subtract)]  'kill-current-buffer-and-window)
;; TIM |
;; TIM |(require 'picture)
;; TIM |(global-set-key [(shift right)] 'picture-forward-column)	       	       	       	       	
