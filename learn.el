    (start-process "SCB" scb-buffer-name 
		   "bash"
		   "-c"
		   "grep ba learn.el")

    (start-process "SCB" scb-buffer-name 
		   "grep"
           "-n"
		   "start"
		   "learn.el")

(split-string-and-unquote
			   (replace-regexp-in-string
			    "\n" " "
			    (with-current-buffer (find-file-noselect
						  (scb-project-file-list "emacs.d"))
			      (buffer-string))))
(setq scb-current-project "emacs.d")
(scb-get-file-list)


(shell-command "etags -a scb.el")
(call-process "etags" nil nil nil "-a scb.el")
(call-process "etags" nil nil nil "-a" "scb.el")
(call-process "etags" "e:/home/Dropbox/project/silly_code_browser/input" nil nil "-a")
(delete-file "TAGS")

    (shell-command (format "xargs etags -a -o %s"
                           (scb-project-tag-file-name scb-current-project)
                           (scb-project-file-list scb-current-project)))

(shell-command "cd e:/home/Dropbox/project/silly_code_browser/; grep.exe list learn.el")



(defun my-anything-etags-create-buffer ()
  "Create buffer from tag file."
  (anything-aif (anything-etags-get-tag-file)
      (with-current-buffer (get-buffer-create "*MMM*")
        ;;(set-syntax-table (with-current-buffer anything-current-buffer
        (set-syntax-table (with-current-buffer (get-buffer-create "*MMM*")
                            (syntax-table)))
        (insert-file-contents it))
    (message "Can't find tag file: %s" it)))
(my-anything-etags-create-buffer)


(setq a (make-hash-table :test 'equal))
(puthash "base-dir" "~/../silly" a)
(puthash "base-dir" "~/my/silly" a)
(message "%S" a)
(gethash "base-dir" a)

(make-scb-config :a b :c d)
(scb-config-get )

(make-scb-config )

;; (defun make-scb-config (&rest version base-dir suffix)
(defun* make-scb-config (&key (version "0.01") (base-dir) (suffix))
  (let ((h (make-hash-table :test 'equal)))
    (puthash "version" version h)
    (puthash "base-dir" base-dir h)
    (puthash "suffix" suffix h)
    h))
(defun scb-config-get (key table)
  (gethash key table))

(setq a (make-scb-config :version 0.01 :base-dir "~/hh/aa" :suffix ".c .h"))
(scb-config-get "base-dir" a)
(scb-config-get "suffix" a)




(file-newer-than-file-p (expand-file-name (scb-project-tag-file-name scb-current-project))
			"c:/Users/luooofuq/Dropbox/project/silly_code_browser/learn.el")

