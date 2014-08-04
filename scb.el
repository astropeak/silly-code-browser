;;;; Installation
;;(add-to-list 'load-path "~/work/projects/silly_code_browser")
;;(require 'scb)

;; TODO: history-print: highlight the header of current line if it is not the child of previous line.

(defconst scb-version "scb version 0.20. Done some refactory: don't use the unix command `grep' and `find' in this program, use lisp function and perl instead to make this program more protable. Add feature: update project file list. Fix a bugs of scb-history-print, change the header of the printed bookmakr in history buffer. Add scb-history-print, provide and require utils. Add scb-dump-variable. Add version function. Refactory the bookmark format. Refactor the code, escepicial codes related to tree, fix many bugs.. Add save and load history. ...,. Fix a bug. Improve history function, adding save the current pos. Add jump back function")
(defvar scb-current-project nil)
(defconst scb-root-dir "~/.silly_code_browser" "The root directory of scb, all projects is a sub directory of this directory")
(defconst scb-file-list-name "file_list")
(defconst scb-history-file-name "history")
(defconst scb-tag-file-name "TAGS")
(defconst scb-buffer-name "*SCB*")
(defconst scb-history-buffer-name "*SCB HISTORY*")
(defconst scb-project-config-file-name "config")

(defvar scb-tag-file-ok-p nil)

;; DONE[No problem]: BUG: Adding bookmark under head node is wrong, two bookmarks will be added.
;; DONE: BUGS on history bookmark: all projects share a single scb-jump-history during a session. Should be when open a project, this project's bookmark should be loaded.
;; TODO: BUGS: 1.[FIXED] when history is saved into file, the current positon of bookmark is not saved. 2.[FIXED] If there are two subtree under the head node, then after enter a subtree by "\C-q i", you can not enter to another subtree. The problem is, you can not back to head node. 3. Even a file is not in the current projcet, it will be added to history when jumping.
;; DONE: After open a project, jump to the current history bookmark.

;; DONE: BUGS on history bookmark: 1.[FIXED] The first bookmark can't be deleted. 2[FIXED]. When deleted the last bookmark, then you can't jump to the previous bookmark because scb-jump-current is set to nil.

;; DONE: Add save history in a file.
;; DONE: calculate line count of a file in elisp instead of call "wc" command.

(require 'tree)
(require 'find-lisp)
;; DONE: jump history not save the current position before jump, it only save the goto position.

;; TODO: the jump history function can be extracted as a single file.
;; DONE: Maybe we can use equal to compare elements in the tree, then access to a element will much easier(random access). If a same element is added(this will not happen, since every element has a current time element), first delete the original element, and then add the element at current position.

;;(setq scb-jump-history (tree-create))
;;(setq scb-jump-current scb-jump-history)
(defvar scb-jump-history (tree-create) "a list that save the jump trace, a element is a list of filename and line number and the content")
(defvar scb-jump-current (car scb-jump-history) "the current element in the jump trace")
;; DONE: jump history forward and backward.
(defvar scb-current-file nil "the current of file name of the project, used to save it to history before jump. The line number will be get from the current pos.")

(defun scb-version ()
  (interactive)
  (message "%s" scb-version))

;; scb bookmark format
(defstruct scb-bookmark 
  fname linum content time)

;; TODO: think about how to handle the wrong type parameter
(defun scb-project-dir (project)
  "Get the directory of project"
  (and (stringp project)
       (concat scb-root-dir "/" project)))

(defun scb-project-file-list (project)
  "Get the file list file of project"
  (and (stringp project)
       (concat (scb-project-dir project) "/" scb-file-list-name)))

(defun scb-project-config-file (project)
  "Get the config file path of project"
  (and (stringp project)
       (concat (scb-project-dir project) "/" scb-project-config-file-name)))

(defun scb-project-history (project)
  "Get the history file of project"
  (and (stringp project)
       (concat (scb-project-dir project) "/" scb-history-file-name)))

(defun scb-project-tag-file-name (project)
  "Get the tag file name of the project"
  (and (stringp project)
       (concat (scb-project-dir project) "/" scb-tag-file-name)))

(defun scb-project-exists-p (project)
  "Whether `project' is exists or not"
  (and (stringp project)
       (file-exists-p (scb-project-dir project))))

(defun scb-project-file-count (project)
  "Calculate the file count of `project'. If `project' not exist, then return nil"
  (and (file-exists-p (scb-project-file-list project))
       (with-current-buffer 
	   (find-file-noselect 
	    (scb-project-file-list project))
	 (count-lines 1 (point-max)))))


(defun scb-create-project (project dir suffix)
  "project is the project name to be created. dir is the root dir of this project. 
suffix is the file suffix to be mattched, multiple suffixes seperated by blanks."
  (interactive "sProject name: \nDRoot dir: \nsSuffix: ")

  (or (file-exists-p scb-root-dir) (mkdir scb-root-dir))
  (if (scb-project-exists-p project)
      (progn
	(message "Error: project %s already exist" project))

    (mkdir (scb-project-dir project))
    (setq scb-current-project project)

    ;; add file
    (scb-add-files dir suffix)
    
    ;; init the history file
    (scb-history-clear)
    (scb-history-save)

    ;; save the config data
    (with-current-buffer (find-file-noselect (scb-project-config-file project))
      (insert (format "%S" (list dir suffix)))
      (save-buffer))

    (scb-open-project project)
    (message "Project %s created, %s files added." 
	     project 
	     (scb-project-file-count scb-current-project))
    ))

(defun scb-update-project ()
  "Update a project, such as the file list"
  (interactive)
  (if (file-exists-p (scb-project-config-file scb-current-project))
      (with-current-buffer (find-file-noselect (scb-project-config-file scb-current-project))
	(goto-char (point-min))
	(let* ((config (read (current-buffer)))
	       (base-dir (nth 0 config))
	       (suffix (nth 1 config)))
	  (scb-add-files base-dir suffix 'overwrite)))
    (message "Config file not exist, not update")))

;;(scb-create-project "elisp" "~/.emacs.d" "el")

(defun point-to-linum (point)
  "Convert a point value to line number"
  (count-lines 1 (1+ point))
  )

(defun scb-jump-to-bookmark (bookmark &optional fn)
  "Jump to a bookmark, its type is scb-bookmark. If the line number is changed, using content to jump to the new line, and update the line number in the bookmark, and return the updateed bookmark. This function is destructive, cause it modify `scb-jump-history'. `fn' is the funtion that select a file, it can be `find-file', `find-file-other-window'. If omitted, `find-file' will be used."
  (or (scb-bookmark-p bookmark)
      (error "Parameter wrong: %s not a bookmark" bookmark))

  (or fn (setq fn 'find-file))
  (funcall fn (scb-bookmark-fname bookmark))

  ;; adjust the position in case the code is modifyed.
  (let ((f)
	(b)
	(m (scb-bookmark-linum bookmark))
	(bmk))

    (goto-line m)

    (move-beginning-of-line nil)
    (if (search-forward (scb-bookmark-content bookmark)
				 (point-max) t)
	(setq f (point-to-linum (point)))
      (setq f 9999999))

    (move-beginning-of-line nil)
    (if (search-backward (scb-bookmark-content bookmark)
				(point-min) t)
	(setq b (point-to-linum (point)))
      (setq b -9999999))

    ;; here choose a line number closer to bookmark line number in f and b
    (if (and (= f 9999999) (= b -9999999))
	(message "Waring: can't jump to bookmark %s, its content(%s) not found, just jump to the original line" 
		 bookmark (scb-bookmark-content bookmark))

      ;; else update the line number
      (if (<= (- f m) (- m b))
	  (setq m f)
	(setq m b)
	))

    (goto-line m)

    ;; TODO: does the content need to be updated?
    ;; To update the bookmark, we must modify the element in the tree, so we need to find the pointer to it in the tree.
    ;; This can be optimized, when line-number is not changed, then the blow code is not needed
    (setq bmk (tree-get-element scb-jump-history bookmark))
    (setf (scb-bookmark-linum bmk) m)
    bmk))

(defun scb-jump-to-bookmark-other-window (bookmark)
  "Same as scb-jump-to-bookmark, but jump to the bookmakr other window"
  (scb-jump-to-bookmark bookmark 'find-file-other-window))

(defun scb-open-project (project)
  "Open a project"
  (interactive 
   (list
    (completing-read 
    "Project name: " (directory-files scb-root-dir) nil t)))

  (if (and (not (equal project "")) 	;if not char is entered, this will be a empty string.
	   (scb-project-exists-p project))
      (progn
	(setq scb-current-project project)

	;; load the history. The current history value need not be saved because it is already saved in the history file.
	(if (file-exists-p (scb-project-history project))
	    (scb-history-load)
	  ;; history file not exist, so init it.
	  (scb-history-clear)
	  (scb-history-save))

	;; jump to the current bookmark
	(or (tree-head-element-p scb-jump-current)
	    (scb-jump-to-bookmark scb-jump-current))

	;; prepare tag file
	(if (file-exists-p (scb-project-tag-file-name scb-current-project))
	    (scb-setup-tag)
	  (scb-create-tag-table))
 
	(message "Project %s opened, %d files in the project." 
		 project 
		 (scb-project-file-count scb-current-project)))
    (message "Error: project %s not exist." project)))


(defun scb-delete-project (project)
  "Delete a project."
  (interactive 
   (list
    (completing-read 
     "Name: " (directory-files scb-root-dir) nil t)))

  (and (= (length project) 0)
       (setq project "NO_NAME"))
  (if (and (stringp project) (> (length project) 0)
	   (scb-project-exists-p project))
      (progn
	(shell-command (concat "rm -rv " 
			       (scb-project-dir project)))
	(message "Project %s deleted" project))
    (message "Error: project %s not exist" project)))

(defun scb-show-project-file-list ()
  "Show file list of current project"
  (interactive)
  
  (find-file (scb-project-file-list scb-current-project)))


(defun scb-make-regex-suffix (suffix)
  "The suffix is \".c .sh lisp\" and so on, it can has . or without ., and convert it to the format for -regex value of find command"
  (concat "\".*\\("
	  (mapconcat (lambda (str)
		       (replace-regexp-in-string "\\." "\\\\\." str))
		     (split-string suffix "[ \t]+" t)
		     "\\|")
	  "\\)\""))


(defun scb-add-files (dir suffix &optional mode)
  "Add files to the current project. Mode can be 'append(default value) or 'overwrite"
  (interactive "DRoot dir: \nsSuffix: ")

  (or mode (setq mode 'append))

  (if (scb-project-exists-p scb-current-project)
      (progn
	(with-current-buffer (find-file-noselect 
			      (scb-project-file-list scb-current-project))
	  (if (eq mode 'overwrite)
	      (erase-buffer))

	  (goto-char (point-max))
	  (insert "\n")

	  (mapcar (lambda (line)
		    (insert (format "\"%s\"\n" line)))
		  (find-lisp-find-files
		   dir
		   (concat (replace-regexp-in-string
			    "\"" ""
			    (scb-make-regex-suffix suffix))
			   "$")))

	  (save-buffer))
	;; TODO: make sure all lines of the file is unique
	)
    (message "Error: project %s not exist" scb-current-project)))


(defun scb-file-exist-in-project-p (file project)
  "Check whether the `file' is in the `project'"
  (with-current-buffer (find-file-noselect 
			(scb-project-file-list project))
    (save-excursion
      (goto-char 0)
      (search-forward file nil t))))


(defun scb-history-set-current-file ()
  (interactive)
  ;; TODO: bugs. can't select the correct file. 
  ;; Because jump will be done in *SCB* buffer/window, so we can consider the file in the recent window is the current file.
  ;; Current this only be ok for there are only two windows. If there are more than two windows, then the selection might be wrong.
  (let ((window (selected-window))
	(file (buffer-file-name (other-window 1))))
    (message "in scb-history-set-current-file: file=%s" file)
    (and 
     (stringp file)
     (scb-file-exist-in-project-p file scb-current-project)
     (setq scb-current-file file))
    (select-window window)))

(defun scb-history-get-current-file ()
  scb-current-file)

(defun scb-search-text-i (project pattern)
  (interactive 
   (list
    (completing-read 
     "Project: " (directory-files scb-root-dir) nil t)
    (read-string (format "Search text:[%s] " 
			 (thing-at-point 'symbol) )
		 nil nil
		 (thing-at-point 'symbol))))

  ;;(switch-to-buffer-other-window (get-buffer-create scb-buffer-name))
  (scb-recover-buffer)
  (with-current-buffer (get-buffer-create scb-buffer-name)
    (erase-buffer)
    
    (insert (format "project: %s, pattern: %s\n\n" 
		    project pattern
		    ))
    ;;(shell-command-to-string "date")
    (start-process "SCB" scb-buffer-name 
		   "bash"
		   "-c"
		   (format 
		    "perl-grep.pl %s `cat %s`"

		    ;; Fix a bug: when the pattern is enclosed by "", then search it literal
		    (if (and (string= (substring pattern 0 1) "\"")
			     (string= (substring pattern -1) "\"")
			     )
			(progn 
			  (message "%s" pattern)
			  pattern)
		      (scb-make-distribution-pattern pattern) ;TODO: this line is not used... the pattern is directly returned by the next line
		      pattern)
		    (scb-project-file-list project)
		    ))

    (goto-char 0)
    (scb-mode)
    ;;(scb-redisplay-buffer-1)
    ;;(scb-redisplay-buffer)
    ))

(defun scb-search-text (pattern)
  (interactive 
   (list
    (read-string (format "Search text(%s):[%s] " 
			 scb-current-project
			 (thing-at-point 'symbol) )
		 nil nil
		 (thing-at-point 'symbol))))

  (scb-search-text-i scb-current-project pattern)
  )

(defun scb-history-construct-bookmark (fname line-number)
  "Construct a bookmark element"
  (list fname line-number (current-time)))

(defun scb-history-parse-bookmark (bookmark)
  "Parse a bookmark element, return the fname and line-number as a list" 
  (list (nth 0 bookmark) (nth 1 bookmark)))


(defun scb-history-add-i (bookmark)
  "Add the `bookmark' to the history list. If it equals to the current saved one, then not add"
  ;; check parameter
  (or (scb-bookmark-p bookmark)
      (error "Parameter wrong: %s not a scb-bookmark type" bookmark))

  (or 
   ;; check `bookmark' not the same as `scb-jump-current'
   (and 
    (scb-bookmark-p scb-jump-current)
    (equal (scb-bookmark-fname bookmark)
	   (scb-bookmark-fname scb-jump-current))
    (equal (scb-bookmark-linum bookmark)
	   (scb-bookmark-linum scb-jump-current)))
     
   (progn
     (tree-add-element scb-jump-history 
		       scb-jump-current
		       bookmark)
     (setq scb-jump-current bookmark)
       
     ;; save the modification to file
     (scb-history-save))))

(defun scb-history-add ()
  "Add current file and pos to history"
  (interactive)

  (scb-history-add-i 
   (make-scb-bookmark 
    :fname (buffer-file-name (current-buffer))
    :linum (count-lines 1 (1+ (point)))
    :content (buffer-substring (line-beginning-position) (line-end-position))
    :time (current-time))))

(defun scb-history-reset ()
  "Reset the current position to the head of history"
  (interactive)
  (setq scb-jump-current (car scb-jump-history)))

(defun scb-history-clear ()
  "Clear all history bookmarks"
  (interactive)
  (setq scb-jump-history (tree-create))
  (setq scb-jump-current (car scb-jump-history)))


(defun scb-history-load ()
  "Load history form history file"
  (interactive)
  (load-file (scb-project-history scb-current-project))

  ;; This is not needed
  ;; ;; Modify the current jump position, make it point to a element in scb-jump-history instead of a new created element. Because el is used to compare element when navigating through the history list, so this modification is needed. 
  ;; ;; DONE: Maybe we can use equal to find element in the history list because each elemetn is unique in the list.
  ;; (setq scb-jump-current
  ;; 	(car (tree-get-element-and-parent scb-jump-history 
  ;; 					  (car scb-jump-current) 'equal)))

  )

(defun scb-history-delete ()
  "Delete current element from history"
  (interactive)
  (setq scb-jump-current 
	(tree-delete-element scb-jump-history scb-jump-current))

  ;; after delete the bookmark, jump to the new current pos
  (or (tree-head-element-p scb-jump-current)
      (scb-jump-to-bookmark scb-jump-current))

  ;; save the modification to file
  (scb-history-save)
  )


(defun scb-dump-variable (variable &optional limit)
  ;; Copy from `recentf-dump-variable'
  "Insert a \"(setq VARIABLE value)\" in the current buffer.
When the value of VARIABLE is a list, optional argument LIMIT
specifies a maximum number of elements to insert.  By default insert
the full list."
  (let ((value (symbol-value variable)))
    (if (atom value)
        (insert (format "\n(setq %S '%S)\n" variable value))

      ;; (when (and (integerp limit) (> limit 0))
      ;;   (setq value (recentf-trunc-list value limit)))

      (insert (format "\n(setq %S\n      '(" variable))
      (dolist (e value)
        (insert (format "\n        %S" e)))
      (insert "\n        ))\n"))))


(defun scb-history-save ()
  "Save the history list to a file. Then this file can be used to restore the history list when the project is opened next time."
  (interactive)
  (let ((buffer (find-file-noselect (scb-project-history scb-current-project)))
	)
    
    (with-current-buffer buffer
      (erase-buffer)
      
      (scb-dump-variable 'scb-jump-history)

      (scb-dump-variable 'scb-jump-current)

      ;; (insert "(setq scb-jump-history '")
      ;; (insert (format "%s" scb-jump-history))
      ;; (insert ")\n\n")
      
      ;; currently the current jump postion can't be saved(if ), only set it to the head of the history list.
      ;;(insert "(setq scb-jump-current scb-jump-history)\n\n")

      (save-buffer)
      )
    
    )
  )

(defvar scb-history-header-function '(lambda (dpt)
				       (if (equal dpt 0)
					   " "
					 (format "[%02d] " dpt))))
;;"^[*]+[ ]+"
(defvar scb-history-header-pattern "^\\[[0-9][0-9]\\] *")


;; mode
(define-derived-mode scb-history-mode text-mode "Scb-History")
(define-key scb-history-mode-map (kbd "g") 'scb-history-goto)
(define-key scb-history-mode-map (kbd "n") 'next-line)
(define-key scb-history-mode-map (kbd "p") 'previous-line)
(define-key scb-history-mode-map (kbd "v") 'scb-history-view)
(define-key scb-history-mode-map (kbd "t") 'scb-history-toggle-display)

(defun scb-history-toggle-display ()
  "Toggle display style of header of bookmarks in history buffer"
  (interactive)
  (if (string= scb-history-header-pattern "^\\[[0-9][0-9]\\] *")
      (progn (setq scb-history-header-pattern "^[*]+[ ]+")
	     (setq scb-history-header-function 
		   '(lambda (dpt)
		      (format "%s " (make-string dpt ?*)))))
    (progn (setq scb-history-header-pattern "^\\[[0-9][0-9]\\] *")
	   (setq scb-history-header-function 
		 '(lambda (dpt)
		    (if (equal dpt 0)
			" "
		      (format "[%02d] " dpt)))))
    )

  (scb-history-print)
  )

(defun scb-history-print ()
  "Show history value of current project in `scb-history-buffer-name'."
  (interactive)
  
  
  (let ((buf (get-buffer-create scb-history-buffer-name)))
    (or (eq buf (current-buffer))
	(switch-to-buffer-other-window buf)))

  (toggle-read-only -1)
  (erase-buffer)
  ;;(org-mode)
  (insert (format "Scb Bookmark History. Project: %s\n" scb-current-project))
  (tree-print scb-jump-history
	      (lambda (e)
		(if (scb-bookmark-p e)
		    (format "%S\n" e)

		    ;; (format "%S:+:%S:+:%S:+:%S"
		    ;; 	    ;;(scb-shorter-str (replace-regexp-in-string "^[^:]*/" "" (scb-bookmark-fname e)) 16)
		    ;; 	    (scb-bookmark-fname e)
		    ;; 	    (scb-bookmark-linum e)
		    ;; 	    (scb-bookmark-content e)
		    ;; 	    ;;(format-time-string "%y/%m/%d %H:%M" (scb-bookmark-time e))
		    ;; 	    (scb-bookmark-time e)
		    ;; 	    )
		  (format "%s\n" e)))
	      scb-history-header-function)
  (toggle-read-only 1)

  (scb-history-parse-buffer)
  (goto-line 3)
  (scb-history-mode)
  (toggle-truncate-lines 1)
  )

(defun scb-history-parse-buffer ()
  "Redisplay the history buffer"
  (interactive)
  (with-current-buffer (get-buffer scb-history-buffer-name)
    (goto-char 0)

    (let ((bmk) (from) (to) (ovl))
      (while (setq from (re-search-forward scb-history-header-pattern (point-max) t))
	(setq bmk (read (current-buffer)))
	
	(unless (overlays-in from (line-end-position))
	  (setq ovl
		(make-overlay from (line-end-position)))
	  (overlay-put ovl
	   'display (if (scb-bookmark-p bmk)
			(format "%-21s %s"
				(scb-shorter-str
				 (format "%s:%d"
					 (scb-shorter-str (replace-regexp-in-string "^[^:]*/" "" (scb-bookmark-fname bmk)) 16)
					 (scb-bookmark-linum bmk)) 21)
				(replace-regexp-in-string 
				 "^[ \t]*" "" 
				 (scb-bookmark-content bmk))
				)
		      "Not a scb bookmark"))
	  (overlay-put ovl
	   'face 'font-lock-keyword-face))

	))))

(defun scb-history-goto ()
  "Goto the bookmark under curser in `scb-history-buffer-name'"
  (interactive)
  
  (or (eq (current-buffer)
	  (get-buffer scb-history-buffer-name))
      (error "Buffer should be %s" scb-history-buffer-name))

  (let ((bmk))
    (save-excursion
      (beginning-of-line)
      (re-search-forward scb-history-header-pattern (point-max) t)
      (setq bmk (read (current-buffer)))
      (setq scb-jump-current
          (scb-jump-to-bookmark-other-window bmk))
      )))


(defun scb-history-view ()
  "View a bookmark other window without selected, and move cursor to next line"
  (interactive)
  (let ((window (selected-window)))
    (when (scb-history-goto)
      (select-window window)
      (forward-line))))

(defun scb-goto-file ()
  "Goto file other window.
   TODO: this function should be refactored. The let part is not universal."
  (interactive)
  (if (eq (current-buffer) (get-buffer scb-buffer-name))
      (progn
	(move-beginning-of-line nil)
	(let* ((pos (scb-get-pos (point)))
	       (fname (buffer-substring (point) (1- (nth 0 pos))))
	       (line-number (string-to-number 
			     (buffer-substring (nth 0 pos) (1- (nth 1 pos)))))
	       (content (buffer-substring (nth 2 pos) (1- (nth 3 pos))))
	       (bmk))

	  ;; Save the current file before going
	  (scb-history-set-current-file)

	  ;; Record the jump history. 
	  ;; Change algorithm. Also saved the position before jumping
	  ;; Save the current file and position
	  (and scb-current-file
	       (with-current-buffer (find-file-noselect scb-current-file)
		 (scb-history-add)))

	  ;; Record the jump history. 
	  ;; Save the position after jump.
	  (scb-history-add-i 
	   (setq bmk (make-scb-bookmark 
		      :fname fname
		      :linum line-number
		      :content content
		      :time (current-time))))
	  
	  ;; TODO: here should be refactor: using scb-jump-to-bookmark-other-window.
	  (scb-jump-to-bookmark-other-window bmk)

	  t
	  ))
    
    (message "Not in %s buffer" scb-buffer-name)
    nil
    ))

(defun scb-previous-jump ()
  "Goto the provious jump trace"
  (interactive)
  (if (tree-head-element-p scb-jump-current)
      (message "No previous bookmark")	
    (let ((parent (tree-get-parent scb-jump-history scb-jump-current)))
      (if (tree-head-element-p parent)
	  (message "No previous bookmark")	

	(scb-jump-to-bookmark parent)
	;;(message "previous pos=%s" pos)
	)
      (setq scb-jump-current parent))))

(defun scb-next-jump ()
  "Goto the next jump trace"
  (interactive)

  (let* ((children (tree-get-children scb-jump-history scb-jump-current))
	 (select-list
	  (mapcar (lambda (bookmark)
		    (format "%s:%d %s" 
			    (scb-shorter-str 
			     (replace-regexp-in-string "^[^:]*/" "" (scb-bookmark-fname bookmark)) 16)
			    
			    (scb-bookmark-linum bookmark)
			    (scb-bookmark-content bookmark)))
		  children))
	 (select))

    (if children
	(progn 
	  ;; do the selection
	  (if (= (length select-list) 1)
	      (setq select (car select-list))
	    (setq select
		  ;; TODO: here to be improved
		  (completing-read 
		   "Select by number: " 
		   select-list
		   )))

	  ;; find which one is selected and jump to it.
	  (let ((a children) (b select-list))
	    (while (and a b )
	      (when (equal (car b) select)
		(scb-jump-to-bookmark (car a))
		;;(recenter)
		(setq scb-jump-current (car a)))

	      (setq a (cdr a))
	      (setq b (cdr b))
	      )))
      (message "No next bookmark"))))

(defun scb-view-file ()
  "View file other window without selected, and move cursor to next line"
  (interactive)
  (let ((window (selected-window)))
    (when (scb-goto-file)
      (select-window window)
      (forward-line))))

(defun scb-recover-buffer ()
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create scb-buffer-name))
  (toggle-truncate-lines 1)
  ;;(toggle-read-only 1)
  )

(defun scb-find-file (pattern)
  "TODO: this fucntion is incomplete"
  (interactive "sFind file by name: ")

  (let ((rst (delete-dups
	      (split-string
	      (shell-command-to-string 
	       (format "grep %s %s"
		       pattern
		       (scb-project-file-list scb-current-project)))
	      "\n" t))))

    (message "rst=%s" rst)
    (if (not rst)
	(message "No file match pattern: %s" pattern)
      (setq rst (mapcar (lambda (str)
			  (replace-regexp-in-string 
			     "\"\\(.*\\)\"" "\\1" 
			     str)) rst))
      (message "after rst=%s" rst)      
      (if (= (length rst) 1) 
          (find-file (car rst))
        (find-file
         (completing-read "Select: "
                          rst
                          nil t))))))

(define-derived-mode scb-mode text-mode "Scb")
(define-key scb-mode-map  (kbd "r") 'scb-redisplay-buffer)
(define-key scb-mode-map  (kbd "g") 'scb-goto-file)
(define-key scb-mode-map  (kbd "v") 'scb-view-file)

(progn
  (defcustom ctl-q-map-prefix-key "\C-q"
	"*The prefix key for all `ctl-q-map' commands.")
  (define-prefix-command 'ctl-q-map)	;create a keymap
  (global-set-key ctl-q-map-prefix-key 'ctl-q-map));set the keymap's prefix key

(define-key ctl-q-map  (kbd "b") 'scb-recover-buffer)
(define-key ctl-q-map  (kbd "S") 'scb-search-text-i)
(define-key ctl-q-map  (kbd "s") 'scb-search-text)
(define-key ctl-q-map  (kbd "o") 'scb-open-project)
(define-key ctl-q-map  (kbd "n") 'scb-create-project)
(define-key ctl-q-map  (kbd "u") 'scb-previous-jump)
(define-key ctl-q-map  (kbd "i") 'scb-next-jump)
(define-key ctl-q-map  (kbd "f") 'scb-find-file)

;; from overlay file
(defun scb-get-pos (start)
  "a: end of the first `:', b: end of the second `:', 
   c: end of the blanks, d: end of the line "
  (save-excursion
    (let ((a) (b) (c) (d))
      (goto-char start)
      (setq a (re-search-forward ":" nil t))
      (setq b (re-search-forward ":" nil t))
      (setq c (re-search-forward "[ \t]*" nil t))
      (setq d (re-search-forward "\n" nil t))
      
      (list a b c d)
      )
    )
  )

(defun scb-redisplay-buffer-i (from to)
  (interactive "nFrom: \nnTo: ")
  (save-excursion
    (goto-char from)
    (let ((rst) (str) (ovl))
      (while (<= from to)
	(setq rst (scb-get-pos from))
	(setq str (buffer-substring from (nth 2 rst)))
	
	(unless (overlays-in (nth 0 rst) (nth 2 rst))
	  (setq ovl
		(make-overlay from (nth 2 rst)))
	  (overlay-put ovl
	   'display (car (scb-convert-string str)))
	  (overlay-put ovl
	   'face 'font-lock-keyword-face))

	(setq from (nth 3 rst))
	)))
  )


(defun scb-redisplay-buffer ()

  (interactive)
  (let ((start) (end) (lines (window-height)))
    (save-excursion
      
      (move-beginning-of-line (* lines -1))
      (setq start (point))
      (move-end-of-line (* lines 2))
      (setq end (point))
      
      (scb-redisplay-buffer-i start end)
      )
    )
  )

;; from anything-learning.el
(defun scb-get-element (str)
  (let ((fname) (line-number) (content)
	(begin 0) (end 0))
    (setq end (string-match ":" str begin))
    (setq fname (substring str begin end))

    (setq begin (+ end 1))
    (setq end (string-match ":" str begin))
    (setq line-number (string-to-number (substring str begin end)))
    
    (setq content (substring str (+ end 1)))
    
    (list fname line-number content)))

(defun scb-shorter-str (str length)
  "Make a str shorter if it is too long"
  (if (> (length str) length)
      (concat (substring str 0 (- length 4))
	      "*"
	      (substring str (- (length str) 3)))
    str
    ))

(defun scb-convert-string (str)
  "Convert a str to a list. str is result of grep. list is (DISP . REAL) pair for anything candidates"
  (let ((tmp))
    (setq tmp (scb-get-element str))

    (cons (format "%04d:%-16s    %s" (nth 1 tmp) 
    		  (scb-shorter-str 
    		   (replace-regexp-in-string "^[^:]*/" "" (nth 0 tmp)) 16)
		  (replace-regexp-in-string "^[ \t]*" "" (nth 2 tmp)))
    	  str)))

(defun scb-make-distribution-pattern (pattern)
  "Convert a pattern to subword and make distribution of them."
  (mapconcat (lambda (lst) 
	       (concat "-e \"" 
		       (mapconcat (lambda (s) s) lst ".*")
		       "\""))
	     ;;'(("this" "is") ("is" "this"))
	     (make-distribution 
	      (split-string pattern "[ \t\n]+" t))
	     " "))


;; Practice of operates a list: shift elemets of a list
(defun list-ops-shift (lst &optional num)
  "left shift elements of a list by num steps"
  (let ((tmp lst))
    (or num (setq num 1))
    (while (> num 0)
      (setq tmp (cdr lst))
      (setq lst (add-to-list 'tmp (car lst) t))
      (setq num (1- num))
      ))
  lst)

(defun make-distribution (lst)
  "Make a list of distributions of the elements of the given list
e.g. lst=(\"this\"  \"is\"), result is: 
((\"this\" \"is\") (\"is\" \"this\"))"
  (if (= (length lst) 1)
      (list lst)
    (let ((idx (length lst)) (rst) (elm))
      (while (> idx 0)
	(setq elm (car lst))

	;;(setq rst (append rst (mapcar 'A1 (make-distribution (cdr lst)))))
	(setq rst (append rst (mapcar 
			       (lambda (lst) (add-to-list 'lst elm))
			       (make-distribution (cdr lst)))))

	(setq lst (list-ops-shift lst))
	(setq idx (1- idx)))
      rst)))

;; copy from minibuffer.el
;; When jump to next bookmark, if more than one choices, using `\C-f' key to switch to the next one.
(setq scb-next-select-idx -1)
(defun scb-minibuffer-completion ()
  (interactive)
  ;;(message "idx=%d, table=%s" scb-next-select-idx minibuffer-completion-table)
  (delete-region (field-beginning) (field-end))
  (incf scb-next-select-idx)
  ;; DONE: BUG: the first element is deleted when do the selection. Remove the sort function will fix this problem.

;; idx=1, table=(request.lisp:59                         (snmp-request session 'get-request-pdu bindings compiler.lisp:14   #-lispworks)
;; selection=(compiler.lisp:14   #-lispworks request.lisp:59                         (snmp-request session 'get-request-pdu bindings)

  ;; here it is deleted.
;; idx=0, table=(request.lisp:59                         (snmp-request session 'get-request-pdu bindings)
;; selection=(request.lisp:59                         (snmp-request session 'get-request-pdu bindings)

;; idx=0, table=(request.lisp:59                         (snmp-request session 'get-request-pdu bindings)
;; selection=(request.lisp:59                         (snmp-request session 'get-request-pdu bindings)
;; idx=0, table=(request.lisp:59                         (snmp-request session 'get-request-pdu bindings)
;; selection=(request.lisp:59                         (snmp-request session 'get-request-pdu bindings)



  (let ((selection (sort 
		    (delete-dups (copy-list minibuffer-completion-table))
		    'string<)))
;;  (let ((selection (delete-dups minibuffer-completion-table)))  
    ;;(message "selection=%s" selection)
    (if (>= scb-next-select-idx (length selection))
	(setq scb-next-select-idx 0))

    (insert (format "%s" (nth scb-next-select-idx selection))))
    ;;(message "table: %s" minibuffer-completion-table)
  )

(define-key minibuffer-local-completion-map (kbd "C-f") 'scb-minibuffer-completion)

(defun scb-setup-tag ()
  (visit-tags-table (scb-project-tag-file-name scb-current-project))
  (setq scb-tag-file-ok-p t))

;; TODO: if many file in the file list, maybe wrong
(defun scb-create-tag-table ()
  ;; The string describing the event looks like one of the following:
  ;; * "finished\n".
  ;; * "exited abnormally with code exitcode\n".
  ;; * "name-of-signal\n".
  ;; * "name-of-signal (core dumped)\n".
  (setq scb-tag-file-ok-p nil)
  (set-process-sentinel
   (start-process "*SCB-TAG*" "*SCB-TAG*"
		  "bash"
		  "-c"
		  (format  "etags -f %s %s"
			  (expand-file-name (scb-project-tag-file-name scb-current-project))
			  (replace-regexp-in-string 
			   "\n" " "
			   (with-current-buffer (find-file-noselect 
						 (scb-project-file-list scb-current-project))
			     (buffer-string)))
			  ))
   
   (lambda (process event)
     (if (string-equal event "finished\n")
	 (progn 
	   (scb-setup-tag)
	   (message "tags file created for project %s" scb-current-project))
       (setq scb-tag-file-ok-p nil)       
	 ;;(princ
	 ;;(format "Process: %s had the event `%s'" process event))
       ))))

(defun scb-find-definition (pattern)
  (interactive 
   (list
    (read-string (format "Search text(%s):[%s] " 
			 scb-current-project
			 (thing-at-point 'symbol) )
		 nil nil
		 (thing-at-point 'symbol))))
  (if scb-tag-file-ok-p
      (let ((buffer)
	    (point))
	(save-excursion 
	  (setq buffer (find-tag-noselect pattern))
	  (setq point (with-current-buffer buffer
			(point))))

	;; Save the current file before going
	(scb-history-set-current-file)

	;; Record the jump history. 
	;; Change algorithm. Also saved the position before jumping
	;; Save the current file and position
	(and scb-current-file
	     (with-current-buffer (find-file-noselect scb-current-file)
	       (scb-history-add)))

	(switch-to-buffer-other-window buffer)
	(goto-char point)
	
	(scb-history-add)
	)
    (message "tags file is not ok, please wait...")))

(while nil
;; set tags file path
(visit-tags-table (scb-project-tag-file-name scb-current-project))
(find-tag-noselect "w3m")
(find-tag "w3m")
;;(tags-search)
;;(tags-loop-continue)
(find-tag-in-order PATTERN SEARCH-FORWARD-FUNC ORDER
NEXT-LINE-AFTER-FAILURE-P MATCHING FIRST-SEARCH)

  (message "search-forward-func=%s, %s, %s, %s, %s" 
	   search-forward-func
	   order
	   next-line-after-failure-p
	   matching
	   first-search)


(find-tag-in-order "w3m" 'search-forward '(tag-exact-file-name-match-p tag-file-name-match-p tag-exact-match-p tag-implicit-name-match-p tag-symbol-match-p tag-word-match-p tag-partial-file-name-match-p tag-any-match-p)  nil "containing" t)


)
(provide 'scb)
