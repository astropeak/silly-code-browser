(defun aspk-etags-delete-file-tag (file)
  "Delete all the tags items concerns file in current buffer, which should be a TAGS buffer. Return the start pos if succeed, else nil"
  (goto-char (point-min))
  (let ((p1)(p2))
    (if (search-forward (format "%c%c%s" 12 10 file) nil t)
        (progn
          (setq p1 (search-backward (format "%c%c" 12 10)))
          (goto-char (+ p1 1))
          (setq p2 (search-forward (format "%c%c" 12 10) nil t))
          (if p2
              (setq p2 (- p2 2))
            (setq p2 (point-max)))
          (delete-region p1 p2)
	  (save-buffer)
          (goto-char p1)
          )
      )))


(defun aspk-etags-update-file-tag (file-list tag-file)
  "Update the file's tag info if file changed. If file is newer that the tag file, then the file's tag info will be updated."
  (let ((files (remove nil (mapcar (lambda (f)
                                     (when (file-newer-than-file-p f tag-file) f))
                                   file-list))))
    ;;(message "files=%s" files)
    (when (> (length files) 0)
      (message "%s file's tag info updated" (length files))
      (with-current-buffer (find-file-noselect tag-file)
        (mapc 'aspk-etags-delete-file-tag files))
      (aspk-etags-create-tag-table files tag-file t))))

;;(aspk-etags-update-file-tag (scb-get-file-list) (expand-file-name (scb-project-tag-file-name scb-current-project)))


(defun aspk-etags-create-tag-table (file-list output-file &optional appendp)
  "Create tag file for `file-list', and output to `output-file'. If appendp is t, then append to the `output-file', else overwrite. If failed, return nil"
  (let* ((count (length file-list))
         (i 0)
         (max-file-name-length
          (apply 'max (mapcar (lambda (f) (length f)) file-list)))
         (step (/ 35000 max-file-name-length)) ;; I tested manually and find that the max lenght of command line is 35000
         (result))
    (and (not appendp) (file-exists-p output-file) (delete-file output-file))
    (while (and (< i count) (> step 0))
      (setq result
            (ignore-errors
              (apply 'call-process
                     "etags" nil nil nil
                     "-a" "-o" output-file
                     (subseq file-list i (min count (+ i step))))))
      ;; (message "result: %s" result)
      (if result
          (progn
            (incf i step)
            ;; (setq step original-step)
            (message "Creating TAGS... completed %d files, total %d files" (min count i) count))
        ;; (and (file-exists-p output-file) (delete-file output-file))
        (if (<= step 50)
            (setq step (/ step 2))
          (setq step (- step 50)))

        (message "Creating TAGS failed, shell code: %s. Will decrease step to %d and try angin" result step)
        ))
    (if (< i count)
        (progn (message "Creating TAGS failed") nil)
      (message "Creating TAGS succeed, saved to %s" output-file))))


;; (aspk-etags-create-tag-table (list "e:/home/Dropbox/project/silly_code_browser/aspk-etags.el") "XXXX")

;;(with-current-buffer (find-file-noselect "e:/home/Dropbox/project/silly_code_browser/TAGS")
;;  (aspk-etags-delete-file-tag "c:/Users/luooofuq/Dropbox/project/silly_code_browser/grep.el")
;;  )

(provide 'aspk-etags)
