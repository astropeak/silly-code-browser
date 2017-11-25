(defun current-line-number ()
  "Get current buffer's line number, return as a digit"
  ;; This implementation is faster that line-number-at-pos
  ;;http://emacs.stackexchange.com/questions/3821/a-faster-method-to-obtain-line-number-at-pos-in-large-buffers
  (let ((buffer (current-buffer)))
    (string-to-number (format-mode-line "%l" 'default (get-buffer-window buffer) buffer))))
;; (message "line number: %d"  (current-buffer-line-number ))

(defun current-line-number ()
  "Get current buffer's line number, return as a digit"
  (string-to-number (format-mode-line "%l")))

(defun current-line-number ()
  (line-number-at-pos))

(defun aspk-grep (search-str files output-buffer &optional display-line-number-p)
  (let ((count (if (listp files) (length files) 1))
        (i 0))
    (mapc
     (lambda (ξfp)
       ;; (message "fp=%s, files=%s" ξfp files)
       (with-temp-buffer
         ;; (goto-char (point-min))
         (insert-file-contents ξfp)
         (while (search-forward search-str nil "NOERROR if not found")
           ;; (message "Current buffer=%s, line number=%d" (current-buffer) (current-buffer-line-number))
           (setq p1 (progn (beginning-of-line) (point)))
           (setq p2 (progn (end-of-line) (point)))
           ;;(setq matched-line-str (buffer-substring-no-properties p1 p2))
           (if display-line-number-p
               (setq line-number (current-line-number))
             (setq line-number 0))
           (with-current-buffer output-buffer
             (insert (format "%s:%d:" ξfp line-number
                             )))
           (append-to-buffer output-buffer p1 (min (point-max) (+ p2 1)))))
       (incf i)
       ;;(message "i =%d, count=%d" i count)
       (message "Searching '%s' in project '%s'...%d%%" search-str scb-current-project (/ (* 100 i) count)))
     (if (listp files)
         files
       (list files)))
    ))

;; (my-grep-1 "list" "scb.el" (get-buffer-create "*MYGREP*"))
(provide 'aspk-util)
