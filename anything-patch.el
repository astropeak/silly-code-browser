;; modify this function for scb-find-definition
(defun anything-c-etags-get-tag-file (&optional directory)
  "Return the path of etags file if found."
  ;; Get tag file from `default-directory' or upper directory.
  (let ((current-dir (anything-c-etags-find-tag-file-directory
                      (or directory (scb-project-dir scb-current-project)))))
    ;; Return nil if not find tag file.
    (when current-dir
      ;; Set tag file directory.
      (setq anything-c-etags-tag-file-dir current-dir)
      (expand-file-name anything-c-etags-tag-file-name current-dir))))

(defun aspk-anything-c-source-etags-transformer (candidate)
  (if (file-exists-p (scb-project-config-file scb-current-project))
      (with-current-buffer (find-file-noselect (scb-project-config-file scb-current-project))
        (goto-char (point-min))
        (let* ((config (read (current-buffer)))
               (base-dir (scb-config-get 'base-dir config))
               (len (length base-dir)))
          (mapcar (lambda (cand)
                    (if (listp cand) ;; cand is of format (DIS . VALUE)
                        (cons (substring (car cand) len) (cdr cand))
                      (cons (substring cand len) cand)))
                  candidate)))
    candidate))

(setq anything-c-source-etags-select
  '((name . "Etags")
    (header-name . anything-c-source-etags-header-name)
    (init . anything-c-etags-init)
    (candidates-in-buffer)
    (candidate-transformer . aspk-anything-c-source-etags-transformer)
    (search . (anything-c-etags-search-fn))
    (mode-line . anything-etags-mode-line-string)
    (action . anything-c-etags-default-action)
    (persistent-action . (lambda (candidate)
                           (anything-c-etags-default-action candidate)
                           (anything-match-line-color-current-line)))))

(defun anything-c-etags-select (arg)
  "Preconfigured anything for etags.
Called with one prefix arg use symbol at point as initial input.
Called with two prefix arg reinitialize cache.
If tag file have been modified reinitialize cache."
  (interactive "P")
  (let ((tag  (anything-c-etags-get-tag-file))
        (init (cond ((equal arg '(4)) (thing-at-point 'symbol))
                    ((equal arg '(5)) (concat (file-name-with-one-directory (or (buffer-file-name) "")) " " (thing-at-point 'symbol)))))
        (anything-quit-if-no-candidate t)
        (anything-execute-action-at-once-if-one t)
        (anything-compile-source-functions
         (if anything-c-etags-use-regexp-search
             ;; rule out anything-match-plugin because the input is one regexp.
             (delq 'anything-compile-source--match-plugin
                   (copy-sequence anything-compile-source-functions))
             anything-compile-source-functions)))
    (when (or (equal arg '(16))
              (and anything-c-etags-mtime-alist
                   (anything-c-etags-file-modified-p tag)))
      (remhash tag anything-c-etags-cache))
    (if (and tag (file-exists-p tag))
        (anything :sources 'anything-c-source-etags-select
                  :keymap anything-c-etags-map
                  :input init
                  :buffer "*anything etags*")
        (message "Error: No tag file found, please create one with etags shell command."))))

(provide 'anything-patch)