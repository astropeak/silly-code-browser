(message "%s" (file-name-directory load-file-name))
(setq scb-source-dir (file-name-directory load-file-name))
(add-to-list 'load-path scb-source-dir)
(add-to-list 'load-path (format "%s/anything" scb-source-dir))

(add-to-list 'exec-path scb-source-dir)
;; (add-to-list 'exec-path (format "%s/bin" scb-source-dir))
;;(add-to-list 'exec-path "E:/cygwin/bin")
(require 'scb)


;; test of ergo emacs
;; (add-to-list 'load-path (format "%s/ergoemacs-1d1c8dbbb31f/packages/" scb-source-dir))
;; (require 'xah_file_util)


;;(read-directory-name "Directory: " default-directory default-directory "MUSTMATCH")


;; test anything etags
;;(require 'anything-etags)
;; (setq anything-sources '(anything-c-source-etags-select ))
;; anything-c-source-etags-select
