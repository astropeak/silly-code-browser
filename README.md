silly-code-browser
==================

Installation
(add-to-list 'load-path "path/to/silly_code_browser")
(require 'scb)

(add-to-list 'load-path "~/Dropbox/project/silly_code_browser")

Install anything
;; ========== anything | START ========== 
;;(global-unset-key "\C-w")
(add-to-list 'load-path "~/Dropbox/project/silly_code_browser/anything")
(require 'anything)
(require 'anything-match-plugin)
(require 'anything-config) ;; can't work 20120518
;;(setq anything-command-map-prefix-key "\C-w")
;; ========== anything | END ============ 

