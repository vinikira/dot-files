;;; lang-python.el --- Python Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function straight-use-package "ext:straight")

;; Python Mode
;; =============================================================================
(customize-set-variable 'python-shell-interpreter "ipython")
(customize-set-variable 'python-shell-interpreter-args "-i --simple-prompt")
;; =============================================================================

(provide 'lang-python)

;;; lang-python.el ends here
