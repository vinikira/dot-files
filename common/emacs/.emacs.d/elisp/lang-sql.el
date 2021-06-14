;;; lang-sql.el --- SQL Config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function straight-use-package "ext:straight")

;; SQL Format
;; =============================================================================
(straight-use-package 'sqlformat)

(customize-set-variable 'sqlformat-command 'pgformatter)
(customize-set-variable 'sqlformat-args '("-s2" "-g" "-u1"))

(declare-function sqlformat-on-save-mode "ext:sqlformat")

(add-hook 'sql-mode-hook #'sqlformat-on-save-mode)
;; =============================================================================

(provide 'lang-sql)

;;; lang-sql.el ends here
