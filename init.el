;;; init.el --- Bootstrap to Emacs init <siery@comic.com>
;;;
;;; Commentary:
;;; GNU Emacs > 27.1 Configuration. This init.el is here mainly to load the
;;; config.el code or compile config.org description file.
;;;
;;; Note:
;;; AN INTERMEDIATE DOCUMENT VERSION
;;;
;;; Code:

;; Preset
(setq package-enable-at-startup nil)
(setq debug-on-error t)
(setq visible-bell t)

;; Packages Configuration
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package el-patch
  :straight t)

;; Personal configuration
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

(provide 'init.el)
;;; init.el ends here
