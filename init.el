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

;; PACKAGES CONFIGURATION
(setq debug-on-error t)

(require 'package)

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

'(use-package-compute-statistics)

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Silent
(setq visible-bell t)

;; Update repos
(package-refresh-contents)

;; Evaluation of packages and all personal configuration is exported
;; to  `~/.emacs.d/myinit.org`.
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

(provide 'init.el)
;;; init.el ends here
