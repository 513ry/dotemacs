;;; init.el --- Bootstrap to Emacs init <siery@comic.com>
;;;
;;; Commentary:
;;; GNU Emacs > 27.1 Configuration. This init.el is here mainly to load the
;;; config.el code or compile config.org description file.
;;;
;;; Note:
;;; This configuration in not backward compatible. You can try to dig in my
;;; github repo for older versions of this configuration.
;;;
;;; Code:

;;; PACKAGES CONFIGURATION
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

;; Ensure system binaries keyword
(use-package use-package-ensure-system-package
  :ensure t)

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Silent
(setq visible-bell t)

;; Evaluation of packages and all personal configuration is exported
;; to  `~/.emacs.d/myinit.org`.
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;; TODO:
;; Check for errors/warnings and send feedback to status.org

(provide 'init.el)
;;; init.el ends here
