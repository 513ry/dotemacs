;;; init.el --- Bootstrap to Emacs init <siery@comic.com>
;;;
;;; Commentary:
;;; Feel free to use it as you want!
;;;
;;; Code:

;;; PACKAGES CONFIGURATION
(require 'package)
(add-to-list 'package-archives
	     ;; Enable Melpa Unstable archive
	     '("melpa" . "https://melpa.org/packages/"))
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

;; Evaluation of packages and all personal configuration is exported
;; to  `~/.emacs.d/myinit.org`.
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;; TODO:
;; Check for errors/warnings and send feedback to myinit.org

(provide 'init.el)
;;; init.el ends here
