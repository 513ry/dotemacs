;;; init.el --- Bootstrap to Emacs init -*- lexical-binding: t -*-

;; Copyright (C) 2025 by Daniel Sierpiński

;; Author: Daniel Sierpiński <siery@comic.com>
;; Keywords: init config
;; Version: 1

;;; Commentary:

;; GNU Emacs > 27.1 Configuration.

;;; Installation:

;; init.el will compile config.org to config.el and run the configuration, all
;; you need is to place those files into your ~/.emacs.d.

;;; Code:


;; Preset

(setq package-enable-at-startup nil)
(setq debug-on-error nil)
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
