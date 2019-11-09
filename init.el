;;; init.el --- Emacs init file
;;  Author: Filip Lindahl
;;; Commentary:
;;; Heavily influenced by Yay-Evil Better Defaults
;;; Code:
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist-original file-name-handler-alist
      file-name-handler-alist nil
      site-run-file nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 20000000
                  gc-cons-percentage 0.1
                  file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))

(add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold 40000000)))
(add-hook 'minibuffer-exit-hook (lambda ()
                                  (garbage-collect)
                                  (setq gc-cons-threshold 20000000)))


;; Set up package

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; Disables any loaded themes to avoid loading several custom themes at once.
(mapcar #'disable-theme custom-enabled-themes)

;; Making sure use-package is installed and installs if it is missing.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t))

(require 'bind-key)

;; Dump custom-set-variables to a garbage file and don't load it
(setq custom-file "~/.emacs.d/custom-dump.el")

(org-babel-load-file (concat user-emacs-directory "config.org"))
