;;More garbage
(setq gc-cons-threshold 400000000)

;; Set up package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(package-initialize)

;; Disables any loaded themes to avoid loading several custom themes at once.
(mapcar #'disable-theme custom-enabled-themes)

;; Making sure use-package is installed and installs if it is missing.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; From use-package README
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; Start emacs-server
(require 'server)
(unless (server-running-p)
  (server-start))

(org-babel-load-file (concat user-emacs-directory "config.org"))

;; Less garbage
(setq gc-cons-threshold 800000)
