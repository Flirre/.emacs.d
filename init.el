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
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Disables any loaded themes to avoid loading several custom themes at once.
(mapcar #'disable-theme custom-enabled-themes)

;; Making sure use-package is installed and installs if it is missing.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t))

;; From use-package README
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(org-babel-load-file (concat user-emacs-directory "config.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yasnippet-snippets which-key web-mode web-beautify vterm use-package tide telephone-line swap-buffers sublimity sqlformat spotify solarized-theme shut-up sane-term rjsx-mode rainbow-delimiters projectile prettier-js popwin popup-kill-ring pdf-tools package-utils org-bullets omnisharp nov move-dup meghanada magit-popup lua-mode indium impatient-mode ido-vertical-mode howdoi highlight-operators highlight-numbers highlight-escape-sequences helm-gtags helm-ag general function-args forge flycheck-irony flycheck-inline flx-ido emmet-mode elpy edts diminish dashboard company-web company-tern company-irony company-glsl company-auctex browse-kill-ring base16-theme atom-one-dark-theme ag add-node-modules-path ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
