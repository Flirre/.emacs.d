;;More garbage
(setq gc-cons-threshold 400000000)

;; Set up package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(mapcar #'disable-theme custom-enabled-themes) ;disables any loaded themes to avoid loading several custom themes at once.
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(show-paren-mode)
(delete-selection-mode 1)
(when (eq system-type 'gnu/linux) 
  (pdf-tools-install) ; PDF viewer (needs this separate installation)
  (setq TeX-view-program-selection '((output-pdf "pdf-tools")))
  (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view"))))

;;Making sure use-package is installed and intstalls if it is missing.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; From use-package README
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; Start emacs-server
(server-start)

(org-babel-load-file (concat user-emacs-directory "config.org"))

;;Less garbage
(setq gc-cons-threshold 800000)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (spacemacs use-package whitespace-cleanup-mode undo-tree theme-changer swap-buffers spotify spacemacs-theme solarized-theme request rainbow-delimiters professional-theme pdf-tools org-beautify-theme org micgoline matlab-mode magit jdee helm gandalf-theme forest-blue-theme elpy dracula-theme csharp-mode auctex ahungry-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
