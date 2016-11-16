
(package-initialize)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (spacemacs-light)))
 '(custom-safe-themes
   (quote
    ("77c65d672b375c1e07383a9a22c9f9fc1dec34c8774fe8e5b21e76dca06d3b09" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "4af6fad34321a1ce23d8ab3486c662de122e8c6c1de97baed3aa4c10fe55e060" "6f441c0e5d8199f08eb4b73e9c697710282bcae95e5925b7649ddfa8cea2e24c" "66881e95c0eda61d34aa7f08ebacf03319d37fe202d68ecf6a1dbfd49d664bc3" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "427fed191e7a766152e59ef0e2904283f436dbbe259b9ccc04989f3acde50a55" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "c7acfcee1e410808d646b8a18e2d13f8296679ea82f9c0245958d973f3674bfa" default)))
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (whitespace-cleanup-mode theme-changer auctex matlab-mode undo-tree swap-buffers spotify request helm rainbow-delimiters micgoline gandalf-theme professional-theme spacemacs-theme org-beautify-theme solarized-theme pdf-tools org magit jdee forest-blue-theme elpy dracula-theme csharp-mode anaconda-mode ahungry-theme)))
 '(pdf-view-continuous nil)
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; =============================================================================
;; =============================================================================






;;Package stuff
(require 'package) ;;
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Startup crap
; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; What packages should be installed?
(defvar ip '(magit spacemacs-theme elpy pdf-tools rainbow-delimiters request company helm ))

; install the missing packages
(dolist (package ip)
  (unless (package-installed-p package)
    (package-install package)))

;; Initialization stuff
(mapcar #'disable-theme custom-enabled-themes) ;disables any loaded themes to avoid loading several custom themes at once.
(tool-bar-mode -1)
(scroll-bar-mode -1)
(load-theme 'spacemacs-dark t)
(column-number-mode 1)
(elpy-enable)
(global-company-mode)
(show-paren-mode)
(delete-selection-mode 1)
(spotify-enable-song-notifications)
(when (eq system-type 'gnu/linux) 
  (pdf-tools-install) ; PDF viewer (needs this separate installation)
  (setq TeX-view-program-selection '((output-pdf "pdf-tools")))
  (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view"))))

;;Helm bindings
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

;;Buffer bindings
(global-set-key (kbd "M-s M-s") 'swap-buffers)

;;Spotify bindings
(global-set-key (kbd "M-s M-n") 'spotify-next)
(global-set-key (kbd "M-s M-p") 'spotify-previous)
(global-set-key (kbd "M-p") 'spotify-pause)
(global-set-key (kbd "M-ö") 'spotify-play)

;;Programming hooks
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'undo-tree-mode)

;;Elisp-hook
(require 'rainbow-delimiters)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; Less prompting
(fset 'yes-or-no 'y-or-n)
;; Themeswitch depending on if sun is out. Checks every 15min.
;(add-to-list 'load-path "~/.emacs.d/theme-switch/")
;(require 'theme-switch)
;(run-with-timer 0 (* 60 1) 'switch-theme 'spacemacs-dark 'spacemacs-dark)

;Set the location:
(setq calendar-location-name "Gothenburg, SE")
(setq calendar-latitude 57.70887000)
(setq calendar-longitude 11.97456000)

;Specify the day and night themes:
(require 'theme-changer)
(change-theme 'solarized-dark 'spacemacs-dark)
