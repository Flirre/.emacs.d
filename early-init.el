(when (eq system-type 'gnu/linux)
(add-to-list 'default-frame-alist
             '(font . "FantasqueSansMono Nerd Font-14")))

(when (eq system-type 'darwin)
(add-to-list 'default-frame-alist
             '(font . "FantasqueSansMono Nerd Font-18")))
(load-theme 'doom-molokai-fixed t)
