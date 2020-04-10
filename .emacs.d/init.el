;; Prevent 'Saving Customizations' to be written in this file
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p "custom.el") (load "custom"))

;; custom stuff
(add-to-list 'load-path "~/.emacs.d/customizations")
(load "packages")
(load "defaults")
(load "modes")
(load "keybindings")
