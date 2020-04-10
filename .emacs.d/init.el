;; Prevent 'Saving Customizations' to be written in this file
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p "custom.el") (load "custom"))

(add-to-list 'load-path "~/.emacs.d/customizations")

(load "defaults")
(load "keybindings")
