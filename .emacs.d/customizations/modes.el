;; IDO
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(ido-mode t)

;; Doom Mode Line
(doom-modeline-mode t)

;; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Git Gutter
(require 'git-gutter-fringe)
(global-git-gutter-mode +1) ;; Use git gutter in all the files
(setq-default left-fringe-width  20) ;; Use git gutter in all the files

;; Centaur tabs
(setq centaur-tabs-set-bar 'under
      x-underline-at-descent-line t
      centaur-tabs-height 25
      centaur-tabs-set-icons t
      centaur-tabs-gray-out-icons 'buffer
      centaur-tabs-set-modified-marker t
      centaur-tabs-modified-marker "¥")

(centaur-tabs-mode t)

;; Web Mod - http://web-mode.org/
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2
      web-mode-attr-indent-offset 2)
