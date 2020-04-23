;; IDO
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(ido-mode t)

;; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Projectile + NeoTree <3
(setq projectile-switch-project-action 'neotree-projectile-action)

;; NeoTree
(global-set-key [f8] 'neotree-toggle)
(global-set-key (kbd "C-c f") 'neotree-find)
(setq neo-theme 'arrow) ; 'classic, 'nerd, 'ascii, 'arrow

;; Doom Mode Line
(doom-modeline-mode t)
