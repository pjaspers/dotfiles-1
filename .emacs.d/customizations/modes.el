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
