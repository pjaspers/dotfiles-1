(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; list packages here:
(straight-use-package 'doom-themes)
(straight-use-package 'nord-theme)
(straight-use-package 'projectile)
(straight-use-package 'magit)
(straight-use-package 'reveal-in-osx-finder)
(straight-use-package 'ag)
(straight-use-package 'doom-modeline)
(straight-use-package 'all-the-icons)
(straight-use-package 'git-gutter-fringe)
(straight-use-package 'centaur-tabs)
(straight-use-package 'dashboard)
(straight-use-package 'treemacs)
(straight-use-package 'treemacs-projectile)
(straight-use-package 'web-mode)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'org-bullets)
