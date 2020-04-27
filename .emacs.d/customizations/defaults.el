(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(add-to-list 'default-frame-alist '(font . "Liga Roboto Mono"))

(delete-selection-mode t)                                  ;; Act like a normal text editor
(global-hl-line-mode t)                                    ;; Highlight current row
(column-number-mode t)                                     ;; Show current column
(show-paren-mode t)                                        ;; Highlight matching parenthesis
(set-default 'truncate-lines t)                            ;; Turn of wrapping when line is too long

(load-theme 'doom-nord t)
(load-theme 'nord t)

;; Line numbers
(define-global-minor-mode my-global-linum-mode linum-mode
  (lambda ()
    (when (not (memq major-mode
                     (list 'term-mode 'treemacs-mode)))
      (linum-mode))))

(my-global-linum-mode t)
(setq linum-format "%3d \u2502")

;; dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)

(setq dashboard-banner-logo-title "Welcome to Emacs Dashboard") ;; Set the title
(setq dashboard-startup-banner "~/dotfiles/assets/banner.png") ;; Set the banner
(setq dashboard-center-content nil) ;; Content is not centered by default. To center, set
(setq dashboard-show-shortcuts nil) ;; To disable shortcut "jump" indicators for each section, set
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-items '((recents  . 5)
                        (projects . 20)))

(setq dashboard-set-navigator t)
;; Format: "(icon title help action face prefix suffix)"
(setq dashboard-navigator-buttons
      `(;; line1
        ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
         "Github"
         "Browse homepage"
         (lambda (&rest _) (browse-url "https://github.com/TomBosmans"))))))

;; OS X has an issue with picking up the right system env
;; Explictly setting it here (So shell-command and buddies can use it)
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
