(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(delete-selection-mode t)                                  ;; Act like a normal text editor
(global-hl-line-mode t)                                    ;; Highlight current row
(column-number-mode t)                                     ;; Show current column
(show-paren-mode t)                                        ;; Highlight matching parenthesis
(set-default 'truncate-lines t)                            ;; Turn of wrapping when line is too long

(load-theme 'doom-nord t)
(load-theme 'nord t)

;; use clipboard
(setq select-enable-clipboard nil)
(setq select-enable-primary t)
(setq mouse-drag-copy-region 'region)

;; Line numbers
(global-linum-mode t)
(setq linum-format "%3d \u2502")

;; use clipboard hack for terminal
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

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
