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
