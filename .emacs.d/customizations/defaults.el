(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(global-linum-mode t)                                      ;; Line numbers
(delete-selection-mode t)                                  ;; Act like a normal text editor
(global-hl-line-mode t)                                    ;; Highlight current row
(column-number-mode t)                                     ;; Show current column
(show-paren-mode t)                                        ;; Highlight matching parenthesis
(set-default 'truncate-lines t)                            ;; Turn of wrapping when line is too long

(load-theme 'nord t)
