(add-hook 'sh-set-shell-hook 'flymake-shell-load)

(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("zshrc$" . shell-script-mode))


(provide 'init-sh)
