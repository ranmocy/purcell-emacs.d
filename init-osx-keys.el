(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq mac-function-modifier 'hyper)
  (setq default-input-method "MacOSX")
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(0.001))
  (when *is-cocoa-emacs*
    ;; Woohoo!!
    (global-unset-key (kbd "s-p"))
    (global-unset-key (kbd "s-o"))

    (global-set-key (kbd "s-n") 'new-empty-buffer)
    (global-set-key (kbd "s-N") 'ns-new-frame)
    (global-set-key (kbd "s-c") 'ns-copy-including-secondary)
    (global-set-key (kbd "s-v") 'ns-paste-secondary)

    (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
    (eval-after-load 'nxml-mode
      '(define-key nxml-mode-map (kbd "M-h") nil))
    (global-set-key (kbd "M-ˍ") 'ns-do-hide-others) ;; what describe-key reports

    (global-set-key (kbd "M-`") 'ns-next-frame)
    (global-set-key (kbd "s-`") 'other-window)
    (global-set-key (kbd "M-K") 'kill-buffer-and-window)
    (global-set-key (kbd "M-[") 'previous-multiframe-window)
    (global-set-key (kbd "M-]") 'next-multiframe-window)
    ))


(provide 'init-osx-keys)
