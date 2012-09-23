(eval-after-load "twittering-mode"
  (progn
    (setq twittering-use-master-password t)
    (setq twittering-icon-mode t)           ; Show icons
    (setq twittering-timer-interval 300)         ; Update your timeline each 300 seconds (5 minutes)
    (setq twittering-url-show-status nil)        ; Keeps the echo area from showing all the http processes

    (setq twittering-timeline-spec-alias
          '(("FRIENDS" . "my-account/friends-list")
            ("related-to" .
             (lambda (username)
               (if username
                   (format ":search/to:%s OR from:%s OR @%s/"
                           username username username)
                 ":home")))))
    (setq twittering-initial-timeline-spec-string
          '(":home"
            ":replies"
            ":direct_messages"
            ))

    (add-hook 'twittering-mode-hook
              (lambda ()
                (mapc (lambda (pair)
                        (let ((key (car pair))
                              (func (cdr pair)))
                          (define-key twittering-mode-map
                            (read-kbd-macro key) func)))
                      '(("F" . twittering-friends-timeline)
                        ("R" . twittering-replies-timeline)
                        ("U" . twittering-user-timeline)
                        ("W" . twittering-update-status-interactive)))))
    (add-hook 'twittering-edit-mode-hook
              (lambda () (ispell-minor-mode) (flyspell-mode)))
    ))

(provide 'init-twittering)
