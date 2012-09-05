;; ---------------------------------------------------------------------------
;; Support GTK fullscreen
;; ---------------------------------------------------------------------------
(when (system-type-p :linux)
    (defun gtk-toggle-fullscreen ()
      (interactive)
      (x-send-client-message
       nil 0 nil "_NET_WM_STATE" 32
       '(2 "_NET_WM_STATE_FULLSCREEN" 0))
      )
    (global-set-key (kbd "M-s-f") 'gtk-toggle-fullscreen)
    )


;;----------------------------------------------------------------------------
;; Stop C-z from minimizing windows under OS X
;;----------------------------------------------------------------------------
(defun maybe-suspend-frame ()
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))

(global-set-key (kbd "C-z") 'maybe-suspend-frame)


;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)


;;----------------------------------------------------------------------------
;; Show a marker in the left fringe for lines not in the buffer
;;----------------------------------------------------------------------------
(setq indicate-empty-lines t)


;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(defun adjust-opacity (frame incr)
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(when (fboundp 'ns-toggle-fullscreen)
  (defadvice ns-toggle-fullscreen (after mark-full-screen activate)
    (set-frame-parameter nil
                         'is-full-screen
                         (not (frame-parameter nil 'is-full-screen))))



  ;; Command-Option-f to toggle fullscreen mode
  (global-set-key (kbd "M-s-f") 'ns-toggle-fullscreen))

(global-set-key (kbd "M-C-8") '(lambda () (interactive) (adjust-opacity nil -5)))
(global-set-key (kbd "M-C-9") '(lambda () (interactive) (adjust-opacity nil 5)))
(global-set-key (kbd "M-C-0") '(lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))


;;----------font-setting----------
(defcustom frame-font-name nil
  "Recommended fonts: 'WenQuanYi Zen Hei Mono-16' 'Inconsolata-18' 'YaHei_Consolas-16'"
  :group 'font)


;;---------------------------------------------------------------------------
;; New frame hooks
;;---------------------------------------------------------------------------
(defun set-frame (frame)
  "Init for new FRAME."
  (with-selected-frame frame
    (when (window-system-p :gui)
      ;; (set-frame-parameter nil 'menu-bar-lines 0)

      ;;----------set-font----------
      (set-frame-font frame-font-name)

      ;;----------turn-to-90%-opacity-when-emacs-deactive----------
      (set-frame-parameter nil 'alpha '(100 90))
      )))

(add-hook 'after-make-frame-functions 'set-frame)
(eval-after-init
 '(unless (eq nil (selected-frame))
    (set-frame (selected-frame))))


(provide 'init-gui-frames)
