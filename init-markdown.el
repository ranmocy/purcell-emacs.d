(autoload 'markdown-mode "markdown-mode" "Mode for editing Markdown documents" t)
(setq auto-mode-alist
      (cons '("\\.\\(md\\|markdown\\|text\\)$" . markdown-mode) auto-mode-alist))

(provide 'init-markdown)
