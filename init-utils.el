;;---------------------------------------------------------------------------
;; eval-after-init hook
;;---------------------------------------------------------------------------
;; `eval-after-init` is modified from https://github.com/technomancy/emacs-starter-kit/commit/5efa136c2ffced48fb5a25948e92ea11b850cedb
(defun eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.
If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))


;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;;----------------------------------------------------------------------------
;; String utilities missing from core emacs
;;----------------------------------------------------------------------------
(defun string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

(defun string-rtrim (str)
  "Remove trailing whitespace from `STR'."
  (replace-regexp-in-string "[ \t\n]*$" "" str))


;;----------------------------------------------------------------------------
;; Find the directory containing a given library
;;----------------------------------------------------------------------------
(autoload 'find-library-name "find-func")
(defun directory-of-library (library-name)
  "Return the directory in which the `LIBRARY-NAME' load file is found."
  (file-name-as-directory (file-name-directory (find-library-name library-name))))


;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file filename new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

;;----------------------------------------------------------------------------
;; Browse current HTML file
;;----------------------------------------------------------------------------
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (browse-url (concat "file://" (buffer-file-name))))


(require 'cl)

(defmacro with-selected-frame (frame &rest forms)
  (let ((prev-frame (gensym))
        (new-frame (gensym)))
    `(progn
       (let* ((,new-frame (or ,frame (selected-frame)))
              (,prev-frame (selected-frame)))
         (select-frame ,new-frame)
         (unwind-protect
             (progn ,@forms)
           (select-frame ,prev-frame))))))


;;---------------------------------------------------------------------------
;; System type p
;;---------------------------------------------------------------------------
;; `gnu'         compiled for a GNU Hurd system.
;; `gnu/linux'   compiled for a GNU/Linux system.
;; `darwin'      compiled for Darwin (GNU-Darwin, Mac OS X, ...).
;; `ms-dos'      compiled as an MS-DOS application.
;; `windows-nt'  compiled as a native W32 application.
;; `cygwin'      compiled using the Cygwin library.
;; `linux'       for `gnu' and `gnu/linux'
;; `darwin'      for `darwin'
;; `windows'     for `ms-dos', `windows-nt' and `cygwin'
;; `unixlike'    for `linux' and `darwin'

;;; system-type-p
(defun system-type-p (type)
  (case type
    (:linux (or (system-type-p :gnu) (system-type-p :gnu/linux)))
    (:windows (or (system-type-p :ms-dos) (system-type-p :windos-nt) (system-type-p :cygwin)))
    (:unixlike (or (system-type-p :darwin) (system-type-p :linux)))
    (:any t)
    (t (equal (subseq (symbol-name type) 1 nil) (symbol-name system-type)))))


;;---------------------------------------------------------------------------
;; Window system p
;;---------------------------------------------------------------------------
;; Judge the window-system whether belongs to given type.
;; e.x. (window-system-p :gui)
;; `nil' text-only
;; `ns'  MacOS
;; `w32' Windows
;; `cui' for `nil'
;; TODO: Where is Linux?
;; `gui' for `ns' and `w32'

(defun window-system-p (system)
  (case system
    (:cui (window-system-p :nil))
    (:gui (or (window-system-p :ns) (window-system-p :w32)))
    (t (equal (subseq (symbol-name system) 1 nil) (symbol-name window-system)))))

(provide 'init-utils)
