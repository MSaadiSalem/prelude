;;; Commentary
;; preload.el -- Emacs Basic Configuration
;;; Code:

;; Check OS type
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
    (message "Microsoft Windows")))

 ((string-equal system-type "darwin") ; Mac OS X
  (progn
    (message "Mac OS X")))

 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (message "Linux"))))

;; Fullscreen mode
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))


;; Semantic built-in is a package that provides language-aware editing commands based on source code parsers.
;; When Semantic mode is enabled, Emacs automatically attempts to parse each file you visit. Currently, Semantic understands C, C++, Scheme, Javascript, Java, HTML, and Make.
(semantic-mode 1)


;; Execute Path in Emacs
(add-to-list 'exec-path "/usr/local/bin/")
(add-to-list 'exec-path "~/.local/bin/")

;; Apropos can sort results by relevancy
(setq apropos-sort-by-scores t)


;; Disable Scroll and tool bars
(when (window-system)
  (scroll-bar-mode 0) ;; to disable scroll bar
  (tool-bar-mode 0))  ;; to disable tool bar


;; Backup files
;;(setq make-backupfile nil) ;; to disable backup files
(setq backup-directory-alist
      `((".*" . ,"~/emacs_backups/"))) ;; placing all backup files in one directory


;; Auto-saves
;;(setq auto-save-default nil) ;; to disable autosave
;(setq auto-save-file-name-transforms
;      `((".*" ,"~/emacs_autosaves" t))) ;; placing all auto_saves files in one directory


;; Numbering
;;(column-number-mode)  ;; show column numbers in line mode
(global-linum-mode) ;; show line numbers in vertical side
(line-number-mode) ;; displaying line numbers in a buffer frame


;; Lines
(global-visual-line-mode t)


;; Control the frame opacity
;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))
(global-set-key (kbd "C-c 1") 'toggle-transparency)

;; Bidirectional Display
(setq bidi-display-reordering 1)
(setq bidi-paragraph-direction nil)

;; Undisplayable Characters
;; Coding Systems for Terminal I/O
(set-keyboard-coding-system nil)

;;; preload.el ends here
