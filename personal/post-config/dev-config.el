(use-package typescript-mode
  :ensure t)

(use-package tide
  :ensure t)

(use-package ts-comint
  :ensure t)

(use-package company-tern
  :ensure t
  :init
  ;(eval-after-load 'company '(add-to-list 'company-backends 'company-tern))
  (add-to-list 'company-backends 'company-tern)
  )

(use-package js2-mode
  :ensure t)
(use-package js-doc
  :ensure t)
(use-package rjsx-mode
  :ensure t)


;; js2-refactor requires:
;; js2-mode-20101228, s-1.9.0, multiple-cursors-1.0.0, dash-1.0.0, s-1.0.0, yasnippet-0.9.0.1
(use-package js2-refactor
  :ensure t)

(require 'js2-mode)
(require 'rjsx-mode)
(require 'js2-refactor)
(require 'js-doc)

;; use rjsx-mode for all JS files
(add-to-list 'auto-mode-alist '("\\.js\\'"    . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'"    . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.pac\\'"   . rjsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . rjsx-mode))

;; We use js2r-refactor-mode which implies using js2-mode.
;; see https://github.com/magnars/js2-refactor.el
;;
;; all refactorings start with C-c C-r (for refactor!)
(js2r-add-keybindings-with-prefix "C-c C-r")
(add-hook 'js2-mode-hook 'js2-refactor-mode)

;; 2 space tab width
(custom-set-variables '(js-indent-level 2)
                      '(js2-basic-offset 2))

;; setup jsdoc: https://github.com/mooz/js-doc
;;
;; We use the same prefix for js2r `C-c C-r' because it's an "advanced"
;; refactory-y type thing. The additional `i' prefix is for "insert"
(define-key js2-refactor-mode-map (kbd "C-c C-r i d") #'js-doc-insert-function-doc)
(define-key js2-refactor-mode-map "@" #'js-doc-insert-tag)

;; TypeScript:
;;
;; setup tide mode, the typescript IDE for Emacs
;; This is lifted straight from the suggested setup on the TIDE
;; README https://github.com/ananthakumaran/tide
(defun fs/setup-tide-mode()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(add-hook 'typescript-mode-hook #'fs/setup-tide-mode)

;; setup formatting options. The full list can be found at
;; https://github.com/Microsoft/TypeScript/blob/87e9506/src/services/services.ts#L1244-L1272
(setq tide-format-options
      '(:indentSize 2 :tabSize 2))


;;; parse node.js stack traces in compilation buffer.s
(require 'compile)
(add-to-list 'compilation-error-regexp-alist 'node)
(add-to-list 'compilation-error-regexp-alist-alist
             '(node "^[[:blank:]]*at \\(.*(\\|\\)\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 2 3 4))


(provide 'frontmacs-javascript)
;;; frontmacs-javascript.el ends here

(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  )

(require 'python)
(define-key python-mode-map (kbd "C-c C-c")
  (lambda () (interactive) (python-shell-send-buffer t)))

(defadvice py-execute-buffer (after advice-delete-output-window activate)
  (delete-windows-on "*Python Output*"))

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(use-package pylint
  :ensure t
  :init
  (autoload 'pylint "pylint")
;;  (add-hook 'python-mode-hook 'pylint-add-menu-items)
;;  (add-hook 'python-mode-hook 'pylint-add-key-bindings)
  (add-hook 'elpy-mode-hook 'pylint-add-menu-items)
  (add-hook 'elpy-mode-hook 'pylint-add-key-bindings)
  )

;;; company-files.el --- company-mode completion backend for file names

;; Copyright (C) 2009-2011, 2014-2015  Free Software Foundation, Inc.

;; Author: Nikolaj Schumacher

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

(require 'company)
(require 'cl-lib)

(defgroup company-files nil
  "Completion backend for file names."
  :group 'company)

(defcustom company-files-exclusions nil
  "File name extensions and directory names to ignore.
The values should use the same format as `completion-ignored-extensions'."
  :type '(const string)
  :package-version '(company . "0.9.1"))

(defun company-files--directory-files (dir prefix)
  ;; Don't use directory-files. It produces directories without trailing /.
  (condition-case err
      (let ((comp (sort (file-name-all-completions prefix dir)
                        (lambda (s1 s2) (string-lessp (downcase s1) (downcase s2))))))
        (when company-files-exclusions
          (setq comp (company-files--exclusions-filtered comp)))
        (if (equal prefix "")
            (delete "../" (delete "./" comp))
          comp))
    (file-error nil)))

(defun company-files--exclusions-filtered (completions)
  (let* ((dir-exclusions (cl-delete-if-not #'company-files--trailing-slash-p
                                           company-files-exclusions))
         (file-exclusions (cl-set-difference company-files-exclusions
                                             dir-exclusions)))
    (cl-loop for c in completions
             unless (if (company-files--trailing-slash-p c)
                        (member c dir-exclusions)
                      (cl-find-if (lambda (exclusion)
                                    (string-suffix-p exclusion c))
                                  file-exclusions))
             collect c)))

(defvar company-files--regexps
  (let* ((root (if (eq system-type 'windows-nt)
                   "[a-zA-Z]:/"
                 "/"))
         (begin (concat "\\(?:\\.\\{1,2\\}/\\|~/\\|" root "\\)")))
    (list (concat "\"\\(" begin "[^\"\n]*\\)")
          (concat "\'\\(" begin "[^\'\n]*\\)")
          (concat "\\(?:[ \t=]\\|^\\)\\(" begin "[^ \t\n]*\\)"))))

(defun company-files--grab-existing-name ()
  ;; Grab the file name.
  ;; When surrounded with quotes, it can include spaces.
  (let (file dir)
    (and (cl-dolist (regexp company-files--regexps)
           (when (setq file (company-grab-line regexp 1))
             (cl-return file)))
         (company-files--connected-p file)
         (setq dir (file-name-directory file))
         (not (string-match "//" dir))
         (file-exists-p dir)
         file)))

(defun company-files--connected-p (file)
  (or (not (file-remote-p file))
      (file-remote-p file nil t)))

(defun company-files--trailing-slash-p (file)
  ;; `file-directory-p' is very expensive on remotes. We are relying on
  ;; `file-name-all-completions' returning directories with trailing / instead.
  (let ((len (length file)))
    (and (> len 0) (eq (aref file (1- len)) ?/))))

(defvar company-files--completion-cache nil)

(defun company-files--complete (prefix)
  (let* ((dir (file-name-directory prefix))
         (file (file-name-nondirectory prefix))
         (key (list file
                    (expand-file-name dir)
                    (nth 5 (file-attributes dir))))
         (completion-ignore-case read-file-name-completion-ignore-case))
    (unless (company-file--keys-match-p key (car company-files--completion-cache))
      (let* ((candidates (mapcar (lambda (f) (concat dir f))
                                 (company-files--directory-files dir file)))
             (directories (unless (file-remote-p dir)
                            (cl-remove-if-not (lambda (f)
                                                (and (company-files--trailing-slash-p f)
                                                     (not (file-remote-p f))
                                                     (company-files--connected-p f)))
                                              candidates)))
             (children (and directories
                            (cl-mapcan (lambda (d)
                                         (mapcar (lambda (c) (concat d c))
                                                 (company-files--directory-files d "")))
                                       directories))))
        (setq company-files--completion-cache
              (cons key (append candidates children)))))
    (all-completions prefix
                     (cdr company-files--completion-cache))))

(defun company-file--keys-match-p (new old)
  (and (equal (cdr old) (cdr new))
       (string-prefix-p (car old) (car new))))

;;;###autoload
(defun company-files (command &optional arg &rest ignored)
  "`company-mode' completion backend existing file names.
Completions works for proper absolute and relative files paths.
File paths with spaces are only supported inside strings."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-files))
    (prefix (company-files--grab-existing-name))
    (candidates (company-files--complete arg))
    (location (cons (dired-noselect
                     (file-name-directory (directory-file-name arg))) 1))
    (post-completion (when (company-files--trailing-slash-p arg)
                       (delete-char -1)))
    (sorted t)
    (no-cache t)))

(provide 'company-files)
;;; company-files.el ends here

(use-package importmagic
  :ensure t
  :config
  (add-hook 'python-mode-hook 'importmagic-mode))

(use-package py-isort
  :ensure t
  :init
  (add-hook 'before-save-hook 'py-isort-before-save)
  :config
  (setq py-isort-options '("--lines=50"))
  )

(use-package py-autopep8
  :ensure t
  :init
  ;; enable autopep8 formatting on save
  ;; ignoring:
  ;; - E501 - Try to make lines fit within --max-line-length characters.
  ;; - W293 - Remove trailing whitespace on blank line.
  ;; - W391 - Remove trailing blank lines.
  ;; - W690 - Fix various deprecated code (via lib2to3).
  (setq py-autopep8-options '("--ignore=E501,W293,W391,W690"))
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  )
