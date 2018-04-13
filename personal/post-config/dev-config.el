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

(load-file (concat user-emacs-directory "personal/post-config/company-files.el"))

(use-package importmagic
  :ensure t
  :config
  (add-hook 'python-mode-hook 'importmagic-mode))

(use-package py-isort
  :ensure t
  :init
  ;;(add-hook 'before-save-hook 'py-isort-before-save)
  :config
  (setq py-isort-options '("--lines=4"))
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
