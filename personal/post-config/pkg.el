(use-package lorem-ipsum
  :ensure t
  :bind
  ("C-c C-l s". lorem-ipsum-insert-sentences)
  ("C-c C-l p". lorem-ipsum-insert-paragraphs)
  ("C-c C-l l". lorem-ipsum-insert-list)
  )

(use-package multiple-cursors
  :ensure t
  :bind(
  ;; When you have an active region that spans multiple lines,
  ;; the following will add a cursor to each line:
  ("C-S-c C-S-c". mc/edit-lines)

  ;; When you want to add multiple cursors not based on continuous lines,
  ;; but based on keywords in the buffer:
  ("C->". mc/mark-next-like-this)
  ("C-<". mc/mark-previous-like-this)
  ("C-c C-<". mc/mark-all-like-this))
  )

(use-package color-identifiers-mode
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-color-identifiers-mode)
  )

(use-package auto-yasnippet
  :ensure t
  :bind
  ("H-w". aya-create)
  ("H-y". aya-expand)
  )

(use-package yasnippet
  :ensure t
  :diminish yas
  :init
  (yas/initialize)
  (yas/global-mode 1)
  (yas-load-directory "~/.emacs.d/snippets/")
  )
