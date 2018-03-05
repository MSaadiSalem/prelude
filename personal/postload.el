;;; Commentary
;; postload.el -- Emacs Basic Configuration
;;; Code:

(require 'org)
(require 'ob-tangle)

(load-file (concat user-emacs-directory "personal/post-config/use-package-setup.el"))

(org-babel-load-file (concat user-emacs-directory "personal/post-config/org-config.org"))
(org-babel-load-file (concat user-emacs-directory "personal/post-config/pkg.org"))
(org-babel-load-file (concat user-emacs-directory "personal/post-config/dev-config.org"))

;;; postload.el ends here
