;;; Commentary
;; org-setup.el
;;; Code:

(unless package-archive-contents    ;; Refresh the packages descriptions
  (package-refresh-contents))
(setq package-load-list '(all))     ;; List of packages to load
(unless (package-installed-p 'org)  ;; Make sure the Org package is
  (package-install 'org))           ;; installed, install it if not
(package-initialize)                ;; Initialize & Install Package

;(unless package-archive-contents    ;; Refresh the packages descriptions
;  (package-refresh-contents))
;(setq package-load-list '(all))     ;; List of packages to load
;(unless (package-installed-p 'org-plus-contrib)  ;; Make sure the Org package is
;  (package-install 'org-plus-contrib))           ;; installed, install it if not
;(package-initialize)                ;; Initialize & Install Package

;; org-setup.el ends here
