;;; Commentary:
;; bootstrap-use-package.el
;;; Code:

(setq load-prefer-newer t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;; bootstrap-use-package.el ends here
