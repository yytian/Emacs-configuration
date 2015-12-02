(require 'package)

(eval-when-compile
  (require 'use-package))
;(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(setq use-package-always-ensure t)

(use-package magit
	     :ensure t)

(use-package org
	     :ensure t)

(use-package clojure-mode
	     :ensure t)

(provide 'install-packages)
