(require 'package)

(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
;(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

(setq use-package-always-ensure t) ;; A bit redundant but that's fine

(defvar extern-directory (concat user-emacs-directory "extern/"))
(defun extern-file-exists (file-name)
  (let ((path (concat extern-directory file-name)))
    "Return whether a file exists in the extern .emacs.d sub-directory."
    (or (file-exists-p path)
	(file-symlink-p path)
	(file-directory-p path)))
  )

;; Split into logical groups somehow

(use-package company
  :ensure t
  :defer t
  :pin melpa-stable)

(use-package company-flow
  :config (add-to-list 'company-backends 'company-flow))

(use-package ido
  :init (progn (ido-mode 1)
               (ido-everywhere 1))
  :config
  (progn
    (setq ido-case-fold t)
    (setq ido-everywhere t)
    (setq ido-enable-prefix nil)
    (setq ido-enable-flex-matching t)
    (setq ido-create-new-buffer 'always)
    (setq ido-max-prospects 10)
    (setq ido-use-faces nil)))

(use-package flx-ido ;; Fuzzy matching
  :init (flx-ido-mode 1)
  :config (setq gc-cons-threshold 20000000)
  ;; author recommendation since it uses a lot of memory
  ;; Twenty megabytes and still swapping?!
  )

(use-package smex
  :init (smex-initialize)
  :bind ("M-x" . smex))

(use-package paredit
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode))

(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :init
  (progn
    (setq TeX-auto-save t
	  TeX-parse-self t
	  TeX-save-query nil
	  TeX-PDF-mode t)))

(use-package rust-mode
  :defer t
  :ensure t)


;; Need to install racer itself first
(when (extern-file-exists "rust-src")
  (use-package racer
    :ensure t
    :config
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
    (setq company-tooltip-align-annotations t)
    :init
    (setq racer-rust-src-path (concat extern-directory "rust-src"))))

;; Projectile setup (thanks to https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-projectile.el)
(defconst modi/rg-arguments
  `("--no-ignore-vcs"                   ; Ignore files/dirs ONLY from `.ignore'
    "--line-number"                     ; line numbers
    "--smart-case"
    "--follow"                          ; follow symlinks
    ,(concat "--ignore-file /home/" (getenv "USER") "/.ignore"))
  "Default rg arguments used in the functions in `counsel' and `projectile'
packages.")

(use-package projectile
  :config
  (progn
    (setq projectile-enable-caching t)
    (defun modi/advice-projectile-use-rg ()
      "Always use `rg' for getting a list of all files in the project."
      (let* ((prj-user-ignore-name (concat (projectile-project-root)
                                           ".ignore." (getenv "USER")))
             (prj-user-ignore (when (file-exists-p prj-user-ignore-name)
                                (concat "--ignore-file " prj-user-ignore-name))))
        (mapconcat 'identity
                   (append '("\\rg") ; used unaliased version of `rg': \rg
                           modi/rg-arguments
                           `(,prj-user-ignore) ; If nil, this will not be appended
                           '("--null" ; output null separated results,
                             "--files")) ; get names of all the to-be-searched files
                                        ; same as the "-g ''" argument in ag
                   " ")))
    ;; Use `rg' all the time if available
    (when (executable-find "rg")
	(advice-add 'projectile-get-ext-command :override #'modi/advice-projectile-use-rg))))

;; org-mode setup (put where?)
(require 'org)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-default-notes-file (concat org-directory "/capture.org"))
(setq org-catch-invisible-edits 'error)

(provide 'install-packages)
