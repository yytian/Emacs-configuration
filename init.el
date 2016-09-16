;; http://stackoverflow.com/questions/16676750/windows-emacs-git-bash-and-shell-command
;; Generalize this later (make a file for system-specific setup)
(if (equal system-type 'windows-nt)
    (progn
      (unless (gnutls-available-p) (error "GNU TLS is not available and you are using Windows. You may have to manually install it."))
      (setq explicit-shell-file-name "C:/Program Files/Git/bin/sh.exe") ; Check for failure
      (setq shell-file-name explicit-shell-file-name)
      (add-to-list 'exec-path "C:/Program Files/Git/bin")
      (setq explicit-sh.exe-args '("--login" "-i"))
      (setenv "SHELL" shell-file-name)
      (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))


(desktop-save-mode 1)

;; Remove scrollbars, menu bars, and toolbars
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (wheatgrass))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defvar root-dir (file-name-directory load-file-name)
  "The root dir of the Emacs configuration.")

;; Is lib still needed?
(defvar library-dir (expand-file-name "lib" root-dir)
  "Where external libraries are stored. Must manually update.")

(defvar package-dir (expand-file-name "pkg" root-dir)
  "Package management.")

(add-to-list 'load-path library-dir)
(add-to-list 'load-path package-dir)
(require 'install-packages)
