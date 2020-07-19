;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; Update packages
;; This only needs to be executed once, during the first time that
;; this init.el is loaded, to ensure that the selected packages are
;; present.
(package-refresh-contents)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (atom-one-dark-theme doom-modeline evil org))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Check that all packages are present, and install any that are not.
(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (package-install pkg)))

;; Misc
(global-display-line-numbers-mode t)

;; UI packages
(load-theme 'atom-one-dark t)

;; Evil mode
(setq evil-toggle-key "C-c v")
(setq evil-default-state 'emacs)
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode t)

;; Org mode
(require 'org)

;; Doom modeline
(require 'doom-modeline)
(doom-modeline-mode 1)
