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
 '(package-selected-packages
   (quote
    (anzu atom-one-dark-theme centaur-tabs company doom-modeline evil flycheck ivy org which-key))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Check that all packages are present, and install any that are not.
(dolist (pkg package-selected-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Built-in configs
(global-display-line-numbers-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; UI packages
; Editor theme
(require 'atom-one-dark-theme)
(load-theme 'atom-one-dark t)
; Editor tabs
(require 'centaur-tabs)
(centaur-tabs-mode 1)
; Modeline
(require 'doom-modeline)
(doom-modeline-mode 1)
(require 'anzu)
(global-anzu-mode 1)

;; Non-editing tools
; Ivy
(require 'ivy)
(ivy-mode 1)
; Which-key
(require 'which-key)
(which-key-mode)

;; Editing tools
; Flycheck
(require 'flycheck)
(global-flycheck-mode 1)
; Autocompletion
(require 'company)
(global-company-mode 1)
; Evil mode
(setq evil-toggle-key "C-c v")
(setq evil-default-state 'emacs)
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)

;; Auxiliary packages
; Org mode
(require 'org)
