;;; init.el --- Emacs editor configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs editor configuration.

;;; Code:

;; Configure emacs built-in preferences.
(setq inhibit-startup-screen t)
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(column-number-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Add MELPA to list of package archives.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (anzu atom-one-dark-theme centaur-tabs company doom-modeline
    evil evil-anzu flycheck ivy org use-package which-key))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Update package archive if currently empty.
;; Ensures that the local package repository is up-to-date the first
;; time this init.el is run.
(when (not package-archive-contents)
  (package-refresh-contents))

;; Check that all packages are present, and install any that are not.
(dolist (pkg package-selected-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Bootstrap use-package.
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)
;; All remaining packages are managed with use-package.
;; Use the :disabled keyword to selectively disable individual
;; packages.

;; UI packages

;; Atom One Dark theme
(use-package atom-one-dark-theme
  :config (load-theme 'atom-one-dark t))
;; Editor tabs
(use-package centaur-tabs
  :config (centaur-tabs-mode 1))
;; Modeline
(use-package doom-modeline
  :config (doom-modeline-mode 1))
;; Modeline incremental search
(use-package anzu
  :config (global-anzu-mode 1))
(use-package evil-anzu
  :if (featurep 'evil))

;; Non-editing tools

;; Completion
(use-package ivy
  :config (ivy-mode 1))
;; Keybinding help
(use-package which-key
  :config (which-key-mode 1))

;; Editing tools

;; Syntax checking
(use-package flycheck
  :config (global-flycheck-mode 1))
;; Autocompletion
(use-package company
  :disabled
  :init
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 1)
  :config (global-company-mode 1))
;; Vim emulation
(use-package evil
  :init
    (setq evil-toggle-key "C-c v")
    (setq evil-default-state 'emacs)
    (setq evil-want-C-u-scroll t)
  :config (evil-mode 1))

;; Auxiliary packages

;; Org mode
(use-package org)

;;; init.el ends here
