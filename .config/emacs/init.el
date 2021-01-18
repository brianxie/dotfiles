;;; init.el --- Emacs editor configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs editor configuration. Written for 27.1.

;; See also early-init.el, which is loaded earlier in the startup sequence and
;; performs a limited set of configurations.

;; This file is static and should be tracked under version control. However, any
;; modifications made by the Custom UI, any of the customize-* functions, or
;; anything else that saves state in custom-set-variables will not be written to
;; this file; they are instead written to custom-file, which is not tracked
;; under version control. This is because custom variables can be changed
;; arbitrarily and destructively, so keeping them separate from the init file
;; prevents unwanted modifications to the init script, but the consequence is
;; that any configuration changes intended to be both portable and persistent
;; must be specified in this file.

;;; Code:

;; Define the location of the custom file, which stores some local configuration
;; settings generated by Emacs (for example, Customize settings and
;; package-selected-packages).
(setq custom-file
  (file-truename (concat (getenv "XDG_CONFIG_HOME") "/emacs/custom.el")))
;; Call (load custom-file) to use these customizations.

;; Configure emacs built-in preferences.
(require 'cl-lib)
(cl-float-limits)
(setq echo-keystrokes cl-least-positive-float)
(setq garbage-collection-messages t)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(setq-default fill-column 80)
(setq-default sentence-end-double-space nil)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)

;; Add MELPA to list of package archives.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; Update package archive if currently empty.
;; Ensures that the local package repository is up-to-date the first time this
;; init.el is run.
(package-read-all-archive-contents)
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if it is not already installed.
(dolist (pkg '(use-package))
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Bootstrap use-package.
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

;; All remaining packages are managed (and installed if necessary) by
;; use-package.
;; Use the :disabled keyword to selectively disable individual packages.
;; Packages which are :disabled will not be installed!

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
  :if (featurep 'evil)
  :after (evil))

;; Non-editing tools

;; Completion
(use-package ivy
  :config (ivy-mode 1))
;; Keybinding help
(use-package which-key
  :init (setq which-key-idle-delay cl-least-positive-float)
  :config (which-key-mode 1))

;; Editing tools

;; Syntax checking
(use-package flycheck
  :init (setq flycheck-check-syntax-automatically '(mode-enabled save)))
  :config (global-flycheck-mode 1)
;; Autocompletion
(use-package company
  :commands company-mode
  :init
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 1))
;; Vim emulation
(use-package evil
  :init
    (setq evil-toggle-key "C-c v")
    (setq evil-default-state 'emacs)
    (setq evil-want-C-u-scroll t)
  :config
    (add-hook 'evil-emacs-state-entry-hook
      (lambda () (setq display-line-numbers t)))
    (add-hook 'evil-emacs-state-exit-hook
      (lambda () (setq display-line-numbers 'relative)))
    (evil-mode 1))
(use-package evil-goggles
  :disabled
  :if (featurep 'evil)
  :after (evil)
  :init (setq evil-goggles-duration 0.0625)
  :config (evil-goggles-mode))

;; Auxiliary packages

;; Org mode
(use-package org
  :init (setq org-adapt-indentation nil))
;; Org-roam
(use-package org-roam
  :hook (after-init . org-roam-mode)
  :init
    (setq org-roam-db-update-method 'immediate)
    ; Overwrite the default org-roam template.
    (setq org-roam-capture-templates
      '(("d" "default" plain (function org-roam--capture-get-point) "%?"
        :file-name "${slug}"
        :head "#+TITLE: ${title}\n"
        :immediate-finish t
        :unnarrowed t))))

;;; init.el ends here
