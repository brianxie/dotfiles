;;; init.el --- Emacs editor configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs editor configuration. Written for 27.1.

;; See also early-init.el, which is loaded earlier in the startup sequence and
;; performs a limited set of configurations.

;;; Code:

;; Do not use the Custom UI, any of the customize-* functions, or anything else
;; that saves state in custom-set-variables.

;; While M-x customize and customize-set-variable/customize-save-variable have
;; the advantage of properly respecting defcustom keywords (:set and
;; :initialize) and being understood by the Custom UI, custom-set-variables is
;; otherwise more limited than manually setting variables or calling functions.
;; It does not permit structural organization or metadata, as
;; custom-set-variables is clobbered each time a change is made (e.g. by M-x
;; customize or M-x package-install); it does not permit any conditional
;; customization; it does not provide any way to manage ordering dependencies;
;; and it bifurcates the logic between settings which are exposed by Custom and
;; those which are not. If executed as elisp, customize-set-variable and
;; customize-save-variable may modify this init file when settings are persisted
;; to custom-set-variables, and/or result in duplicated declarations of the same
;; setting.

;; Customizations should be made in elisp using only setq/setq-default or
;; function calls, in order to preserve a single source-of-truth for a given
;; setting's configuration state. Additional defcustom requirements should be
;; handled by manually executing the corresponding elisp.

;; An exception is made for package-selected-packages, which is used by the
;; package manager to identify dependencies. This is not strictly necessary for
;; bootstrapping, as the :ensure keyword of use-package installs all of the
;; necessary packages without writing to package-selected-packages, but storing
;; package-selected-packages allows package-autoremove to work properly. The
;; declaration of package-selected-packages does not interfere with individual
;; use-package declarations, as they serve different purposes; the former
;; reflects the packages that must be installed on the system, while the latter
;; configures whether they are loaded and how they are configured.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(anzu atom-one-dark-theme centaur-tabs company doom-modeline evil evil-anzu evil-goggles flycheck ivy org org-roam use-package which-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

;; Check that all packages are present, and install any that are not.
(dolist (pkg package-selected-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Bootstrap use-package.
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)
;; All remaining packages are managed with use-package.
;; Use the :disabled keyword to selectively disable individual packages.

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
  :init (setq which-key-idle-delay cl-least-positive-float)
  :config (which-key-mode 1))

;; Editing tools

;; Syntax checking
(use-package flycheck
  :config
    (global-flycheck-mode 1)
    (setq flycheck-check-syntax-automatically '(mode-enabled save)))
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
(use-package evil-goggles
  :if (featurep 'evil)
  :after (evil)
  :config
    (evil-goggles-mode)
    (setq evil-goggles-duration 0.0625))

;; Auxiliary packages

;; Org mode
(use-package org)
;; Org-roam
(use-package org-roam
  :hook (after-init . org-roam-mode)
  :init
    ; Parent directory for all org-roam source files.
    (setq org-roam-directory (file-truename "~/.org-roam/"))
    (setq org-roam-db-update-method 'immediate)
  :config
    ; Overwrite the default org-roam template.
    (setq org-roam-capture-templates
      '(("d" "default" plain (function org-roam--capture-get-point) "%?"
        :file-name "${slug}"
        :head "#+TITLE: ${title}\n"
        :immediate-finish t
        :unnarrowed t))))

;;; init.el ends here
