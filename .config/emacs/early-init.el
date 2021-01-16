;;; early-init.el --- Emacs editor configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs editor configuration. Written for 27.1.

;; See also init.el, which is loaded later in the startup sequence and contains
;; the bulk of configuration changes.

;; The contents of this file are executed before the package system and GUI are
;; initialized, while init.el is loaded after. Placing configurations in this
;; file may enable additional optimizations or customization; however, some
;; properties are not yet available at this point in the startup sequence and so
;; must go in init.el instead.

;;; Code:

;; Disable some GUI elements.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; early-init.el ends here
