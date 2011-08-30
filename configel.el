;;; configel.el --- Handles loading of multiple packages and init files

;; Filename: configel.el
;; Description:
;; Author: Wes Hardaker

;; Copyright © 2011 Wes Hardaker

;; Created: Tue Aug 30 06:01:54 PDT 2011
;; Version: 0.1
;; URL: https://github.com/hardaker/configel
;; Keywords:
;; Compatibility: GNU Emacs 23.2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;; Installation:
;;; Code:

(defcustom configel-search-paths '("~/lib/elisp")
  "A list of directories to search through for packages and init files to load"
  :type 'list
  :group 'configel)

(defcustom configel-load-every-package nil
  "If t, load every found package even if no init file exists"
  :type 'boolean
  :group 'configel)

;;
;; functions to actually load everything
;;

;; (configel-load-everything)

(defun configel-load-everything ()
  "Loads all the packages it can find"
  (interactive)
  (mapcar 'configel-load-package configel-search-paths)
)

(defun configel-load-path (path)
  "Loads everything usable in a given path"
  (interactive)
  (message (concat "loading: " path))
)

(defun configel-load-package (path)
  "Load a singular package with a full path"
  (interactive)
  (message (concat "loading: " path))
)
