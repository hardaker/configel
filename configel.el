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
;; 
;;  The purpose of the configel package is to auto-find and load other
;;  packages that exist in a directory or set of directories.
;; 
;;  For example, the following configuration can be used to bootstap
;;  everything else in a given set of directories:
;; 
;;    (setq configel-search-paths '("~/lib/elisp/emacs"))
;;    (add-to-list 'load-path "~/src/configel")
;;    (require 'configel)
;;    (configel-load-everything)
;;
;;  By default *if there is a .el file too* above each sub-directory
;;  found in the configel-search-paths list of directories, the package will:
;;
;;    - add each directory to the search path (or dir/lisp if it exists)
;;    - load the .elc or .el file above the directory
;;    - perform a (require 'name) if it could find one of:
;;      - name/name.elc
;;      - name/name.el
;;      - name/lisp/name.elc
;;      - name/lisp/name.el
;;  
;;; Installation:
;;; Code:

(defcustom configel-search-paths '("~/lib/elisp/emacs")
  "A list of directories to search through for packages and init files to load"
  :type 'list
  :group 'configel)

(defcustom configel-load-every-package nil
  "If t, load every found package even if no init file exists"
  :type 'boolean
  :group 'configel)

(defcustom configel-auto-require t
  "If t, attempt to 'require' a package of the same name as the dir found"
  :type 'boolean
  :group 'configel)

(defvar configel-current-path nil)

;;
;; functions to actually load everything
;;

;; (configel-load-everything)

(defun configel-load-everything ()
  "Loads all the packages it can find"
  (interactive)
  (mapcar 'configel-load-path configel-search-paths)
)

(defun configel-load-path (path)
  "Loads everything usable in a given path"
  (interactive)
  (setq configel-current-path path)
  (let ((dirs (directory-files-and-attributes path)))
    (mapcar 'configel-load-package dirs)
    ))

(defun configel-load-package (attributes)
  "Load a singular package with a full path"
  (interactive)
  (let*
      ((item  (car attributes))
       (fullitem (concat configel-current-path "/" item))
       (loadpath fullitem)
       (isdir (cadr attributes))
       (elfile (concat fullitem ".el"))
       (elcfile (concat elfile "c"))
       (configexists (or (file-exists-p elfile) (file-exists-p elcfile)))
       )
    (when (and isdir
	       (or configexists configel-load-every-package)
	       (not (equal item "."))
	       (not (equal item ".."))
	       )
      (message "Loading and configuring package %s for you..." item)
      ; if there is a lisp subdir, use that
      (if (file-exists-p (concat fullitem "/lisp"))
	  ;; XXX: this assumes it isn't a file
	  (setq loadpath (concat fullitem "/lisp")))
      ;; add the load path to our path list
      (add-to-list 'load-path loadpath)
      ;; load our config first
      (if (file-exists-p elcfile)
	  (load-file elcfile)
	(if (file-exists-p elfile)
	    (load-file elfile)))
      ;; then require the sub-package if we can
      (if (and configel-auto-require
	       (or
		(file-exists-p (concat loadpath "/" item ".elc"))
		(file-exists-p (concat loadpath "/" item ".el"))))
	  (require (intern item))))
    ))

(provide 'configel)
