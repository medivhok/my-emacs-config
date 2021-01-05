;;; init.el --- My Emacs Config -*- lexical-binding: t; -*-
;; Author: Jean Gregory Verret <gregory.verret@gmail.com>
;; URL: https://github.com/medivhok/my-emacs-config
;;; Commentary: My emacs configuration file.
;;; Code:

;; Profiling

;; Make startup faster by reducing the frequency of garbage collection and then
;; use a hook to measure Emacs startup time. The default is 800k (mesured in
;; bytes).


(setq gc-cons-threshold (* 50 1000 1000))



;; Profile emacs startup.

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Configuration Definitions

;; The configuration directory.


(defconst medivhok:config-directory
  (file-name-as-directory user-emacs-directory)
  "The root directory containing the configuration files.")

(defconst medivhok:config-exwm-directory
  (file-name-as-directory
   (expand-file-name "+exwm" medivhok:config-directory))
  "The exwm module configurations directory.")

(defconst medivhok:config-ui-directory
  (file-name-as-directory
   (expand-file-name "+ui" medivhok:config-directory)))

(defconst medivhok:config-app-directory
 (file-name-as-directory (expand-file-name "+app" medivhok:config-directory)))

(defconst medivhok:config-ides-directory
 (file-name-as-directory (expand-file-name "+ides" medivhok:config-directory)))

;; Cache Configuration

;; To keep our configuration directory clean, we are gonna use another directory
;; for our cache. I don’t want a bunch of transient files showing up as untracked
;; in the Git repository. But first, we save the root directory containing the
;; configuration files.


(setq user-emacs-directory "~/.cache/emacs/"
      backup-directory-alist `(("." . ,(expand-file-name "backups"
                                                         user-emacs-directory)))
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-"
                                                   user-emacs-directory)
      projectile-known-projects-file (expand-file-name
                                      "projectile-bookmarks.eld"
                                      user-emacs-directory))

;; Keep customization settings in a temporary file (thanks Ambrevar!).
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid))
                          temporary-file-directory)))
(load custom-file t)

(setq user-full-name "Jean Gregory Verret"
      user-mail-address "gregory.verret@gmail.com")

;; ~straight.el~

;; Packages need to be installed first (if not already installed), and loaded
;; before we can use them.

;; To install them, [[https://github.com/raxod502/straight.el][straight.el]], the next-generation, purely functional
;; package manager for the Emacs hacker is used and to load them, [[https://github.com/jwiegley/use-package][use-package]] is
;; used and integrated to straight.


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; ~use-package~

;; Using straight, we can now download, install and load /use-package/.


(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-verbose t)

;; Configuration Modules


(defconst init-file-name "init.el"
  "The name of the configuration file of a module to be loaded.")

;; Emacs X Window Manager

;; The [[./+exwm/][exwm]] module loading.


(load-file (expand-file-name init-file-name medivhok:config-exwm-directory))

;; The User Interface

;; The [[./+ui/][ui]] module loading.


(load-file (expand-file-name init-file-name medivhok:config-ui-directory))

;; Applications

;; The [[./+app/][app]] module loading.


(load-file (expand-file-name init-file-name medivhok:config-app-directory))

;; Integrated Development Environments

;; The [[./+ides/][ides]] module loading.


(load-file (expand-file-name init-file-name medivhok:config-ides-directory))
