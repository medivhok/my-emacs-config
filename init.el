;;; init.el --- My Emacs Config -*- lexical-binding: t; -*-
;; Author: Jean Gregory Verret
;; Url: https://github.com/medivhok/my-emacs-config
;; Code:
(defconst medivhok:init-directory
  (file-name-as-directory user-emacs-directory)
  "The directory of the base init.el.")

;; Bootstrapping


(defconst medivhok:bootstrapping-directory
  (file-name-as-directory
   (expand-file-name "bootstrapping" medivhok:init-directory))
  "The bootstrapping configurations directory.")

(load-file (expand-file-name "init.el"
                             medivhok:bootstrapping-directory))

;; EXWM


(defconst medivhok:exwm-directory
  (file-name-as-directory
   (expand-file-name "exwm" medivhok:init-directory))
  "The Emacs X Window Manager configurations directory.")

(load-file (expand-file-name "init.el"
                             medivhok:exwm-directory))

;; User Interface


(defconst medivhok:user-interface-directory
  (file-name-as-directory
   (expand-file-name "user-interface" medivhok:init-directory))
  "The user interface configurations directory.")

(load-file (expand-file-name "init.el"
                             medivhok:user-interface-directory))

;; Applications


(defconst medivhok:applications-directory
  (file-name-as-directory
   (expand-file-name "applications" medivhok:init-directory))
  "The applications configurations directory.")

(load-file (expand-file-name "init.el"
                             medivhok:applications-directory))

;; IDEs


(defconst medivhok:ides-directory
  (file-name-as-directory
   (expand-file-name "ides" medivhok:init-directory))
  "The applications configurations directory.")

(load-file (expand-file-name "init.el"
                             medivhok:ides-directory))
