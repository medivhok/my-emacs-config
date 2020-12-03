;;; init.el --- My Emacs Config -*- lexical-binding: t; -*-
;; Author: Jean Gregory Verret
;; Url: https://github.com/medivhok/my-emacs-config
;; Code:
(setq user-full-name "Jean Gregory Verret"
      user-mail-address "gregory.verret@gmail.com")

(let* ((init-file-name "init.el")
         (medivhok:cfg-directory
          (file-name-as-directory user-emacs-directory))
         (medivhok:cfg:bootstrap-directory
          (file-name-as-directory (expand-file-name "+bootstrap" medivhok:cfg-directory)))
         (medivhok:cfg:exwm-directory
          (file-name-as-directory (expand-file-name "+exwm" medivhok:cfg-directory)))
         (medivhok:cfg:ui-directory
          (file-name-as-directory (expand-file-name "+ui" medivhok:cfg-directory)))
         (medivhok:cfg:app-directory
          (file-name-as-directory (expand-file-name "+app" medivhok:cfg-directory)))
         (medivhok:cfg:ides-directory
          (file-name-as-directory (expand-file-name "+ides" medivhok:cfg-directory))))

          (load-file (expand-file-name init-file-name medivhok:cfg:bootstrap-directory))
          (load-file (expand-file-name init-file-name medivhok:cfg:exwm-directory))
          (load-file (expand-file-name init-file-name medivhok:cfg:ui-directory))
          (load-file (expand-file-name init-file-name medivhok:cfg:app-directory))
          (load-file (expand-file-name init-file-name medivhok:cfg:ides-directory)))
;; init.el ends here.
