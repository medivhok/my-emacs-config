;;; init.el --- My Emacs Config -*- lexical-binding: t; -*-



;; Author: Jean Gregory Verret

;; Url: https://github.com/medivhok/my-emacs-config

;; Code:

(add-to-list 'load-path "~/.config/emacs/")

;; Profiling

;; Make startup faster by reducing the frequency of garbage collection and then use
;; a hook to measure Emacs startup time.

;; The default is 800k (mesured in bytes).


(setq gc-cons-threshold (* 50 1000 1000))



;; Profile emacs startup.


(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Cache Directory

;; To keep our config directory clean, we are gonna use another directory
;; for our cache. I don’t want a bunch of transient files showing up as
;; untracked in the Git repository.


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

;; ~user-full-name~


(setq user-full-name "Jean Gregory Verret")

;; ~user-mail-address~


(setq user-mail-address "gregory.verret@gmail.com")

;; ~medivhok:app-directory~


(setq medivhok:app-directory (file-name-as-directory "~/org"))

;; ~medivhok:agenda-directory~


(setq medivhok:agenda-directory
      (file-name-as-directory
       (expand-file-name "agenda"
                         medivhok:app-directory)))

;; ~medivhok:budget-directory~


(setq medivhok:budget-directory
      (file-name-as-directory
       (expand-file-name "budget"
                         medivhok:app-directory)))

;; ~medivhok:slip-box-directory~


(setq medivhok:slip-box-directory
      (file-name-as-directory
       (expand-file-name "roam"
                         medivhok:app-directory)))

;; X Window Management

;; [fn:: Source]


(defconst medivhok:exwm-enabled
  (and (eq window-system 'x)
       (seq-contains command-line-args "--use-exwm")))

(use-package exwm
  :init
  (setq exwm-layout-show-all-buffers t
        exwm-workspace-number 5
        exwm-workspace-show-all-buffers t
        exwm-workspace-warp-cursor t
        focus-follows-mouse t
        mouse-autoselect-window nil)

  :config
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  (require 'exwm-randr)
  (setq exwm-randr-workspace-output-plist '(0 "HDMI-0" 1 "DP-1"))
  (exwm-randr-enable)

  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  (setq exwm-input-global-keys
        `(([?\s-r] . exwm-reset)
          ([?\s-w] . exwm-worspace-switch)
          ([?\s-p] . counsel-linux-app)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-enable))

;; Theme

;; We configure the theme.

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	      doom-themes-enable-italic t)
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Fonts

;; The fonts.


(custom-theme-set-faces
 'user
 `(default ((t (:family "Roboto Mono" :height 140 :weight light))))
 `(fixed-pitch ((t (:family "FiraCode" :height 130 :weight light)))))

;; ~all-the-icons~
;; :PROPERTIES:
;; :Custom_ID: use-package--all-the-icons
;; :END:

;; #+begin_quote
;; A utility package to collect various Icon Fonts and propertize them within Emacs. -- [[https://github.com/domtronn/all-the-icons.el][all-the-icons]]
;; #+end_quote


(use-package all-the-icons

;; Preface (~:preface~)
;; :PROPERTIES:
;; :Custom_ID: use-package--all-the-icons--preface
;; :END:

;; #+begin_quote
;; NOTE: This code is executed right away.
;; #+end_quote

;; We start the [[https://github.com/jwiegley/use-package#add-preface-occurring-before-everything-except-disabled][:preface]] section of the [[#use-package--all-the-icons][use-package]].

;; Show some nice symbols (ex.: ~lambda~ becomes $\lambda$)


:preface
(global-prettify-symbols-mode 1)

;; Closing Paren


)

;; Frames

;; Set the frame transparency.


(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;; Scrolling

;; Improve scrolling.


;; One line at a time.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Don't accelerate scrolling.
(setq mouse-wheel-progressive-speed nil)

;; Scroll window under mouse.
(setq mouse-wheel-follow-mouse 't)

;; Keyboard scroll one line at a time.
(setq scroll-step 1)

;; Visuals

;; Line and column numbers.


(setq display-line-numbers-type 'relative
      display-line-numbers-width-start t)
;; (global-display-line-numbers-mode t)
(column-number-mode)



;; Set up the visible bell.


(setq visible-bell t)



;; Highlight current line.


(global-hl-line-mode t)



;; Time format.


(setq display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)

(setq-default fill-column 80)

;; Widgets

;; Disable the scroll bar and tooltips.


(scroll-bar-mode -1)
(tooltip-mode -1)

;; We disable the tool and menu bar.
(tool-bar-mode -1)
(menu-bar-mode -1)



;; Give some breathing room.


(set-fringe-mode 10)

;; Windows

;; Maximize windows by default.


(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; ~evil~
;; :PROPERTIES:
;; :Custom_ID: user-interface--keybindings--evil
;; :END:

;; #+begin_quote
;; Now you see that evil will always triumph, because good is dumb. -- Dark Helmet
;; #+end_quote


(use-package evil
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump nil)
  (evil-respect-visual-line-mode t)

  :config
  (evil-mode 1))

;; ~evil-collection~
;; :PROPERTIES:
;; :Custom_ID: user-interface--keybindings--evil-collection
;; :END:


(use-package evil-collection
  :defer t
  :after evil

  :custom
  (evil-collection-outline-bind-tab-p nil))

;; ~hydra~
;; :PROPERTIES:
;; :Custom_ID: user-interface--keybindings--hydra
;; :END:

;; #+begin_quote
;; make Emacs bindings that stick around. -- [[https://github.com/abo-abo/hydra][hydra]]
;; #+end_quote


(use-package hydra
  :defer t)

;; ~which-key~
;; :PROPERTIES:
;; :Custom_ID: use-package--which-key
;; :END:

;; #+begin_quote
;; Emacs package that displays available keybindings in popup. -- [[https://github.com/justbur/emacs-which-key][which-key]]
;; #+end_quote


(use-package which-key
  :custom
  (which-key-idle-delay 0.3)

  :config
  (which-key-mode))

;; ~general~
;; :PROPERTIES:
;; :Custom_ID: use-package--general
;; :END:

;; #+begin_quote
;; More convenient key definitions in emacs. -- [[https://github.com/noctuid/general.el][general.el]]
;; #+end_quote


(use-package general
  :config
  (general-evil-setup t))

;; Keybindings
;; :PROPERTIES:
;; :Custom_ID: user-interface--keybindings
;; :END:


(defvar medivhok:leader-key "SPC"
  "The leader key.")

(defconst medivhok:non-normal-leader-key "M-SPC"
  "The 'non normal state' leader key.")

(defun medivhok/expand-menu-key (menu-key &optional non-normal-menu-key)
  "Returns a string of the MENU-KEY prefixed by `medivhok:leader-key' and
a space. If NON-NORMAL-MENU-KEY is non nil, `medivhok:non-local-leader-key'
is used instead."
  (if (null non-normal-menu-key)
      (concat medivhok:leader-key " " menu-key)
    (concat medivhok:non-normal-leader-key " " menu-key)))



;; ~medivhok:leader-menu~

(defhydra hydra-zoom ()
  "zoom"
  ("-" text-scale-decrease "out")
  ("=" text-scale-increase "in"))

(general-create-definer medivhok:main-menu
  :prefix medivhok:leader-key
  :non-normal-prefix medivhok:non-normal-leader-key
  :keymaps 'override)

(medivhok:main-menu
  :states 'normal
  ":" 'execute-extended-command
  "-" '(hydra-zoom/text-scale-decrease
        :which-key "text-scale-decrease")
  "=" '(hydra-zoom/text-scale-increase
        :which-key "text-scale-increase"))



;; ~medivhok:local-mode-menu~

(general-create-definer medivhok:local-mode-menu
  :prefix (medivhok/expand-menu-key "m")
  :non-normal-prefix (medivhok/expand-menu-key "m" t)
  :keymaps 'override)



;; ~medivhok:applications-menu~

(general-create-definer medivhok:applications-menu
  :prefix (medivhok/expand-menu-key "a")
  :non-normal-prefix (medivhok/expand-menu-key "a" t)
  :keymaps 'override
  nil '(:ignore t :which-key "applications"))



;; ~medivhok:buffer-menu~

(general-create-definer medivhok:buffer-menu
  :prefix (medivhok/expand-menu-key "b")
  :non-normal-prefix (medivhok/expand-menu-key "b" t)
  :keymaps 'override
  nil '(:ignore t :which-key "buffer"))

(medivhok:buffer-menu
  :states 'normal
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  "d" 'kill-current-buffer)



;; ~medivhok:emacs-menu~

(general-create-definer medivhok:emacs-menu
  :prefix (medivhok/expand-menu-key "e")
  :non-normal-prefix (medivhok/expand-menu-key "e" t)
  :keymaps 'override
  nil '(:ignore t :which-key "emacs"))

(medivhok:emacs-menu
  :states 'normal
  "e" '((lambda ()
          (interactive)
          (find-file
           (expand-file-name "README.org"
                             (file-name-directory user-init-file))))
        :which-key "edit literate config")
  "E" '((lambda ()
          (interactive)
          (find-file
           (expand-file-name "init.el"
                             (file-name-directory user-init-file))))
        :which-key "edit config"))



;; ~medivhok:file-menu~

(general-create-definer medivhok:file-menu
  :prefix (medivhok/expand-menu-key "f")
  :non-normal-prefix (medivhok/expand-menu-key "f" t)
  :keymaps 'override
  nil '(:ignore t :which-key "file"))

(medivhok:file-menu
  :states 'normal
  "f" 'find-file
  "r" 'counsel-recentf)



;; ~medivhok:help-menu~

(general-create-definer medivhok:help-menu
  :prefix (medivhok/expand-menu-key "h")
  :non-normal-prefix (medivhok/expand-menu-key "h" t)
  :keymaps 'override
  nil '(:ignore t :which-key "help"))

(medivhok:help-menu
  :states 'normal
  "a" 'apropos-command
  "b" 'describe-bindings
  "c" 'describe-face
  "f" 'describe-function
  "i" 'info
  "k" 'general-describe-keybindings
  "s" 'counsel-describe-symbol
  "v" 'describe-variable)



;; ~medivhok:notes-menu~

(general-create-definer medivhok:notes-menu
  :prefix (medivhok/expand-menu-key "n")
  :non-normal-prefix (medivhok/expand-menu-key "n" t)
  :keymaps 'override
  nil '(:ignore t :which-key "notes"))



;; ~medivhok:window-menu~

(general-create-definer medivhok:window-menu
  :prefix (medivhok/expand-menu-key "w")
  :non-normal-prefix (medivhok/expand-menu-key "w" t)
  :keymaps 'override
  nil '(:ignore t :which-key "window"))

(medivhok:window-menu
  :states 'normal
  "q" 'delete-window
  "s" 'split-window-below
  "\\" 'split-window-right)



;; ~medivhok:quit-menu~

(general-create-definer medivhok:quit-menu
  :prefix (medivhok/expand-menu-key "q")
  :non-normal-prefix (medivhok/expand-menu-key "q" t)
  :keymaps 'override
  nil '(:ignore t :which-key "quit"))

(medivhok:quit-menu
  :states 'normal
  "q" 'save-buffers-kill-terminal)

;; Dashboard


(use-package dashboard
  :preface
  (setq inhibit-startup-message t)

  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
        dashboard-items '((recents . 5)
                          (agenda . 5)))
  (evil-collection-init 'dashboard))

;; ~amx~
;; :PROPERTIES:
;; :Custom_ID: use-package--amx
;; :END:

;; #+begin_quote
;; An alternative M-x interface for Emacs. -- [[https://github.com/DarwinAwardWinner/amx][amx]]
;; #+end_quote


(use-package amx
  :after ivy

  :custom
  (amx-backend 'ivy)

  :config
  (amx-mode))

;; ~counsel~
;; :PROPERTIES:
;; :Custom_ID: use-package--counsel
;; :END:

;; #+begin_quote
;; Ivy - a generic completion frontend for Emacs, Swiper - isearch with an
;; overview, and more. Oh, man! -- [[https://github.com/abo-abo/swiper][swiper]]
;; #+end_quote

;; #+begin_center
;; NOTE: By installing ~counsel~, ~ivy~ and ~swiper~ will automatically be installed as
;; dependencies.
;; #+end_center


(use-package counsel

;; Load Package After... (~:after~)
;; :PROPERTIES:
;; :Custom_ID: use-package--counsel--after
;; :END:

;; We start the [[https://github.com/jwiegley/use-package#loading-packages-in-sequence][:after]] section of the [[#use-package--counsel][use-package]].


:after evil-collection

;; Configurations (~:config~)
;; :PROPERTIES:
;; :Custom_ID: use-package--counsel--config
;; :END:

;; #+begin_quote
;; NOTE: This code is executed AFTER the package is loaded.
;; #+end_quote

;; We start the [[https://github.com/jwiegley/use-package#getting-started][:config]] section of the [[#use-package--counsel][use-package]].


:config
(evil-collection-init 'ivy)
(setq ivy-use-virtual-buffers t
      ivy-count-format "(%d/%d) ")
(ivy-mode 1)
(counsel-mode 1)

;; Closing Paren


)

;; ~ivy-rich~
;; :PROPERTIES:
;; :Custom_ID: use-package--ivy-rich
;; :END:

;; #+begin_quote
;; More friendly interface for ivy. -- [[https://github.com/Yevgnen/ivy-rich][ivy-rich]]
;; #+end_quote


(use-package ivy-rich

;; Load Package After... (~:after~)
;; :PROPERTIES:
;; :Custom_ID: use-package--ivy-rich--after
;; :END:

;; We start the [[https://github.com/jwiegley/use-package#loading-packages-in-sequence][:after]] section of the [[#use-package--ivy-rich][use-package]].


:after ivy

;; Configurations (~:config~)
;; :PROPERTIES:
;; :Custom_ID: use-package--ivy-rich--config
;; :END:

;; #+begin_quote
;; NOTE: This code is executed AFTER the package is loaded.
;; #+end_quote

;; We start the [[https://github.com/jwiegley/use-package#getting-started][:config]] section of the [[#use-package--ivy-rich][use-package]].


:config
(ivy-rich-mode 1)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

;; Closing Paren


)

;; ~ivy-posframe~

;; #+begin_quote
;; ivy-posframe is a ivy extension, which let ivy use posframe to show
;; its candidate menu. -- [[https://github.com/tumashu/ivy-posframe][ivy-posframe]]
;; #+end_quote


(use-package ivy-posframe
  :after ivy

  :custom
  (ivy-posframe-display-functions-alist
   '((t . ivy-posframe-display-at-frame-center)))

  :config
  (ivy-posframe-mode 1))

;; doom-modeline


(use-package doom-modeline
  :hook
  (window-setup . doom-modeline-mode)

  ;; :custom-face
  ;; (mode-line ((t (:height 0.85))))
  ;; (mode-line-inactive ((t (:height 0.85))))

  :init
  (setq doom-modeline-bar-width 6
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-buffer-state-icon t
        doom-modeline-github nil
        doom-modeline-height 15
        doom-modeline-icon (display-graphic-p)
        doom-modeline-irc nil
        doom-modeline-lsp t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-minor-modes t
        doom-modeline-mu4e nil
        doom-modeline-persp-name nil
        doom-modeline-project-detection 'projectile))

;; minions

;; Configuration of the modeline.


(use-package minions
  :init
  (setq minions-mode-line-lighter " ")

  :config
  (minions-mode 1))

;; Notifications

;; Don't warn for large files.


(setq large-file-warning-threshold nil)



;; Don't warn for following symlinked files.


(setq vc-follow-symlinks t)



;; Don't warn when advice is added for functions.


(setq ad-redefinition-action 'accept)

;; Help Interface (~helpful~)


(use-package helpful
  :after
  (counsel evil-collection)

  :config
  (evil-collection-init 'helpful)
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable))

;; Org Ecosystem

;; #+NAME: medivhok:org-directory

(defconst medivhok:org-directory
  (file-name-as-directory "~/org")
  "The root directory of the org ecosystem.")

(defconst medivhok:agenda-directory
  (file-name-as-directory
   (expand-file-name "agenda" medivhok:org-directory))
  "The directory of my agenda files.")

(defconst medivhok:pdf-directory
  (file-name-as-directory
   (expand-file-name "readings" medivhok:org-directory))
  "The directory of my pdf files.")

(defconst medivhok:roam-directory
  (file-name-as-directory
   (expand-file-name "roam" medivhok:org-directory))
  "The directory of my roam files.")

(defconst medivhok:bibtex-file
  (expand-file-name "zotero.bib" medivhok:pdf-directory)
  "My default bibliography file.")

;; The One to Rule Them All

;; #+begin_quote
;; Org mode is for keeping notes, maintaining TODO lists, planning projects, and
;; authoring documents with a fast and effective plain-text system. -- [[https://orgmode.org/][org]]
;; #+end_quote

;; #+NAME: package:org

(use-package org
  :init
  (setq org-directory medivhok:org-directory)

  :hook
  (org-mode . org-indent-mode)
  (org-mode . variable-pitch-mode)
  (org-mode . visual-line-mode)

  :custom-face
  (org-block ((t (:inherit fixed-pitch))))
  (org-document-title ((t (:height 1.5 :weight light))))
  (org-level-1 ((t (:inherit outline-1 :height 1.3 :weight light))))
  (org-level-2 ((t (:inherit outline-2 :height 1.2 :weight light))))
  (org-level-3 ((t (:inherit outline-3 :height 1.1 :weight light))))
  (org-link ((t (:inherit link :underline nil :weight regular))))
  (org-meta-line ((t (:inherit fixed-pitch))))
  (org-table ((t (:inherit org-default :family "Roboto Mono"))))
  (org-table-header ((t (:inherit org-table :family "Roboto Mono"))))

  :general
  (medivhok:local-mode-menu 'normal
    org-mode-map
    nil '(:ignore t :which-key "org")
    "e" 'org-export-dispatch
    "t" '(:ignore t :which-key "toggle")
    "tl" 'org-toggle-link-display)

  :custom
  (org-fontify-quote-and-verse-blocks t)

  :config
  (setq org-agenda-files (list medivhok:agenda-directory)
        org-catch-invisible-edits 'show
        org-cycle-separator-lines 2
        org-directory medivhok:app-directory
        org-edit-src-content-indentation 0
        org-ellipsis " ▼"
        org-hide-block-startup nil
        org-hide-emphasis-markers t
        org-log-done 'time
        org-log-into-drawer t
        org-outline-path-complete-in-steps nil
        org-return-follows-link t
        org-src-fontify-natively t
        org-src-preserve-indentation nil
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-startup-folded t)

  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 4.0))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (haskell . t)
     (ledger . t)
     (R . t)))

  (add-hook 'org-src-mode-hook
            (lambda ()
              (when (eq major-mode 'emacs-lisp-mode)
                (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc)))))

  ;; Replace list hyphen with dot.
  (require 'org-indent)
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

;; Org Babel


(use-package ob-core
  :defer t
  :straight org
  :custom
  (org-confirm-babel-evaluate nil))

;; Package Loading


(use-package ob-tangle
  :defer t

  :straight org

  :preface
  (defun medivhok/tangle-on-save ()
    "Disable confirmation before tangling."
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)))

;; Hooks


:hook
(org-mode . (lambda ()
  (add-hook 'after-save-hook
        	  #'medivhok/tangle-on-save
            'run-at-end
            'only-in-org-mode))))

;; Better Bullets

;; #+begin_quote
;; Make org-mode stars a little more super -- [[https://github.com/integral-dw/org-superstar-mode][org-superstar-mode]]
;; #+end_quote

;; Use bullet characters instead of asterisks, plus set the header font sizes to
;; something more palatable.


(use-package org-superstar
  :after org

  :hook
  (org-mode . org-superstar-mode)

  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

;; TOC Auto Generation

;; #+begin_quote
;; Automatic tables of contents for Org files. -- [[https://github.com/alphapapa/org-make-toc/][org-make-toc]]
;; #+end_quote

;; It’s nice to have a table of contents section for long literate configuration
;; files (like this one!) so I use [[https://github.com/alphapapa/org-make-toc][org-make-toc]] to automatically update the ToC in
;; any header with a property named TOC.


(use-package org-make-toc
  :hook
  (org-mode . org-make-toc-mode))

;; ODT Exporter


(use-package ox-odt
  :after org

  :straight org

  :custom
  (org-odt-convert-process "unoconv")
  (org-odt-convert-processes '(("unoconv"
                                "unoconv -f %f %i")))
  (org-odt-preferred-output-format "docx")
  (org-odt-prettify-xml t)
  (org-odt-content-template-file (expand-file-name
                                  "OrgOdtContentTemplate.xml"
                                  (concat straight-base-dir
                                          "straight/repos/org/etc/styles/")))
  (org-odt-styles-file (expand-file-name
                        "OrgOdtStyles.xml"
                        (concat straight-base-dir
                                "straight/repos/org/etc/styles/"))))

  ;; :config
  ;; (org-odt-add-automatic-style "TNOrgTitle"
  ;;                             '(("style:family" "paragraph")
  ;;                               ("style:parent-style-name" "OrgTitle")
  ;;                               ("style:master-page-name" "OrgTitlePage"))))

;; Evil Keybindings

;; #+begin_quote
;; Supplemental evil-mode keybindings to emacs org-mode. -- [[https://github.com/Somelauw/evil-org-mode/][evil-org]]
;; #+end_quote


(use-package evil-org
  :hook
  ((org-mode . evil-org-mode)
   (org-agenda-mode . evil-org-mode)
   (evil-org-mode . (lambda ()
	                    (evil-org-set-key-theme))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; ~helm-bibtex~

;; #+NAME: package:helm-bibtex

(use-package helm-bibtex
  :after (helm org)
  :init
  (setq bibtex-completion-bibliography medivhok:bibtex-file
        bibtex-completion-notes-path medivhok:roam-directory
        bibtex-completion-pdf-field "File"))

;; ~org-ref~

;; #+NAME: package:org-ref

(use-package org-ref
  :after
  (org helm-bibtex)

  :init
  (setq org-ref-completion-library 'org-ref-helm-cite
        org-ref-default-bibliography (list medivhok:bibtex-file)))

;; Getting Things Done

;; The environment of the /GTD/ workflow is done with ~org-agenda~, which is part of
;; the [[https://orgmode.org][Org Mode]] ecosystem.


(defun medivhok/open-agenda ()
  "Opens my GTD agenda."
  (interactive)
  (org-agenda nil " "))

;; The Agenda


(use-package org-agenda
  :defer t

  :after (org)

  :straight org

  :init
  (defconst medivhok:gtd-file
    (expand-file-name "gtd.org" medivhok:agenda-directory)
    "My 'getting things done' agenda file.")

  :custom
  (org-agenda-window-setup 'current-window)
  (org-agenda-block-separator nil)
  (org-agenda-dim-blocked-tasks 'invisible)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-start-with-log-mode t)
  (org-agenda-custom-commands
   `((" "
      "GTD Agenda"
      ((agenda ""
	             ((org-agenda-span 'week)
	              (org-deadline-warning-days 14)))
       (tags-todo "@inbox"
		              ((org-agenda-overriding-header "Inbox")))
       (tags-todo "@tâches"
		              ((org-agenda-overriding-header "Tâches")))
       (tags-todo "@teluq"
		              ((org-agenda-overriding-header "Teluq")))
       (tags-todo "@projets"
		              ((org-agenda-overriding-header "Projets"))))))))

;; Agenda Entries


(use-package org-capture
  :straight org

  :commands (org-capture)
  :bind
  (("<f4>" . (lambda () (interactive) (org-capture nil "i"))))

  :config
  (setq org-capture-templates
	`(("i" "inbox" entry
	   (file+headline ,medivhok:gtd-file "Inbox")
	   "* TODO [#C] %?\n:PROPERTIES:\n:Effort: 1\n:END:\n")

	  ("e" "email" entry
	   (file+headline ,medivhok:gtd-file "Emails")
	   "* TODO [#A] Reply: %a"
	   :immediate-finish t))))

;; Entries Refiling


(use-package org-refile
  :straight org

  :config
  (setq org-refile-allow-creating-parent-nodes 'confirm
	      org-refile-use-outline-path 'file
	      org-refile-targets '((nil :tag . "@tâches")
	                     	   (nil :tag . "@cours")
				   (nil :tag . "@projet")
				   (nil :tag . "@teluq"))))

;; The Slip Box
;; :PROPERTIES:
;; :package-name: org-roam
;; :package-url: https://www.orgroam.com
;; :END:


(use-package org-roam
  :after
  (org helm)

  :straight
  (org-roam :host github :repo "org-roam/org-roam")

  :hook
  (after-init . org-roam-mode)

  :commands
  (org-roam-db-query)

  :general
  (medivhok:notes-menu
    :states 'normal
    "b" '(:ignore t :which-key "annotated bibliography")
    "bf" 'medivhok/find-annotated-bibliography-card
    "f" 'medivhok/find-note-card
    "g" '(:ignore t :which-key "glossaries")
    "gf" 'medivhok/find-glossary-card
    "G" 'org-roam-graph
    "i" 'org-roam-insert
    "r" 'org-roam-buffer-toggle-display)

  :init
  (defconst medivhok:roam-templates-directory
    (file-name-as-directory
     (expand-file-name "templates" medivhok:roam-directory))
    "The slip box cards templates directory.")

  (setq org-roam-completion-system 'helm
        org-roam-directory medivhok:roam-directory
        org-roam-file-exclude-regexp "setupfiles\\|templates"
        org-roam-index-file "index_file.org"
        org-roam-tag-sources '(prop)
        org-roam-title-sources '(title alias)
        org-roam-capture-templates
        `(("n" "note card" plain
           (function org-roam--capture-get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}
#+CREATED: %T
#+LAST_MODIFIED: %T

- tags ::"
           :unnarrowed t))

        org-roam-ref-capture-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n- source :: ${ref}"
           :unnarrowed t)))

  (setq time-stamp-active t
        time-stamp-pattern "-10/^#\\+LAST_MODIFIED: <%Y-%02m-%02d %a %02H:%02M>$"
        time-stamp-format "%Y-%02m-%02d %a %02H:%02M")

  (defun medivhok/sanitize-roam-tags (item &optional item-list)
    (let ((current-list (or item-list (list))))
      (if (stringp item)
          (unless (member item current-list)
            (push item current-list))
        (dolist (current-item item)
          (setq current-list (medivhok/sanitize-roam-tags current-item current-list))))
      current-list))

  (defun medivhok/get-roam-tags ()
    "Return a list of the tags in my roam files."
    (medivhok/sanitize-roam-tags (org-roam-db-query [:select tags :from tags])))

  (defun medivhok/get-roam-tag-alist ()
    (let ((tag-list (medivhok/get-roam-tags))
          (tag-alist))
      (dolist (tag tag-list tag-alist)
        (push (cons tag nil) tag-alist))))

  (defun medivhok/select-roam-tags ()
    "Return a list of selected tags."
    (let ((available-tag-list (medivhok/get-roam-tags))
          (selected-tag-list)
          (selected-tag)
          (done-selection "[done]")
          (finished nil))
      (add-to-list 'available-tag-list done-selection)
      (while (not finished)
        (let ((selected-tag
               (completing-read
                (format "Tags %s: " (or selected-tag-list ""))
                available-tag-list)))
          (cond ((or (string= "" selected-tag)
                     (string= done-selection selected-tag))
                 (setq finished t))
                ((not selected-tag)
                 (message "Return is nil"))
                (t
                 (delete selected-tag available-tag-list)
                 (add-to-list 'selected-tag-list selected-tag t)))))
      selected-tag-list)))

;; ~org-roam-bibtex~

;; #+begin_quote
;; Connector between Org-roam, BibTeX-completion, and Org-ref. -- [[https://github.com/org-roam/org-roam-bibtex][org-roam-bibtex]]
;; #+end_quote


(use-package org-roam-bibtex
  :straight
  (org-roam-bibtex :host github :repo "org-roam/org-roam-bibtex")

  :after
  (org-roam helm-bibtex org-ref)

  :hook
  (org-roam-mode . org-roam-bibtex-mode)

  :init
  (setq org-ref-notes-function 'orb-edit-notes
        orb-preformat-keywords '(("citekey" . "=key=")
                                 "title"
                                 "url"
                                 "file"
                                 "author-or-editor"
                                 "keywords")
        orb-templates `(("r" "ref" plain (function org-roam-capture--get-point)
                         ""
                         :file-name "%<%Y%m%d%H%M%S>-${citekey}"
                         :head
                         ,(concat "#+TITLE: ${title}\n"
                                  "#+ROAM_KEY: ${ref}\n"
                                  "* Notes\n"
                                  ":PROPERTIES:\n"
                                  ":Custom_ID: ${citekey}\n"
                                  ":URL: ${url}\n"
                                  ":AUTHOR: ${author-or-editor}\n"
                                  ":NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")\n"
                                  ":END:\n\n")
                         :unnarrowed t)

                        ("w" "webpage" plain (function org-roam-capture--get-point)
                         ""
                         :file-name "%<%Y%m%d%H%M%S>-${slug}"
                         :head
                         ,(concat "#+TITLE: ${title}\n"
                                  "#+ROAM_KEY: ${url}\n\n"
                                  "* Notes\n"
                                  ":PROPERTIES:\n"
                                  ":Custom_ID: ${citekey}\n"
                                  ":URL: ${url}\n"
                                  ":END:\n\n")
                         :unnarrowed t))))

;; ~ledger-mode~

;; #+begin_quote
;; Emacs Lisp files for interacting with the C++Ledger accounting system. -- [[https://github.com/ledger/ledger-mode][ledger-mode]]
;; #+end_quote


(use-package ledger-mode
  :defer t)

;; ~flycheck-ledger~

;; #+begin_quote
;; A flychecker for checking ledger files. -- [[https://github.com/purcell/flycheck-ledger][flycheck-ledger]]
;; #+end_quote


(use-package flycheck-ledger
  :after flycheck)

;; ~evil-ledger~

;; #+begin_quote
;; More Evil in ledger-mode. -- [[https://github.com/atheriel/evil-ledger][evil-ledger]]
;; #+end_quote


(use-package evil-ledger
  :hook
  (ledger-mode . evil-ledger-mode)

  :general
  (medivhok:local-mode-menu 'normal
    "s" 'evil-ledger-sort))

;; Statistics With R


(use-package ess
  :defer t)

(use-package ess-R-data-view
  :defer t)

(use-package polymode
  :defer t)

(use-package poly-R
  :defer t)

;; Company & Co

;; #+begin_quote
;; Modular in-buffer completion framework for Emacs. -- [[https://github.com/company-mode/company-mode][company-mode]]
;; #+end_quote


(use-package company
  :hook
  (after-init . global-company-mode)

  :config
  (setq company-box-icons-all-the-icons
        (let ((all-the-icons-scale-factor 0.8))
          `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
            (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
            (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
            (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
            (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
            (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
            (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
            (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
            (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
            (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
            (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
            (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
            (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
            (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
            (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
            (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
            (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
            (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
            (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
            (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
            (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
            (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
            (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))
            (ElispFunction . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (ElispVariable . ,(all-the-icons-material "check_circle"             :face 'all-the-icons-blue))
            (ElispFeature  . ,(all-the-icons-material "stars"                    :face 'all-the-icons-orange))
            (ElispFace     . ,(all-the-icons-material "format_paint"             :face 'all-the-icons-pink)))))

  (defun medivhok/company-backend-with-yas (backends)
    "Add :with company-yasnippet to company BACKENDS.
Taken from https://github.com/syl20bnr/spacemacs/pull/179."
    (if (and (listp backends) (memq 'company-yasnippet backends))
	      backends
	    (append (if (consp backends)
		              backends
		            (list backends))
		          '(:with company-yasnippet))))

  ;; add yasnippet to all backends
  (setq company-backends
        (mapcar #'medivhok/company-backend-with-yas company-backends)))

;; ~company-dict~

;; #+begin_quote
;; A port of ac-source-dictionary to company-mode, plus annotation and documentation support. -- [[https://github.com/hlissner/emacs-company-dict][company-dict]]
;; #+end_quote


(use-package company-dict
  :after company)

;; ~company-box~

;; #+begin_quote
;; A company front-end with icons. -- [[https://github.com/sebastiencs/company-box][company-box]]
;; #+end_quote


(use-package company-box
  :after company

  :hook
  (company-mode . company-box-mode))

;; Helm & Co
;; #+begin_quote
;; Emacs incremental completion and selection narrowing framework. -- [[https://github.com/emacs-helm/helm][helm]]
;; #+end_quote


(use-package helm
  :config
  (require 'helm-config))

;; helm-lsp


(use-package helm-lsp
  :after
  (helm lsp-mode))

;; LSP Mode

;; #+begin_quote
;; Emacs client/library for the Language Server Protocol. -- [[https://github.com/emacs-lsp/lsp-mode/][lsp-mode]]
;; #+end_quote


(use-package lsp-mode
  :defer t
  :after which-key
  :hook
  (lsp-mode . lsp-enable-which-key-integration))

;; PDF Reader
;; :PROPERTIES:
;; :package_name: pdf-tools
;; :package_url: https://github.com/politza/pdf-tools/
;; :END:

;; #+begin_quote
;; Emacs support library for PDF files. -- [[https://github.com/politza/pdf-tools/][pdf-tools]]
;; #+end_quote


(use-package pdf-tools
  :defer t
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-loader-install)
  (evil-collection-init 'pdf)
  (evil-collection-pdf-setup))

;; Generic Editing Configurations
;; Default to an indentation size of 2 spaces.

(setq-default tab-width 2)
(setq-default evil-shift-with tab-width)
(global-auto-revert-mode t)



;; Use spaces instead of tabs for indentation.

(setq-default indent-tabs-mode nil)

;; Folding (~hideshow~)

(use-package hideshow)

;; Parens & Co. (~smartparens~)

;; #+begin_quote
;; Minor mode for Emacs that deals with parens pairs and tries to be smart about it.
;; #+end_quote


(use-package smartparens
  :hook
  (prog-mode . smartparens-mode)
  (prog-mode . smartparens-strict-mode)

  :config
  (require 'smartparens-config))

;; evil-nerd-commenter

;; Commenting lines.

(use-package evil-nerd-commenter
  :bind
  ("M-/" . evilnc-comment-or-uncomment-lines))

;; ws-butler

;; Automatically clean whitespace.

(use-package ws-butler
  :hook
  ((text-mode . ws-butler-mode)
   (prog-mode . ws-butler-mode)))

;; parinfer

;; Use Parinfer for Lispy languages.

(use-package parinfer
  :hook ((clojure-mode . parinfer-mode)
         (emacs-lisp-mode . parinfer-mode)
         (common-lisp-mode . parinfer-mode)
         (scheme-mode . parinfer-mode)
         (lisp-mode . parinfer-mode))
  :config
  (setq parinfer-extensions
      '(defaults       ; should be included.
        pretty-parens  ; different paren styles for different modes.
        evil           ; If you use Evil.
        smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
        smart-yank)))  ; Yank behavior depend on mode.

;; rainbow-delimiters


(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; ~yasnippet~
;; :PROPERTIES:
;; :Custom_ID: use-package--yasnippet
;; :END:

;; #+begin_quote
;; A template system for Emacs. -- [[https://github.com/joaotavora/yasnippet][yasnippet]]
;; #+end_quote


(use-package yasnippet
  :custom
  (yas-snippet-dirs
     (list
      (file-name-as-directory
       (expand-file-name "snippets"
                         (file-name-directory user-init-file)))))
  :general
  :config
  (yas-global-mode 1)
  )

;; magit


(use-package magit
  :commands
  (magit-status magit-get-current-branch)

  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-execpt-diff-v1))

;; evil-magit


(use-package evil-magit
  :after magit)

;; git-gutter


(use-package git-gutter
  :hook
  ((text-mode . git-gutter-mode)
   (prog-mode . git-gutter-mode))

  :config
  (setq git-gutter:update-interval 2))

;; git-link


(use-package git-link
  :commands git-link

  :config
  (setq git-link-open-in-browser t))

;; magit-todos


(use-package magit-todos
  :after magit)

;; projectile


(use-package projectile
  :config
  (projectile-mode))

;; counsel-projectile


(use-package counsel-projectile
  :after projectile)

;; Syntax Checking (~flycheck~)


(use-package flycheck
  :config (global-flycheck-mode))

;; CSS


(use-package css-mode
  :defer t

  :after
  (flycheck)

  :init
  (setq flycheck-css-csslint-executable "~/.yarn/bin/csslint"))

;; ~cask~

;; #+begin_quote
;; Project management tool for Emacs. -- [[https://github.com/cask/cask][cask]]
;; #+end_quote


(use-package cask)

;; ~cask-mode~


(use-package cask-mode
  :defer t)

;; ~buttercup~


(use-package buttercup
  :defer t)

;; ~haskell-mode~
;; :PROPERTIES:
;; :Custom_ID: use-package--haskell-mode
;; :END:

;; #+begin_quote
;; #+end_quote


(use-package haskell-mode

;; Keybindings (~:general~)
;; :PROPERTIES:
;; :Custom_ID: use-package--haskell-mode--general
;; :END:

;; We start the [[https://github.com/noctuid/general.el#general-keyword][:general]] section of our [[#use-package--haskell-mode][use-package]] definition.


:general

;; Initializations (~:init~)
;; :PROPERTIES:
;; :Custom_ID: use-package--haskell-mode--init
;; :END:

;; #+begin_quote
;; NOTE: This code is executed BEFORE the package is loaded.
;; #+end_quote

;; We start the [[https://github.com/jwiegley/use-package#getting-started][:init]] section of our [[#use-package--haskell-mode][use-package]] definition.


:init

;; Configurations (~:config~)
;; :PROPERTIES:
;; :Custom_ID: use-package--haskell-mode--config
;; :END:

;; #+begin_quote
;; NOTE: This code is executed AFTER the package is loaded.
;; #+end_quote

;; We start the [[https://github.com/jwiegley/use-package#getting-started][:config]] section of the [[#use-package--haskell-mode][use-package]].


:config

;; Closing Paren


)

;; ~dante~
;; :PROPERTIES:
;; :Custom_ID: use-package--dante
;; :END:

;; #+begin_quote
;; #+end_quote


(use-package dante

;; Keybindings (~:general~)
;; :PROPERTIES:
;; :Custom_ID: use-package--dante--general
;; :END:

;; We start the [[https://github.com/noctuid/general.el#general-keyword][:general]] section of our [[#use-package--dante][use-package]] definition.


:general

;; Initializations (~:init~)
;; :PROPERTIES:
;; :Custom_ID: use-package--dante--init
;; :END:

;; #+begin_quote
;; NOTE: This code is executed BEFORE the package is loaded.
;; #+end_quote

;; We start the [[https://github.com/jwiegley/use-package#getting-started][:init]] section of our [[#use-package--dante][use-package]] definition.


:init

;; Configurations (~:config~)
;; :PROPERTIES:
;; :Custom_ID: use-package--dante--config
;; :END:

;; #+begin_quote
;; NOTE: This code is executed AFTER the package is loaded.
;; #+end_quote

;; We start the [[https://github.com/jwiegley/use-package#getting-started][:config]] section of the [[#use-package--dante][use-package]].


:config

;; Closing Paren


)

;; ~attrap~
;; :PROPERTIES:
;; :Custom_ID: use-package--attrap
;; :END:

;; #+begin_quote
;; #+end_quote


(use-package attrap

;; Keybindings (~:general~)
;; :PROPERTIES:
;; :Custom_ID: use-package--attrap--general
;; :END:

;; We start the [[https://github.com/noctuid/general.el#general-keyword][:general]] section of our [[#use-package--attrap][use-package]] definition.


:general

;; Initializations (~:init~)
;; :PROPERTIES:
;; :Custom_ID: use-package--attrap--init
;; :END:

;; #+begin_quote
;; NOTE: This code is executed BEFORE the package is loaded.
;; #+end_quote

;; We start the [[https://github.com/jwiegley/use-package#getting-started][:init]] section of our [[#use-package--attrap][use-package]] definition.


:init

;; Configurations (~:config~)
;; :PROPERTIES:
;; :Custom_ID: use-package--attrap--config
;; :END:

;; #+begin_quote
;; NOTE: This code is executed AFTER the package is loaded.
;; #+end_quote

;; We start the [[https://github.com/jwiegley/use-package#getting-started][:config]] section of the [[#use-package--attrap][use-package]].


:config

;; Closing Paren


)

;; Json (~json-mode~)

;; #+begin_quote
;; Major mode for editing JSON files with emacs.
;; #+end_quote

;; [[https://github.com/joshwnj/json-mode][json-mode]]


(use-package json-mode
  :custom
  (json-reformat:indent-width 2)

  :general
  (medivhok/local-leader-def 'normal
    json-mode-map
    nil '(:ignore t :which-key "json")
    "d" 'json-decrement-number-at-point
    "f" 'json-mode-beautify
    "i" 'json-increment-number-at-point
    "k" 'json-nullify-sexp
    "p" 'json-mode-show-path
    "P" 'json-mode-kill-path
    "t" 'json-toggle-boolean))

;; LaTeX


(use-package auctex
  :defer t
  :custom
  (TeX-engine 'luatex))

(use-package company-auctex
  :defer t
  :after company)

(use-package company-reftex
  :defer t
  :after company)

(use-package company-math
  :defer t
  :after company)

(use-package ox-latex
  :defer t
  :after org
  :straight org
  :config
  (setq org-latex-caption-above nil
        org-latex-compiler "lualatex"
        org-latex-listings 'minted
        org-latex-minted-options
        '(("frame" "lines")
          ("framesep" "2mm")
          ("baselinestretch" "1.2")
          ("style" "pastie"))
        org-latex-packages-alist
        '(("" "fontspec" t ("lualatex"))
          ("AUTO" "babel" t ("pdflatex" "lualatex"))
          ("" "booktabs" t)
          ("" "fancyhdr" t)
          ("" "minted")
          ("" "xcolor"))
        org-latex-pdf-process
        '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (add-to-list 'org-latex-minted-langs '(R "r")))

;; YAML


(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; XML (~nxml~)

(use-package nxml-mode
  :straight nxml

  :hook
  (nxml-mode . hs-minor-mode)

  :general
  (normal nxml-mode-map "TAB" 'hs-toggle-hiding)
  (medivhok/local-leader-def 'normal
    nxml-mode-map
    nil '(:ignore t :which-key "xml")
    "t" 'hs-toggle-hiding)

  :config
  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                "<!--\\|<[^/>]*[^/]>"
                "-->\\|</[^/>]*[^/]>"
                "<!--"
                nxml-forward-element
                nil)))
