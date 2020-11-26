(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	      doom-themes-enable-italic t)
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(custom-theme-set-faces
 'user
 `(default ((t (:family "Roboto Mono" :height 140 :weight light))))
 `(fixed-pitch ((t (:family "FiraCode" :height 130 :weight light)))))

(use-package all-the-icons

:preface
(global-prettify-symbols-mode 1)

)

(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;; One line at a time.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Don't accelerate scrolling.
(setq mouse-wheel-progressive-speed nil)

;; Scroll window under mouse.
(setq mouse-wheel-follow-mouse 't)

;; Keyboard scroll one line at a time.
(setq scroll-step 1)

(setq display-line-numbers-type 'relative
      display-line-numbers-width-start t)
;; (global-display-line-numbers-mode t)
(column-number-mode)

(setq visible-bell t)

(global-hl-line-mode t)

(setq display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)

(setq-default fill-column 80)

(scroll-bar-mode -1)
(tooltip-mode -1)

;; We disable the tool and menu bar.
(tool-bar-mode -1)
(menu-bar-mode -1)

(set-fringe-mode 10)

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package evil
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump nil)
  (evil-respect-visual-line-mode t)

  :config
  (evil-mode 1))

(use-package evil-collection
  :defer t
  :after evil

  :custom
  (evil-collection-outline-bind-tab-p nil))

(use-package hydra
  :defer t)

(use-package which-key
  :custom
  (which-key-idle-delay 0.3)

  :config
  (which-key-mode))

(use-package general
  :config
  (general-evil-setup t))

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

(general-create-definer medivhok:local-mode-menu
  :prefix (medivhok/expand-menu-key "m")
  :non-normal-prefix (medivhok/expand-menu-key "m" t)
  :keymaps 'override)

(general-create-definer medivhok:applications-menu
  :prefix (medivhok/expand-menu-key "a")
  :non-normal-prefix (medivhok/expand-menu-key "a" t)
  :keymaps 'override
  nil '(:ignore t :which-key "applications"))

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

(general-create-definer medivhok:file-menu
  :prefix (medivhok/expand-menu-key "f")
  :non-normal-prefix (medivhok/expand-menu-key "f" t)
  :keymaps 'override
  nil '(:ignore t :which-key "file"))

(medivhok:file-menu
  :states 'normal
  "f" 'find-file
  "r" 'counsel-recentf)

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

(general-create-definer medivhok:notes-menu
  :prefix (medivhok/expand-menu-key "n")
  :non-normal-prefix (medivhok/expand-menu-key "n" t)
  :keymaps 'override
  nil '(:ignore t :which-key "notes"))

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

(general-create-definer medivhok:quit-menu
  :prefix (medivhok/expand-menu-key "q")
  :non-normal-prefix (medivhok/expand-menu-key "q" t)
  :keymaps 'override
  nil '(:ignore t :which-key "quit"))

(medivhok:quit-menu
  :states 'normal
  "q" 'save-buffers-kill-terminal)

(use-package dashboard
  :preface
  (setq inhibit-startup-message t)

  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
        dashboard-items '((recents . 5)
                          (agenda . 5)))
  (evil-collection-init 'dashboard))

(use-package amx
  :after ivy

  :custom
  (amx-backend 'ivy)

  :config
  (amx-mode))

(use-package counsel

:after evil-collection

:config
(evil-collection-init 'ivy)
(setq ivy-use-virtual-buffers t
      ivy-count-format "(%d/%d) ")
(ivy-mode 1)
(counsel-mode 1)

)

(use-package ivy-rich

:after ivy

:config
(ivy-rich-mode 1)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

)

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

(use-package minions
  :init
  (setq minions-mode-line-lighter " ")

  :config
  (minions-mode 1))

(setq large-file-warning-threshold nil)

(setq vc-follow-symlinks t)

(setq ad-redefinition-action 'accept)

(use-package helpful
  :after
  (counsel evil-collection)

  :config
  (evil-collection-init 'helpful)
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable))
