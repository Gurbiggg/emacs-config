(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
	(elpaca-use-package-mode))

;; necessary Emacs tweaks
(setq user-emacs-directory "~/.config/emacs/")

;;(setq backup-directory-alist '(("." . "~/.config/emacs/backup/")))
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
;;(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
;;(dolist (mode '(org-mode-hook
;;		term-mode-hook
;;		shell-mode-hook
;;		treemacs-mode-hook
;;		eshell-mode-hook
;;		vterm-mode-hook))
;;  (add-hook mode (lambda () (display-line-numbers-mode 0))))
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; Font
(set-face-attribute 'default nil :font "Fira Code" :height 100)

;; Set ESC as a quit bind
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode/org/elpa")
			 ("elpa" . "https://elpa.gn.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;;; Themes
;; Usable ones:
;; ef-dream
;; doom-ayu-mirage
;; doom-dark+
;; doom-challenger-deep
;; doom-material-dark
;; doom-one
;; doom-tomorrow-night
;; modus-vivendi
(use-package ef-themes
  :init (load-theme 'ef-dream t))
  
(use-package doom-themes)
  ;;:config (load-theme 'doom-tomorrow-night t))
;;(use-package color-theme-sanityinc-tomorrow)
;;(load-theme 'doom-tomorrow-night t)


;;defun efs/run-in-background (command)
;; (let ((command-parts (split-string command "[ ]+")))
;;   (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))
;;
;;defun dw/exwm-init-hook ()
;; (exwm-workspace-switch-create 1)
;; (efs/run-in-background "nm-applet"))
;;
;;defun efs/set-wallpaper ()
;; (interactive)
;; (start-prcoess-shell-command
;;  "feh" nil "feh --bg-scale ~/Pictures/Wallpapers/ScarletTreeDark.png"))
;;
;;use-package exwm
;; :init
;; (setq mouse-autoselect-window t
;;	focus-follows-mouse t)
;; :config
;; (setq exwm-workspace-show-all-buffers t)
;; (setq exwm-layout-show-all-buffers t)
;; (setq exwm-workspace-number 5)
;; (setq exwm-input-prefix-keys
;;	'(?\C-x
;;	  ?\C-u
;;	  ?\C-h
;;	  ?\M-x
;;	  ?\M-`
;;	  ?\M-&
;;	  ?\M-:
;;	  ?\C-\M-j
;;	  ?\C-\ ))
;;
;; (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
;;
;; (setq exwm-input-global-keys
;;	`(
;;	  ([?\s-r] . exwm-reset)
;;	  ([s-h] . windmove-left)
;;	  ([s-j] . windmove-down)
;;	  ([s-k] . windmove-up)
;;	  ([s-l] . windmove-right)
;;
;;	  ([?\s-&] . (lambda (command)
;;		       (interactive (list (read-shell-command "$ ")))
;;		       (start-process-shell-command command nil command)))
;;	  
;;	  ([?\s-w] . exwm-workspace-switch)
;;
;;	  ,@(mapcar (lambda (i)
;;		      `(,(kbd (format "s-%d" i)) .
;;			(lambda ()
;;			  (interactive)
;;			  (exwm-workspace-switch-create ,i))))
;;		    (number-sequence 0 9))))
;; 
;; (add-hook 'exwm-update-class-hook
;;	    (lambda ()
;;	      (exwm-workspace-rename-buffer exwm-class-name)))
;; (setq exwm-input-prefix-keys '())
;;
;; (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; 
;; (require 'exwm-randr)
;; (exwm-randr-mode)
;; 
;; (start-process-shell-command "xrandr" nil "")
;; (efs/set-wallpaper)
;; 
;; (require 'exwm-systemtray)
;; (exwm-systemtray-mode)
;; (add-hook 'exwm-init-hook #'efs/after-exwm-init)
;; 
;; (exwm-enable))
;;
;;
;;use-package desktop-environment
;; :after exwm
;; :config (desktop-environment-mode)
;; :custom
;; (desktop-environment-brightness-small-increment "2%+")
;; (desktop-environment-brightness-small-decrement "2%+")
;; (desktop-environment-brightness-normal-increment "5%+")
;; (desktop-environment-brightness-normal-decrement "5%+"))
    
(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))
(defun efs/org-font-setup ()
  (font-lock-add-keywords 'org-mode
			  '(("^ *\\([-])\\) "
			     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (dolist (face '((org-level-1 . 1.2)
		  (org-level-2 . 1.1)
		  (org-level-3 . 1.05)
		  (org-level-4 . 1.0)
		  (org-level-5 . 1.1)
		  (org-level-6 . 1.1)
		  (org-level-7 . 1.1)
		  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil	:inherit 'fixed-pitch)
  (set-face-attribute 'org-line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-line-number-current-line nil :inherit 'fixed-pitch))
		  
(use-package org
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-elipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files
	'("~/org/agendas/school.org")))  

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;;(use-package no-littering)
;;(setq auto-save-file-name-transforms
;;      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; completion engine/better minibuffer
(use-package vertico
    :init (vertico-mode))

; orderless completion style, it might be better than the generic emacs one
(use-package orderless  
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :init (global-corfu-mode))

;; Treesitter config
(use-package tree-sitter
  :hook
  (tree-sitter-after-on-hook . tree-sitter-hl-mode)
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode))

(use-package treesit-auto
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (require 'tree-sitter-langs)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map
	("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 40))

(use-package nerd-icons)

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-shell "zsh")
  (setq vterm-max-scrollback 10000))

;; eldoc
(use-package eldoc
  :ensure (:wait t)
  :init
  (global-eldoc-mode))

;; jsonrpc dep
(use-package jsonrpc)

;; lsp mode
(use-package eglot
  :hook
  (rust-mode . eglot-ensure))

;; to make it faster...
;;use-package eglot-booster
;; :after eglot
;; :config (eglot-booster-mode))

;; rust mode
(use-package rust-mode
  :init 
  (setq rust-mode-treesitter-derive t))

;; nix mode
(use-package nix-ts-mode
  :mode "\\.nix\\'")

;; clojure mode
(use-package clojure-ts-mode)

;;(use-package magit)

(use-package dashboard
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook)

  (setq dashboard-banner-logo-title "Welcome to Emacs!")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-cener-content t)
  (setq dashboard-show-shortcuts t)
  (setq dashboard-items '((recents   . 5)
			  (bookmarks . 5)
			  (projects  . 5)
			  (agenda    . 5)
			  (registers . 5)))
  (setq dashboard-startupify-list '(dashboard-insert-banner
				    dashboard-insert-newline
				    dashboard-insert-banner-title
				    dashboard-insert-navigator
				    dashboard-insert-init-info
				    dashboard-insert-items
				    dashboard-insert-newline
				    dashboard-insert-footer))
  
  (setq dashboard-item-names '(("Recent Files:" . "Recently opened files:")
			       ("Agenda for today:" . "Today's agenda:")
			       ("Agenda for the coming week:" . "Agenda:")))

  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t))
 
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   ;; Use SPC . to open files
   '("." . find-file)
   '("s" . save-buffer))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(use-package meow
  :init
  (require 'meow)
  (meow-setup)
  (meow-global-mode 1))
