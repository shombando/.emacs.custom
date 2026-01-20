;;directory_begin
(setq user-emacs-directory "~/.emacs/.custom/")
;;directory_end

;;setup_begin
(setq byte-compile-warnings nil)

(defvar bootstrap-version)
(let ((bootstrap-file
	   (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	  (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
	(with-current-buffer
		(url-retrieve-synchronously
		 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
		 'silent 'inhibit-cookies)
	  (goto-char (point-max))
	  (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)

(straight-use-package 'use-package)
(straight-use-package 'org)
;;setup_end

;;visual_begin
(tool-bar-mode -1)
(menu-bar-mode 0)
(setq visible-bell 1)
(global-visual-line-mode 1)
(global-display-line-numbers-mode 1)
(column-number-mode t)
(setq org-hide-emphasis-markers t)
(setq org-image-actual-width nil)

(defun sb/load-light-theme ()
  (interactive)
  (load-theme 'doom-nord-light t))

(defun sb/load-dark-theme ()
  (interactive)
  (load-theme 'doom-nord t))
;;visual_end

;;built-in_begin
(defalias 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)

(setq-default indent-tabs-mode t)
(setq-default tab-width 2)
;;built-in_end

;;spellcheck_begin
(setq-default ispell-program-name "aspell")

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
;;spellcheck_end

;;dired_begin
(use-package dired
  :straight nil
  :after evil-collection
  :commands (dired dired-jump)
  :custom (dired-listing-switches "-agho --group-directories-first")
  :config
  (setq dired-dwim-target t)
  (evil-collection-define-key 'normal 'dired-mode-map
	"o" '(lambda () (interactive) (dired-find-file-other-window))
	"h" '(lambda () (interactive) (find-alternate-file ".."))
	"l" 'dired-find-alternate-file))
;;dired_end

;;no-littering_begin
(use-package no-littering
  :straight t
  :init
  (setq auto-save-file-name-transforms
		`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  :config
  (no-littering-theme-backups))
;;no-littering_end

;;modeline_begin
(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))
;; run (all-the-icons-install-fonts)) if icons are missing
;; install NerdFontsSymbolsOnly from https://github.com/ryanoasis/nerd-fonts/releases

(with-eval-after-load 'doom-modeline
  ;; Mostly using @daviwil's config
  (defun dw/set-tab-bar-faces ()
	(let ((color (face-attribute 'doom-modeline-bar :background nil t)))
	  (set-face-attribute 'tab-bar-tab t :foreground unspecified :background unspecified :weight 'semi-bold :underline `(:color ,color) :inherit nil)
	  (set-face-attribute 'tab-bar nil :font "JetBrains Mono Bold" :height 0.95 :underline `(:color ,color) :foreground nil :inherit 'mode-line)))

  (setq tab-bar-close-button-show nil
		tab-bar-format '(dw/set-tab-bar-faces
						 tab-bar-format-menu-bar
						 tab-bar-format-history
						 tab-bar-format-tabs
						 tab-bar-separator
						 tab-bar-format-add-tab
						 tab-bar-format-align-right
						 tab-bar-format-global
						 tab-bar-separator))

  ;; remove battery from doom-modeline
  (doom-modeline-def-modeline 'default
	'(bar window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
	'(vcs major-mode process objed-state grip debug repl lsp minor-modes input-method indent-info buffer-encoding))
  (doom-modeline-set-modeline 'default t)
  (add-to-list 'global-mode-string '("" doom-modeline--battery-status))
  (add-to-list 'global-mode-string '("" tracking-mode-line-buffers))

  (display-time-mode 1)
  ;; (display-battery-mode 1)

  (setq tab-bar-separator " | ")

  ;; Redefine tab-bar-format-menu-bar since there's no option for changing the menu text, taken from karthinks.com
  (defun tab-bar-frmat-menu-bar ()
	"Produce the Menu button for the tab bar that shows the menu bar."
	`((menu-bar menu-item (propertize " ξ " 'face 'tab-bar-tab-inactive)
				tab-bar-menu-bar :help "Menu Bar")))

  (tab-bar-mode t))
;;modeline_end

;;cua_begin
(use-package simpleclip
  :straight t
  :after evil-collection
  :bind (("C-S-x" . 'simpleclip-cut)
		 ("C-S-c" . 'simpleclip-copy)
		 ("C-S-v" . 'simpleclip-paste)))

(use-package vundo
	:straight t
  :after evil-collection
  :defer t
	:config
	(setq vundo-popup-mode 1
				vundo-compact-display t)
  :bind (:map global-map
				("C-M-z" . vundo)
				:map evil-insert-state-map
			  ("C-z" . undo)
			  :map evil-normal-state-map
			  ("C-z" . undo)))
;;cua_end

;;which-key_begin
(use-package which-key
  :straight t
  :config
  (which-key-mode)
  :init
  (setq which-key-idle-delay 0.1))
;;which-key_end

;;magit_begin
(use-package magit
  :straight t
	:after evil-leader
  :config
	(evil-leader/set-key
	"g" '("magit"          . (keymap))
	"ga" '("add"           . magit-stage-buffer-file)
	"gc" '("commit"        . magit-commit)
	"gf" '("fetch"         . magit-fetch)
	"gg" '("status"        . magit-status)
	"gr" '("status"        . magit-refresh))

	(setq magit-diff-refine-hunk t
				magit-diff-paint-whitespace-lines t
				magit-diff-highlight-indentation '(("*" . tabs))))
;;magit_end

;;evil_begin
(use-package evil
  :straight t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-integration t
		evil-want-keybinding nil
		evil-disable-insert-state-bindings t
		evil-undo-system 'undo-redo)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :straight t
  :config
  (evil-collection-init))

(use-package evil-leader
  :after evil-collection
  :straight t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader-mode))
;;evil_end

;;theme_begin
(load custom-file)
(use-package doom-themes
  :straight t
  :init (load-theme 'doom-nord t)
	:config
	(evil-leader/set-key
		"t" '("theme"          . (keymap))
		"td" '("dark"          . sb/load-dark-theme)
		"tl" '("light"         . sb/load-light-theme)))

(use-package auto-dark
  :straight t
	:custom
	(auto-dark-themes '((doom-nord) (doom-nord-light)))
	:init
	(auto-dark-mode))

(use-package all-the-icons
  :straight t
  :config
  (unless (require 'all-the-icons nil 'noerror)
	(all-the-icons-install-fonts)))

(use-package rainbow-delimiters
  :straight t
  :config (electric-pair-mode)
  :hook (prog-mode . rainbow-delimiters-mode))

(set-frame-font "JetBrainsMono Nerd Font" 16 nil t)
(set-frame-parameter (selected-frame) 'alpha-background 90)
(set-frame-parameter (selected-frame) 'alpha 100)
(setq default-frame-alist '((undecorated . t)))
;;theme_end

;;history_begin
(use-package savehist
  :straight t
  :init
  (savehist-mode))

(use-package recentf
  :straight t
  :init
  (recentf-mode t)
  (run-at-time nil 600 'recentf-save-list))
;;history_end

;;completions_begin
(use-package vertico
  :straight t
  :init
  (vertico-mode)
  (setq vertico-cycle t)
  :bind (:map vertico-map
			  ("C-j" . vertico-next)
			  ("C-k" . vertico-previous)))

(use-package orderless
  :straight t
  :custom (completion-styles '(orderless)))
(orderless-define-completion-style orderless+initialism
  (orderless-matching-styles '(orderless-initialism
							   orderless-literal
							   orderless-regexp)))
(setq completion-category-overrides
	  '((command (styles orderless+initialism))
		(symbol (styles orderless+initialism))
		(variable (styles orderless+initialism))
		(file (styles . (partial-completion
						 orderless+initialism)))))

(use-package marginalia
  :after vertico
  :straight t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package consult
  :straight t
  :after vertico)

(use-package consult-proj
  :straight (consult-proj :type git :host github :repo "Qkessler/consult-proj")
  :bind
  (("C-c p f" . consult-proj)
   ("C-c p o" . consult-proj-other-window)))

(use-package corfu
  :straight t
  :after vertico
  :after evil
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-scroll-margin 5)        ;; Use scroll margin
  (corfu-auto-delay 0.3)
  :bind (:map evil-insert-state-map
			  ("C-j" . corfu-next)
			  ("C-k" . corfu-previous)
			  :map corfu-map
			  ("<tab>" . corfu-next)
			  ("<backtab>" . corfu-previous))
  :init
  (global-corfu-mode))

(use-package cape
  :straight t
  :after corfu
  :bind (:map evil-insert-state-map
			  ("M-'" . completion-at-point))
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (setq-local completion-at-point-functions
			  (list (cape-capf-super
					 #'cape-dabbrev
					 #'cape-keyword))))

(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(setq tab-always-indent 'complete
	  tab-first-completion 'word-or-paren-or-punct)
;;completions_end

;;embark_begin
(use-package embark
  :straight t
  :bind
  (("C-'" . embark-act))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt
  (defun embark-which-key-indicator ()
	"An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
	(lambda (&optional keymap targets prefix)
	  (if (null keymap)
		  (which-key--hide-popup-ignore-command)
		(which-key--show-keymap
		 (if (eq (plist-get (car targets) :type) 'embark-become)
			 "Become"
		   (format "Act on %s '%s'%s"
				   (plist-get (car targets) :type)
				   (embark--truncate-target (plist-get (car targets) :target))
				   (if (cdr targets) "…" "")))
		 (if prefix
			 (pcase (lookup-key keymap prefix 'accept-default)
			   ((and (pred keymapp) km) km)
			   (_ (key-binding prefix 'accept-default)))
		   keymap)
		 nil nil t (lambda (binding)
					 (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
		'(embark-which-key-indicator
		  embark-highlight-indicator
		  embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
	"Hide the which-key indicator immediately when using the completing-read prompter."
	(which-key--hide-popup-ignore-command)
	(let ((embark-indicators
		   (remq #'embark-which-key-indicator embark-indicators)))
	  (apply fn args)))

  (advice-add #'embark-completing-read-prompter
			  :around #'embark-hide-which-key-indicator))

(use-package embark-consult
  :straight t)
;;embark_end

;;window_begin
(use-package ace-window
  :straight t
  :init
  (global-set-key (kbd "M-q") 'aw-show-dispatch-help)
  (setq aw-dispatch-always t)
	:config
	(evil-leader/set-key
	  "w" '("window"         . (keymap))
	  "wd" '("delete"        . delete-window)
	  "wb" '("split-below"   . split-window-below)
	  "wr" '("split-right"   . split-window-right)
	  "wo" '("delete other"  . delete-other-windows)
	  "ww" '("ace-window"    . aw-show-dispatch-help)))
;;window_end

;;org-transclusion_begin
(use-package org
  :straight t
  :config (setq org-directory "~/org"
				org-support-shift-select t
				org-log-done 'time
				org-pretty-entities-include-sub-superscripts t))

(use-package org-transclusion
  :after org
  :straight( org-transclusion
  :type git
  :host github
  :repo "nobiot/org-transclusion")
  :init
  (setq org-transclusion-remember-transclusions t))
;;org-transclusion_end

;;visualNonPhone_begin
;;;https://config.daviwil.com/emacs#system-settings
(setq sb/is-termux
	  (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))

(unless sb/is-termux
  (scroll-bar-mode -1)
  (set-fringe-mode '(20 . 10))

  (use-package git-gutter-fringe
	:straight t
	:init
	(require 'git-gutter-fringe)
	(global-git-gutter-mode t)
	:config
	(setq git-gutter:update-interval 2
		  git-gutter:modified-sign "&"
		  git-gutter:added-sign "+"
		  git-gutter:deleted-sign "-")
	(set-face-foreground 'git-gutter-fr:modified "LightGoldenrod")
	(set-face-foreground 'git-gutter-fr:added    "LightGreen")
	(set-face-foreground 'git-gutter-fr:deleted  "LightCoral"))

  (use-package org-bars
	:after org
	:straight (org-bars :type git :host github :repo "tonyaldon/org-bars")
	:init
	(defun org-no-ellipsis-in-headlines ()
	  "Remove use of ellipsis in headlines."
	  (remove-from-invisibility-spec '(outline . t))
	  (add-to-invisibility-spec 'outline))
	(add-hook 'org-mode-hook #'org-bars-mode)
	(add-hook 'org-mode-hook 'org-no-ellipsis-in-headlines))

  (use-package all-the-icons-dired
	:straight t
	:hook (dired-mode . all-the-icons-dired-mode))

  (use-package vertico-posframe
	:after vertico
	:straight t
	:init
	(vertico-posframe-mode 1)))
;;visualNonPhone_end

;;evil-leader_begin
(defun sb/set-global-key-bindings ()
  (evil-leader/set-key
	"." '("file"           . find-file)
	"," '("buffers"        . consult-buffer)
	";" '("project"        . consult-project-buffer)
	"c" '("capture"        . org-capture)
	"/" '("find"           . consult-ripgrep)
	"=" '("indent"         . org-indent-region)
	"SPC" '("M-x"          . execute-extended-command)

	"e" '("eval"           . (keymap))
	"eb" '("buffer"        . eval-buffer)
	"er" '("region"        . eval-region)

	"i" '("insert"         . (keymap))
	"iu" '("uuid"          . sb/generate-uuid)

	"q" '("quit"           . (keymap))
	"qb" '("buffer"        . kill-current-buffer)
	"qq" '("save & quit"   . save-buffers-kill-terminal)

	"h" '("help"           . (keymap))
	"hf" '("function"      . describe-function)
	"hk" '("key"           . describe-key)
	"hv" '("variable"      . describe-variable))

  (global-set-key (kbd "C-s") 'save-buffer)
  (global-set-key (kbd "C-S-s") 'write-file)
  (global-set-key (kbd "C-a") 'mark-whole-buffer)
  (global-set-key (kbd "M-a") 'other-window)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C-S-p") 'execute-extended-command)
  (global-set-key (kbd "C-<tab>") #'consult-buffer)
  (global-set-key (kbd "C-S-M-v") #'yank-from-kill-ring)
  (define-key evil-normal-state-map "gb" 'revert-buffer-quick))

(with-eval-after-load 'evil-collection
  (sb/set-global-key-bindings))
;;evil-leader_end

;;user-config_begin 
(load (concat user-emacs-directory "userConfig.el"))
;;user-config_end 
