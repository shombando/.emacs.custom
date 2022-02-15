;directory_begin
(setq user-emacs-directory "~/.emacs/.custom/")
;directory_end

;setup_begin
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
;setup_end

;visual_begin
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq visible-bell 1)
(global-visual-line-mode 1)
(global-linum-mode 1)
(column-number-mode t)
(setq org-hide-emphasis-markers t)
(setq org-image-actual-width nil)
;visual_end

;built-in_begin
(defalias 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
;built-in_end

;spellcheck_begin
(setq-default ispell-program-name "aspell")

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
;spellcheck_end

;dired_begin
(use-package dired
  :straight nil
  :after evil-collection
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
    (evil-collection-define-key 'normal 'dired-mode-map
    "h" '(lambda () (interactive) (find-alternate-file ".."))
    "l" 'dired-find-alternate-file))
;dired_end

;no-littering_begin
(use-package no-littering
  :straight t
  :init
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))
;no-littering_end

;modeline_begin
(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))
;modeline_end

;cua_begin
(use-package simpleclip
  :straight t
  :after evil-collection
  :bind (("C-S-x" . 'simpleclip-cut)
	 ("C-S-c" . 'simpleclip-copy)
	 ("C-S-v" . 'simpleclip-paste))) 

(use-package undo-fu
  :straight t
  :after evil-collection
  :defer t
  :bind (:map evil-insert-state-map
	      ("C-z" . undo-fu-only-undo)
	      ("C-S-z" . undo-fu-only-redo)
	 :map evil-normal-state-map
	      ("C-z" . undo-fu-only-undo)
	      ("C-S-z" . undo-fu-only-redo)))
;cua_end

;which-key_begin
(use-package which-key
  :straight t
  :config
  (which-key-mode)
  :init
  (setq which-key-idle-delay 0.1))
;which-key_end

(use-package magit
  :straight t
  :defer t)

;evil_begin
(use-package evil
  :straight t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :straight t
  :config
  (evil-collection-init))

(use-package evil-leader
  :straight t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader-mode))
;evil_end

;theme_begin
(use-package doom-themes
  :straight t
  :init (load-theme 'doom-one t))
;theme_end

;history_begin
(use-package savehist
  :straight t
  :init
  (savehist-mode))

(use-package recentf
  :straight t
  :init
  (recentf-mode t)
  (run-at-time nil 600 'recentf-save-list))
;history_end

;completions_begin
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
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-scroll-margin 5)        ;; Use scroll margin
  (corfu-auto-delay 0.0)
  :bind (:map evil-insert-state-map
	      ("C-j" . corfu-next)
	      ("C-k" . corfu-previous)
	      :map corfu-map
	      ("<tab>" . corfu-next)
	      ("<backtab>" . corfu-previous))
  :init
  (setq tab-always-indent 'complete)
  (corfu-global-mode))

(use-package cape
  :straight t
  :after corfu
  :bind (:map evil-insert-state-map
	      ("M-'" . completion-at-point))
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  (setq-local completion-at-point-functions
              (list (cape-super-capf
		     #'cape-file
		     #'cape-dabbrev
		     #'cape-ispell
		     #'cape-keyword))))

(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
;completions_end

;embark_begin
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
;embark_end

;window_begin
(use-package ace-window
  :straight t
  :init
  (global-set-key (kbd "M-q") 'aw-show-dispatch-help)
  (setq aw-dispatch-always t))
  ;; :custom-face
  ;;   '(aw-leading-char-face
  ;;     :foreground "white" :background "red"
  ;;     :weight bold :height 5 :box (:line-width 10 :color "red")))
;window_end

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;org-transclusion_begin
(use-package org-transclusion
  :after org
  :straight t
  :init
  (setq org-transclusion-remember-transclusions t))
;org-transclusion_end

;;https://config.daviwil.com/emacs#system-settings
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

(setq org-support-shift-select t)
(setq org-log-done 'time)

(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-S-s") 'write-file)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "M-a") 'other-window)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-S-p") 'execute-extended-command)
(global-set-key (kbd "C-<tab>") #'consult-buffer)
(global-set-key (kbd "C-f") #'consult-ripgrep)
(global-set-key (kbd "C-i") #'consult-imenu)
(global-set-key (kbd "C-z") #'undo-fu-only-undo)
(global-set-key (kbd "C-S-z") #'undo-fu-only-redo)

;evil-leader_begin
(evil-leader/set-key
  "." 'find-file
  "," 'consult-buffer
  ";" 'consult-proj
  "SPC" 'execute-extended-command

  "e" '("eval" . (keymap))
  "eb" '("buffer" . eval-buffer)
  "er" '("region" . eval-region)

  "g" '("magit" . (keymap))
  "gc" '("commit" . magit-commit)
  "gf" '("fetch" . magit-fetch)
  "gg" '("status" . magit-status)

  "q" '("quit" . (keymap))  
  "qb" '("buffer" . kill-this-buffer)
  "qq" '("save&quit" . save-buffers-kill-terminal)

  "h" '("help" . (keymap))
  "hf" '("function" . describe-function)
  "hk" '("key" . describe-key)
  "hv" '("variable" . describe-variable)

  "w" '("window" . (keymap))
  "wd" '("delete" . delete-window)
  "wo" '("delete other" . delete-other-windows)
  "ww" '("ace-window" . aw-show-dispatch-help))
;evil-leader_end

(load (concat user-emacs-directory "userConfig.el"))
