
# Table of Contents

1.  [Startup](#Startup)
    1.  [User Emacs Directory](#User-Emacs-Directory)
    2.  [Package Management](#Package-Management)
    3.  [Visual elements](#Visual-elements)
    4.  [Early Init](#Early-Init)
2.  [Emacs Behavior](#Emacs-Behavior)
    1.  [Spellcheck](#Spellcheck)
    2.  [Dired](#Dired)
    3.  [No-littering](#No-littering)
    4.  [Misc](#Misc)
3.  [Packages](#Packages)
    1.  [Configuration Documentation/Annotation](#Configuration-Documentation-Annotation)
    2.  [Theme](#Theme)
    3.  [Muscle memory shortcuts](#Muscle-memory-shortcuts)
    4.  [Keybindings](#Keybindings)
    5.  [Evil-Mode](#Evil-Mode)
    6.  [History](#History)
    7.  [Completions](#Completions)
    8.  [User Config](#User-Config)
4.  [Repo meta](#Repo-meta)
    1.  [Useful anchors](#Useful-anchors)
    2.  [Exporting Readme markdown](#Exporting-Readme-markdown)
    3.  [Remote as "mirrors"](#Remote-as--mirrors-)

This is my custom config based on my [Emacs journey](https://shom.dev/posts/20211121_emacs-custom-configuration/). I wanted to create a literate config but did not want to slowdown startup with tangling, so currently I'm trying to get "the best of both worlds" via `org-transclusion`. The file `config.org` in this repo contains the "source" and `org-transclusion` directives and is rendered out to `README.md` (markdown is better supported for auto-rendering by more forges currently). I'll eventually automate this process, likely through a git-hook, currently it is rendered every time this file is saved using org's `after-save-hook`. However, the rendered output is never guaranteed to include all of my config, just the sections that have been manually commented, `init.el` and includes will remain the source of truth. 


<a id="Startup"></a>

# Startup

I would like to have the option to have several Emacs "distributions" or configurations that are independent of each other. This allows for experimentation without breaking things (git helps with the actual config but this allows for the packages to be independent). I'm using Chemacs2 but that's outside the scope of the config, for now.


<a id="User-Emacs-Directory"></a>

## User Emacs Directory

I've chosen to keep my custom config and all related packages, etc. in this specific location so that's the first thing in the config.

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
    (menu-bar-mode -1)
    (setq visible-bell 1)
    (global-visual-line-mode 1)
    (global-linum-mode 1)
    (column-number-mode t)
    (setq org-hide-emphasis-markers t)
    (setq org-image-actual-width nil)
    ;;visual_end
    
    ;;built-in_begin
    (defalias 'yes-or-no-p 'y-or-n-p)
    (global-auto-revert-mode 1)
    
    (setq-default indent-tabs-mode t)
    (setq-default tab-width 4)
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
      (evil-collection-define-key 'normal 'dired-mode-map
        "h" '(lambda () (interactive) (find-alternate-file ".."))
        "l" 'dired-find-alternate-file))
    ;;dired_end
    
    ;;no-littering_begin
    (use-package no-littering
      :straight t
      :init
      (setq auto-save-file-name-transforms
            `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
      (setq custom-file (no-littering-expand-etc-file-name "custom.el")))
    ;;no-littering_end
    
    ;;modeline_begin
    (use-package doom-modeline
      :straight t
      :init (doom-modeline-mode 1))
    ;;modeline_end
    
    ;;cua_begin
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
    ;;cua_end
    
    ;;which-key_begin
    (use-package which-key
      :straight t
      :config
      (which-key-mode)
      :init
      (setq which-key-idle-delay 0.1))
    ;;which-key_end
    
    (use-package magit
      :straight t
      :defer t)
    
    ;;evil_begin
    (use-package evil
      :straight t
      :bind (("<escape>" . keyboard-escape-quit))
      :init
      (setq evil-want-integration t
            evil-want-keybinding nil
            evil-disable-insert-state-bindings t
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
    ;;evil_end
    
    ;;theme_begin
    (use-package doom-themes
      :straight t
      :init (load-theme 'doom-one t))
    
    (use-package all-the-icons
      :straight t
      :config
      (unless (require 'all-the-icons nil 'noerror)
        (all-the-icons-install-fonts)))
    
    (use-package rainbow-delimiters
      :straight t
      :hook (prog-mode . rainbow-delimiters-mode))
    
    (set-frame-font "JetBrainsMono Nerd Font" 16 nil t)
    (set-frame-parameter (selected-frame) 'alpha 90)
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
      (add-to-list 'completion-at-point-functions #'cape-ispell)
      (setq-local completion-at-point-functions
                  (list (cape-capf-super
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
    ;;embark_end
    
    ;;window_begin
    (use-package ace-window
      :straight t
      :init
      (global-set-key (kbd "M-q") 'aw-show-dispatch-help)
      (setq aw-dispatch-always t))
    ;; :custom-face
    ;;   '(aw-leading-char-face
    ;;     :foreground "white" :background "red"
    ;;     :weight bold :height 5 :box (:line-width 10 :color "red")))
    ;;window_end
    
    ;;usefulanchors_begin
    ;; From @alphapapa's unpackaged repo https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors
    (use-package ox
      :config
      (define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
        "Attempt to export Org as HTML with useful link IDs.
    Instead of random IDs like \"#orga1b2c3\", use heading titles,
    made unique when necessary."
        :global t
        (if unpackaged/org-export-html-with-useful-ids-mode
            (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)
          (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))
    
      (defun unpackaged/org-export-get-reference (datum info)
        "Like `org-export-get-reference', except uses heading titles instead of random numbers."
        (let ((cache (plist-get info :internal-references)))
          (or (car (rassq datum cache))
              (let* ((crossrefs (plist-get info :crossrefs))
                     (cells (org-export-search-cells datum))
                     ;; Preserve any pre-existing association between
                     ;; a search cell and a reference, i.e., when some
                     ;; previously published document referenced a location
                     ;; within current file (see
                     ;; `org-publish-resolve-external-link').
                     ;;
                     ;; However, there is no guarantee that search cells are
                     ;; unique, e.g., there might be duplicate custom ID or
                     ;; two headings with the same title in the file.
                     ;;
                     ;; As a consequence, before re-using any reference to
                     ;; an element or object, we check that it doesn't refer
                     ;; to a previous element or object.
                     (new (or (cl-some
                               (lambda (cell)
                                 (let ((stored (cdr (assoc cell crossrefs))))
                                   (when stored
                                     (let ((old (org-export-format-reference stored)))
                                       (and (not (assoc old cache)) stored)))))
                               cells)
                              (when (org-element-property :raw-value datum)
                                ;; Heading with a title
                                (unpackaged/org-export-new-title-reference datum cache))
                              ;; NOTE: This probably breaks some Org Export
                              ;; feature, but if it does what I need, fine.
                              (org-export-format-reference
                               (org-export-new-reference cache))))
                     (reference-string new))
                ;; Cache contains both data already associated to
                ;; a reference and in-use internal references, so as to make
                ;; unique references.
                (dolist (cell cells) (push (cons cell new) cache))
                ;; Retain a direct association between reference string and
                ;; DATUM since (1) not every object or element can be given
                ;; a search cell (2) it permits quick lookup.
                (push (cons reference-string datum) cache)
                (plist-put info :internal-references cache)
                reference-string))))
    
      (defun unpackaged/org-export-new-title-reference (datum cache)
        "Return new reference for DATUM that is unique in CACHE."
        (cl-macrolet ((inc-suffixf (place)
                                   `(progn
                                      (string-match (rx bos
                                                        (minimal-match (group (1+ anything)))
                                                        (optional "--" (group (1+ digit)))
                                                        eos)
                                                    ,place)
                                      ;; HACK: `s1' instead of a gensym.
                                      (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                                 (match-string 2 ,place)))
                                              (suffix (if suffix
                                                          (string-to-number suffix)
                                                        0)))
                                        (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
          (let* ((title (org-element-property :raw-value datum))
                 (ref (replace-regexp-in-string "%.." "-" (url-hexify-string (substring-no-properties title)))) ;replace all encoded characters with dashes
                 (parent (org-element-property :parent datum)))
            (while (--any (equal ref (car it))
                          cache)
              ;; Title not unique: make it so.
              (if parent
                  ;; Append ancestor title.
                  (setf title (concat (org-element-property :raw-value parent)
                                      "--" title)
                        ref (url-hexify-string (substring-no-properties title))
                        parent (org-element-property :parent parent))
                ;; No more ancestors: add and increment a number.
                (inc-suffixf ref)))
            ref))))
    ;;usefulanchors_end
    
    ;;org-transclusion_begin
    (use-package org-transclusion
      :after org
      :straight t
      :init
      (setq org-transclusion-remember-transclusions t))
    ;;org-transclusion_end
    
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
    
    ;;evil-leader_begin
    (evil-leader/set-key
      "." 'find-file
      "," 'consult-buffer
      ";" 'consult-proj
      "c" 'org-capture
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
    ;;evil-leader_end
    
    ;;user-config_begin 
    (load (concat user-emacs-directory "userConfig.el"))
    ;;user-config_end 


<a id="Package-Management"></a>

## Package Management

I'm using `straight.el` as a package manager with the `use-package` syntax. Straight is bootstrapped, this ensures that a clean install of Emacs can get up and running without any manual intervention just by cloning this repo.

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
    (menu-bar-mode -1)
    (setq visible-bell 1)
    (global-visual-line-mode 1)
    (global-linum-mode 1)
    (column-number-mode t)
    (setq org-hide-emphasis-markers t)
    (setq org-image-actual-width nil)
    ;;visual_end
    
    ;;built-in_begin
    (defalias 'yes-or-no-p 'y-or-n-p)
    (global-auto-revert-mode 1)
    
    (setq-default indent-tabs-mode t)
    (setq-default tab-width 4)
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
      (evil-collection-define-key 'normal 'dired-mode-map
        "h" '(lambda () (interactive) (find-alternate-file ".."))
        "l" 'dired-find-alternate-file))
    ;;dired_end
    
    ;;no-littering_begin
    (use-package no-littering
      :straight t
      :init
      (setq auto-save-file-name-transforms
            `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
      (setq custom-file (no-littering-expand-etc-file-name "custom.el")))
    ;;no-littering_end
    
    ;;modeline_begin
    (use-package doom-modeline
      :straight t
      :init (doom-modeline-mode 1))
    ;;modeline_end
    
    ;;cua_begin
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
    ;;cua_end
    
    ;;which-key_begin
    (use-package which-key
      :straight t
      :config
      (which-key-mode)
      :init
      (setq which-key-idle-delay 0.1))
    ;;which-key_end
    
    (use-package magit
      :straight t
      :defer t)
    
    ;;evil_begin
    (use-package evil
      :straight t
      :bind (("<escape>" . keyboard-escape-quit))
      :init
      (setq evil-want-integration t
            evil-want-keybinding nil
            evil-disable-insert-state-bindings t
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
    ;;evil_end
    
    ;;theme_begin
    (use-package doom-themes
      :straight t
      :init (load-theme 'doom-one t))
    
    (use-package all-the-icons
      :straight t
      :config
      (unless (require 'all-the-icons nil 'noerror)
        (all-the-icons-install-fonts)))
    
    (use-package rainbow-delimiters
      :straight t
      :hook (prog-mode . rainbow-delimiters-mode))
    
    (set-frame-font "JetBrainsMono Nerd Font" 16 nil t)
    (set-frame-parameter (selected-frame) 'alpha 90)
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
      (add-to-list 'completion-at-point-functions #'cape-ispell)
      (setq-local completion-at-point-functions
                  (list (cape-capf-super
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
    ;;embark_end
    
    ;;window_begin
    (use-package ace-window
      :straight t
      :init
      (global-set-key (kbd "M-q") 'aw-show-dispatch-help)
      (setq aw-dispatch-always t))
    ;; :custom-face
    ;;   '(aw-leading-char-face
    ;;     :foreground "white" :background "red"
    ;;     :weight bold :height 5 :box (:line-width 10 :color "red")))
    ;;window_end
    
    ;;usefulanchors_begin
    ;; From @alphapapa's unpackaged repo https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors
    (use-package ox
      :config
      (define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
        "Attempt to export Org as HTML with useful link IDs.
    Instead of random IDs like \"#orga1b2c3\", use heading titles,
    made unique when necessary."
        :global t
        (if unpackaged/org-export-html-with-useful-ids-mode
            (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)
          (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))
    
      (defun unpackaged/org-export-get-reference (datum info)
        "Like `org-export-get-reference', except uses heading titles instead of random numbers."
        (let ((cache (plist-get info :internal-references)))
          (or (car (rassq datum cache))
              (let* ((crossrefs (plist-get info :crossrefs))
                     (cells (org-export-search-cells datum))
                     ;; Preserve any pre-existing association between
                     ;; a search cell and a reference, i.e., when some
                     ;; previously published document referenced a location
                     ;; within current file (see
                     ;; `org-publish-resolve-external-link').
                     ;;
                     ;; However, there is no guarantee that search cells are
                     ;; unique, e.g., there might be duplicate custom ID or
                     ;; two headings with the same title in the file.
                     ;;
                     ;; As a consequence, before re-using any reference to
                     ;; an element or object, we check that it doesn't refer
                     ;; to a previous element or object.
                     (new (or (cl-some
                               (lambda (cell)
                                 (let ((stored (cdr (assoc cell crossrefs))))
                                   (when stored
                                     (let ((old (org-export-format-reference stored)))
                                       (and (not (assoc old cache)) stored)))))
                               cells)
                              (when (org-element-property :raw-value datum)
                                ;; Heading with a title
                                (unpackaged/org-export-new-title-reference datum cache))
                              ;; NOTE: This probably breaks some Org Export
                              ;; feature, but if it does what I need, fine.
                              (org-export-format-reference
                               (org-export-new-reference cache))))
                     (reference-string new))
                ;; Cache contains both data already associated to
                ;; a reference and in-use internal references, so as to make
                ;; unique references.
                (dolist (cell cells) (push (cons cell new) cache))
                ;; Retain a direct association between reference string and
                ;; DATUM since (1) not every object or element can be given
                ;; a search cell (2) it permits quick lookup.
                (push (cons reference-string datum) cache)
                (plist-put info :internal-references cache)
                reference-string))))
    
      (defun unpackaged/org-export-new-title-reference (datum cache)
        "Return new reference for DATUM that is unique in CACHE."
        (cl-macrolet ((inc-suffixf (place)
                                   `(progn
                                      (string-match (rx bos
                                                        (minimal-match (group (1+ anything)))
                                                        (optional "--" (group (1+ digit)))
                                                        eos)
                                                    ,place)
                                      ;; HACK: `s1' instead of a gensym.
                                      (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                                 (match-string 2 ,place)))
                                              (suffix (if suffix
                                                          (string-to-number suffix)
                                                        0)))
                                        (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
          (let* ((title (org-element-property :raw-value datum))
                 (ref (replace-regexp-in-string "%.." "-" (url-hexify-string (substring-no-properties title)))) ;replace all encoded characters with dashes
                 (parent (org-element-property :parent datum)))
            (while (--any (equal ref (car it))
                          cache)
              ;; Title not unique: make it so.
              (if parent
                  ;; Append ancestor title.
                  (setf title (concat (org-element-property :raw-value parent)
                                      "--" title)
                        ref (url-hexify-string (substring-no-properties title))
                        parent (org-element-property :parent parent))
                ;; No more ancestors: add and increment a number.
                (inc-suffixf ref)))
            ref))))
    ;;usefulanchors_end
    
    ;;org-transclusion_begin
    (use-package org-transclusion
      :after org
      :straight t
      :init
      (setq org-transclusion-remember-transclusions t))
    ;;org-transclusion_end
    
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
    
    ;;evil-leader_begin
    (evil-leader/set-key
      "." 'find-file
      "," 'consult-buffer
      ";" 'consult-proj
      "c" 'org-capture
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
    ;;evil-leader_end
    
    ;;user-config_begin 
    (load (concat user-emacs-directory "userConfig.el"))
    ;;user-config_end 


<a id="Visual-elements"></a>

## Visual elements

The Emacs GUI is a bit dated, especially the toolbar and menu bar. Also since I'll be customizing my keymaps they're going to be mostly superfluous and take up real-estate. There are a few other preferences relative to how things are displayed here, the rest of the visuals are configured by the theme.

    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (setq visible-bell 1)
    (global-visual-line-mode 1)
    (global-linum-mode 1)
    (column-number-mode t)
    (setq org-hide-emphasis-markers t)
    (setq org-image-actual-width nil)
    ;;visual_end
    
    ;;built-in_begin
    (defalias 'yes-or-no-p 'y-or-n-p)
    (global-auto-revert-mode 1)
    
    (setq-default indent-tabs-mode t)
    (setq-default tab-width 4)
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
      (evil-collection-define-key 'normal 'dired-mode-map
        "h" '(lambda () (interactive) (find-alternate-file ".."))
        "l" 'dired-find-alternate-file))
    ;;dired_end
    
    ;;no-littering_begin
    (use-package no-littering
      :straight t
      :init
      (setq auto-save-file-name-transforms
            `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
      (setq custom-file (no-littering-expand-etc-file-name "custom.el")))
    ;;no-littering_end
    
    ;;modeline_begin
    (use-package doom-modeline
      :straight t
      :init (doom-modeline-mode 1))
    ;;modeline_end
    
    ;;cua_begin
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
    ;;cua_end
    
    ;;which-key_begin
    (use-package which-key
      :straight t
      :config
      (which-key-mode)
      :init
      (setq which-key-idle-delay 0.1))
    ;;which-key_end
    
    (use-package magit
      :straight t
      :defer t)
    
    ;;evil_begin
    (use-package evil
      :straight t
      :bind (("<escape>" . keyboard-escape-quit))
      :init
      (setq evil-want-integration t
            evil-want-keybinding nil
            evil-disable-insert-state-bindings t
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
    ;;evil_end
    
    ;;theme_begin
    (use-package doom-themes
      :straight t
      :init (load-theme 'doom-one t))
    
    (use-package all-the-icons
      :straight t
      :config
      (unless (require 'all-the-icons nil 'noerror)
        (all-the-icons-install-fonts)))
    
    (use-package rainbow-delimiters
      :straight t
      :hook (prog-mode . rainbow-delimiters-mode))
    
    (set-frame-font "JetBrainsMono Nerd Font" 16 nil t)
    (set-frame-parameter (selected-frame) 'alpha 90)
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
      (add-to-list 'completion-at-point-functions #'cape-ispell)
      (setq-local completion-at-point-functions
                  (list (cape-capf-super
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
    ;;embark_end
    
    ;;window_begin
    (use-package ace-window
      :straight t
      :init
      (global-set-key (kbd "M-q") 'aw-show-dispatch-help)
      (setq aw-dispatch-always t))
    ;; :custom-face
    ;;   '(aw-leading-char-face
    ;;     :foreground "white" :background "red"
    ;;     :weight bold :height 5 :box (:line-width 10 :color "red")))
    ;;window_end
    
    ;;usefulanchors_begin
    ;; From @alphapapa's unpackaged repo https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors
    (use-package ox
      :config
      (define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
        "Attempt to export Org as HTML with useful link IDs.
    Instead of random IDs like \"#orga1b2c3\", use heading titles,
    made unique when necessary."
        :global t
        (if unpackaged/org-export-html-with-useful-ids-mode
            (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)
          (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))
    
      (defun unpackaged/org-export-get-reference (datum info)
        "Like `org-export-get-reference', except uses heading titles instead of random numbers."
        (let ((cache (plist-get info :internal-references)))
          (or (car (rassq datum cache))
              (let* ((crossrefs (plist-get info :crossrefs))
                     (cells (org-export-search-cells datum))
                     ;; Preserve any pre-existing association between
                     ;; a search cell and a reference, i.e., when some
                     ;; previously published document referenced a location
                     ;; within current file (see
                     ;; `org-publish-resolve-external-link').
                     ;;
                     ;; However, there is no guarantee that search cells are
                     ;; unique, e.g., there might be duplicate custom ID or
                     ;; two headings with the same title in the file.
                     ;;
                     ;; As a consequence, before re-using any reference to
                     ;; an element or object, we check that it doesn't refer
                     ;; to a previous element or object.
                     (new (or (cl-some
                               (lambda (cell)
                                 (let ((stored (cdr (assoc cell crossrefs))))
                                   (when stored
                                     (let ((old (org-export-format-reference stored)))
                                       (and (not (assoc old cache)) stored)))))
                               cells)
                              (when (org-element-property :raw-value datum)
                                ;; Heading with a title
                                (unpackaged/org-export-new-title-reference datum cache))
                              ;; NOTE: This probably breaks some Org Export
                              ;; feature, but if it does what I need, fine.
                              (org-export-format-reference
                               (org-export-new-reference cache))))
                     (reference-string new))
                ;; Cache contains both data already associated to
                ;; a reference and in-use internal references, so as to make
                ;; unique references.
                (dolist (cell cells) (push (cons cell new) cache))
                ;; Retain a direct association between reference string and
                ;; DATUM since (1) not every object or element can be given
                ;; a search cell (2) it permits quick lookup.
                (push (cons reference-string datum) cache)
                (plist-put info :internal-references cache)
                reference-string))))
    
      (defun unpackaged/org-export-new-title-reference (datum cache)
        "Return new reference for DATUM that is unique in CACHE."
        (cl-macrolet ((inc-suffixf (place)
                                   `(progn
                                      (string-match (rx bos
                                                        (minimal-match (group (1+ anything)))
                                                        (optional "--" (group (1+ digit)))
                                                        eos)
                                                    ,place)
                                      ;; HACK: `s1' instead of a gensym.
                                      (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                                 (match-string 2 ,place)))
                                              (suffix (if suffix
                                                          (string-to-number suffix)
                                                        0)))
                                        (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
          (let* ((title (org-element-property :raw-value datum))
                 (ref (replace-regexp-in-string "%.." "-" (url-hexify-string (substring-no-properties title)))) ;replace all encoded characters with dashes
                 (parent (org-element-property :parent datum)))
            (while (--any (equal ref (car it))
                          cache)
              ;; Title not unique: make it so.
              (if parent
                  ;; Append ancestor title.
                  (setf title (concat (org-element-property :raw-value parent)
                                      "--" title)
                        ref (url-hexify-string (substring-no-properties title))
                        parent (org-element-property :parent parent))
                ;; No more ancestors: add and increment a number.
                (inc-suffixf ref)))
            ref))))
    ;;usefulanchors_end
    
    ;;org-transclusion_begin
    (use-package org-transclusion
      :after org
      :straight t
      :init
      (setq org-transclusion-remember-transclusions t))
    ;;org-transclusion_end
    
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
    
    ;;evil-leader_begin
    (evil-leader/set-key
      "." 'find-file
      "," 'consult-buffer
      ";" 'consult-proj
      "c" 'org-capture
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
    ;;evil-leader_end
    
    ;;user-config_begin 
    (load (concat user-emacs-directory "userConfig.el"))
    ;;user-config_end 

The modeline is another important visual element to show important information about the buffer and modes. Doom's modeline is a significant visual upgrade to the default.

    (use-package doom-modeline
      :straight t
      :init (doom-modeline-mode 1))
    ;;modeline_end
    
    ;;cua_begin
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
    ;;cua_end
    
    ;;which-key_begin
    (use-package which-key
      :straight t
      :config
      (which-key-mode)
      :init
      (setq which-key-idle-delay 0.1))
    ;;which-key_end
    
    (use-package magit
      :straight t
      :defer t)
    
    ;;evil_begin
    (use-package evil
      :straight t
      :bind (("<escape>" . keyboard-escape-quit))
      :init
      (setq evil-want-integration t
            evil-want-keybinding nil
            evil-disable-insert-state-bindings t
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
    ;;evil_end
    
    ;;theme_begin
    (use-package doom-themes
      :straight t
      :init (load-theme 'doom-one t))
    
    (use-package all-the-icons
      :straight t
      :config
      (unless (require 'all-the-icons nil 'noerror)
        (all-the-icons-install-fonts)))
    
    (use-package rainbow-delimiters
      :straight t
      :hook (prog-mode . rainbow-delimiters-mode))
    
    (set-frame-font "JetBrainsMono Nerd Font" 16 nil t)
    (set-frame-parameter (selected-frame) 'alpha 90)
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
      (add-to-list 'completion-at-point-functions #'cape-ispell)
      (setq-local completion-at-point-functions
                  (list (cape-capf-super
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
    ;;embark_end
    
    ;;window_begin
    (use-package ace-window
      :straight t
      :init
      (global-set-key (kbd "M-q") 'aw-show-dispatch-help)
      (setq aw-dispatch-always t))
    ;; :custom-face
    ;;   '(aw-leading-char-face
    ;;     :foreground "white" :background "red"
    ;;     :weight bold :height 5 :box (:line-width 10 :color "red")))
    ;;window_end
    
    ;;usefulanchors_begin
    ;; From @alphapapa's unpackaged repo https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors
    (use-package ox
      :config
      (define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
        "Attempt to export Org as HTML with useful link IDs.
    Instead of random IDs like \"#orga1b2c3\", use heading titles,
    made unique when necessary."
        :global t
        (if unpackaged/org-export-html-with-useful-ids-mode
            (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)
          (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))
    
      (defun unpackaged/org-export-get-reference (datum info)
        "Like `org-export-get-reference', except uses heading titles instead of random numbers."
        (let ((cache (plist-get info :internal-references)))
          (or (car (rassq datum cache))
              (let* ((crossrefs (plist-get info :crossrefs))
                     (cells (org-export-search-cells datum))
                     ;; Preserve any pre-existing association between
                     ;; a search cell and a reference, i.e., when some
                     ;; previously published document referenced a location
                     ;; within current file (see
                     ;; `org-publish-resolve-external-link').
                     ;;
                     ;; However, there is no guarantee that search cells are
                     ;; unique, e.g., there might be duplicate custom ID or
                     ;; two headings with the same title in the file.
                     ;;
                     ;; As a consequence, before re-using any reference to
                     ;; an element or object, we check that it doesn't refer
                     ;; to a previous element or object.
                     (new (or (cl-some
                               (lambda (cell)
                                 (let ((stored (cdr (assoc cell crossrefs))))
                                   (when stored
                                     (let ((old (org-export-format-reference stored)))
                                       (and (not (assoc old cache)) stored)))))
                               cells)
                              (when (org-element-property :raw-value datum)
                                ;; Heading with a title
                                (unpackaged/org-export-new-title-reference datum cache))
                              ;; NOTE: This probably breaks some Org Export
                              ;; feature, but if it does what I need, fine.
                              (org-export-format-reference
                               (org-export-new-reference cache))))
                     (reference-string new))
                ;; Cache contains both data already associated to
                ;; a reference and in-use internal references, so as to make
                ;; unique references.
                (dolist (cell cells) (push (cons cell new) cache))
                ;; Retain a direct association between reference string and
                ;; DATUM since (1) not every object or element can be given
                ;; a search cell (2) it permits quick lookup.
                (push (cons reference-string datum) cache)
                (plist-put info :internal-references cache)
                reference-string))))
    
      (defun unpackaged/org-export-new-title-reference (datum cache)
        "Return new reference for DATUM that is unique in CACHE."
        (cl-macrolet ((inc-suffixf (place)
                                   `(progn
                                      (string-match (rx bos
                                                        (minimal-match (group (1+ anything)))
                                                        (optional "--" (group (1+ digit)))
                                                        eos)
                                                    ,place)
                                      ;; HACK: `s1' instead of a gensym.
                                      (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                                 (match-string 2 ,place)))
                                              (suffix (if suffix
                                                          (string-to-number suffix)
                                                        0)))
                                        (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
          (let* ((title (org-element-property :raw-value datum))
                 (ref (replace-regexp-in-string "%.." "-" (url-hexify-string (substring-no-properties title)))) ;replace all encoded characters with dashes
                 (parent (org-element-property :parent datum)))
            (while (--any (equal ref (car it))
                          cache)
              ;; Title not unique: make it so.
              (if parent
                  ;; Append ancestor title.
                  (setf title (concat (org-element-property :raw-value parent)
                                      "--" title)
                        ref (url-hexify-string (substring-no-properties title))
                        parent (org-element-property :parent parent))
                ;; No more ancestors: add and increment a number.
                (inc-suffixf ref)))
            ref))))
    ;;usefulanchors_end
    
    ;;org-transclusion_begin
    (use-package org-transclusion
      :after org
      :straight t
      :init
      (setq org-transclusion-remember-transclusions t))
    ;;org-transclusion_end
    
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
    
    ;;evil-leader_begin
    (evil-leader/set-key
      "." 'find-file
      "," 'consult-buffer
      ";" 'consult-proj
      "c" 'org-capture
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
    ;;evil-leader_end
    
    ;;user-config_begin 
    (load (concat user-emacs-directory "userConfig.el"))
    ;;user-config_end 


<a id="Early-Init"></a>

## Early Init

I'm not proficient in understanding the underpinnings of early init type optimizations so I'm borrowing from [Doom](https://github.com/hlissner/doom-emacs/) and [System Crafters](https://systemcrafters.net). here.

    ;; -*- lexical-binding: t; -*-
    
    ;; The default is 800 kilobytes.  Measured in bytes.
    (setq gc-cons-threshold (* 50 1000 1000))
    
    ;; Profile emacs startup
    (add-hook 'emacs-startup-hook
              (lambda ()
                (message "*** Emacs loaded in %s seconds with %d garbage collections."
                         (emacs-init-time "%.2f")
                         gcs-done)))
    
    ;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
    ;; packages are compiled ahead-of-time when they are installed and site files
    ;; are compiled when gccemacs is installed.
    ;; REVIEW Remove after a month
    (setq comp-deferred-compilation nil
          native-comp-deferred-compilation nil)


<a id="Emacs-Behavior"></a>

# Emacs Behavior

A lot the configuration revolves around packages but there are some built-in settings and behavior within Emacs that also need to be tweaked.


<a id="Spellcheck"></a>

## Spellcheck

Spellchecking while typing is useful and I want to use it everywhere but in programming modes only comments should be spell checked not the whole buffer.
Prose and grammar linting is provided by `flycheck-vale` which requires [vale](https://vale.sh/generator) to be setup with a `.vale.ini` file per project.

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
      (evil-collection-define-key 'normal 'dired-mode-map
        "h" '(lambda () (interactive) (find-alternate-file ".."))
        "l" 'dired-find-alternate-file))
    ;;dired_end
    
    ;;no-littering_begin
    (use-package no-littering
      :straight t
      :init
      (setq auto-save-file-name-transforms
            `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
      (setq custom-file (no-littering-expand-etc-file-name "custom.el")))
    ;;no-littering_end
    
    ;;modeline_begin
    (use-package doom-modeline
      :straight t
      :init (doom-modeline-mode 1))
    ;;modeline_end
    
    ;;cua_begin
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
    ;;cua_end
    
    ;;which-key_begin
    (use-package which-key
      :straight t
      :config
      (which-key-mode)
      :init
      (setq which-key-idle-delay 0.1))
    ;;which-key_end
    
    (use-package magit
      :straight t
      :defer t)
    
    ;;evil_begin
    (use-package evil
      :straight t
      :bind (("<escape>" . keyboard-escape-quit))
      :init
      (setq evil-want-integration t
            evil-want-keybinding nil
            evil-disable-insert-state-bindings t
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
    ;;evil_end
    
    ;;theme_begin
    (use-package doom-themes
      :straight t
      :init (load-theme 'doom-one t))
    
    (use-package all-the-icons
      :straight t
      :config
      (unless (require 'all-the-icons nil 'noerror)
        (all-the-icons-install-fonts)))
    
    (use-package rainbow-delimiters
      :straight t
      :hook (prog-mode . rainbow-delimiters-mode))
    
    (set-frame-font "JetBrainsMono Nerd Font" 16 nil t)
    (set-frame-parameter (selected-frame) 'alpha 90)
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
      (add-to-list 'completion-at-point-functions #'cape-ispell)
      (setq-local completion-at-point-functions
                  (list (cape-capf-super
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
    ;;embark_end
    
    ;;window_begin
    (use-package ace-window
      :straight t
      :init
      (global-set-key (kbd "M-q") 'aw-show-dispatch-help)
      (setq aw-dispatch-always t))
    ;; :custom-face
    ;;   '(aw-leading-char-face
    ;;     :foreground "white" :background "red"
    ;;     :weight bold :height 5 :box (:line-width 10 :color "red")))
    ;;window_end
    
    ;;usefulanchors_begin
    ;; From @alphapapa's unpackaged repo https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors
    (use-package ox
      :config
      (define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
        "Attempt to export Org as HTML with useful link IDs.
    Instead of random IDs like \"#orga1b2c3\", use heading titles,
    made unique when necessary."
        :global t
        (if unpackaged/org-export-html-with-useful-ids-mode
            (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)
          (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))
    
      (defun unpackaged/org-export-get-reference (datum info)
        "Like `org-export-get-reference', except uses heading titles instead of random numbers."
        (let ((cache (plist-get info :internal-references)))
          (or (car (rassq datum cache))
              (let* ((crossrefs (plist-get info :crossrefs))
                     (cells (org-export-search-cells datum))
                     ;; Preserve any pre-existing association between
                     ;; a search cell and a reference, i.e., when some
                     ;; previously published document referenced a location
                     ;; within current file (see
                     ;; `org-publish-resolve-external-link').
                     ;;
                     ;; However, there is no guarantee that search cells are
                     ;; unique, e.g., there might be duplicate custom ID or
                     ;; two headings with the same title in the file.
                     ;;
                     ;; As a consequence, before re-using any reference to
                     ;; an element or object, we check that it doesn't refer
                     ;; to a previous element or object.
                     (new (or (cl-some
                               (lambda (cell)
                                 (let ((stored (cdr (assoc cell crossrefs))))
                                   (when stored
                                     (let ((old (org-export-format-reference stored)))
                                       (and (not (assoc old cache)) stored)))))
                               cells)
                              (when (org-element-property :raw-value datum)
                                ;; Heading with a title
                                (unpackaged/org-export-new-title-reference datum cache))
                              ;; NOTE: This probably breaks some Org Export
                              ;; feature, but if it does what I need, fine.
                              (org-export-format-reference
                               (org-export-new-reference cache))))
                     (reference-string new))
                ;; Cache contains both data already associated to
                ;; a reference and in-use internal references, so as to make
                ;; unique references.
                (dolist (cell cells) (push (cons cell new) cache))
                ;; Retain a direct association between reference string and
                ;; DATUM since (1) not every object or element can be given
                ;; a search cell (2) it permits quick lookup.
                (push (cons reference-string datum) cache)
                (plist-put info :internal-references cache)
                reference-string))))
    
      (defun unpackaged/org-export-new-title-reference (datum cache)
        "Return new reference for DATUM that is unique in CACHE."
        (cl-macrolet ((inc-suffixf (place)
                                   `(progn
                                      (string-match (rx bos
                                                        (minimal-match (group (1+ anything)))
                                                        (optional "--" (group (1+ digit)))
                                                        eos)
                                                    ,place)
                                      ;; HACK: `s1' instead of a gensym.
                                      (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                                 (match-string 2 ,place)))
                                              (suffix (if suffix
                                                          (string-to-number suffix)
                                                        0)))
                                        (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
          (let* ((title (org-element-property :raw-value datum))
                 (ref (replace-regexp-in-string "%.." "-" (url-hexify-string (substring-no-properties title)))) ;replace all encoded characters with dashes
                 (parent (org-element-property :parent datum)))
            (while (--any (equal ref (car it))
                          cache)
              ;; Title not unique: make it so.
              (if parent
                  ;; Append ancestor title.
                  (setf title (concat (org-element-property :raw-value parent)
                                      "--" title)
                        ref (url-hexify-string (substring-no-properties title))
                        parent (org-element-property :parent parent))
                ;; No more ancestors: add and increment a number.
                (inc-suffixf ref)))
            ref))))
    ;;usefulanchors_end
    
    ;;org-transclusion_begin
    (use-package org-transclusion
      :after org
      :straight t
      :init
      (setq org-transclusion-remember-transclusions t))
    ;;org-transclusion_end
    
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
    
    ;;evil-leader_begin
    (evil-leader/set-key
      "." 'find-file
      "," 'consult-buffer
      ";" 'consult-proj
      "c" 'org-capture
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
    ;;evil-leader_end
    
    ;;user-config_begin 
    (load (concat user-emacs-directory "userConfig.el"))
    ;;user-config_end 


<a id="Dired"></a>

## Dired

The built-in file explorer (directory editor, dired) doesn't need to be installed but can be configured using use-package to keep the form consistent. In order for  Emacs to not spawn a new buffer for each directory and to make it more evil friendly I'm using `*find-alternate-file`.

    (use-package dired
      :straight nil
      :after evil-collection
      :commands (dired dired-jump)
      :custom (dired-listing-switches "-agho --group-directories-first")
      :config
      (evil-collection-define-key 'normal 'dired-mode-map
        "h" '(lambda () (interactive) (find-alternate-file ".."))
        "l" 'dired-find-alternate-file))
    ;;dired_end
    
    ;;no-littering_begin
    (use-package no-littering
      :straight t
      :init
      (setq auto-save-file-name-transforms
            `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
      (setq custom-file (no-littering-expand-etc-file-name "custom.el")))
    ;;no-littering_end
    
    ;;modeline_begin
    (use-package doom-modeline
      :straight t
      :init (doom-modeline-mode 1))
    ;;modeline_end
    
    ;;cua_begin
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
    ;;cua_end
    
    ;;which-key_begin
    (use-package which-key
      :straight t
      :config
      (which-key-mode)
      :init
      (setq which-key-idle-delay 0.1))
    ;;which-key_end
    
    (use-package magit
      :straight t
      :defer t)
    
    ;;evil_begin
    (use-package evil
      :straight t
      :bind (("<escape>" . keyboard-escape-quit))
      :init
      (setq evil-want-integration t
            evil-want-keybinding nil
            evil-disable-insert-state-bindings t
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
    ;;evil_end
    
    ;;theme_begin
    (use-package doom-themes
      :straight t
      :init (load-theme 'doom-one t))
    
    (use-package all-the-icons
      :straight t
      :config
      (unless (require 'all-the-icons nil 'noerror)
        (all-the-icons-install-fonts)))
    
    (use-package rainbow-delimiters
      :straight t
      :hook (prog-mode . rainbow-delimiters-mode))
    
    (set-frame-font "JetBrainsMono Nerd Font" 16 nil t)
    (set-frame-parameter (selected-frame) 'alpha 90)
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
      (add-to-list 'completion-at-point-functions #'cape-ispell)
      (setq-local completion-at-point-functions
                  (list (cape-capf-super
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
    ;;embark_end
    
    ;;window_begin
    (use-package ace-window
      :straight t
      :init
      (global-set-key (kbd "M-q") 'aw-show-dispatch-help)
      (setq aw-dispatch-always t))
    ;; :custom-face
    ;;   '(aw-leading-char-face
    ;;     :foreground "white" :background "red"
    ;;     :weight bold :height 5 :box (:line-width 10 :color "red")))
    ;;window_end
    
    ;;usefulanchors_begin
    ;; From @alphapapa's unpackaged repo https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors
    (use-package ox
      :config
      (define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
        "Attempt to export Org as HTML with useful link IDs.
    Instead of random IDs like \"#orga1b2c3\", use heading titles,
    made unique when necessary."
        :global t
        (if unpackaged/org-export-html-with-useful-ids-mode
            (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)
          (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))
    
      (defun unpackaged/org-export-get-reference (datum info)
        "Like `org-export-get-reference', except uses heading titles instead of random numbers."
        (let ((cache (plist-get info :internal-references)))
          (or (car (rassq datum cache))
              (let* ((crossrefs (plist-get info :crossrefs))
                     (cells (org-export-search-cells datum))
                     ;; Preserve any pre-existing association between
                     ;; a search cell and a reference, i.e., when some
                     ;; previously published document referenced a location
                     ;; within current file (see
                     ;; `org-publish-resolve-external-link').
                     ;;
                     ;; However, there is no guarantee that search cells are
                     ;; unique, e.g., there might be duplicate custom ID or
                     ;; two headings with the same title in the file.
                     ;;
                     ;; As a consequence, before re-using any reference to
                     ;; an element or object, we check that it doesn't refer
                     ;; to a previous element or object.
                     (new (or (cl-some
                               (lambda (cell)
                                 (let ((stored (cdr (assoc cell crossrefs))))
                                   (when stored
                                     (let ((old (org-export-format-reference stored)))
                                       (and (not (assoc old cache)) stored)))))
                               cells)
                              (when (org-element-property :raw-value datum)
                                ;; Heading with a title
                                (unpackaged/org-export-new-title-reference datum cache))
                              ;; NOTE: This probably breaks some Org Export
                              ;; feature, but if it does what I need, fine.
                              (org-export-format-reference
                               (org-export-new-reference cache))))
                     (reference-string new))
                ;; Cache contains both data already associated to
                ;; a reference and in-use internal references, so as to make
                ;; unique references.
                (dolist (cell cells) (push (cons cell new) cache))
                ;; Retain a direct association between reference string and
                ;; DATUM since (1) not every object or element can be given
                ;; a search cell (2) it permits quick lookup.
                (push (cons reference-string datum) cache)
                (plist-put info :internal-references cache)
                reference-string))))
    
      (defun unpackaged/org-export-new-title-reference (datum cache)
        "Return new reference for DATUM that is unique in CACHE."
        (cl-macrolet ((inc-suffixf (place)
                                   `(progn
                                      (string-match (rx bos
                                                        (minimal-match (group (1+ anything)))
                                                        (optional "--" (group (1+ digit)))
                                                        eos)
                                                    ,place)
                                      ;; HACK: `s1' instead of a gensym.
                                      (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                                 (match-string 2 ,place)))
                                              (suffix (if suffix
                                                          (string-to-number suffix)
                                                        0)))
                                        (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
          (let* ((title (org-element-property :raw-value datum))
                 (ref (replace-regexp-in-string "%.." "-" (url-hexify-string (substring-no-properties title)))) ;replace all encoded characters with dashes
                 (parent (org-element-property :parent datum)))
            (while (--any (equal ref (car it))
                          cache)
              ;; Title not unique: make it so.
              (if parent
                  ;; Append ancestor title.
                  (setf title (concat (org-element-property :raw-value parent)
                                      "--" title)
                        ref (url-hexify-string (substring-no-properties title))
                        parent (org-element-property :parent parent))
                ;; No more ancestors: add and increment a number.
                (inc-suffixf ref)))
            ref))))
    ;;usefulanchors_end
    
    ;;org-transclusion_begin
    (use-package org-transclusion
      :after org
      :straight t
      :init
      (setq org-transclusion-remember-transclusions t))
    ;;org-transclusion_end
    
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
    
    ;;evil-leader_begin
    (evil-leader/set-key
      "." 'find-file
      "," 'consult-buffer
      ";" 'consult-proj
      "c" 'org-capture
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
    ;;evil-leader_end
    
    ;;user-config_begin 
    (load (concat user-emacs-directory "userConfig.el"))
    ;;user-config_end 


<a id="No-littering"></a>

## No-littering

I don't want Emacs to put backup files in the file's directory and mess with git ignore at each repo so this package collects those and also all custom file and puts them all in defined locations.

    (use-package no-littering
      :straight t
      :init
      (setq auto-save-file-name-transforms
            `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
      (setq custom-file (no-littering-expand-etc-file-name "custom.el")))
    ;;no-littering_end
    
    ;;modeline_begin
    (use-package doom-modeline
      :straight t
      :init (doom-modeline-mode 1))
    ;;modeline_end
    
    ;;cua_begin
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
    ;;cua_end
    
    ;;which-key_begin
    (use-package which-key
      :straight t
      :config
      (which-key-mode)
      :init
      (setq which-key-idle-delay 0.1))
    ;;which-key_end
    
    (use-package magit
      :straight t
      :defer t)
    
    ;;evil_begin
    (use-package evil
      :straight t
      :bind (("<escape>" . keyboard-escape-quit))
      :init
      (setq evil-want-integration t
            evil-want-keybinding nil
            evil-disable-insert-state-bindings t
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
    ;;evil_end
    
    ;;theme_begin
    (use-package doom-themes
      :straight t
      :init (load-theme 'doom-one t))
    
    (use-package all-the-icons
      :straight t
      :config
      (unless (require 'all-the-icons nil 'noerror)
        (all-the-icons-install-fonts)))
    
    (use-package rainbow-delimiters
      :straight t
      :hook (prog-mode . rainbow-delimiters-mode))
    
    (set-frame-font "JetBrainsMono Nerd Font" 16 nil t)
    (set-frame-parameter (selected-frame) 'alpha 90)
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
      (add-to-list 'completion-at-point-functions #'cape-ispell)
      (setq-local completion-at-point-functions
                  (list (cape-capf-super
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
    ;;embark_end
    
    ;;window_begin
    (use-package ace-window
      :straight t
      :init
      (global-set-key (kbd "M-q") 'aw-show-dispatch-help)
      (setq aw-dispatch-always t))
    ;; :custom-face
    ;;   '(aw-leading-char-face
    ;;     :foreground "white" :background "red"
    ;;     :weight bold :height 5 :box (:line-width 10 :color "red")))
    ;;window_end
    
    ;;usefulanchors_begin
    ;; From @alphapapa's unpackaged repo https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors
    (use-package ox
      :config
      (define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
        "Attempt to export Org as HTML with useful link IDs.
    Instead of random IDs like \"#orga1b2c3\", use heading titles,
    made unique when necessary."
        :global t
        (if unpackaged/org-export-html-with-useful-ids-mode
            (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)
          (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))
    
      (defun unpackaged/org-export-get-reference (datum info)
        "Like `org-export-get-reference', except uses heading titles instead of random numbers."
        (let ((cache (plist-get info :internal-references)))
          (or (car (rassq datum cache))
              (let* ((crossrefs (plist-get info :crossrefs))
                     (cells (org-export-search-cells datum))
                     ;; Preserve any pre-existing association between
                     ;; a search cell and a reference, i.e., when some
                     ;; previously published document referenced a location
                     ;; within current file (see
                     ;; `org-publish-resolve-external-link').
                     ;;
                     ;; However, there is no guarantee that search cells are
                     ;; unique, e.g., there might be duplicate custom ID or
                     ;; two headings with the same title in the file.
                     ;;
                     ;; As a consequence, before re-using any reference to
                     ;; an element or object, we check that it doesn't refer
                     ;; to a previous element or object.
                     (new (or (cl-some
                               (lambda (cell)
                                 (let ((stored (cdr (assoc cell crossrefs))))
                                   (when stored
                                     (let ((old (org-export-format-reference stored)))
                                       (and (not (assoc old cache)) stored)))))
                               cells)
                              (when (org-element-property :raw-value datum)
                                ;; Heading with a title
                                (unpackaged/org-export-new-title-reference datum cache))
                              ;; NOTE: This probably breaks some Org Export
                              ;; feature, but if it does what I need, fine.
                              (org-export-format-reference
                               (org-export-new-reference cache))))
                     (reference-string new))
                ;; Cache contains both data already associated to
                ;; a reference and in-use internal references, so as to make
                ;; unique references.
                (dolist (cell cells) (push (cons cell new) cache))
                ;; Retain a direct association between reference string and
                ;; DATUM since (1) not every object or element can be given
                ;; a search cell (2) it permits quick lookup.
                (push (cons reference-string datum) cache)
                (plist-put info :internal-references cache)
                reference-string))))
    
      (defun unpackaged/org-export-new-title-reference (datum cache)
        "Return new reference for DATUM that is unique in CACHE."
        (cl-macrolet ((inc-suffixf (place)
                                   `(progn
                                      (string-match (rx bos
                                                        (minimal-match (group (1+ anything)))
                                                        (optional "--" (group (1+ digit)))
                                                        eos)
                                                    ,place)
                                      ;; HACK: `s1' instead of a gensym.
                                      (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                                 (match-string 2 ,place)))
                                              (suffix (if suffix
                                                          (string-to-number suffix)
                                                        0)))
                                        (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
          (let* ((title (org-element-property :raw-value datum))
                 (ref (replace-regexp-in-string "%.." "-" (url-hexify-string (substring-no-properties title)))) ;replace all encoded characters with dashes
                 (parent (org-element-property :parent datum)))
            (while (--any (equal ref (car it))
                          cache)
              ;; Title not unique: make it so.
              (if parent
                  ;; Append ancestor title.
                  (setf title (concat (org-element-property :raw-value parent)
                                      "--" title)
                        ref (url-hexify-string (substring-no-properties title))
                        parent (org-element-property :parent parent))
                ;; No more ancestors: add and increment a number.
                (inc-suffixf ref)))
            ref))))
    ;;usefulanchors_end
    
    ;;org-transclusion_begin
    (use-package org-transclusion
      :after org
      :straight t
      :init
      (setq org-transclusion-remember-transclusions t))
    ;;org-transclusion_end
    
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
    
    ;;evil-leader_begin
    (evil-leader/set-key
      "." 'find-file
      "," 'consult-buffer
      ";" 'consult-proj
      "c" 'org-capture
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
    ;;evil-leader_end
    
    ;;user-config_begin 
    (load (concat user-emacs-directory "userConfig.el"))
    ;;user-config_end 


<a id="Misc"></a>

## Misc

Because it's easier to type one letter than a word, let's replace the common yes/no prompt with y/n. Also, if the underlying file changes, the buffer should update (revert) automatically.

    (defalias 'yes-or-no-p 'y-or-n-p)
    (global-auto-revert-mode 1)
    
    (setq-default indent-tabs-mode t)
    (setq-default tab-width 4)
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
      (evil-collection-define-key 'normal 'dired-mode-map
        "h" '(lambda () (interactive) (find-alternate-file ".."))
        "l" 'dired-find-alternate-file))
    ;;dired_end
    
    ;;no-littering_begin
    (use-package no-littering
      :straight t
      :init
      (setq auto-save-file-name-transforms
            `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
      (setq custom-file (no-littering-expand-etc-file-name "custom.el")))
    ;;no-littering_end
    
    ;;modeline_begin
    (use-package doom-modeline
      :straight t
      :init (doom-modeline-mode 1))
    ;;modeline_end
    
    ;;cua_begin
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
    ;;cua_end
    
    ;;which-key_begin
    (use-package which-key
      :straight t
      :config
      (which-key-mode)
      :init
      (setq which-key-idle-delay 0.1))
    ;;which-key_end
    
    (use-package magit
      :straight t
      :defer t)
    
    ;;evil_begin
    (use-package evil
      :straight t
      :bind (("<escape>" . keyboard-escape-quit))
      :init
      (setq evil-want-integration t
            evil-want-keybinding nil
            evil-disable-insert-state-bindings t
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
    ;;evil_end
    
    ;;theme_begin
    (use-package doom-themes
      :straight t
      :init (load-theme 'doom-one t))
    
    (use-package all-the-icons
      :straight t
      :config
      (unless (require 'all-the-icons nil 'noerror)
        (all-the-icons-install-fonts)))
    
    (use-package rainbow-delimiters
      :straight t
      :hook (prog-mode . rainbow-delimiters-mode))
    
    (set-frame-font "JetBrainsMono Nerd Font" 16 nil t)
    (set-frame-parameter (selected-frame) 'alpha 90)
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
      (add-to-list 'completion-at-point-functions #'cape-ispell)
      (setq-local completion-at-point-functions
                  (list (cape-capf-super
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
    ;;embark_end
    
    ;;window_begin
    (use-package ace-window
      :straight t
      :init
      (global-set-key (kbd "M-q") 'aw-show-dispatch-help)
      (setq aw-dispatch-always t))
    ;; :custom-face
    ;;   '(aw-leading-char-face
    ;;     :foreground "white" :background "red"
    ;;     :weight bold :height 5 :box (:line-width 10 :color "red")))
    ;;window_end
    
    ;;usefulanchors_begin
    ;; From @alphapapa's unpackaged repo https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors
    (use-package ox
      :config
      (define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
        "Attempt to export Org as HTML with useful link IDs.
    Instead of random IDs like \"#orga1b2c3\", use heading titles,
    made unique when necessary."
        :global t
        (if unpackaged/org-export-html-with-useful-ids-mode
            (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)
          (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))
    
      (defun unpackaged/org-export-get-reference (datum info)
        "Like `org-export-get-reference', except uses heading titles instead of random numbers."
        (let ((cache (plist-get info :internal-references)))
          (or (car (rassq datum cache))
              (let* ((crossrefs (plist-get info :crossrefs))
                     (cells (org-export-search-cells datum))
                     ;; Preserve any pre-existing association between
                     ;; a search cell and a reference, i.e., when some
                     ;; previously published document referenced a location
                     ;; within current file (see
                     ;; `org-publish-resolve-external-link').
                     ;;
                     ;; However, there is no guarantee that search cells are
                     ;; unique, e.g., there might be duplicate custom ID or
                     ;; two headings with the same title in the file.
                     ;;
                     ;; As a consequence, before re-using any reference to
                     ;; an element or object, we check that it doesn't refer
                     ;; to a previous element or object.
                     (new (or (cl-some
                               (lambda (cell)
                                 (let ((stored (cdr (assoc cell crossrefs))))
                                   (when stored
                                     (let ((old (org-export-format-reference stored)))
                                       (and (not (assoc old cache)) stored)))))
                               cells)
                              (when (org-element-property :raw-value datum)
                                ;; Heading with a title
                                (unpackaged/org-export-new-title-reference datum cache))
                              ;; NOTE: This probably breaks some Org Export
                              ;; feature, but if it does what I need, fine.
                              (org-export-format-reference
                               (org-export-new-reference cache))))
                     (reference-string new))
                ;; Cache contains both data already associated to
                ;; a reference and in-use internal references, so as to make
                ;; unique references.
                (dolist (cell cells) (push (cons cell new) cache))
                ;; Retain a direct association between reference string and
                ;; DATUM since (1) not every object or element can be given
                ;; a search cell (2) it permits quick lookup.
                (push (cons reference-string datum) cache)
                (plist-put info :internal-references cache)
                reference-string))))
    
      (defun unpackaged/org-export-new-title-reference (datum cache)
        "Return new reference for DATUM that is unique in CACHE."
        (cl-macrolet ((inc-suffixf (place)
                                   `(progn
                                      (string-match (rx bos
                                                        (minimal-match (group (1+ anything)))
                                                        (optional "--" (group (1+ digit)))
                                                        eos)
                                                    ,place)
                                      ;; HACK: `s1' instead of a gensym.
                                      (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                                 (match-string 2 ,place)))
                                              (suffix (if suffix
                                                          (string-to-number suffix)
                                                        0)))
                                        (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
          (let* ((title (org-element-property :raw-value datum))
                 (ref (replace-regexp-in-string "%.." "-" (url-hexify-string (substring-no-properties title)))) ;replace all encoded characters with dashes
                 (parent (org-element-property :parent datum)))
            (while (--any (equal ref (car it))
                          cache)
              ;; Title not unique: make it so.
              (if parent
                  ;; Append ancestor title.
                  (setf title (concat (org-element-property :raw-value parent)
                                      "--" title)
                        ref (url-hexify-string (substring-no-properties title))
                        parent (org-element-property :parent parent))
                ;; No more ancestors: add and increment a number.
                (inc-suffixf ref)))
            ref))))
    ;;usefulanchors_end
    
    ;;org-transclusion_begin
    (use-package org-transclusion
      :after org
      :straight t
      :init
      (setq org-transclusion-remember-transclusions t))
    ;;org-transclusion_end
    
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
    
    ;;evil-leader_begin
    (evil-leader/set-key
      "." 'find-file
      "," 'consult-buffer
      ";" 'consult-proj
      "c" 'org-capture
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
    ;;evil-leader_end
    
    ;;user-config_begin 
    (load (concat user-emacs-directory "userConfig.el"))
    ;;user-config_end 


<a id="Packages"></a>

# Packages

The rest of the functionality is provided by packages, all of which is managed by straight.


<a id="Configuration-Documentation-Annotation"></a>

## Configuration Documentation/Annotation

As mentioned in the intro, this whole configuration documentation/annotation/"inverse literate" config is made possible by [org-transclusion](https://github.com/nobiot/org-transclusion). The goal of the package is to provide the ability to include text snippets from other text documents without having to copy-paste it and get materials out of sync. With the added support to include code snippets wrapped in begin/end<sub>src</sub> block, it's perfect for "inverse literate" config. Not having to tangle your code makes startup faster and the org file doesn't need to talk about every line/block you have in your config only what you want to highlight.

An important option to set is `org-transclusion-remember-transclusions` so that just the buffer contains the transcluded text and it's not actually saved out to the file on disk. *This is the way* to preserve the pointer/link.

    (use-package org-transclusion
      :after org
      :straight t
      :init
      (setq org-transclusion-remember-transclusions t))
    ;;org-transclusion_end
    
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
    
    ;;evil-leader_begin
    (evil-leader/set-key
      "." 'find-file
      "," 'consult-buffer
      ";" 'consult-proj
      "c" 'org-capture
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
    ;;evil-leader_end
    
    ;;user-config_begin 
    (load (concat user-emacs-directory "userConfig.el"))
    ;;user-config_end 


<a id="Theme"></a>

## Theme

Since I migrated from Doom, I really enjoy the Doom themes, mostly preferring the default `doom-one` theme but I also use the `doom-nord` theme.

    (use-package doom-themes
      :straight t
      :init (load-theme 'doom-one t))
    
    (use-package all-the-icons
      :straight t
      :config
      (unless (require 'all-the-icons nil 'noerror)
        (all-the-icons-install-fonts)))
    
    (use-package rainbow-delimiters
      :straight t
      :hook (prog-mode . rainbow-delimiters-mode))
    
    (set-frame-font "JetBrainsMono Nerd Font" 16 nil t)
    (set-frame-parameter (selected-frame) 'alpha 90)
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
      (add-to-list 'completion-at-point-functions #'cape-ispell)
      (setq-local completion-at-point-functions
                  (list (cape-capf-super
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
    ;;embark_end
    
    ;;window_begin
    (use-package ace-window
      :straight t
      :init
      (global-set-key (kbd "M-q") 'aw-show-dispatch-help)
      (setq aw-dispatch-always t))
    ;; :custom-face
    ;;   '(aw-leading-char-face
    ;;     :foreground "white" :background "red"
    ;;     :weight bold :height 5 :box (:line-width 10 :color "red")))
    ;;window_end
    
    ;;usefulanchors_begin
    ;; From @alphapapa's unpackaged repo https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors
    (use-package ox
      :config
      (define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
        "Attempt to export Org as HTML with useful link IDs.
    Instead of random IDs like \"#orga1b2c3\", use heading titles,
    made unique when necessary."
        :global t
        (if unpackaged/org-export-html-with-useful-ids-mode
            (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)
          (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))
    
      (defun unpackaged/org-export-get-reference (datum info)
        "Like `org-export-get-reference', except uses heading titles instead of random numbers."
        (let ((cache (plist-get info :internal-references)))
          (or (car (rassq datum cache))
              (let* ((crossrefs (plist-get info :crossrefs))
                     (cells (org-export-search-cells datum))
                     ;; Preserve any pre-existing association between
                     ;; a search cell and a reference, i.e., when some
                     ;; previously published document referenced a location
                     ;; within current file (see
                     ;; `org-publish-resolve-external-link').
                     ;;
                     ;; However, there is no guarantee that search cells are
                     ;; unique, e.g., there might be duplicate custom ID or
                     ;; two headings with the same title in the file.
                     ;;
                     ;; As a consequence, before re-using any reference to
                     ;; an element or object, we check that it doesn't refer
                     ;; to a previous element or object.
                     (new (or (cl-some
                               (lambda (cell)
                                 (let ((stored (cdr (assoc cell crossrefs))))
                                   (when stored
                                     (let ((old (org-export-format-reference stored)))
                                       (and (not (assoc old cache)) stored)))))
                               cells)
                              (when (org-element-property :raw-value datum)
                                ;; Heading with a title
                                (unpackaged/org-export-new-title-reference datum cache))
                              ;; NOTE: This probably breaks some Org Export
                              ;; feature, but if it does what I need, fine.
                              (org-export-format-reference
                               (org-export-new-reference cache))))
                     (reference-string new))
                ;; Cache contains both data already associated to
                ;; a reference and in-use internal references, so as to make
                ;; unique references.
                (dolist (cell cells) (push (cons cell new) cache))
                ;; Retain a direct association between reference string and
                ;; DATUM since (1) not every object or element can be given
                ;; a search cell (2) it permits quick lookup.
                (push (cons reference-string datum) cache)
                (plist-put info :internal-references cache)
                reference-string))))
    
      (defun unpackaged/org-export-new-title-reference (datum cache)
        "Return new reference for DATUM that is unique in CACHE."
        (cl-macrolet ((inc-suffixf (place)
                                   `(progn
                                      (string-match (rx bos
                                                        (minimal-match (group (1+ anything)))
                                                        (optional "--" (group (1+ digit)))
                                                        eos)
                                                    ,place)
                                      ;; HACK: `s1' instead of a gensym.
                                      (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                                 (match-string 2 ,place)))
                                              (suffix (if suffix
                                                          (string-to-number suffix)
                                                        0)))
                                        (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
          (let* ((title (org-element-property :raw-value datum))
                 (ref (replace-regexp-in-string "%.." "-" (url-hexify-string (substring-no-properties title)))) ;replace all encoded characters with dashes
                 (parent (org-element-property :parent datum)))
            (while (--any (equal ref (car it))
                          cache)
              ;; Title not unique: make it so.
              (if parent
                  ;; Append ancestor title.
                  (setf title (concat (org-element-property :raw-value parent)
                                      "--" title)
                        ref (url-hexify-string (substring-no-properties title))
                        parent (org-element-property :parent parent))
                ;; No more ancestors: add and increment a number.
                (inc-suffixf ref)))
            ref))))
    ;;usefulanchors_end
    
    ;;org-transclusion_begin
    (use-package org-transclusion
      :after org
      :straight t
      :init
      (setq org-transclusion-remember-transclusions t))
    ;;org-transclusion_end
    
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
    
    ;;evil-leader_begin
    (evil-leader/set-key
      "." 'find-file
      "," 'consult-buffer
      ";" 'consult-proj
      "c" 'org-capture
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
    ;;evil-leader_end
    
    ;;user-config_begin 
    (load (concat user-emacs-directory "userConfig.el"))
    ;;user-config_end 


<a id="Muscle-memory-shortcuts"></a>

## Muscle memory shortcuts

There are some shortcuts that I have lots of muscle memory with and also work in other applications that I find convenient to use in Emacs. I also use the evil-mode keys when in normal mode, based on whatever is most convenient.

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
    ;;cua_end
    
    ;;which-key_begin
    (use-package which-key
      :straight t
      :config
      (which-key-mode)
      :init
      (setq which-key-idle-delay 0.1))
    ;;which-key_end
    
    (use-package magit
      :straight t
      :defer t)
    
    ;;evil_begin
    (use-package evil
      :straight t
      :bind (("<escape>" . keyboard-escape-quit))
      :init
      (setq evil-want-integration t
            evil-want-keybinding nil
            evil-disable-insert-state-bindings t
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
    ;;evil_end
    
    ;;theme_begin
    (use-package doom-themes
      :straight t
      :init (load-theme 'doom-one t))
    
    (use-package all-the-icons
      :straight t
      :config
      (unless (require 'all-the-icons nil 'noerror)
        (all-the-icons-install-fonts)))
    
    (use-package rainbow-delimiters
      :straight t
      :hook (prog-mode . rainbow-delimiters-mode))
    
    (set-frame-font "JetBrainsMono Nerd Font" 16 nil t)
    (set-frame-parameter (selected-frame) 'alpha 90)
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
      (add-to-list 'completion-at-point-functions #'cape-ispell)
      (setq-local completion-at-point-functions
                  (list (cape-capf-super
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
    ;;embark_end
    
    ;;window_begin
    (use-package ace-window
      :straight t
      :init
      (global-set-key (kbd "M-q") 'aw-show-dispatch-help)
      (setq aw-dispatch-always t))
    ;; :custom-face
    ;;   '(aw-leading-char-face
    ;;     :foreground "white" :background "red"
    ;;     :weight bold :height 5 :box (:line-width 10 :color "red")))
    ;;window_end
    
    ;;usefulanchors_begin
    ;; From @alphapapa's unpackaged repo https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors
    (use-package ox
      :config
      (define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
        "Attempt to export Org as HTML with useful link IDs.
    Instead of random IDs like \"#orga1b2c3\", use heading titles,
    made unique when necessary."
        :global t
        (if unpackaged/org-export-html-with-useful-ids-mode
            (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)
          (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))
    
      (defun unpackaged/org-export-get-reference (datum info)
        "Like `org-export-get-reference', except uses heading titles instead of random numbers."
        (let ((cache (plist-get info :internal-references)))
          (or (car (rassq datum cache))
              (let* ((crossrefs (plist-get info :crossrefs))
                     (cells (org-export-search-cells datum))
                     ;; Preserve any pre-existing association between
                     ;; a search cell and a reference, i.e., when some
                     ;; previously published document referenced a location
                     ;; within current file (see
                     ;; `org-publish-resolve-external-link').
                     ;;
                     ;; However, there is no guarantee that search cells are
                     ;; unique, e.g., there might be duplicate custom ID or
                     ;; two headings with the same title in the file.
                     ;;
                     ;; As a consequence, before re-using any reference to
                     ;; an element or object, we check that it doesn't refer
                     ;; to a previous element or object.
                     (new (or (cl-some
                               (lambda (cell)
                                 (let ((stored (cdr (assoc cell crossrefs))))
                                   (when stored
                                     (let ((old (org-export-format-reference stored)))
                                       (and (not (assoc old cache)) stored)))))
                               cells)
                              (when (org-element-property :raw-value datum)
                                ;; Heading with a title
                                (unpackaged/org-export-new-title-reference datum cache))
                              ;; NOTE: This probably breaks some Org Export
                              ;; feature, but if it does what I need, fine.
                              (org-export-format-reference
                               (org-export-new-reference cache))))
                     (reference-string new))
                ;; Cache contains both data already associated to
                ;; a reference and in-use internal references, so as to make
                ;; unique references.
                (dolist (cell cells) (push (cons cell new) cache))
                ;; Retain a direct association between reference string and
                ;; DATUM since (1) not every object or element can be given
                ;; a search cell (2) it permits quick lookup.
                (push (cons reference-string datum) cache)
                (plist-put info :internal-references cache)
                reference-string))))
    
      (defun unpackaged/org-export-new-title-reference (datum cache)
        "Return new reference for DATUM that is unique in CACHE."
        (cl-macrolet ((inc-suffixf (place)
                                   `(progn
                                      (string-match (rx bos
                                                        (minimal-match (group (1+ anything)))
                                                        (optional "--" (group (1+ digit)))
                                                        eos)
                                                    ,place)
                                      ;; HACK: `s1' instead of a gensym.
                                      (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                                 (match-string 2 ,place)))
                                              (suffix (if suffix
                                                          (string-to-number suffix)
                                                        0)))
                                        (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
          (let* ((title (org-element-property :raw-value datum))
                 (ref (replace-regexp-in-string "%.." "-" (url-hexify-string (substring-no-properties title)))) ;replace all encoded characters with dashes
                 (parent (org-element-property :parent datum)))
            (while (--any (equal ref (car it))
                          cache)
              ;; Title not unique: make it so.
              (if parent
                  ;; Append ancestor title.
                  (setf title (concat (org-element-property :raw-value parent)
                                      "--" title)
                        ref (url-hexify-string (substring-no-properties title))
                        parent (org-element-property :parent parent))
                ;; No more ancestors: add and increment a number.
                (inc-suffixf ref)))
            ref))))
    ;;usefulanchors_end
    
    ;;org-transclusion_begin
    (use-package org-transclusion
      :after org
      :straight t
      :init
      (setq org-transclusion-remember-transclusions t))
    ;;org-transclusion_end
    
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
    
    ;;evil-leader_begin
    (evil-leader/set-key
      "." 'find-file
      "," 'consult-buffer
      ";" 'consult-proj
      "c" 'org-capture
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
    ;;evil-leader_end
    
    ;;user-config_begin 
    (load (concat user-emacs-directory "userConfig.el"))
    ;;user-config_end 


<a id="Keybindings"></a>

## Keybindings

For all the keys I don't have muscle memory for, there's `which-key`. It progressively shows the keybindings that are available in that mode.

    (use-package which-key
      :straight t
      :config
      (which-key-mode)
      :init
      (setq which-key-idle-delay 0.1))
    ;;which-key_end
    
    (use-package magit
      :straight t
      :defer t)
    
    ;;evil_begin
    (use-package evil
      :straight t
      :bind (("<escape>" . keyboard-escape-quit))
      :init
      (setq evil-want-integration t
            evil-want-keybinding nil
            evil-disable-insert-state-bindings t
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
    ;;evil_end
    
    ;;theme_begin
    (use-package doom-themes
      :straight t
      :init (load-theme 'doom-one t))
    
    (use-package all-the-icons
      :straight t
      :config
      (unless (require 'all-the-icons nil 'noerror)
        (all-the-icons-install-fonts)))
    
    (use-package rainbow-delimiters
      :straight t
      :hook (prog-mode . rainbow-delimiters-mode))
    
    (set-frame-font "JetBrainsMono Nerd Font" 16 nil t)
    (set-frame-parameter (selected-frame) 'alpha 90)
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
      (add-to-list 'completion-at-point-functions #'cape-ispell)
      (setq-local completion-at-point-functions
                  (list (cape-capf-super
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
    ;;embark_end
    
    ;;window_begin
    (use-package ace-window
      :straight t
      :init
      (global-set-key (kbd "M-q") 'aw-show-dispatch-help)
      (setq aw-dispatch-always t))
    ;; :custom-face
    ;;   '(aw-leading-char-face
    ;;     :foreground "white" :background "red"
    ;;     :weight bold :height 5 :box (:line-width 10 :color "red")))
    ;;window_end
    
    ;;usefulanchors_begin
    ;; From @alphapapa's unpackaged repo https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors
    (use-package ox
      :config
      (define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
        "Attempt to export Org as HTML with useful link IDs.
    Instead of random IDs like \"#orga1b2c3\", use heading titles,
    made unique when necessary."
        :global t
        (if unpackaged/org-export-html-with-useful-ids-mode
            (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)
          (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))
    
      (defun unpackaged/org-export-get-reference (datum info)
        "Like `org-export-get-reference', except uses heading titles instead of random numbers."
        (let ((cache (plist-get info :internal-references)))
          (or (car (rassq datum cache))
              (let* ((crossrefs (plist-get info :crossrefs))
                     (cells (org-export-search-cells datum))
                     ;; Preserve any pre-existing association between
                     ;; a search cell and a reference, i.e., when some
                     ;; previously published document referenced a location
                     ;; within current file (see
                     ;; `org-publish-resolve-external-link').
                     ;;
                     ;; However, there is no guarantee that search cells are
                     ;; unique, e.g., there might be duplicate custom ID or
                     ;; two headings with the same title in the file.
                     ;;
                     ;; As a consequence, before re-using any reference to
                     ;; an element or object, we check that it doesn't refer
                     ;; to a previous element or object.
                     (new (or (cl-some
                               (lambda (cell)
                                 (let ((stored (cdr (assoc cell crossrefs))))
                                   (when stored
                                     (let ((old (org-export-format-reference stored)))
                                       (and (not (assoc old cache)) stored)))))
                               cells)
                              (when (org-element-property :raw-value datum)
                                ;; Heading with a title
                                (unpackaged/org-export-new-title-reference datum cache))
                              ;; NOTE: This probably breaks some Org Export
                              ;; feature, but if it does what I need, fine.
                              (org-export-format-reference
                               (org-export-new-reference cache))))
                     (reference-string new))
                ;; Cache contains both data already associated to
                ;; a reference and in-use internal references, so as to make
                ;; unique references.
                (dolist (cell cells) (push (cons cell new) cache))
                ;; Retain a direct association between reference string and
                ;; DATUM since (1) not every object or element can be given
                ;; a search cell (2) it permits quick lookup.
                (push (cons reference-string datum) cache)
                (plist-put info :internal-references cache)
                reference-string))))
    
      (defun unpackaged/org-export-new-title-reference (datum cache)
        "Return new reference for DATUM that is unique in CACHE."
        (cl-macrolet ((inc-suffixf (place)
                                   `(progn
                                      (string-match (rx bos
                                                        (minimal-match (group (1+ anything)))
                                                        (optional "--" (group (1+ digit)))
                                                        eos)
                                                    ,place)
                                      ;; HACK: `s1' instead of a gensym.
                                      (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                                 (match-string 2 ,place)))
                                              (suffix (if suffix
                                                          (string-to-number suffix)
                                                        0)))
                                        (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
          (let* ((title (org-element-property :raw-value datum))
                 (ref (replace-regexp-in-string "%.." "-" (url-hexify-string (substring-no-properties title)))) ;replace all encoded characters with dashes
                 (parent (org-element-property :parent datum)))
            (while (--any (equal ref (car it))
                          cache)
              ;; Title not unique: make it so.
              (if parent
                  ;; Append ancestor title.
                  (setf title (concat (org-element-property :raw-value parent)
                                      "--" title)
                        ref (url-hexify-string (substring-no-properties title))
                        parent (org-element-property :parent parent))
                ;; No more ancestors: add and increment a number.
                (inc-suffixf ref)))
            ref))))
    ;;usefulanchors_end
    
    ;;org-transclusion_begin
    (use-package org-transclusion
      :after org
      :straight t
      :init
      (setq org-transclusion-remember-transclusions t))
    ;;org-transclusion_end
    
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
    
    ;;evil-leader_begin
    (evil-leader/set-key
      "." 'find-file
      "," 'consult-buffer
      ";" 'consult-proj
      "c" 'org-capture
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
    ;;evil-leader_end
    
    ;;user-config_begin 
    (load (concat user-emacs-directory "userConfig.el"))
    ;;user-config_end 


<a id="Evil-Mode"></a>

## Evil-Mode

Extensible VI Layer (evil) mode for Emacs provides vi editing modes and keybindings. `evil-collection` provides all the keybindings in different modes so they don't have to be configured individually. Since the individual bindings (minor-modes?) are not loaded until the package is used, it's not bloating the in use configuration.

    (use-package evil
      :straight t
      :bind (("<escape>" . keyboard-escape-quit))
      :init
      (setq evil-want-integration t
            evil-want-keybinding nil
            evil-disable-insert-state-bindings t
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
    ;;evil_end
    
    ;;theme_begin
    (use-package doom-themes
      :straight t
      :init (load-theme 'doom-one t))
    
    (use-package all-the-icons
      :straight t
      :config
      (unless (require 'all-the-icons nil 'noerror)
        (all-the-icons-install-fonts)))
    
    (use-package rainbow-delimiters
      :straight t
      :hook (prog-mode . rainbow-delimiters-mode))
    
    (set-frame-font "JetBrainsMono Nerd Font" 16 nil t)
    (set-frame-parameter (selected-frame) 'alpha 90)
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
      (add-to-list 'completion-at-point-functions #'cape-ispell)
      (setq-local completion-at-point-functions
                  (list (cape-capf-super
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
    ;;embark_end
    
    ;;window_begin
    (use-package ace-window
      :straight t
      :init
      (global-set-key (kbd "M-q") 'aw-show-dispatch-help)
      (setq aw-dispatch-always t))
    ;; :custom-face
    ;;   '(aw-leading-char-face
    ;;     :foreground "white" :background "red"
    ;;     :weight bold :height 5 :box (:line-width 10 :color "red")))
    ;;window_end
    
    ;;usefulanchors_begin
    ;; From @alphapapa's unpackaged repo https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors
    (use-package ox
      :config
      (define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
        "Attempt to export Org as HTML with useful link IDs.
    Instead of random IDs like \"#orga1b2c3\", use heading titles,
    made unique when necessary."
        :global t
        (if unpackaged/org-export-html-with-useful-ids-mode
            (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)
          (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))
    
      (defun unpackaged/org-export-get-reference (datum info)
        "Like `org-export-get-reference', except uses heading titles instead of random numbers."
        (let ((cache (plist-get info :internal-references)))
          (or (car (rassq datum cache))
              (let* ((crossrefs (plist-get info :crossrefs))
                     (cells (org-export-search-cells datum))
                     ;; Preserve any pre-existing association between
                     ;; a search cell and a reference, i.e., when some
                     ;; previously published document referenced a location
                     ;; within current file (see
                     ;; `org-publish-resolve-external-link').
                     ;;
                     ;; However, there is no guarantee that search cells are
                     ;; unique, e.g., there might be duplicate custom ID or
                     ;; two headings with the same title in the file.
                     ;;
                     ;; As a consequence, before re-using any reference to
                     ;; an element or object, we check that it doesn't refer
                     ;; to a previous element or object.
                     (new (or (cl-some
                               (lambda (cell)
                                 (let ((stored (cdr (assoc cell crossrefs))))
                                   (when stored
                                     (let ((old (org-export-format-reference stored)))
                                       (and (not (assoc old cache)) stored)))))
                               cells)
                              (when (org-element-property :raw-value datum)
                                ;; Heading with a title
                                (unpackaged/org-export-new-title-reference datum cache))
                              ;; NOTE: This probably breaks some Org Export
                              ;; feature, but if it does what I need, fine.
                              (org-export-format-reference
                               (org-export-new-reference cache))))
                     (reference-string new))
                ;; Cache contains both data already associated to
                ;; a reference and in-use internal references, so as to make
                ;; unique references.
                (dolist (cell cells) (push (cons cell new) cache))
                ;; Retain a direct association between reference string and
                ;; DATUM since (1) not every object or element can be given
                ;; a search cell (2) it permits quick lookup.
                (push (cons reference-string datum) cache)
                (plist-put info :internal-references cache)
                reference-string))))
    
      (defun unpackaged/org-export-new-title-reference (datum cache)
        "Return new reference for DATUM that is unique in CACHE."
        (cl-macrolet ((inc-suffixf (place)
                                   `(progn
                                      (string-match (rx bos
                                                        (minimal-match (group (1+ anything)))
                                                        (optional "--" (group (1+ digit)))
                                                        eos)
                                                    ,place)
                                      ;; HACK: `s1' instead of a gensym.
                                      (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                                 (match-string 2 ,place)))
                                              (suffix (if suffix
                                                          (string-to-number suffix)
                                                        0)))
                                        (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
          (let* ((title (org-element-property :raw-value datum))
                 (ref (replace-regexp-in-string "%.." "-" (url-hexify-string (substring-no-properties title)))) ;replace all encoded characters with dashes
                 (parent (org-element-property :parent datum)))
            (while (--any (equal ref (car it))
                          cache)
              ;; Title not unique: make it so.
              (if parent
                  ;; Append ancestor title.
                  (setf title (concat (org-element-property :raw-value parent)
                                      "--" title)
                        ref (url-hexify-string (substring-no-properties title))
                        parent (org-element-property :parent parent))
                ;; No more ancestors: add and increment a number.
                (inc-suffixf ref)))
            ref))))
    ;;usefulanchors_end
    
    ;;org-transclusion_begin
    (use-package org-transclusion
      :after org
      :straight t
      :init
      (setq org-transclusion-remember-transclusions t))
    ;;org-transclusion_end
    
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
    
    ;;evil-leader_begin
    (evil-leader/set-key
      "." 'find-file
      "," 'consult-buffer
      ";" 'consult-proj
      "c" 'org-capture
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
    ;;evil-leader_end
    
    ;;user-config_begin 
    (load (concat user-emacs-directory "userConfig.el"))
    ;;user-config_end 

However, there are some keybindings I want to have available everywhere and use the `evil-leader` to configure those.

    (evil-leader/set-key
      "." 'find-file
      "," 'consult-buffer
      ";" 'consult-proj
      "c" 'org-capture
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
    ;;evil-leader_end
    
    ;;user-config_begin 
    (load (concat user-emacs-directory "userConfig.el"))
    ;;user-config_end 


<a id="History"></a>

## History

These packages give Emacs memory so the frequent and recent things are near the top when the completion related packages get used.

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
      (add-to-list 'completion-at-point-functions #'cape-ispell)
      (setq-local completion-at-point-functions
                  (list (cape-capf-super
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
    ;;embark_end
    
    ;;window_begin
    (use-package ace-window
      :straight t
      :init
      (global-set-key (kbd "M-q") 'aw-show-dispatch-help)
      (setq aw-dispatch-always t))
    ;; :custom-face
    ;;   '(aw-leading-char-face
    ;;     :foreground "white" :background "red"
    ;;     :weight bold :height 5 :box (:line-width 10 :color "red")))
    ;;window_end
    
    ;;usefulanchors_begin
    ;; From @alphapapa's unpackaged repo https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors
    (use-package ox
      :config
      (define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
        "Attempt to export Org as HTML with useful link IDs.
    Instead of random IDs like \"#orga1b2c3\", use heading titles,
    made unique when necessary."
        :global t
        (if unpackaged/org-export-html-with-useful-ids-mode
            (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)
          (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))
    
      (defun unpackaged/org-export-get-reference (datum info)
        "Like `org-export-get-reference', except uses heading titles instead of random numbers."
        (let ((cache (plist-get info :internal-references)))
          (or (car (rassq datum cache))
              (let* ((crossrefs (plist-get info :crossrefs))
                     (cells (org-export-search-cells datum))
                     ;; Preserve any pre-existing association between
                     ;; a search cell and a reference, i.e., when some
                     ;; previously published document referenced a location
                     ;; within current file (see
                     ;; `org-publish-resolve-external-link').
                     ;;
                     ;; However, there is no guarantee that search cells are
                     ;; unique, e.g., there might be duplicate custom ID or
                     ;; two headings with the same title in the file.
                     ;;
                     ;; As a consequence, before re-using any reference to
                     ;; an element or object, we check that it doesn't refer
                     ;; to a previous element or object.
                     (new (or (cl-some
                               (lambda (cell)
                                 (let ((stored (cdr (assoc cell crossrefs))))
                                   (when stored
                                     (let ((old (org-export-format-reference stored)))
                                       (and (not (assoc old cache)) stored)))))
                               cells)
                              (when (org-element-property :raw-value datum)
                                ;; Heading with a title
                                (unpackaged/org-export-new-title-reference datum cache))
                              ;; NOTE: This probably breaks some Org Export
                              ;; feature, but if it does what I need, fine.
                              (org-export-format-reference
                               (org-export-new-reference cache))))
                     (reference-string new))
                ;; Cache contains both data already associated to
                ;; a reference and in-use internal references, so as to make
                ;; unique references.
                (dolist (cell cells) (push (cons cell new) cache))
                ;; Retain a direct association between reference string and
                ;; DATUM since (1) not every object or element can be given
                ;; a search cell (2) it permits quick lookup.
                (push (cons reference-string datum) cache)
                (plist-put info :internal-references cache)
                reference-string))))
    
      (defun unpackaged/org-export-new-title-reference (datum cache)
        "Return new reference for DATUM that is unique in CACHE."
        (cl-macrolet ((inc-suffixf (place)
                                   `(progn
                                      (string-match (rx bos
                                                        (minimal-match (group (1+ anything)))
                                                        (optional "--" (group (1+ digit)))
                                                        eos)
                                                    ,place)
                                      ;; HACK: `s1' instead of a gensym.
                                      (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                                 (match-string 2 ,place)))
                                              (suffix (if suffix
                                                          (string-to-number suffix)
                                                        0)))
                                        (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
          (let* ((title (org-element-property :raw-value datum))
                 (ref (replace-regexp-in-string "%.." "-" (url-hexify-string (substring-no-properties title)))) ;replace all encoded characters with dashes
                 (parent (org-element-property :parent datum)))
            (while (--any (equal ref (car it))
                          cache)
              ;; Title not unique: make it so.
              (if parent
                  ;; Append ancestor title.
                  (setf title (concat (org-element-property :raw-value parent)
                                      "--" title)
                        ref (url-hexify-string (substring-no-properties title))
                        parent (org-element-property :parent parent))
                ;; No more ancestors: add and increment a number.
                (inc-suffixf ref)))
            ref))))
    ;;usefulanchors_end
    
    ;;org-transclusion_begin
    (use-package org-transclusion
      :after org
      :straight t
      :init
      (setq org-transclusion-remember-transclusions t))
    ;;org-transclusion_end
    
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
    
    ;;evil-leader_begin
    (evil-leader/set-key
      "." 'find-file
      "," 'consult-buffer
      ";" 'consult-proj
      "c" 'org-capture
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
    ;;evil-leader_end
    
    ;;user-config_begin 
    (load (concat user-emacs-directory "userConfig.el"))
    ;;user-config_end 


<a id="Completions"></a>

## Completions

All the things that help with completion in various contexts are in this section, they deserve elaboration (at a later time).
`Note`: The current completion strategy is very much a work in progress. However, vertico, corfu, cape, and friends seems to be the front-runners. 

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
      (add-to-list 'completion-at-point-functions #'cape-ispell)
      (setq-local completion-at-point-functions
                  (list (cape-capf-super
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
    ;;embark_end
    
    ;;window_begin
    (use-package ace-window
      :straight t
      :init
      (global-set-key (kbd "M-q") 'aw-show-dispatch-help)
      (setq aw-dispatch-always t))
    ;; :custom-face
    ;;   '(aw-leading-char-face
    ;;     :foreground "white" :background "red"
    ;;     :weight bold :height 5 :box (:line-width 10 :color "red")))
    ;;window_end
    
    ;;usefulanchors_begin
    ;; From @alphapapa's unpackaged repo https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors
    (use-package ox
      :config
      (define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
        "Attempt to export Org as HTML with useful link IDs.
    Instead of random IDs like \"#orga1b2c3\", use heading titles,
    made unique when necessary."
        :global t
        (if unpackaged/org-export-html-with-useful-ids-mode
            (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)
          (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))
    
      (defun unpackaged/org-export-get-reference (datum info)
        "Like `org-export-get-reference', except uses heading titles instead of random numbers."
        (let ((cache (plist-get info :internal-references)))
          (or (car (rassq datum cache))
              (let* ((crossrefs (plist-get info :crossrefs))
                     (cells (org-export-search-cells datum))
                     ;; Preserve any pre-existing association between
                     ;; a search cell and a reference, i.e., when some
                     ;; previously published document referenced a location
                     ;; within current file (see
                     ;; `org-publish-resolve-external-link').
                     ;;
                     ;; However, there is no guarantee that search cells are
                     ;; unique, e.g., there might be duplicate custom ID or
                     ;; two headings with the same title in the file.
                     ;;
                     ;; As a consequence, before re-using any reference to
                     ;; an element or object, we check that it doesn't refer
                     ;; to a previous element or object.
                     (new (or (cl-some
                               (lambda (cell)
                                 (let ((stored (cdr (assoc cell crossrefs))))
                                   (when stored
                                     (let ((old (org-export-format-reference stored)))
                                       (and (not (assoc old cache)) stored)))))
                               cells)
                              (when (org-element-property :raw-value datum)
                                ;; Heading with a title
                                (unpackaged/org-export-new-title-reference datum cache))
                              ;; NOTE: This probably breaks some Org Export
                              ;; feature, but if it does what I need, fine.
                              (org-export-format-reference
                               (org-export-new-reference cache))))
                     (reference-string new))
                ;; Cache contains both data already associated to
                ;; a reference and in-use internal references, so as to make
                ;; unique references.
                (dolist (cell cells) (push (cons cell new) cache))
                ;; Retain a direct association between reference string and
                ;; DATUM since (1) not every object or element can be given
                ;; a search cell (2) it permits quick lookup.
                (push (cons reference-string datum) cache)
                (plist-put info :internal-references cache)
                reference-string))))
    
      (defun unpackaged/org-export-new-title-reference (datum cache)
        "Return new reference for DATUM that is unique in CACHE."
        (cl-macrolet ((inc-suffixf (place)
                                   `(progn
                                      (string-match (rx bos
                                                        (minimal-match (group (1+ anything)))
                                                        (optional "--" (group (1+ digit)))
                                                        eos)
                                                    ,place)
                                      ;; HACK: `s1' instead of a gensym.
                                      (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                                 (match-string 2 ,place)))
                                              (suffix (if suffix
                                                          (string-to-number suffix)
                                                        0)))
                                        (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
          (let* ((title (org-element-property :raw-value datum))
                 (ref (replace-regexp-in-string "%.." "-" (url-hexify-string (substring-no-properties title)))) ;replace all encoded characters with dashes
                 (parent (org-element-property :parent datum)))
            (while (--any (equal ref (car it))
                          cache)
              ;; Title not unique: make it so.
              (if parent
                  ;; Append ancestor title.
                  (setf title (concat (org-element-property :raw-value parent)
                                      "--" title)
                        ref (url-hexify-string (substring-no-properties title))
                        parent (org-element-property :parent parent))
                ;; No more ancestors: add and increment a number.
                (inc-suffixf ref)))
            ref))))
    ;;usefulanchors_end
    
    ;;org-transclusion_begin
    (use-package org-transclusion
      :after org
      :straight t
      :init
      (setq org-transclusion-remember-transclusions t))
    ;;org-transclusion_end
    
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
    
    ;;evil-leader_begin
    (evil-leader/set-key
      "." 'find-file
      "," 'consult-buffer
      ";" 'consult-proj
      "c" 'org-capture
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
    ;;evil-leader_end
    
    ;;user-config_begin 
    (load (concat user-emacs-directory "userConfig.el"))
    ;;user-config_end 


<a id="User-Config"></a>

## User Config

There are a few packages and specific configuration that is quite specific to my workflow so they're in a separate file and not transcluded here. If anyone wants to use this `init.el` file, this section needs to be removed.

    (load (concat user-emacs-directory "userConfig.el"))
    ;;user-config_end 


<a id="Repo-meta"></a>

# Repo meta


<a id="Useful-anchors"></a>

## Useful anchors

By default `org-export` regenerates ids for all the headings which creates noise in the git commit history and also in-page anchors can't be reliably linked to a specific part of the document (independent of the git forge's markdown parsing implementation). Using a snippet of [@alphapapa](https://github.com/alphapapa)'s unpackaged configuration, we can advice the export to create unique anchors that won't change between exports (unless the headings themselves have been changed). Replacing space (%20) with dash (-) because Github doesn't parse that correctly.

    ;; From @alphapapa's unpackaged repo https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors
    (use-package ox
      :config
      (define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
        "Attempt to export Org as HTML with useful link IDs.
    Instead of random IDs like \"#orga1b2c3\", use heading titles,
    made unique when necessary."
        :global t
        (if unpackaged/org-export-html-with-useful-ids-mode
            (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)
          (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))
    
      (defun unpackaged/org-export-get-reference (datum info)
        "Like `org-export-get-reference', except uses heading titles instead of random numbers."
        (let ((cache (plist-get info :internal-references)))
          (or (car (rassq datum cache))
              (let* ((crossrefs (plist-get info :crossrefs))
                     (cells (org-export-search-cells datum))
                     ;; Preserve any pre-existing association between
                     ;; a search cell and a reference, i.e., when some
                     ;; previously published document referenced a location
                     ;; within current file (see
                     ;; `org-publish-resolve-external-link').
                     ;;
                     ;; However, there is no guarantee that search cells are
                     ;; unique, e.g., there might be duplicate custom ID or
                     ;; two headings with the same title in the file.
                     ;;
                     ;; As a consequence, before re-using any reference to
                     ;; an element or object, we check that it doesn't refer
                     ;; to a previous element or object.
                     (new (or (cl-some
                               (lambda (cell)
                                 (let ((stored (cdr (assoc cell crossrefs))))
                                   (when stored
                                     (let ((old (org-export-format-reference stored)))
                                       (and (not (assoc old cache)) stored)))))
                               cells)
                              (when (org-element-property :raw-value datum)
                                ;; Heading with a title
                                (unpackaged/org-export-new-title-reference datum cache))
                              ;; NOTE: This probably breaks some Org Export
                              ;; feature, but if it does what I need, fine.
                              (org-export-format-reference
                               (org-export-new-reference cache))))
                     (reference-string new))
                ;; Cache contains both data already associated to
                ;; a reference and in-use internal references, so as to make
                ;; unique references.
                (dolist (cell cells) (push (cons cell new) cache))
                ;; Retain a direct association between reference string and
                ;; DATUM since (1) not every object or element can be given
                ;; a search cell (2) it permits quick lookup.
                (push (cons reference-string datum) cache)
                (plist-put info :internal-references cache)
                reference-string))))
    
      (defun unpackaged/org-export-new-title-reference (datum cache)
        "Return new reference for DATUM that is unique in CACHE."
        (cl-macrolet ((inc-suffixf (place)
                                   `(progn
                                      (string-match (rx bos
                                                        (minimal-match (group (1+ anything)))
                                                        (optional "--" (group (1+ digit)))
                                                        eos)
                                                    ,place)
                                      ;; HACK: `s1' instead of a gensym.
                                      (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                                 (match-string 2 ,place)))
                                              (suffix (if suffix
                                                          (string-to-number suffix)
                                                        0)))
                                        (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
          (let* ((title (org-element-property :raw-value datum))
                 (ref (replace-regexp-in-string "%.." "-" (url-hexify-string (substring-no-properties title)))) ;replace all encoded characters with dashes
                 (parent (org-element-property :parent datum)))
            (while (--any (equal ref (car it))
                          cache)
              ;; Title not unique: make it so.
              (if parent
                  ;; Append ancestor title.
                  (setf title (concat (org-element-property :raw-value parent)
                                      "--" title)
                        ref (url-hexify-string (substring-no-properties title))
                        parent (org-element-property :parent parent))
                ;; No more ancestors: add and increment a number.
                (inc-suffixf ref)))
            ref))))
    ;;usefulanchors_end
    
    ;;org-transclusion_begin
    (use-package org-transclusion
      :after org
      :straight t
      :init
      (setq org-transclusion-remember-transclusions t))
    ;;org-transclusion_end
    
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
    
    ;;evil-leader_begin
    (evil-leader/set-key
      "." 'find-file
      "," 'consult-buffer
      ";" 'consult-proj
      "c" 'org-capture
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
    ;;evil-leader_end
    
    ;;user-config_begin 
    (load (concat user-emacs-directory "userConfig.el"))
    ;;user-config_end 


<a id="Exporting-Readme-markdown"></a>

## Exporting Readme markdown

Since I want to keep the org original with the transclusion blocks and Markdown is supported better by more forges, I want to export this config file as a README.md automatically each time I save.

    (if (file-exists-p "./README.md")
        (delete-file "./README.md"))
    (message "Turn transclusion On!")
    (org-transclusion-add-all)
    (message "Export")
    (unpackaged/org-export-html-with-useful-ids-mode)
    (org-md-export-to-markdown)
    (message "Transclusion Off!")
    (org-transclusion-deactivate)
    (message "Reverting buffer to avoid issues with org-babel")
    (interactive) (revert-buffer t t)


<a id="Remote-as--mirrors-"></a>

## Remote as "mirrors"

Sourcehut is the primary location of this repo with "mirror" on GitHub. But instead of relying on GitHub actions to mirror and manage a separate workflow on the secondary platform, I'm taking shortcut by adding a second push-url to my repo's main remote (origin) so whenever there's a push, it pushes to both Sourcehut and GitHub. When `git remote set-url --push origin --add` is called, it doesn't append to the existing list, it replaces so the primary url also needs to be added.

    git remote set-url --push origin --add git@git.sr.ht:~shom/.emacs.custom
    git remote set-url --push origin --add git@github.com:shombando/.emacs.custom.git

