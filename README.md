- [Startup](#Startup)
  - [User Emacs Directory](#User-Emacs-Directory)
  - [Package Management](#Package-Management)
  - [Visual elements](#Visual-elements)
  - [Early Init](#Early-Init)
- [Emacs Behavior](#Emacs-Behavior)
  - [Spellcheck](#Spellcheck)
  - [Dired](#Dired)
  - [No-littering](#No-littering)
  - [Misc](#Misc)
- [Packages](#Packages)
  - [Version Control](#Version-Control)
  - [Configuration Documentation/Annotation](#Configuration-Documentation-Annotation)
  - [Theme](#Theme)
  - [Muscle memory shortcuts](#Muscle-memory-shortcuts)
  - [Keybindings](#Keybindings)
  - [Evil-Mode](#Evil-Mode)
  - [History](#History)
  - [Completions](#Completions)
- [User Config](#User-Config)
  - [Flycheck](#Flycheck)
  - [Ox-hugo](#Ox-hugo)
  - [Email](#Email)
  - [Yasnippet](#Yasnippet)
- [Repo meta](#Repo-meta)
  - [Useful anchors](#Useful-anchors)
  - [Exporting Readme markdown](#Exporting-Readme-markdown)
  - [Remote as "mirrors"](#Remote-as--mirrors-)

This is my custom config based on my [Emacs journey](https://shom.dev/posts/20211121_emacs-custom-configuration/). I wanted to create a literate config but did not want to slowdown startup with tangling, so currently I'm trying to get "the best of both worlds" via `org-transclusion`. The file `config.org` in this repo contains the "source" and `org-transclusion` directives and is rendered out to `README.md` (markdown is better supported for auto-rendering by more forges currently). I'll eventually automate this process, likely through a git-hook, currently it is rendered every time this file is saved using org's `after-save-hook`. However, the rendered output is never guaranteed to include all of my config, just the sections that have been manually commented, `init.el` and includes will remain the source of truth.


<a id="Startup"></a>

# Startup

I would like to have the option to have several Emacs "distributions" or configurations that are independent of each other. This allows for experimentation without breaking things (git helps with the actual config but this allows for the packages to be independent). I'm using Chemacs2 but that's outside the scope of the config, for now.


<a id="User-Emacs-Directory"></a>

## User Emacs Directory

I've chosen to keep my custom config and all related packages, etc. in this specific location so that's the first thing in the config.

```emacs-lisp
(setq user-emacs-directory "~/.emacs/.custom/")
```


<a id="Package-Management"></a>

## Package Management

I'm using `straight.el` as a package manager with the `use-package` syntax. Straight is bootstrapped, this ensures that a clean install of Emacs can get up and running without any manual intervention just by cloning this repo.

```emacs-lisp
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
```


<a id="Visual-elements"></a>

## Visual elements

The Emacs GUI is a bit dated, especially the toolbar and menu bar. Also since I'll be customizing my keymaps they're going to be mostly superfluous and take up real-estate. There are a few other preferences relative to how things are displayed here, the rest of the visuals are configured by the theme.

```emacs-lisp
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
```

The modeline is another important visual element to show important information about the buffer and modes. Doom's modeline is a significant visual upgrade to the default.

In addition to the modeline at the bottom, I want a nice bar at the top for tabs with a mouse clickable Emacs menu button as well (as a backup to keyboard navigation).

```emacs-lisp
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
```

A few more visual decorations and nice to have only make sense/only supported on a full display. So they're not loaded when Emacs in running in Termux on Android.

```emacs-lisp
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
```


<a id="Early-Init"></a>

## Early Init

I'm not proficient in understanding the underpinnings of early init type optimizations so I'm borrowing from [Doom](https://github.com/hlissner/doom-emacs/) and [System Crafters](https://systemcrafters.net). here.

```emacs-lisp
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

```


<a id="Emacs-Behavior"></a>

# Emacs Behavior

A lot the configuration revolves around packages but there are some built-in settings and behavior within Emacs that also need to be tweaked.


<a id="Spellcheck"></a>

## Spellcheck

Spellchecking while typing is useful and I want to use it everywhere but in programming modes only comments should be spell checked not the whole buffer.

```emacs-lisp
(setq-default ispell-program-name "aspell")

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
```


<a id="Dired"></a>

## Dired

The built-in file explorer (directory editor, dired) doesn't need to be installed but can be configured using use-package to keep the form consistent. In order for Emacs to not spawn a new buffer for each directory and to make it more evil friendly I'm using `*find-alternate-file`.

```emacs-lisp
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
```


<a id="No-littering"></a>

## No-littering

I don't want Emacs to put backup files in the file's directory and mess with git ignore at each repo so this package collects those and also all custom file and puts them all in defined locations.

```emacs-lisp
(use-package no-littering
  :straight t
  :init
  (setq auto-save-file-name-transforms
    `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  :config
  (no-littering-theme-backups))
```


<a id="Misc"></a>

## Misc

Because it's easier to type one letter than a word, let's replace the common yes/no prompt with y/n. Also, if the underlying file changes, the buffer should update (revert) automatically.

```emacs-lisp
(defalias 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)

(setq-default indent-tabs-mode t)
(setq-default tab-width 2)
```


<a id="Packages"></a>

# Packages

The rest of the functionality is provided by packages, all third-party packages managed by straight.


<a id="Version-Control"></a>

## Version Control

[Magit](https://magit.vc/) **the** killer application that brought me to Emacs. Git is great, but also complicated and Magit is the best user interface I've found for git. Being able to diff and work with hunks at a line level is both simple andp powerful.

```emacs-lisp
(use-package magit
  :straight t
  :defer t
  :config (setq
       magit-diff-refine-hunk t
       magit-diff-paint-whitespace-lines t
       magit-diff-highlight-indentation t))
```


<a id="Configuration-Documentation-Annotation"></a>

## Configuration Documentation/Annotation

As mentioned in the intro, this whole configuration documentation/annotation/"inverse literate" config is made possible by [org-transclusion](https://github.com/nobiot/org-transclusion). The goal of the package is to provide the ability to include text snippets from other text documents without having to copy-paste it and get materials out of sync. With the added support to include code snippets wrapped in begin/end<sub>src</sub> block, it's perfect for "inverse literate" config. Not having to tangle your code makes startup faster and the org file doesn't need to talk about every line/block you have in your config only what you want to highlight.

An important option to set is `org-transclusion-remember-transclusions` so that just the buffer contains the transcluded text and it's not actually saved out to the file on disk. *This is the way* to preserve the pointer/link.

```emacs-lisp
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
```


<a id="Theme"></a>

## Theme

Since I migrated from Doom, I really enjoy the Doom themes, mostly preferring the default `doom-one` theme but I also use the `doom-nord` theme. I also use [auto-dark-emacs](https://github.com/LionyxML/auto-dark-emacs) so the theme switches automatically with the system theme.

```emacs-lisp
(use-package doom-themes
  :straight t
  :init (load-theme 'doom-nord t))

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
```


<a id="Muscle-memory-shortcuts"></a>

## Muscle memory shortcuts

There are some shortcuts that I have lots of muscle memory with and also work in other applications that I find convenient to use in Emacs. I also use the evil-mode keys when in normal mode, based on whatever is most convenient. I used `undo-fu` for a familiar approach to undo-redo system but knew I wasn't taking advantage of the power of multi-path undo, until I found [`vundo`](https://github.com/casouri/vundo), visual undo. This package visualizes the history as nodes that can traverse and diff in a very familiar git branch paradigm.

```emacs-lisp
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
```


<a id="Keybindings"></a>

## Keybindings

For all the keys I don't have muscle memory for, there's `which-key`. It progressively shows the keybindings that are available in that mode.

```emacs-lisp
(use-package which-key
  :straight t
  :config
  (which-key-mode)
  :init
  (setq which-key-idle-delay 0.1))
```


<a id="Evil-Mode"></a>

## Evil-Mode

Extensible VI Layer (evil) mode for Emacs provides vi editing modes and keybindings. `evil-collection` provides all the keybindings in different modes so they don't have to be configured individually. Since the individual bindings (minor-modes?) are not loaded until the package is used, it's not bloating the in use configuration.

```emacs-lisp
(use-package evil
  :straight t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-integration t
    evil-want-keybinding nil
    evil-disable-insert-state-bindings t
    ;; evil-undo-system 'undo-fu)
    evil-undo-system 'undo-redo)
  :config
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
```

However, there are some keybindings I want to have available everywhere and use the `evil-leader` to configure those. I'm still debating whether I want to all my Keybindings behind the leader or start inserting more into `evil-<current>-state-local-map`, especially if that leads to 2 keystrokes instead of 3. There are also global binds as `global-set-key`, which probably should be listed with the CUA section. So, it's a bit of a mixed bag, but all the keybindings that are global are at least defined through one function (I hope) after `evil-collection` is done making its changes.

```emacs-lisp
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

  "g" '("magit"          . (keymap))
  "ga" '("add"           . magit-stage-buffer-file)
  "gc" '("commit"        . magit-commit)
  "gf" '("fetch"         . magit-fetch)
  "gg" '("status"        . magit-status)
  "gr" '("status"        . magit-refresh)

  "q" '("quit"           . (keymap))
  "qb" '("buffer"        . kill-current-buffer)
  "qq" '("save & quit"   . save-buffers-kill-terminal)

  "h" '("help"           . (keymap))
  "hf" '("function"      . describe-function)
  "hk" '("key"           . describe-key)
  "hv" '("variable"      . describe-variable)

  "t" '("theme"          . (keymap))
  "td" '("dark"          . sb/load-dark-theme)
  "tl" '("light"         . sb/load-light-theme)

  "w" '("window"         . (keymap))
  "wd" '("delete"        . delete-window)
  "wb" '("split-below"   . split-window-below)
  "wr" '("split-right"   . split-window-right)
  "wo" '("delete other"  . delete-other-windows)
  "ww" '("ace-window"    . aw-show-dispatch-help))

  (global-set-key (kbd "C-s") 'save-buffer)
  (global-set-key (kbd "C-S-s") 'write-file)
  (global-set-key (kbd "C-a") 'mark-whole-buffer)
  (global-set-key (kbd "M-a") 'other-window)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C-S-p") 'execute-extended-command)
  (global-set-key (kbd "C-<tab>") #'consult-buffer)
  (global-set-key (kbd "C-z") #'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") #'undo-fu-only-redo)
  (define-key evil-normal-state-map "gb" 'revert-buffer-quick))

(with-eval-after-load 'evil-collection
  (sb/set-global-key-bindings))
```


<a id="History"></a>

## History

These packages give Emacs memory so the frequent and recent things are near the top when the completion related packages get used.

```emacs-lisp
(use-package savehist
  :straight t
  :init
  (savehist-mode))

(use-package recentf
  :straight t
  :init
  (recentf-mode t)
  (run-at-time nil 600 'recentf-save-list))
```


<a id="Completions"></a>

## Completions

All the things that help with completion in various contexts are in this section, they deserve elaboration (at a later time). `Note`: The current completion strategy is very much a work in progress. However, vertico, corfu, cape, and friends seems to be the front-runners.

```emacs-lisp
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
```


<a id="User-Config"></a>

# User Config

There are a few packages and specific configuration that is quite specific to my workflow so they're in a separate file and not transcluded here. If anyone wants to use this `init.el` file, this section needs to be removed.

```emacs-lisp
(load (concat user-emacs-directory "userConfig.el"))
```


<a id="Flycheck"></a>

## Flycheck

[Flycheck](https://www.flycheck.org/en/latest/) provides syntax checking for Emacs and provides more of the IDE functionality to the text-editor. [Phundrak's config](https://config.phundrak.com/emacs/packages/programming.html#flycheck) is fairly authoritative and I'm using it whole sale.

```emacs-lisp
(use-package flycheck
  :straight (:build t)
  :defer t
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Rerunning checks on every newline is a mote excessive.
  (delq 'new-line flycheck-check-syntax-automatically)
  ;; And don’t recheck on idle as often
  (setq flycheck-idle-change-delay 2.0)

  ;; For the above functionality, check syntax in a buffer that you
  ;; switched to on briefly. This allows “refreshing” the syntax check
  ;; state for several buffers quickly after e.g. changing a config
  ;; file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors a little quicker (default is 0.9s)
  (setq flycheck-display-errors-delay 1))

(use-package flycheck-popup-tip
  :straight (:build t)
  :after flycheck evil
  :hook (flycheck-mode . flycheck-popup-tip-mode)
  :config
  (setq flycheck-popup-tip-error-prefix "X ")
  (add-hook 'evil-insert-state-entry-hook
      #'flycheck-popup-tip-delete-popup)
  (add-hook 'evil-replace-state-entry-hook
      #'flycheck-popup-tip-delete-popup))

(use-package flycheck-posframe
  :straight (:build t)
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (setq flycheck-posframe-warning-prefix "! "
    flycheck-posframe-info-prefix    "··· "
    flycheck-posframe-error-prefix   "X "))
```


<a id="Ox-hugo"></a>

## Ox-hugo

My website/blog is created with [Hugo](https://gohugo.io/): a static site generator. However, I wanted to create an unified workflow and have a central place to write instead of manually managing files and folders. To that effect (and seeing how this is a 'not so small' Emacs config) I decided to go with [ox-hugo](https://ox-hugo.scripter.co/).

Ox-hugo serves as the middle-ware so the "front-end" can be Emacs and the it handles all the content directory and file structure creation before handing it off to Hugo to generate the HTML site. While this is a few levels of abstraction it allows for a very straight-forward and friction free blogging experience. I run an org-capture template that creates all the meta data (front-matter) needed and I can write a post (use yasnippet for other captures like inserting images), commit, and push and the remote server (as of writing [Sourcehut Pages](https://srht.site/)) builds and serves the site.

I've used diagramming on the blog, I want to better integrate it eventually but so far I've used `ob-mermaid` but then had to output and embed png since the Mermaid.js payload is 3MB!

```emacs-lisp
(use-package ox-hugo
  :straight t
  :config
  ;; Org capture template for Hugo posts
  ;; https://ox-hugo.scripter.co/doc/org-capture-setup/
  (with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
       (fname (concat (format-time-string "%Y%m%d_") (org-hugo-slug title))))
    (shell-command-to-string
     (concat "mkdir -p ~/dev/shom.dev/images/posts/" fname))
    (mapconcat #'identity
           `(
           ,(concat "\n* DRAFT " title)
           ":PROPERTIES:\n:EXPORT_FILE_NAME: index"
           ,(concat ":EXPORT_HUGO_BUNDLE: " fname)
           ,(concat ":EXPORT_HUGO_CUSTOM_FRONT_MATTER: :aliases /s/"
                (shell-command-to-string
                 (concat "~/dev/shom.dev/crc32Janky.sh " fname)))
           ,(concat ":EXPORT_HUGO_IMAGES: /posts/" fname "/image.jpg")
           ":EXPORT_HUGO_MENU:\n:END:"
           "%?\n\n#+hugo: more")          ;Place the cursor here finally
           "\n")))

  (defun org-hugo-new-subtree-start-guide-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Start Guide Title: ")) ;Prompt to enter the post title
       (fname (org-hugo-slug title)))
    (shell-command-to-string
     (concat "mkdir -p ~/dev/shom.dev/images/start/" fname))
    (mapconcat #'identity
           `(
           ,(concat "\n* DRAFT " title " :start:")
           ":PROPERTIES:\n:EXPORT_FILE_NAME: index"
           ,(concat ":EXPORT_HUGO_BUNDLE: " fname)
           ,(concat ":EXPORT_HUGO_CUSTOM_FRONT_MATTER: :aliases /s/"
                (shell-command-to-string
                 (concat "~/dev/shom.dev/crc32Janky.sh " fname)))
           ,(concat ":EXPORT_HUGO_IMAGES: /start/" fname "/image.jpg")
           ":EXPORT_HUGO_MENU:\n:END:"
           "%?\n\n#+hugo: more")          ;Place the cursor here finally
           "\n")))

  (add-to-list 'org-capture-templates
         '("s"
           "Hugo Start Guide"
           entry
           (file+olp "~/dev/shom.dev/start.org" "Start")
           (function org-hugo-new-subtree-start-guide-capture-template)
           :prepend t))
  (add-to-list 'org-capture-templates
         '("p"                ;`org-capture' binding + h
           "Hugo Post"
           entry
           (file+olp "~/dev/shom.dev/posts.org" "Content")
           (function org-hugo-new-subtree-post-capture-template)
           :prepend t))))

(use-package ob-mermaid
  :straight t
  :config
  (setq ob-mermaid-cli-path "/home/shom/.config/nvm/versions/node/v18.16.1/bin/mmdc")
  (org-babel-do-load-languages
       'org-babel-load-languages
       '((mermaid .t)
       (shell . t)
       (scheme . t))))
```


<a id="Email"></a>

## Email

I use mu4e and org-msg for doing email through Emacs. I'm not a prolific mail user so my setup is pretty simple. I have written a post about [setting up Proton Mail in Emacs](https://shom.dev/posts/20220108_setting-up-protonmail-in-emacs/) that covers the setup in more detail.

```emacs-lisp
(unless sb/is-termux
  (use-package mu4e
  :straight nil
  :defer 20 ; Wait until 20 seconds after startup
  :config

  (setq mu4e-change-filenames-when-moving t ; avoid sync conflicts
      mu4e-update-interval (* 10 60) ; check mail 10 minutes
      mu4e-compose-format-flowed t ; re-flow mail so it's not hard wrapped
      mu4e-get-mail-command "mbsync -a"
      mu4e-maildir "~/mail/proton")

  (setq mu4e-drafts-folder "/proton/Drafts"
      mu4e-sent-folder   "/proton/Sent"
      mu4e-refile-folder "/proton/All Mail"
      mu4e-trash-folder  "/proton/Trash")

  (setq mu4e-maildir-shortcuts
      '(("/proton/inbox"     . ?i)
      ("/proton/Sent"      . ?s)
      ("/proton/Trash"     . ?t)
      ("/proton/Drafts"    . ?d)
      ("/proton/All Mail"  . ?a)))

  (setq message-send-mail-function 'smtpmail-send-it
      auth-sources '("~/.authinfo") ;need to use gpg version but only local smtp stored for now
      smtpmail-smtp-server "127.0.0.1"
      smtpmail-smtp-service 1025
      smtpmail-stream-type  'ssl))

  (use-package org-msg
  :straight t
  :after mu4e
  :config
  (setq mail-user-agent 'mu4e-user-agent)
  (require 'org-msg)
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
      org-msg-startup "hidestars indent inlineimages"
      org-msg-default-alternatives '((new		. (text html))
                     (reply-to-html	. (text html))
                     (reply-to-text	. (text)))
      org-msg-convert-citation t)
  (org-msg-mode)))
```


<a id="Yasnippet"></a>

## Yasnippet

Yasnippet is a tenplating package, it's autocomplete on steroids. You define templates that are relevant for specific modes (org/lisp/rust/html/etc) and when in that mode and a keyphrase is typed and activated it will pull in that template with multiple variables and multi-line typing. I currently use it **very** simplistically but need to integrate it more into my workflow.

```emacs-lisp
(use-package yasnippet
  :straight t
  :config
  (setq yas-snippet-dirs '("~/.emacs/.custom/snippets"))
  (setq warning-suppress-types (cons 'warning-suppress-types '(yasnippet backquote-change)))
  :config
  (yas-global-mode 1)
  (yas-reload-all))
```


<a id="Repo-meta"></a>

# Repo meta


<a id="Useful-anchors"></a>

## Useful anchors

By default `org-export` regenerates ids for all the headings which creates noise in the git commit history and also in-page anchors can't be reliably linked to a specific part of the document (independent of the git forge's markdown parsing implementation). Using a snippet of [@alphapapa](https://github.com/alphapapa)'s unpackaged configuration, we can advice the export to create unique anchors that won't change between exports (unless the headings themselves have been changed). Replacing space (%20) with dash (-) because Github doesn't parse that correctly.

I'm also using [ox-gfm](https://github.com/larstvei/ox-gfm) to produce GitHub flavored Markdown to generate code fences with language specifier so it syntax-highlighting is rendered on the web forges.

```emacs-lisp
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

(use-package ox-gfm
  :straight t)
```


<a id="Exporting-Readme-markdown"></a>

## Exporting Readme markdown

Since I want to keep the org original with the transclusion blocks and Markdown is supported better by more forges, I want to export this config file as a README.md automatically each time I save.

```emacs-lisp
(defun sb/generate-readme()
  (if (file-exists-p "./README.md")
      (delete-file "./README.md"))
  (message "Turn transclusion On!")
  (org-transclusion-add-all)
  (message "Export")
  (unpackaged/org-export-html-with-useful-ids-mode)
  (org-gfm-export-to-markdown)
  (message "Transclusion Off!")
  (org-transclusion-deactivate)
  (message "Reverting buffer to avoid issues with org-babel")
  (interactive) (revert-buffer t t))

(sb/generate-readme)
```


<a id="Remote-as--mirrors-"></a>

## Remote as "mirrors"

Sourcehut is the primary location of this repo with "mirror" on GitHub. But instead of relying on GitHub actions to mirror and manage a separate workflow on the secondary platform, I'm taking shortcut by adding a second push-url to my repo's main remote (origin) so whenever there's a push, it pushes to both Sourcehut and GitHub. When `git remote set-url --push origin --add` is called, it doesn't append to the existing list, it replaces so the primary url also needs to be added.

```shell
#!/usr/bin/sh
set -eu
git remote set-url --push origin --add git@codeberg.org:shom/.emacs.custom.git 
git remote set-url --push origin --add git@git.sr.ht:~shom/.emacs.custom
git remote set-url --push origin --add git@github.com:shombando/.emacs.custom.git
```
