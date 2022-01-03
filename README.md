
# Table of Contents

1.  [Startup](#orge356d6f)
    1.  [User Emacs Directory](#orge3187af)
    2.  [Package Management](#org19d1bde)
    3.  [Visual elements](#org5de3cd7)
    4.  [Early Init](#org03e9fd2)
2.  [Emacs Behavior](#orgda8e5f3)
    1.  [Spellcheck](#org8991229)
    2.  [Dired](#org9b5f140)
    3.  [No-littering](#org4089e5c)
    4.  [Misc](#orgcf37b62)
3.  [Packages](#org7b7833b)
    1.  [Theme](#orgec704dd)
    2.  [Muscle memory shortcuts](#org1c4070f)
    3.  [Keybindings](#org4d3e132)
    4.  [Evil-Mode](#org5a3e6aa)
    5.  [History](#orgbf31bd1)
    6.  [Completions](#orgfac51bb)

This is my custom config based on my [Emacs journey](https://shom.dev/posts/20211121_emacs-custom-configuration/). I wanted to create a literate config but did not want slow down startup with tangling, so currently I'm trying to get "the best of both worlds" via `org-transclusion`. The file `config.org` in this repo contains the "source" and `org-transclusion` directives and is rendered out to `README.md` (markdown is better supported for auto-rendering by more forges currently). I'll eventually automate this process, likely through a git-hook. However, the rendered output is never guaranteed to include all of my config, just the sections that have been manually commented, `init.el` and includes will remain the source of truth. 


<a id="orge356d6f"></a>

# Startup

I would like to have the option to have several Emacs "distributions" or configurations that are independent of each other. This allows for experimentation without breaking things (git helps with the actual config but this allows for the packages to be independent). I'm using Chemacs2 but that's outside the scope of the config, for now.


<a id="orge3187af"></a>

## User Emacs Directory

I've chosen to keep my custom config and all related packages, etc. in this specific location so that's the first thing in the config.

    (setq user-emacs-directory "~/.emacs/.custom/")


<a id="org19d1bde"></a>

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


<a id="org5de3cd7"></a>

## Visual elements

The Emacs GUI is a bit dated, especially the toolbar and menu bar. Also since I'll be customizing my keymaps they're going to be mostly superfluous and take up real-estate. There are a few other preferences relative to how things are displayed here, the rest of the visuals are configured by the theme.

    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (setq visible-bell 1)
    (global-visual-line-mode 1)
    (global-linum-mode 1)
    (column-number-mode t)

The modeline is another important visual element to show important information about the buffer and modes. Doom's modeline is a significant visual upgrade to the default.

    (use-package doom-modeline
      :straight t
      :init (doom-modeline-mode 1))


<a id="org03e9fd2"></a>

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


<a id="orgda8e5f3"></a>

# Emacs Behavior

A lot the configuration revolves around packages but there are some built-in settings and behavior within Emacs that also need to be tweaked.


<a id="org8991229"></a>

## Spellcheck

Spellchecking while typing is useful and I want to use it everywhere but in programming modes only comments should be spell checked not the whole buffer.

    (setq-default ispell-program-name "aspell")
    
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)


<a id="org9b5f140"></a>

## Dired

The built-in file explorer (directory editor, dired) doesn't need to be installed but can be configured using use-package to keep the form consistent. In order for  Emacs to not spawn a new buffer for each directory and to make it more evil friendly I'm using `*find-alternate-file`.

    (use-package dired
      :straight nil
      :after evil-collection
      :commands (dired dired-jump)
      :custom ((dired-listing-switches "-agho --group-directories-first"))
      :config
        (evil-collection-define-key 'normal 'dired-mode-map
        "h" '(lambda () (interactive) (find-alternate-file ".."))
        "l" 'dired-find-alternate-file))


<a id="org4089e5c"></a>

## No-littering

I don't want Emacs to put backup files in the file's directory and mess with git ignore at each repo so this package collects those and also all custom file and puts them all in defined locations.

    (use-package no-littering
      :straight t
      :init
      (setq auto-save-file-name-transforms
    	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
      (setq custom-file (no-littering-expand-etc-file-name "custom.el")))


<a id="orgcf37b62"></a>

## Misc

Because it's easier to type one letter than a word, let's replace the common yes/no prompt with y/n. Also, if the underlying file changes, the buffer should update (revert) automatically.

    (defalias 'yes-or-no-p 'y-or-n-p)
    (global-auto-revert-mode 1)


<a id="org7b7833b"></a>

# Packages

The rest of the functionality is provided by packages, all of which is managed by straight.


<a id="orgec704dd"></a>

## Theme

Since I migrated from Doom, I really enjoy the Doom themes, mostly preferring the default `doom-one` theme but I also use the `doom-nord` theme.

    (use-package doom-themes
      :straight t
      :init (load-theme 'doom-one t))


<a id="org1c4070f"></a>

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


<a id="org4d3e132"></a>

## Keybindings

For all the keys I don't have muscle memory for, there's `which-key`. It progressively shows the keybindings that are available in that mode.

    (use-package which-key
      :straight t
      :config
      (which-key-mode)
      :init
      (setq which-key-idle-delay 0.1))


<a id="org5a3e6aa"></a>

## Evil-Mode

Extensible VI Layer (evil) mode for Emacs provides vi editing modes and keybindings. `evil-collection` provides all the keybindings in different modes so they don't have to be configured individually. Since the individual bindings (minor-modes?) are not loaded until the package is used, it's not bloating the in use configuration.

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

However, there are some keybindings I want to have available everywhere and use the `evil-leader` to configure those.

    (evil-leader/set-key
      "." 'find-file
      "," 'consult-buffer
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


<a id="orgbf31bd1"></a>

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


<a id="orgfac51bb"></a>

## Completions

All the things that help with completion in various contexts are in this section, they deserve elaboration (at a later time).

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
    
    (use-package fancy-dabbrev
      :straight t
      :after consult
      :init
      (global-fancy-dabbrev-mode))
    
    (use-package corfu
      :straight t
      :after fancy-dabbrev
      :custom
      (corfu-cycle t)
      (corfu-auto t)
      (corfu-auto-delay 0.1)
      :bind (:map evil-insert-state-map
    	      ("C-j" . corfu-next)
    	      ("C-k" . corfu-previous)
    	      ("<tab>" . fancy-dabbrev-expand-or-indent)
    	      :map corfu-map
    	      ("<tab>" . corfu-next)
    	      ("<backtab>" . corfu-previous))
      :init
      (corfu-global-mode))
    
    (use-package cape
      :straight t
      :after corfu
      :bind (:map evil-insert-state-map
    	      ("M-'" . completion-at-point))
      :init
      (setq completion-at-point-functions
    	      (list (cape-super-capf
    		     #'cape-file-capf
    		     #'cape-dabbrev-capf
    		     #'cape-ispell-capf
    		     #'cape-keyword-capf))))

