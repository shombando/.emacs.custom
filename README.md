
# Table of Contents

1.  [Startup](#org73b823f)
    1.  [User Emacs Directory](#org0d8cf6d)
    2.  [Package Management](#orgad8ec0e)
    3.  [Visual elements](#org6953171)
    4.  [Early Init](#org799bc00)

This is my custom config based on my [Emacs journey](https://shom.dev/posts/20211121_emacs-custom-configuration/). I wanted to create a literate config but did not want slow down startup with tangling, so currently I'm trying to get "the best of both worlds" via `org-transclusion`. The file `config.org` in this repo contains the "source" and `org-transclusion` directives and is rendered out to `README.md` (markdown is better supported for auto-rendering by more forges currently). I'll eventually automate this process, likely through a git-hook. However, the rendered output is never guaranteed to include all of my config, just the sections that have been manually commented, `init.el` and includes will remain the source of truth. 


<a id="org73b823f"></a>

# Startup

I would like to have the option to have several Emacs "distributions" or configurations that are independent of each other. This allows for experimentation without breaking things (git helps with the actual config but this allows for the packages to be independent). I'm using Chemacs2 but that's outside the scope of the config, for now.


<a id="org0d8cf6d"></a>

## User Emacs Directory

I've chosen to keep my custom config and all related packages, etc. in this specific location so that's the first thing in the config.

    (setq user-emacs-directory "~/.emacs/.custom/")


<a id="orgad8ec0e"></a>

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


<a id="org6953171"></a>

## Visual elements

The Emacs GUI is a bit dated, especially the toolbar and menu bar. Also since I'll be customizing my keymaps they're going to be mostly superfluous and take up real-estate. There are a few other preferences relative to how things are displayed here, the rest of the visuals are configured by the theme.

    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (setq visible-bell 1)
    (global-visual-line-mode 1)
    (global-linum-mode 1)
    (column-number-mode t)


<a id="org799bc00"></a>

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

