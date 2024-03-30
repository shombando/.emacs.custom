(setq user-full-name "Shom Bandopadhaya"
	  user-mail-address "shom@bandopadhaya.com")

;; Optional packages
;; (load (concat user-emacs-directory "mailConfig.el"))
;; (load (concat user-emacs-directory "guileConfig.el"))
(load (concat user-emacs-directory "ox-hugoConfig.el"))

(use-package yasnippet
  :straight t
  :config
  (setq yas-snippet-dirs '("~/.emacs/.custom/snippets"))
  (setq warning-suppress-types (cons 'warning-suppress-types '(yasnippet backquote-change)))
  :config
  (yas-global-mode 1)
  (yas-reload-all))

(use-package yaml-mode
  :straight t)

;; from: https://config.phundrak.com/emacs.html#Packages-Configuration-Programming-languages-Flycheckb446fl6184j0
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
  (defun tab-bar-format-menu-bar ()
	"Produce the Menu button for the tab bar that shows the menu bar."
	`((menu-bar menu-item (propertize " ξ " 'face 'tab-bar-tab-inactive)
				tab-bar-menu-bar :help "Menu Bar")))

  (tab-bar-mode t))

(use-package nov
  :straight t
  :config (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package markdown-mode
  :straight t)

