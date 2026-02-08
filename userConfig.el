(setq user-full-name "Shom Bandopadhaya"
	  user-mail-address "shom@bandopadhaya.com")

;; Optional packages
;; (load (concat user-emacs-directory "modules/mailConfig.el"))
;; (load (concat user-emacs-directory "modules/guileConfig.el"))
(load (concat user-emacs-directory "modules/ox-hugoConfig.el"))
(load (concat user-emacs-directory "modules/flycheckConfig.el"))
(load (concat user-emacs-directory "modules/useful-anchors.el"))
(load (concat user-emacs-directory "modules/org-social.el"))

;;yasnippet_begin
(use-package yasnippet
  :straight t
  :config
  (setq yas-snippet-dirs '("~/.emacs/.custom/snippets"))
  (setq warning-suppress-types (cons 'warning-suppress-types '(yasnippet backquote-change)))
  :config
	(add-hook 'git-commit-setup-hook
          (lambda ()
            (when (derived-mode-p 'text-mode)
							(yas-activate-extra-mode 'text-mode+git-commit-mode))))
  (yas-global-mode 1)
  (yas-reload-all))

(use-package yasnippet-snippets
	:straight t)
;;yasnippet_end

(use-package yaml-mode
  :straight t)

(use-package nov
  :straight t
  :config (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package markdown-mode
  :straight t)

(use-package nix-mode
  :straight t)

(use-package avy
  :straight t
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char-timer)
  (setq avy-timeout-seconds .5))

(use-package 0x0
  :straight t)

(use-package olivetti
  :straight t
	:config
	(setq-default olivetti-body-width 0.7)
	(setq-default olivetti-style t))

(straight-use-package
  '(project-butler :type git :host codeberg :repo "jabbo/project-butler"))

;;(use-package paren
;;	:ensure nil
;;	:hook (after-init . show-paren-mode)
;;	:custom
;;	(show-paren-style 'mixed)
;;	(show-paren-context-when_offscreen t))


(use-package caddyfile-mode
  :straight t
	 :mode (("Caddyfile\\'" . caddyfile-mode)
         ("caddy\\.conf\\'" . caddyfile-mode)))

(defun sb/generate-uuid ()
	"Generate and insert a UUID string"
	(interactive)
	(insert (shell-command-to-string "uuidgen")))

;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(go-ts-mode . ("gopls" ""))))

(use-package go-ts-mode
	:straight t
	:init
	(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
	:config
	;; https://jeffbowman.writeas.com/crafted-emacs-example-adding-go
	;; Optional: install eglot-format-buffer as a save hook.
	;; The depth of -10 places this before eglot's willSave notification,
	;; so that that notification reports the actual contents that will be saved.
	(defun eglot-format-buffer-on-save ()
		(add-hook 'before-save-hook #'eglot-format-buffer -10 t))
	(add-hook 'go-ts-mode-hook 'eglot-ensure 10)
	(add-hook 'go-ts-mode-hook #'eglot-format-buffer-on-save)
  :bind (:map evil-normal-state-map
				("C->" . 'eglot-code-actions)))

(use-package eldoc-box
	:straight t
  :bind (:map evil-normal-state-map
				("C-." . 'eldoc-box-help-at-point)))

(provide 'userConfig)
;;; userConfig.el ends here
