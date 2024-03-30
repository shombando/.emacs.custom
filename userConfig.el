(setq user-full-name "Shom Bandopadhaya"
	  user-mail-address "shom@bandopadhaya.com")

;; Optional packages
;; (load (concat user-emacs-directory "modules/mailConfig.el"))
;; (load (concat user-emacs-directory "modules/guileConfig.el"))
(load (concat user-emacs-directory "modules/ox-hugoConfig.el"))
(load (concat user-emacs-directory "modules/flycheckConfig.el"))

;;yasnippet_begin
(use-package yasnippet
  :straight t
  :config
  (setq yas-snippet-dirs '("~/.emacs/.custom/snippets"))
  (setq warning-suppress-types (cons 'warning-suppress-types '(yasnippet backquote-change)))
  :config
  (yas-global-mode 1)
  (yas-reload-all))
;;yasnippet_end

(use-package yaml-mode
  :straight t)

(use-package nov
  :straight t
  :config (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package markdown-mode
  :straight t)

