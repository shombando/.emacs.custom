(setq user-full-name "Shom Bandopadhaya"
	  user-mail-address "shom@bandopadhaya.com")

;; Optional packages
;; (load (concat user-emacs-directory "modules/mailConfig.el"))
;; (load (concat user-emacs-directory "modules/guileConfig.el"))
(load (concat user-emacs-directory "modules/ox-hugoConfig.el"))
(load (concat user-emacs-directory "modules/flycheckConfig.el"))
(load (concat user-emacs-directory "modules/useful-anchors.el"))

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
  :straight t)

(straight-use-package
  '(project-butler :type git :host codeberg :repo "jabbo/project-butler"))
 (use-package org-social
	 :straight( :package "org-social"
							:type git :host github :repo "tanrax/org-social.el")
	:config
	(setq org-social-file "~/dev/shom.dev/static/social.org")
	(add-to-list 'evil-emacs-state-modes 'org-social-timeline-mode)
	(evil-leader/set-key
		"s" '("social"     . (keymap))
		"st" '("timeline"  . org-social-timeline)
		"sn" '("new"       . org-social-new-post)
		"so" '("open"      . org-social-open-file))
	:init
	(add-hook 'org-social-after-save-file-hook
						(lambda ()
							(call-process-shell-command
							 (format "scp %s %s"
											 org-social-file
											 "conmanBananerd:/home/conman/prod/caddy/site/shom.dev/social.org")
							 nil 0))))

