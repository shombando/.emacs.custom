(use-package org-social
	 :straight( :package "org-social"
							:type git :host github :repo "tanrax/org-social.el")
	:config
	(setq org-social-file "/ssh:conmanBananerd:/home/conman/prod/caddy/site/shom.dev/social.org")
	(add-to-list 'evil-emacs-state-modes 'org-social-timeline-mode)
	(evil-leader/set-key
		"s" '("social"     . (keymap))
		"st" '("timeline"  . org-social-timeline)
		"sn" '("new"       . org-social-new-post)
		"so" '("open"      . org-social-open-file))
