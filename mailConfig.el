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
