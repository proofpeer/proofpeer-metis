(require 'magit)
(require 'ensime)

(add-hook 'scala-mode-hook 'ensime-mode)

(defun scala-hook ()
   ;; sbt-find-definitions is a command that tries to find (with grep)
   ;; the definition of the thing at point.
   (local-set-key (kbd "M-.") 'sbt-find-definitions)

   ;; use sbt-run-previous-command to re-compile your code after changes
   (local-set-key (kbd "C-x '") 'sbt-run-previous-command))
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'scala-mode-hook 'scala-hook)
(add-hook 'scala-mode-hook 'whitespace-mode)
(add-hook 'scala-mode-hook 'subword-mode)
(global-set-key (kbd "C-c l") 'org-store-link)
(require 'helm-config)
