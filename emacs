(require 'package)

(setq package-enable-at-startup nil)
(package-initialize)

(evil-mode 1)

(add-hook 'after-init-hook 'global-company-mode)

(tool-bar-mode -1)
(menu-bar-mode -1)
"
(global-nlinum-mode 1)
(nlinum-relative-setup-evil)
(nlinum-relative-on)
"

"
haskell
======
"
(add-hook 'haskell-mode-hook
	  (lambda ()
	    (set (make-local-variable 'company-backends)
		 (append '((company-capf company-dabbrev-code))
			 company-backends))))

(setq haskell-process-wrapper-function
      (lambda (args) (apply 'nix-shell-command (nix-current-sandbox) args)))
(require 'haskell-interactive-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(require 'haskell-process)

"
error highligting
================
"
(setq flycheck-command-wrapper-function
      (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
      flycheck-executable-find
      (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))


(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-nixos-options))

