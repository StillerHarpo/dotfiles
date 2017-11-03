(require 'package)

(setq package-enable-at-startup nil)
(package-initialize)

(evil-mode 1)

(add-hook 'after-init-hook 'global-company-mode)

(tool-bar-mode -1)
(menu-bar-mode -1)
(global-nlinum-mode 1)
(nlinum-relative-setup-evil)
(nlinum-relative-on)

;; get same path as in shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; save backupfiles in tmp
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))
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
python
=====
"
(elpy-enable)
(setq python-shell-completion-native-enable nil)
;; flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
;; pep8 automation
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

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

;; Hide the startup message
(setq inhibit-startup-message t)

