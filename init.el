;; Use ibuffer instead of default one
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; Show line numbers
(global-linum-mode t)


;; Add sources
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)


;; Auto install packages
(dolist (package '(package clang-format ggtags sr-speedbar auto-complete jedi epc deferred python-environment ctable flycheck rtags company company-rtags))
 (unless (package-installed-p package)
   (package-install package)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-rtags company rtags flycheck jedi auto-complete sr-speedbar ggtags clang-format))))


;; Load theme: zenburn/flatland/busybee
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'zenburn t)


;; Set Consolas font
(set-frame-font "Consolas-15")


;; Highlight matching parentheses
(show-paren-mode 1)


;; No indentation within namespaces in c++-mode, open speedbar automatically
(defun my-cpp-setup ()
  (c-set-offset 'innamespace [0]))
(add-hook 'c++-mode-hook 'my-cpp-setup)


;; Open headers in c++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


;; Python indent is 3
(setq python-indent-offset 3)


;; C, C++ and Java offset
(setq-default c-basic-offset 4)


;; highlight trailing whitespace
;; run M-x load-file .emacs.d/highlight-chars/highlight-chars.el
(load "~/.emacs.d/highlight-chars/highlight-chars.el")
(require 'highlight-chars)
(add-hook 'font-lock-mode-hook 'hc-highlight-tabs)
(add-hook 'font-lock-mode-hook 'hc-highlight-trailing-whitespace)


;; Activate windmove keybindings
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))


;; Window switching keybindings (like i3)
(global-set-key (kbd "C-s-j")  'windmove-left)
(global-set-key (kbd "C-s-ø") 'windmove-right)
(global-set-key (kbd "C-s-;") 'windmove-right)
(global-set-key (kbd "C-s-l")    'windmove-up)
(global-set-key (kbd "C-s-k")  'windmove-down)


;; Window resizing keybindings (like i3)
(global-set-key (kbd "C-M-s-j") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-s-ø") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-s-;") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-s-k") 'shrink-window)
(global-set-key (kbd "C-M-s-l") 'enlarge-window)


;; Put auto-saved and backup files to /tmp/
(setq backup-directory-alist
      `(("." . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `(("^.*\\/" ,temporary-file-directory t)))


;; Toggle between beginning-of-indentation and beginning-of-line when pressing C-a
(defun move-smart-beginning-of-line ()
  "Move point to `beginning-of-line'. If repeat command it cycle
position between `back-to-indentation' and `beginning-of-line'."
  (interactive "^")
  (if (eq last-command 'move-smart-beginning-of-line)
      (if (= (line-beginning-position) (point))
          (back-to-indentation)
        (beginning-of-line)) ; inner else
    (back-to-indentation))) ; outer else

(global-set-key (kbd "C-a") 'move-smart-beginning-of-line)


;; ;; clang-format
;; ;; Requires:
;; ;; sudo apt install clang-format-5.0
;; (load "/home/mikhailv/.emacs.d/elpa/clang-format-20180406.814/clang-format.el")
;; (require 'clang-format)
;; (setq clang-format-style-option "file")
;; (setq clang-format-executable "clang-format-5.0")
;; (global-set-key [C-M-tab] 'clang-format-buffer)


;; ;; ggtags-mode (automatically on for c, c++ and java)
;; ;; Requires:
;; ;; sudo apt install global
;; (require 'ggtags)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;;               (ggtags-mode 1))))
;; (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
;; (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
;; (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
;; (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
;; (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
;; (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)


;; sr-speedbar and keybinding
(require 'sr-speedbar)
(global-set-key (kbd "C-c b t") 'sr-speedbar-toggle)
(global-set-key (kbd "C-c b u") (lambda() (interactive) (sr-speedbar-refresh)) ) ; update
(setq sr-speedbar-width 30)
(setq sr-speedbar-right-side nil)


;; ;; auto completion
;; (require 'auto-complete)
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (setq ac-use-quick-help t)
;; (setq ac-delay 0.2)
;; (setq ac-auto-show-menu t)


;; RTAGS
(require 'rtags)
(setq rtags-autostart-diagnostics t)
(setq rtags-completions-enabled t)
;; (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
;; (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
;; (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)

(add-hook 'c-mode-common-hook (lambda ()
				(rtags-start-process-unless-running)
				(rtags-enable-standard-keybindings)
				(global-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
				(global-set-key (kbd "M-,") 'rtags-find-references-at-point)
				(global-set-key (kbd "C-c r {") 'rtags-previous-match)
				(global-set-key (kbd "C-c r }") 'rtags-next-match)
				(rtags-diagnostics)
				(push 'company-rtags company-backends)
                                (setq indent-tabs-mode nil)
				))


;; company (autocompletion)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-backends (delete 'company-semantic company-backends))
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)



;; TODO from here: https://github.com/Andersbakken/rtags#code-completion-in-emacs


;; ;; Python autocompletion (to go to definition: M-x j-def)
;; ;; Requires:
;; ;; M-x jedi:install-server
;; ;; sudo apt install python3-virtualenv
;; ;; pip install jedi
;; ;; pip install epc
;; ;; pip install argparse
;; (require 'ctable)
;; (require 'deferred)
;; (require 'epc)
;; (require 'python-environment)
;; (require 'jedi)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)


;; Lint
;; Requires:
;; For python: pip install flake8
(require 'flycheck)
(add-hook 'python-mode-hook (lambda ()
			      (setq flycheck-flake8rc "/home/mikhailv/.config/flake8/setup.cfg")
			      (flycheck-mode 1)
                              (setq indent-tabs-mode t)))
;; (add-hook 'c-mode-common-hook (lambda ()
;; 				(flycheck-mode 1)))


;; ;; irony-mode C++ autocompletion
;; (require 'irony)
;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'objc-mode-hook 'irony-mode)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
