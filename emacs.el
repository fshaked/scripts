(setq inhibit-splash-screen t)

;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(load-theme 'wombat)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(package-initialize)

;; Use UTF-8 by default
(set-language-environment "UTF-8")

(global-linum-mode)

(setq-default indent-tabs-mode nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-coq proof-general zenburn-theme yasnippet-snippets yaml-mode which-key undo-tree tabbar session rust-mode puppet-mode pod-mode muttrc-mode mutt-alias lsp-ui initsplit ido-completing-read+ htmlize graphviz-dot-mode goto-chg gitignore-mode gitconfig-mode gitattributes-mode git-modes folding ess eproject diminish csv-mode company-lsp browse-kill-ring boxquote bm bar-cursor apache-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)

;; Load company-coq when opening Coq files
(add-hook 'coq-mode-hook #'company-coq-mode)
;; Enable prettify-symbols-mode by default
(setq company-coq-features/prettify-symbols-in-terminals t)

