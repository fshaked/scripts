;;; package --- My personal set-up.
;;; Commentary:
;;; Code:

(server-start)
(add-hook 'server-switch-hook (lambda () (select-frame-set-input-focus (selected-frame))))

(setq inhibit-splash-screen t)

;; Taken from https://stackoverflow.com/a/51686698
(defun set-exec-path-from-shell-PATH ()
  "Reset PATH to the right PATH.
Because Emacs might be executed from a non-interactive shell, the PATH might not
be set correctly.  The following resets PATH."
  (let ((path-from-shell (shell-command-to-string "$SHELL -ic 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when window-system (set-exec-path-from-shell-PATH))

;; (setq visible-bell t)
;; (setq ring-bell-function 'ignore)

;; Use UTF-8 by default
(set-language-environment "UTF-8")

(setq-default fill-column 80)

(setq-default tab-width 4)

(setq-default x-stretch-cursor t)

(setq backup-directory-alist `(("." . "~/.emacs-backup")))
(setq make-backup-files t               ; backup of a file the first time it is
                                        ; saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      ;; delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new
                                        ; numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new
                                        ; numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a
                                        ; file
      auto-save-timeout 20              ; number of seconds idle time before
                                        ; auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between
                                        ; auto-saves (default: 300)
      )

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively  10000)
;; Make it so that PgUp + PgDown comes back to the same position:
(setq scroll-preserve-screen-position t)
;; Jump to the start/end of buffer when there are no more lines to scroll:
(setq scroll-error-top-bottom t)
(setq scroll-margin 2)

;; Change yes/no prompt to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; When 'whitespace-mode' is enabled, show the tail of lines linger than 80.
(setq-default
 whitespace-line-column nil  ;; nil means use 'fill-column'
 whitespace-style       '(face tabs lines-tail))

(setq ediff-split-window-function 'split-window-horizontally)

;; ;; Open all files in read-only-mode, use C-x C-q to toggle read-only-mode.
;; (defvar my-open-read-only t
;;   "When 't', files are opened in 'read-only-mode'.")
;; (defun my-disable-open-read-only ()
;;   "Disable the opening of all files in 'read-only-mode'."
;;   (interactive)
;;   (setq my-open-read-only nil))
;; (defun my-enable-open-read-only ()
;;   "Enable the opening of all files in 'read-only-mode'."
;;   (interactive)
;;   (setq my-open-read-only 1))
;; (defun my-find-file-handler ()
;;   "Called when a file is opened."
;;   (interactive)
;;   (if my-open-read-only
;;     (read-only-mode 1)))
;; (add-hook 'find-file-hook 'my-find-file-handler)

;; ============================== HISTORY RING =================================

(defvar my-pos-history-ring nil
  "Record buffer location before every jump (i.e. change in line number of more than 1 line).")
(make-variable-buffer-local 'my-pos-history-ring)

(defvar my-pos-history-mark nil
  "Current buffer location (not recorded yet).")
(make-variable-buffer-local 'my-pos-history-mark)

(defun my-pos-history-update ()
  "Check if a big jump was made and 'push' the location before the jump."
  (let ((new-pos (point)))
    (when (and my-pos-history-mark
               (not (eq this-command 'my-pos-history-pop))
               (not (eq this-command 'scroll-up-command))
               (not (eq this-command 'scroll-down-command))
               (> (abs (- (line-number-at-pos new-pos)
                          (line-number-at-pos my-pos-history-mark)))
                  1))
      ;; Push the location before the jump
      (push my-pos-history-mark my-pos-history-ring)
      ;; Limit history to 99 records.
      (nbutlast my-pos-history-ring 100))
    (setq my-pos-history-mark new-pos)))

(defun my-pos-history-pop ()
  "Jump back to the last recorded location, and pop it."
  (interactive)
  (if my-pos-history-ring
      (goto-char (pop my-pos-history-ring))))

(add-hook 'post-command-hook #'my-pos-history-update)

;; ================================= MODES =====================================

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Non-nil means automatically save place in each file.
;; This means when you visit a file, point goes to the last place
;; where it was when you previously visited the same file.
(save-place-mode 1)

(delete-selection-mode 1)

;; Show column number in the mode line.
(column-number-mode 1)

(show-paren-mode 1)

;; Reverts any unchanged buffer associated with a file when the file changes on
;; disk.
(global-auto-revert-mode 1)

(setq-default indent-tabs-mode nil)

;; highlight the current line
(global-hl-line-mode 1)

(if (version<= "26.0.50" emacs-version )
    (global-display-line-numbers-mode 1)
  (global-linum-mode 1))

;; From https://github.com/llvm-mirror/llvm/tree/master/utils/emacs
(setq load-path
    (cons (expand-file-name "~/workspace/llvm-emacs") load-path))
(require 'llvm-mode)

;; ================================ PACKAGE ====================================

(require 'package)

;; Add the MELPA repo.
;; This must be after "require 'package" and before "package-initialize".
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Intended to be used before use-package is loaded.
(defun require-install (FEATURE)
  "Load FEATURE, if it's not installed, install it first.

Example: (require-install 'use-package)"
  (when (not (require FEATURE nil 'noerror))
    (package-install FEATURE)))

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;; (add-to-list 'load-path "<path where use-package is installed>")
  (require-install 'use-package))
(require-install 'diminish)
(require 'bind-key)

;; ========================== GENERAL KEY BINDINGS =============================

;; Make Esc work as C-g
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(bind-keys
 ("C-h B" . describe-personal-keybindings)

 ("M-b" . my-pos-history-pop)

 ("S-<return>" . electric-newline-and-maybe-indent)

 ("C-x m" . man)

 ;; Copy selection
 ("C-w" . kill-ring-save)
 ;; Cut selection
 ("M-w" . kill-region)
 ;; Cut current line
 ("C-S-k" . kill-whole-line)

 ;; Paste
 ("C-v" . yank)
 ;; Cycle through the kill-ring after paste
 ("C-S-v" . yank-pop)

 ;; Join with line above
 ("M-<backspace>" . delete-indentation)
 ;; Join with line below
 ("M-<delete>" . (lambda ()
                   (interactive)
                   ;; (next-line)
                   (delete-indentation t)))
 ;; Delete current line
 ("C-k" . (lambda ()
            (interactive)
            (progn (delete-region (line-beginning-position) (line-end-position))
                   (delete-blank-lines))))

 ;; Comment/uncomment current line(s)
 ("C-d" . comment-line)
 ;; Comment/uncomment selection
 ("C-S-d" . comment-or-uncomment-region)

 ("C-<up>" . scroll-down-line)
 ("C-<down>" . scroll-up-line)

 ("C-x C-p" . package-list-packages)

 ("C-x C-d" . (lambda ()
                (interactive)
                (diff-buffer-with-file (buffer-name))))
 ("C-x C-r" . revert-buffer)

 ("C-<tab>" . tab-to-tab-stop)

 ("M-<left>"  . (lambda () (interactive "^") (left-char 10)))
 ("M-<right>" . (lambda () (interactive "^") (right-char 10)))
 ("M-<up>"    . (lambda () (interactive "^") (previous-line 10)))
 ("M-<down>"  . (lambda () (interactive "^") (next-line 10)))

 ("C-M-<left>"  . windmove-left)
 ("C-M-<right>" . windmove-right)
 ("C-M-<up>"    . windmove-up)
 ("C-M-<down>"  . windmove-down)

 ;; The ‘^’ makes Emacs first call the function ‘handle-shift-selection’.
 ;; This is needed to properly handle S-C-<..>.
 ("C-<right>" . (lambda () (interactive "^") (forward-same-syntax)))
 ("C-<left>"  . (lambda () (interactive "^") (forward-same-syntax -1)))

 ;; ("M-<end>" . overwrite-mode)

 ("C-x k" . kill-this-buffer)
 ("C-x C-k" . kill-buffer)
 )

(defun my-fill-sentence ()
  "Apply 'fill-rigion' to the sentence at point."
  (interactive)
  ;; (push-mark)
  (if (use-region-p)
      (fill-region (region-beginning) (region-end))
    (let ((bounds (bounds-of-thing-at-point 'sentence)))
      (fill-region (car bounds) (cdr bounds)))))

(bind-keys
 ("M-q" . my-fill-sentence)
 ("M-Q" . fill-paragraph) ;; Originally M-q
 )

(defun my-delete-word (ARG)
  "Delete a block of characters.
With prefix argument ARG, do it ARG times if positive, or delete
backwards ARG times if negative."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-same-syntax ARG) (point)))))

(defun my-backward-delete-word (ARG)
  "Delete backwards a block of characters.
With prefix argument ARG, do it ARG times if positive, or delete
forwards ARG times if negative."
  (interactive "p")
  (my-delete-word (- ARG)))

(bind-keys
 ("C-<backspace>" . my-backward-delete-word)
 ("C-<delete>"    . my-delete-word)
 )


(defun my-fill-line ()
  "Fill the line, up to 'fill-column', with the char precedding point."
  (interactive)
  (let ((char (char-before)))
    (if (and (not (bolp)) char)
       (insert-char char (- fill-column (current-column))))))

;;; Useful default bindings:

;; "C-h B"               runs the command describe-personal-keybindings

;; "M-1 C-h c <key>"     insert the command describe-key-briefly

;; "M-s h ."             runs the command highlight-symbol-at-point
;; "M-s h u"             runs the command unhighlight-regexp

;; "C-x r m <name>"      runs the command bookmark-set
;; "C-x r b <name>"      runs the command bookmark-jump
;; "C-x r l"             runs the command bookmark-bmenu-list

;; "M-q"                 runs the command fill-paragraph
;; "C-x f"               runs the command set-fill-column

(global-unset-key (kbd "C-x C-z")) ;; Don't runs the command suspend-frame
                                   ;; (minimise under X)

;; ================================ PACKAGES ===================================

(use-package auto-complete
  :ensure t
  :hook (coq-mode . (lambda () (auto-complete-mode -1)))
  :config
  (ac-config-default)
  ;; (ac-set-trigger-key "TAB")
  (add-to-list 'ac-user-dictionary "fshaked@gmail.com")
  (add-to-list 'ac-user-dictionary "sflur@google.com")
  (setq ac-auto-show-menu 0.8)
  :custom
  (ac-use-menu-map t)
  ;; Popup after <n> chars
  (ac-auto-start 4)
  ;; :demand
  :bind (("C-<SPC>" . auto-complete)
         :map ac-completing-map
         ("<backtab>" . ac-previous)))

;; Enable the ivy completion interface
(use-package counsel :ensure t)
(use-package ivy
  :ensure t
  :diminish
  :config (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-on-del-error-function #'ignore "Don't close the minibuffer when pressing backspace.")
  :demand
  :bind (("<f6>" . ivy-resume)
         ("C-s" . swiper)
         ("C-S-s" . swiper-thing-at-point)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("M-y" . counsel-yank-pop)
         ("C-x b" . ivy-switch-buffer)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         ("C-S-v" . counsel-yank-pop)))



(use-package mode-line-bell
  :ensure t
  :config (mode-line-bell-mode))

;; Show bindings after pressing prefix
(use-package which-key
  :ensure t
  :commands (which-key-mode)
  :diminish
  :init (setq which-key-idle-delay 3)
  :hook (after-init . which-key-mode))

(defun my-flyspell-toggle ()
  "Toggle 'flyspell-mode' on/off, also when turned on check the buffer, and when turned off clear the markings."
  (interactive)
  (if flyspell-mode
      (progn (flyspell-mode 0) (flyspell-delete-all-overlays))
    (flyspell-mode 1)
    (flyspell-buffer)))

(use-package flyspell
  :ensure t
  :commands (flyspell-mode)
  :diminish "FS"
  :bind ("C-M-o" . my-flyspell-toggle))

(use-package flyspell-correct
  :ensure t
  :commands (flyspell-correct-wrapper)
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell-correct)

;; On the fly syntax checking
(use-package flycheck
  :ensure t
  :commands (flycheck-mode global-flycheck-mode)
  :diminish "FC"
  :hook (after-init . global-flycheck-mode)
  :bind ("C-c ! q" . flycheck-mode)
  )

(use-package move-text
  :ensure t
  :commands (move-text-down move-text-up)
  :bind (("C-S-<down>" . move-text-down)
         ("C-S-<up>" . move-text-up)))

(use-package undo-tree
  :ensure t
  :diminish
  :config
  (global-undo-tree-mode 1)
  (defalias 'redo 'undo-tree-redo)
  :demand
  :bind (("C-z" . undo)
         ("C-S-z" . redo)))

(use-package magit
  :ensure t
  :commands (magit magit-status)
  :bind ("C-x g" . magit-status))

(use-package define-word
  :ensure t
  :commands (define-word define-word-at-point)
  :bind ("C-c d" . define-word-at-point))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 t
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))
    ;; Close the treemacs window when visiting a file:
    (treemacs-define-RET-action 'file-node-open #'my-treemacs-visit-and-close)
    (treemacs-define-RET-action 'file-node-closed #'my-treemacs-visit-and-close)
    (treemacs-define-RET-action 'tag-node #'my-treemacs-visit-and-close)
    (treemacs-define-TAB-action 'tag-node #'my-treemacs-visit-and-close)
    )
  :bind
  (:map global-map
        ("C-x C-x"      . my-treemacs-toggle)
        ("C-x 1"        . treemacs-delete-other-windows)
        ;; ("M-0"       . treemacs-select-window)
        ;; ("C-x t t"   . treemacs)
        ;; ("C-x t B"   . treemacs-bookmark)
        ;; ("C-x t C-t" . treemacs-find-file)
        ;; ("C-x t M-t" . treemacs-find-tag)
        )
  (:map treemacs-mode-map
        ("f"       . treemacs-find-file)
        ("<next>"  . scroll-up-command)
        ("<prior>" . scroll-down-command)
        )
  )

(defun my-treemacs-toggle ()
  "Toggle visibility of the treemacs window."
  (interactive)
  (if (fboundp 'treemacs-with-toggle)
      (treemacs-with-toggle (treemacs))
    (treemacs)
    )
  )

(defun my-treemacs-visit-and-close (&optional ARG)
  "Run ‘treemacs-default-visit-action’ for the current button and hide treemacs.
A potential prefix ARG is passed on to the executed action, if possible."
  (interactive "P")
  (treemacs-visit-node-default ARG)
  (treemacs))

(use-package treemacs-magit
  :ensure t
  :after treemacs magit)

(defconst my-snippets-dir
  (expand-file-name
   "yas-snippets"
   (file-name-directory
    ;; Copied from ‘f-this-file’ from f.el.
    (cond
     (load-in-progress load-file-name)
     ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
      byte-compile-current-file)
     (:else (buffer-file-name)))))
  "My personal snippets folder: '<this-file-dir>/yas-snippets/'.")

(defun my-yas-expand ()
  "Expand snippet if there is a word at point, otherwise, use ivy to pick a snippet."
  (interactive)
  (if (word-at-point)
      ;; Then
      (yas-expand)
    ;; Else
    (ivy-yasnippet)))

(use-package yasnippet
  :ensure t
  :commands (yas-global-mode yas-minor-mode yas-expand)
  :diminish yas-minor-mode
  :config
  ;; Enable the global mode only after first use
  (yas-global-mode)
  ;; Add my personal snippets folder
  (add-to-list 'yas-snippet-dirs 'my-snippets-dir)
  (yas-load-directory my-snippets-dir t)

  ;; Don't use tab
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; Use this instead,
  :bind ("C-M-<SPC>" . my-yas-expand))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package ivy-yasnippet
  :ensure t
  :after (ivy yasnippet))

;; Remove trailing white spaces from modified lines.
(use-package ws-butler
  :ensure t
  :diminish
  :config
  (setq ws-butler-keep-whitespace-before-point nil)
  (ws-butler-global-mode)
  )

;; =========================== FILE TYPE SPECIFIC ==============================

(use-package proof-general
  :ensure t
  :init (setq proof-splash-enable nil))

;; Load company-coq when opening Coq files
(use-package company-coq
  :ensure t
  :after proof-general
  :hook (coq-mode . company-coq-mode)
  :custom (company-coq-features/prettify-symbols-in-terminals t "Enable prettify-symbols-mode by default"))

;; OCaml mode
(use-package tuareg
  :ensure t
  :commands (tuareg-mode)
  :mode ("\\.ml[ip]?\\'" . tuareg-mode)
  :interpreter (("ocamlrun" . tuareg-mode)
                ("ocaml" . tuareg-mode)))

(use-package flycheck-ocaml :ensure t)

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\(?:\\..*\\)?\\'")

(diminish 'eldoc-mode)

(use-package rustic
  :ensure t
  :commands rustic-mode
  :mode ("\\.rs\\'" . rustic-mode)
  ;; rust-mode is a system package so cann't be remove.  "If you have rust-mode
  ;; installed, ensure it is required before rustic since it has to be removed
  ;; from auto-mode-alist" :ensure t
  :init (require 'rust-mode nil 'noerror))

(defvar my-grip-port 8080
  "Next port to use by grip.")

(defun my-run-grip ()
  "Run grip (GitHub format markdown processor) in the background."
  (interactive)
  (let ((process (concat "grip-" (number-to-string my-grip-port)))
        (buffer  (concat "*grip-" (number-to-string my-grip-port) "*"))
        (url     (concat "127.0.0.1:" (number-to-string my-grip-port))))
    (progn (start-process process buffer "grip" "-b" (buffer-file-name) url)
           (setq my-grip-port (+ my-grip-port 1)))))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown") ; C-c C-c p
  (setq markdown-open-command 'my-run-grip)) ; C-c C-c o

;; ================================= THEME =====================================
;; Load the theme last so it will be immediately obvious if something went wrong

(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t)
  ;; Make the line numbers less obtrusive:
  (set-face-attribute 'line-number nil :background "#222d32")
  (set-face-attribute 'line-number nil :foreground "#545d62")
  (set-face-attribute 'line-number-current-line nil :background "#303e45")
  (set-face-attribute 'line-number-current-line nil :foreground "#8e9498"))

(use-package spaceline
  :ensure t
  :init
  (setq powerline-height 40)
  ;; alternate, arrow, arrow-fade, bar, box, brace, butt, chamfer, contour,
  ;; curve, rounded, roundstub, wave, zigzag, slant, utf-8
  (setq powerline-default-separator 'wave)
  :config
  (spaceline-emacs-theme)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-hud-off)
  ;; spaceline negates the setting of height below, hence we set it here
  ;; (set-face-attribute 'linum nil :height 110)
  ;; (set-face-attribute 'linum nil :foreground "#515B60")
  :custom
  (spaceline-minor-modes-separator " ")
  )

;; sail-mode
(let ((file "~/rems/sail/editors/sail-mode.el"))
  (if (file-exists-p file)
      (load-file file)))

;;; emacs.el ends here
