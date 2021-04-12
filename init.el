(require 'package)

;; backup
(setq make-backup-files nil)
(setq auto-save-file-name-transforms
  `((".*", (expand-file-name "~/.emacs.d/backup/") t)))


;;; Packages
(setq load-prefer-newer t) ;; Please don't load outdated byte code
(setq package-archives
      ;; Package archives, the usual suspects
      '(("GNU ELPA"   . "http://elpa.gnu.org/packages/")
        ("MEL Stable" . "https://stable.melpa.org/packages/")
        ("MEL"        . "https://melpa.org/packages/")))
(setq package-archive-priorities
      ;; Prefer MELPA Stable over GNU over MELPA.
      '(("MEL Stable" . 10)
        ("GNU ELPA"   . 5)
        ("MEL"        . 0)))

(package-initialize)

(define-key global-map (kbd "C-t") 'set-mark-command)
(define-key global-map (kbd "C-x C-f") 'helm-ls-git-ls)
(define-key global-map (kbd "C-x C-d") 'helm-find-files)
(define-key global-map (kbd "C-x C-j") 'helm-git-grep)
(global-set-key [remap kill-ring-save] 'easy-kill)
(define-key global-map (kbd "C-x C-t") 'find-file)


(save-place-mode 1)

;; show line number
;;(global-display-line-numbers-mode)

(setq scroll-conservatively 3)
(setq scroll-margin 10)

;; window move with shift+click
(windmove-default-keybindings)
(setq windmove-wrap-around t)


;; find recent file
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;; color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'vscode-dark-plus t)

;;; auto highlight
(add-hook 'prog-mode-hook 'highlight-symbol-mode)
;;; move symbol with M-p/M-n
(add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)

;;; search
(require 'helm-ag)

(setq helm-ag-base-command "rg --vimgrep --no-heading -i") ;; use ripgrep

;;(setq helm-ag-base-command "grep -rin") ;; use grep in case ripgrep isn't available

(setq helm-ag-insert-at-point 'symbol)

(defun my-helm-ag-set-option (arg)
  "Set ag options with ARG."
  (interactive "Mset rg options: ")
  (setq helm-ag-command-option arg))


;;; list the search result
;; (when (require 'color-moccur nil t)
;;   (define-key global-map (kbd "M-o") 'occur-by-moccur)
;;   (setq moccur-split-word t)
;;   (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store")
;;   (add-to-list 'dmoccur-exclusion-mask "^#.+#$"))

;; helm-occur
(define-key isearch-mode-map (kbd "M-o") 'helm-occur-from-isearch);; helm-swoop https://github.com/emacsorphanage/helm-swoop

;; default editor setting

; 対応する括弧を表示
(show-paren-mode t)
(setq show-paren-delay 0)

;; 全角スペースを強制表示する
(require 'whitespace)
(global-whitespace-mode 1)
(setq whitespace-style '(face
                         trailing
                         tabs
                         spaces
                         empty
                         space-mark
                         tab-mark
                         ))
(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])
        (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
(setq whitespace-space-regexp "\\(\u3000+\\)")

;; not use tab, but space
;;(setq-default indent-tabs-mode t)
;; tab = space * 4
;;(setq-default tab-width 4)
(setq-default tab-width 4 indent-tabs-mode nil)

;;; git
(global-git-gutter-mode t)


;;; company

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-tern)
  (add-to-list 'company-backends 'company-irony))

(require 'company)

;;(company-mode)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'company-mode-hook (lambda ()
                               (setq company-idle-delay 0.1)
                               (setq company-minimum-prefix-length 1))
)

(require 'company-tern)

(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
;;                           (company-mode)
                           (tern-mode)
                           (setq js2-basic-offset 4)))

;; ===== Python

;; (require 'epc)
;; (require 'auto-complete-config)
;; (require 'python)
;; (setenv "PYTHONPATH" "~/.virtualenvs/testing/lib/python3.8/site-packages/")
(setq jedi:server-command (list (executable-find "jediepcserver"))) 
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook (lambda()
                              (defun my-compile ()
                                "Use compile to run python programs"
                                (interactive)
                                (compile (concat "python " (buffer-name))))
                              (setq compilation-scroll-output t)
                              (local-set-key (kbd "C-C C-C") 'my-compile)
                              ))


;; ====== js2
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (swift-mode color-moccur jedi git-gutter highlight-symbol helm-tramp smooth-scrolling company-irony irony easy-kill helm-ls-git helm-git-grep helm-ag web-mode js2-mode company-tern))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ====== web-mode

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-engines-alist
'(("php"    . "\\.phtml\\'")
  ("blade"  . "\\.blade\\.")))

(defun my-web-mode-hook ()
  "Hooks for Web mode."

  ;; インデント設定
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)

  ;; 要素のハイライト
  (setq web-mode-enable-current-element-highlight t)

  ;; フォントの配色
  (set-face-attribute 'web-mode-doctype-face nil :foreground "Pink3")
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "LightGray")
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "Blue")
  (set-face-attribute 'web-mode-html-attr-value-face nil :foreground "orange")
  (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "light blue")

  ;; タグを自動閉じを有効に
  (setq web-mode-auto-close-style 1)
  (setq web-mode-tag-auto-close-style t)
  ;; タグを自動で閉じる
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
)
(add-hook 'web-mode-hook 'my-web-mode-hook)


;;; C, C++

(defun my-c-mode-hook ()
;;  (setq c-mode-markup-indent-offset 10)
  ;;  (setq tab-width 10))
  (setq c-basic-offset 4)
  (setq indent-tabs-mode t)
;;  (company-mode)
)
(add-hook 'c-mode-hook 'my-c-mode-hook)


(require 'irony)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
