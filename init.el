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


(global-unset-key (kbd "C-z"))

(define-key global-map (kbd "C-t") 'set-mark-command)
(define-key global-map (kbd "C-x C-f") 'helm-ls-git-ls)
(define-key global-map (kbd "C-x C-d") 'helm-find-files)
(define-key global-map (kbd "C-x C-j") 'helm-git-grep)
(global-set-key [remap kill-ring-save] 'easy-kill)
(define-key global-map (kbd "C-x C-t") 'find-file)
(define-key global-map (kbd "M-.") 'dumb-jump-go)

;; Window size can be changed with crl+shift+arrow by default
;; (global-set-key (kbd "<C-up>") 'shrink-window)
;; (global-set-key (kbd "<C-down>") 'enlarge-window)
;; (global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
;; (global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)



(global-set-key (kbd "M-x") 'helm-M-x)

(save-place-mode 1)

;; clip board
(setq select-enable-primary nil)
(setq select-enable-clipboard t)
;;(setq x-select-enable-clipboard t)



(setq scroll-conservatively 3)
(setq scroll-margin 10)

;; window move with shift+click
(windmove-default-keybindings)
(setq windmove-wrap-around t)


;; display size
(if (display-graphic-p)
    (progn
      (if (> 2000 (display-pixel-width))
          (defvar left-offset 1000)
        (defvar left-offset 2750))
      (setq initial-frame-alist
            '(
              (width . 100)
              (height . 53)
              (left . 850)
              (top . 30)))
      (setq inhibit-splash-screen t)
      (tool-bar-mode -1)))


;;; color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'vscode-dark-plus t)


;; todo
(defun my-check-fonts()
  (describe-font nil)  ;; shown in subbuffer (or minibuffer)
  (font-family-list)  ;; only shown in scratch buffer
  (message "type 'x-list-fonts <family>' to show XLFD names"))

;; coding用フォント
;; (firacode), roboto(日本語含む), input, liberation-mono(カクカクしてる), noto sans(日本語), adobe source code

;; (set-face-attribute 'default nil
;;                     :family "Menlo"
;;                     ;; :height 200
;; )
;; (set-fontset-font nil 'japanese-jisx0208
;;                   (font-spec :family "Hiragino Kaku Gothic ProN"))
;; (setq face-font-rescale-alist
;;       '((".*Hiragino Kaku Gothic ProN.*" . 1.2)))
;; (add-to-list 'default-frame-alist '(font . "-1ASC-Liberation Mono-bold-italic-normal-*-*-*-*-*-m-0-iso10646-1"))

(set-default-font "-1ASC-Liberation Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
;; (add-to-list 'default-frame-alist '(font . "Liberation Mono" ))

;; (set-face-attribute 'default nil :font "Liberation Mono" )
;; (set-frame-font "Liberation Mono" nil t)

;; garbage collection
(setq garbage-collection-messages t)
;; (setq gc-cons-threshold (* 20 1024 1024))  ;; ロードが遅いときはメモリの閾値が低すぎるのが原因かも


;; show line number
;;(global-display-line-numbers-mode)


(defun copy-file-path ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (kill-new filename)
    (message "%s" filename)))


;; (setq debug-on-error t)


;; neotree
(setq neo-smart-open t)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-vc-integration '(char))

;; neotree custom shortcuts. Needs counsel package for x command.
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(with-eval-after-load 'neotree
  (define-key neotree-mode-map (kbd "a") 'neotree-hidden-file-toggle)
  (define-key neotree-mode-map (kbd "i") 'neotree-quick-look)
  (define-key neotree-mode-map (kbd "j") 'neotree-next-line)
  (define-key neotree-mode-map (kbd "k") 'neotree-previous-line)
  (define-key neotree-mode-map (kbd "u") 'neotree-select-up-node)
  (define-key neotree-mode-map (kbd "w") 'neotree-copy-filepath-to-yank-ring)
  (define-key neotree-mode-map (kbd "x")
    (lambda ()
      (interactive)
      (counsel-locate-action-extern (neo-buffer--get-filename-current-line))))
  ;; (define-key neotree-mode-map (kbd "<enter>") 'neotree-enter-hide)
  (define-key neotree-mode-map (kbd "<left>") 'neotree-select-up-node)
  (define-key neotree-mode-map (kbd "<right>") 'neotree-change-root)
  (define-key neotree-mode-map (kbd "<tab>") 'neotree-quick-look))


;; find recent file
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)


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

;; migemo
(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))

;; Place dict file previously. automatically downloaded with `sudo apt install cmigemo`
;; (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict") ;; for Mac
(setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict") ;; for Ubuntu

(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(load-library "migemo")
(migemo-init)

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


;; org-mode
(setq org-startup-indented t)
(setq org-indent-mode-turns-on-hiding-stars nil)
(setq org-indent-indentation-per-level 4)

;; org-capture
(global-set-key (kbd "C-c c") 'org-capture)
(setq work-directory "~/memo/")
(setq memofile (concat work-directory "memo.org"))
(setq org-capture-templates
      '(
        ("m" "simple memo" entry (file+headline memofile "Memo") "\n* Heading\n%?%i\n")
        ("d" "date" entry (file+datetree memofile "Date Todo") "* %T %?" :empty-lines 1 :jump-to-captured 1)
       )
)


;; code jump : dumb-jump
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;;; git
(global-git-gutter-mode t)


(defun my-code-review-region (beg end)
  "Reformat the region(BEG,END) for code review and copy to clipborad."
  (interactive "r")
  (goto-char beg)
  (let* ((text (s-chomp (buffer-substring-no-properties beg end)))
         (branch (car (vc-git-branches)))
         (line-number (line-number-at-pos))
         (file (buffer-file-name))
         (path (replace-regexp-in-string "^.*branches/" ""
                                         (replace-regexp-in-string
                                          "^.*trunk/" "" file)))
         (relative-path (if (vc-git-registered file)
                            (file-relative-name path (vc-root-dir)) path)))
    (with-temp-buffer
       (insert text)
       (goto-char (point-min))
       (while (re-search-forward "^" nil t)
         (replace-match "| " nil nil))
       (goto-char (point-min))
       (insert (format "+---[%s%s:%s]\n" (if branch (format "Git:%s " branch) "") relative-path line-number))
       (goto-char (point-max))
       (insert "\n+---\n")
       (kill-region (point-min) (point-max))
       (message "Copied"))))


;; yasnippet
;; To install snippets `M-x package-install yasnippet-snippets`
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :bind (:map yas-minor-mode-map
              ("C-x y i" . yas-insert-snippet)
              ("C-x i n" . yas-new-snippet)
              ("C-x i v" . yas-visit-snippet-file)
              ("C-x i l" . yas-describe-tables)
              ("C-x i g" . yas-reload-all))
  :config
  (yas-global-mode 1)
  (setq yas-prompt-functions '(yas-ido-prompt))
  )

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

;; https://github.com/pashky/restclient.el
(require 'restclient)



;; ===== Python

;; (defun my-load-jedi ()
;;   (setq jedi:server-command (list (executable-find "jediepcserver")))
;;   (add-hook 'python-mode-hook 'jedi:setup)
;;   (setq jedi:complete-on-dot t)
;;   (add-hook 'python-mode-hook
;;             (lambda()
;;               (elpy-enable)
;;               (setq elpy-rpc-python-command "python3")
;;               (defun my-compile ()
;;                 "Use compile to run python programs"
;;                 (interactive)
;;                 (compile (concat "python3 " (buffer-name)))
;;                 ))))

;;; pip install jedi && pip install epc
(defun my-load-jedi ()
  (interactive)
  (require 'epc)
  (require 'jedi-core)
  (require 'company-jedi)
  (jedi:setup)
  ;;(setq jedi:setup-keys t);これを入れると手動<C-tab>で立ち上げる

  (company-mode)
  (add-to-list 'company-backends 'company-jedi) ; backendに追加
  (define-key jedi-mode-map (kbd "C-i") 'jedi:goto-definition)
  (define-key jedi-mode-map (kbd "C-S-j") 'jedi:goto-definition-pop-marker)
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t))

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-C C-C") 'my-compile)
            (setq compilation-scroll-output t)
            (my-load-jedi)))


;; ====== js2
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)) ;; 重い？ fly-check 補完はcompany-tern

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yasnippet-snippets yasnippet elpy company-jedi counsel neotree xclip lsp-ui lsp-mode company-go go-mode magit edit-server restclient helm-swoop helm-migemo migemo dumb-jump swift-mode color-moccur jedi git-gutter highlight-symbol helm-tramp smooth-scrolling company-irony irony easy-kill helm-ls-git helm-git-grep helm-ag web-mode js2-mode company-tern))))
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
;;  (setq indent-tabs-mode t)
;;  (company-mode)
)
(add-hook 'c-mode-hook 'my-c-mode-hook)


(require 'irony)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)


;; edit-server : for chrome extension
;; (use-package edit-server
;;   :ensure t
;;   :commands edit-server-start
;;   :init (if after-init-time
;;               (edit-server-start)
;;             (add-hook 'after-init-hook
;;                       #'(lambda() (edit-server-start))))
;;   :config (setq edit-server-new-frame-alist
;;                 '((name . "Edit with Emacs FRAME")
;;                   (top . 200)
;;                   (left . 200)
;;                   (width . 80)
;;                   (height . 25)
;;                   (minibuffer . t)
;;                   (menu-bar-lines . t)
;;                   (window-system . x))))


;; golang
(add-hook 'go-mode-hook
  (lambda ()
    (setq-default)
    (setq tab-width 4)
    (setq standard-indent 4)
    (setq indent-tabs-mode nil)))

(setq eww-search-prefix "http://www.google.co.jp/search?q=")
