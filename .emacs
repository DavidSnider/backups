(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(require 'better-defaults)

; get rid of useless backups I never use and just remove
(setq make-backup-files nil)

;tell me where I am in a line
(column-number-mode 1)

;set return go to newline and indent
(global-set-key (kbd "RET") 'newline-and-indent)

; auto c++mode to .h and .ipp files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))

; auto jinja2-mode to .html files
(add-to-list 'auto-mode-alist '("\\.html\\'" . jinja2-mode))

(setq-default ispell-program-name "aspell")

; m-x compile scrolls automatically
(setq compilation-scroll-output 'first-error)

;for inline googling
(require 'google-this)
(google-this-mode 1)
(global-set-key (kbd "C-c g") 'google-this)

; start debugger if error occurs
; (setq debug-on-error 1)

; line numbers
(global-linum-mode t)
(setq linum-format "%d ")
; ediff split
(setq ediff-split-window-function 'split-window-horizontally)

;fix stupid indent bugs
(setq c-default-style "linux" c-basic-offset 2)
(setq python-indent-offset 4)

;; comment region to C-q
(global-set-key  "\C-q" 'comment-or-uncomment-region)

;set tabs correctly
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

; trailing whitespace
(setq-default show-trailing-whitespace t)
; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; highlight long lines
(require 'whitespace)
(setq whitespace-line-column 80) ; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

; move to left windnow
(global-set-key (kbd "M-<left>") 'windmove-left)
; move to right window
(global-set-key (kbd "M-<right>") 'windmove-right)
; move to upper window
(global-set-key (kbd "M-<up>") 'windmove-up)
; move to downer window
(global-set-key (kbd "M-<down>") 'windmove-down)

; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

(defalias 'rep 'replace-string)
(defalias 'gt 'goto-line)
(defalias 'bcf 'byte-compile-file)
(defalias 'reload 'revert-buffer)
(defalias 'lf 'load-file)


; reload the .emacs
(defun lfe () ""
  (interactive)
  (load-file "/home/david/.emacs"))

(global-set-key (kbd "M-c")         'compile)
(global-set-key (kbd "M-n")         'next-error)
(global-set-key (kbd "C-c n") 'flycheck-next-error)
;(global-set-key [f4]               'kill-compilation)
;(global-set-key [f5]               'gud-next)
;(global-set-key [f10]              'gdb)

;automatically pair brackets
(require 'autopair)
(autopair-global-mode 1)
;(setq autopair-autowrap t)

; completion in emacs shell
(require 'bash-completion)
(bash-completion-setup)

;;auto-complete-stuff
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(require 'company-c-headers)
; M-x customize-group RET company RET to tool with colors
;; set to pretty colors that are easy to see
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-template-field ((t (:background nil :foreground "steelblue"))))
 '(company-tooltip ((t (:foreground "cyan"))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white")))))

;(require 'yasnippet)

(defun indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
      ;(company-complete-common)
      (company-complete)
    (indent-for-tab-command)))
(global-set-key (kbd "TAB") 'indent-or-complete)

(add-to-list 'load-path "~/.emacs.d/lint381/")
(require 'lint381)

;define preferences for flycheck
(setq flycheck-highlighting-mode 'lines)
(setq flycheck-check-syntax-automatically '(new-line save))
(setq flycheck-cppcheck-checks "warning,information,performance")

(defun flycheck-python-setup ()
  (flycheck-mode)
  (defvar flycheck-checker 'python-pylint))

(defun c-cpp-setup()
  (flycheck-mode)
  ;(flyspell-prog-mode)
  (defvar company-clang-arguments)
  (defvar flycheck-clang-language-standard)
  (make-local-variable 'company-clang-arguments)
  (defvar flycheck-checker 'c/c++-clang)
  (add-to-list 'company-backends 'company-c-headers)
  ;(add-to-list 'company-backends 'company-yasnippet)
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/5/"))

(defun c-setup ()
  (c-cpp-setup)
  (setq flycheck-clang-language-standard "c99")
  (setq company-clang-arguments (list "-std=c99"))
  (setq flycheck-lint381-language "c"))

(defun cpp-setup ()
  (c-cpp-setup)
  (setq flycheck-clang-language-standard "c++14")
  (setq company-clang-arguments (list "-std=c++14"))
  (defvar flycheck-cppcheck-language-standard)
  (setq flycheck-cppcheck-language-standard "c++11"))


(add-hook 'c++-mode-hook #'cpp-setup)
(add-hook 'c-mode-hook #'c-setup)
(add-hook 'python-mode-hook #'flycheck-python-setup)

(require 'clang-format)

(require 'py-autopep8)
(setq py-autopep8-options '("-a" "-a"))
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;set up clang format to run on save
(defun do-style-hook () ""
  (if (or (eq major-mode 'c++-mode)
          (eq major-mode 'c-mode))
      (clang-format-buffer)))

(add-hook 'before-save-hook #'do-style-hook)

(require 'jinja2-mode)
