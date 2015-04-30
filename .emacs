; auto c++mode to .h files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq-default ispell-program-name "aspell")

; fix split windows vertically issue
;(setq split-width-threshold 0)

; m-x compile scrolls automatically
(setq compilation-scroll-output 'first-error)

; line numbers
(global-linum-mode t)
(setq linum-format "%d ")

; ediff split
(setq ediff-split-window-function 'split-window-horizontally)

;fix stupid indent bugs
(setq c-default-style "linux"
      c-basic-offset 2)

;; Goto-line short-cut key
(global-set-key "\C-cg" 'goto-line)

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

;(setq whitespace-style '(face empty tabs lines-tail trailing))
;(global-whitespace-mode t)

;tab complete stuff
(require 'hippie-exp)
(setq hippie-expand-try-functions-list
     '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))
;         try-complete-file-name
;         try-complete-lisp-symbol))

(defun clever-hippie-tab (arg)
  "Ordinary tab or dabbrev"
  (interactive "*P")
  (cond
   ((and transient-mark-mode mark-active)
    (indent-region (region-beginning) (region-end) nil))
   ((and (eq (char-syntax (preceding-char)) ?w)
         (not (= (current-column) 0)))
    (hippie-expand arg))
   (t (indent-for-tab-command))))

(global-set-key (kbd "TAB") 'clever-hippie-tab)
(global-set-key (kbd "C-TAB") 'undo)

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

;make M-x rep do M-x replace-string
(defalias 'rep 'replace-string)

;make M-x gt do M-x goto-line
(defalias 'gt 'goto-line)

;make M-x bcf do M-x byte-compile-file
(defalias 'bcf 'byte-compile-file)

;make M-x reload reload the current file
(defalias 'reload 'revert-buffer)

(global-set-key (kbd "M-c")         'compile)
(global-set-key (kbd "M-n")         'next-error)
;(global-set-key [f4]               'kill-compilation)
;(global-set-key [f5]               'gud-next)
;(global-set-key [f10]              'gdb)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;define preferences for flycheck
(setq flycheck-gcc-language-standard "c++11")
(setq flycheck-clang-language-standard "c++11")
(setq flycheck-highlighting-mode 'lines)
(setq flycheck-cppcheck-checks "warning,information,performance")
(setq flycheck-check-syntax-automatically '(new-line save))

(defun flycheck-python-setup ()
  (flycheck-mode))
(add-hook 'python-mode-hook #'flycheck-python-setup)

(defun flycheck-cpp-setup ()
  (flycheck-mode))
(add-hook 'c++-mode-hook #'flycheck-cpp-setup)

(defun autopep8-format-buffer ()
  "Apply autopep8 to the current region or buffer"
  (interactive)
  (unless (region-active-p)
    (mark-whole-buffer))
  (shell-command-on-region
   (region-beginning) (region-end) ;; beginning and end of region or buffer
   "autopep8  -a -a -"             ;; command and parameters
   (current-buffer)                ;; output buffer
   t                               ;; replace?
   "*autopep8 errors*"             ;; name of the error buffer
   t))                             ;; show error buffer?


(load "/usr/share/emacs/site-lisp/clang-format-3.4/clang-format.el")
;(add-hook 'after-init-hook #'global-flycheck-mode)

;set up clang format and autopep8 to run on save
(defun do-style-hook () ""
  (if (eq major-mode 'c++-mode)
      (clang-format-buffer)
    (if (eq major-mode 'python-mode)
      (autopep8-format-buffer))))

(add-hook 'before-save-hook 'do-style-hook)
