;;; scheme-category-calculus.el --- Emacs configuration for Scheme Category Calculus project

;;; Commentary:
;; Project-specific Emacs configuration for Scheme development
;; with Geiser, Guile, org-mode, TRAMP, and Paredit support

;;; Code:

;; Package setup
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Install required packages if not present
(defvar scheme-category-calculus-packages
  '(geiser-guile paredit org tramp))

(dolist (pkg scheme-category-calculus-packages)
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)))

;; Geiser configuration for Guile
(require 'geiser-guile)
(setq geiser-guile-binary "guile3")
(setq geiser-active-implementations '(guile))
(setq geiser-default-implementation 'guile)

;; Configure Geiser REPL
(setq geiser-repl-query-on-kill-p nil)
(setq geiser-repl-history-filename
      (expand-file-name ".geiser-history" (getenv "PROJECT_ROOT")))

;; Add project paths to Guile load path
(when (getenv "PROJECT_ROOT")
  (setq geiser-guile-load-path
        (list (expand-file-name "src" (getenv "PROJECT_ROOT"))
              (expand-file-name "examples" (getenv "PROJECT_ROOT")))))

;; Paredit configuration
(require 'paredit)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook 'geiser-repl-mode-hook #'enable-paredit-mode)

;; Org-mode configuration for literate programming
(require 'org)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((scheme . t)))

;; Set default org-babel scheme implementation
(setq org-babel-scheme-implementation 'guile)

;; TRAMP configuration for remote development
(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-persistency-file-name
      (expand-file-name ".tramp-history" (getenv "PROJECT_ROOT")))

;; Scheme mode configuration
(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.ss\\'" . scheme-mode))

;; Custom key bindings
(global-set-key (kbd "C-c C-z") 'geiser-repl)
(global-set-key (kbd "C-c C-e") 'geiser-eval-last-sexp)
(global-set-key (kbd "C-c C-r") 'geiser-eval-region)
(global-set-key (kbd "C-c C-k") 'geiser-compile-current-buffer)

;; Project-specific settings
(when (getenv "PROJECT_ROOT")
  (setq default-directory (getenv "PROJECT_ROOT")))

;; Enable electric pair mode for matching parentheses
(electric-pair-mode 1)
(setq electric-pair-preserve-balance t)

;; Highlight matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Line numbers and column indicator
(global-display-line-numbers-mode 1)
(column-number-mode 1)

;; Indent configuration for Scheme
(setq scheme-indent-function 'scheme-smart-indent-function)

;; Start Geiser REPL automatically
(add-hook 'scheme-mode-hook 'geiser-mode)

(provide 'scheme-category-calculus)
;;; scheme-category-calculus.el ends here