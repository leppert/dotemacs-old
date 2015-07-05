;; -*- emacs-lisp -*-

;;; Code:

;; Config vars that shouldn't be checked into VCS
(load "~/.emacs.d/leppert-local/private.el")

;; Load various custom functions
(load "~/.emacs.d/leppert-local/functions.el")

;; tell emacs where to read abbrev definitions from
(setq abbrev-file-name
      "~/.emacs.d/leppert-local/abbrev_defs")

;; Enable pallet-mode so that newly installed
;; packages are written to the Cask file
(require 'pallet)
(pallet-mode)

;; web-mode
(defun web-mode-customizations ()
  (setq web-mode-markup-indent-offset 2)
)
(add-hook 'web-mode-hook  'web-mode-customizations)

;; Javascript / JS
(setq-default js2-basic-offset 2)
;; http://blog.binchen.org/posts/why-emacs-is-better-editor.html
(add-hook 'js2-mode-hook 'js2-imenu-extras-mode)

(setq c-default-style "k&r"
      c-basic-offset 4)

(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

(require 'company-tern)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-tern))

;; Coffeescript

(add-to-list 'load-path "~/.emacs.d/leppert-local/ac-coffee/")
(require 'ac-coffee)

(require 'flymake-coffee)
(add-hook 'coffee-mode-hook 'flymake-coffee-load)
(setq flymake-coffee-coffeelint-configuration-file (expand-file-name "leppert-local/coffeelint.json"))

(require 'mustache-mode)

(require 'scss-mode)
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(setq scss-compile-at-save nil)

(require 'textmate)
(textmate-mode)

(require 'rainbow-mode)
(add-to-list 'auto-mode-alist '("\\.s?css$" . rainbow-mode)) 

;; From Jack's dotemacs
(require 'smartparens)
(smartparens-global-mode t)
(sp-use-paredit-bindings)
(smartparens-strict-mode)

;; fights with my preferred navigation keys
(dolist (binding (list (kbd "M-<up>") (kbd "M-<down>") (kbd "C-M-<left>") (kbd "C-M-<right>")))
  (define-key smartparens-mode-map binding nil))

;; make it work like paredit
(define-key smartparens-mode-map (kbd "C-k") 'sp-kill-sexp)

;; smartedit all the parens
(dolist (mode '(scheme emacs-lisp lisp clojure clojurescript))
    (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
              'smartparens-mode))

(skewer-setup)

;; via: http://www.emacswiki.org/emacs/RevertBuffer
(defun revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "Refreshed open files.") )

(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)

(require 'graphene)

;; for the silver surfer highlighting
(setq ag-highlight-search t)
;; Bind search to super shift f
(global-set-key (kbd "s-F") 'ag)

;; vim style kill line
;; via: http://stackoverflow.com/a/2173393/313561
(defun kill-current-line (&optional n)
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (let ((kill-whole-line t))
      (kill-line n))))

;; Multiple Cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Python
(elpy-enable)
(load "~/.emacs.d/leppert-local/pony-mode.el")
(define-key pony-minor-mode-map (kbd "C-c C-c") 'pony-test)

;; Ruby
(require 'rvm)
(rvm-use-default)

(require 'rinari)
(define-key rinari-minor-mode-map (kbd "C-c C-c") 'rinari-test)

(require 'rspec-mode)
(define-key rspec-mode-map (kbd "C-c C-c") 'rspec-verify-single)

;; PHP
(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;; Vagrant TRAMP
(eval-after-load 'tramp
  '(vagrant-tramp-enable))

(provide 'leppert-local)
;;; leppert-local.el ends here
