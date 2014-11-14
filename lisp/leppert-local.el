;; -*- emacs-lisp -*-

;; Use two spaces for tabs in JS
(setq-default js2-basic-offset 2)

(setq c-default-style "k&r"
      c-basic-offset 4)

;; tell emacs where to read abbrev definitions from
(setq abbrev-file-name
      "~/.emacs.d/leppert-local/abbrev_defs")

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
