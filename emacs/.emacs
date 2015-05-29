;;;
;;; General configuration
;;;

(load "~/.emacs.d/machine-settings.el")
(load "~/.emacs.d/packages.el")

(require 'cl)

(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(set-default 'cursor-type 'box)

(server-start)

(add-to-list 'exec-path "/usr/local/bin")

(show-paren-mode 1)

(column-number-mode 1)

(fringe-mode 1)

(setq fill-column 80)

(global-set-key (kbd "C-<tab>") 'completion-at-point)

(define-key global-map (kbd "RET") 'newline-and-indent)

(add-hook 'prog-mode-hook (lambda () (linum-mode 1)))

(setq-default indent-tabs-mode nil)

(setq-default show-trailing-whitespace t)

;;;
;;; Themes
;;;

(setq custom-theme-directory "~/.emacs.d/local/themes")
(setq custom-safe-themes t)

(load-theme 'blackboard)

;;;
;;; Package system
;;;

(require 'package)

(add-to-list 'package-archives 
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

;;;
;;; Modes
;;;

;; EMMS

(when nil
  (require 'emms-setup)

  (emms-standard)
  (emms-default-players)
  (emms-add-directory-tree "~/Music")
  (emms-shuffle)

  (add-hook 'emms-mode (lambda () (setq emms-info-report-each-num-tracks 0))))

;; Shell

(setq explicit-shell-file-name zsh-program-name)
(setenv "SHELL" explicit-shell-file-name)

;; Ansi Term / Multi Term

(require 'term)
(require 'multi-term)

(setq system-uses-terminfo nil)

(global-set-key (kbd "C-c C-t") 'multi-term)

(defun setup-multi-term-keys ()
  (setq term-bind-key-alist
	'(("C-c C-c" . term-interrupt-subjob)
	  ("C-c C-e" . term-send-esc)
	  ("C-p" . previous-line)
	  ("C-n" . next-line)
	  ("C-s" . isearch-forward)
	  ("C-r" . isearch-backward)
	  ("C-m" . term-send-return)
	  ("C-y" . term-paste)
	  ("M-f" . term-send-forward-word)
	  ("M-b" . term-send-backward-word)
	  ("M-o" . term-send-backspace)
	  ("M-p" . term-send-up)
	  ("M-n" . term-send-down)
	  ("M-M" . term-send-forward-kill-word)
	  ("M-N" . term-send-backward-kill-word)
	  ("M-r" . term-send-reverse-search-history)
	  ("M-," . term-send-raw)
	  ("M-." . comint-dynamic-complete)
	  ("M-DEL" . term-send-backward-kill-word)
	  ("C-c n" . multi-term-next)
	  ("C-c p" . multi-term-prev))))

(add-hook 'term-mode-hook
	  (lambda ()
	    (yas-minor-mode -1)))

(add-hook 'term-mode-hook 'setup-multi-term-keys)

;; MMM

(require 'mmm-mode)

(setq mmm-global-mode 'maybe)
(set-face-background 'mmm-default-submode-face "#2C3041")

;; Powerline

(unless (eq system-type 'darwin)
  (require 'powerline)

  (set-face-attribute 'mode-line nil
                      :foreground "Black"
                      :background "DarkOrange"
                      :box nil)

  (setq powerline-arrow-shape 'diagonal)

  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (concat
                    (powerline-rmw 'left nil)
                    (powerline-buffer-id 'left nil powerline-color1)
                    ((point)owerline-minor-modes 'left powerline-color1)
                    (powerline-narrow 'left powerline-color1 powerline-color2)
                    (powerline-vc 'center powerline-color2)
                    (powerline-make-fill powerline-color2)
                    (powerline-row 'right powerline-color1 powerline-color2)
                    (powerline-make-text ":" powerline-color1)
                    (powerline-column 'right powerline-color1)
                    (powerline-percent 'right nil powerline-color1)
                    (powerline-make-text "  " nil)))))

  (add-hook 'after-init-hook 'powerline-default-theme))

;; IDO mode

(require 'ido)

(ido-mode t)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; Yasnippet mode

(require 'yasnippet)

(yas-global-mode 1)

;; Magit

(global-set-key (kbd "C-c C-g") 'magit-status)

(setq magit-last-seen-setup-instructions "1.4.0")

;; Slime

(setq inferior-lisp-program sbcl-program-name)

;; Org-Mode

(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
	("STARTED" . "yellow")))

(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)

;; SML-Mode

(require 'smie)

(defun custom-sml-rules (orig kind token)
  (pcase (cons kind token)
    (`(:before . "d=")
     (if (smie-rule-parent-p "structure" "signature" "functor") 2
       (funcall orig kind token)))
    (`(:after . "struct") 2)
    (_ (funcall orig kind token))))

(add-hook 'sml-mode-hook
	  (lambda ()
	    (add-function :around smie-rules-function #'custom-sml-rules)))

;; Haskell-Mode

(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(custom-set-variables
 '(haskell-mode-hook (quote (turn-on-haskell-indent interactive-haskell-mode)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote ghci)))

;;;
;;; Custom functions
;;;

(defun reorder-exec-path ()
  "Reorder the values on exec-path so that the Emacs directory
takes precedence over the rest."
  (labels ((emacs-binary-dir-p (s)
			       (string-prefix-p "/Applications/Emacs.app" s))
	   (find-matching (p xs)
			  (loop for x in xs
				when (funcall p x)
				collect x)))
    (setq exec-path
	  (append (find-matching #'emacs-binary-dir-p exec-path)
		  (find-matching #'(lambda (x)
				     (not (emacs-binary-dir-p x)))
				 exec-path)))))

;; On Mac OS X, override the execution path so that Emacs finds the
;; correct `emacsclient' binary.
(when (eq system-type 'darwin)
  (reorder-exec-path))
