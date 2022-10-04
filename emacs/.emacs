;;;; -*- mode: emacs-lisp -*-
;;;;
;;;; Emacs configuration file.
;;;;
;;;; <adolfopa@sdfeu.org>
;;;;

(set-face-font 'default "-V.R.-Px437 IBM DOS ISO8-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")

;;; Set up emacsclient server.

(server-start)

(defun apa--raise-frame-and-give-focus ()
  "Force Emacs frame to focus when edit requested by a client."
  (when window-system
    (raise-frame)
    (x-focus-frame (selected-frame))
    (set-mouse-pixel-position (selected-frame) 4 4)))

(add-hook 'server-switch-hook 'apa--raise-frame-and-give-focus)

;;; Configure general built in modes.

(column-number-mode 1)
(fringe-mode nil)
(global-hl-line-mode)
(global-linum-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode)
(tool-bar-mode -1)

;;; Set built in variable values.

(setq inhibit-splash-screen t
      inhibit-startup-message t
      org-agenda-files '("/ssh:adolfopa@sdfeu.org:tasks.org")
      org-priority-default ?B
      org-priority-highest ?A
      org-priority-lowest  ?D
      show-trailing-whitespace t
      sql-postgres-program (shell-command-to-string "echo -n $(which psql)"))

(setq-default indicate-empty-lines t)

;;; Set up package system.

(require 'package)

(setq package-enable-at-startup nil
      use-package-always-ensure t)

(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     t)

(package-initialize)

(package-install 'use-package)

(require 'use-package)

;;; Install and configure third party packages.

(use-package emms
  :config (progn
	    (require 'emms-setup)
	    (emms-all)
	    (setq emms-player-list '(emms-player-vlc))
	    (setq emms-source-file-default-directory "~/Music/")
	    (emms-add-directory-tree emms-source-file-default-directory)))

(use-package haskell-mode)

(use-package magit)

(use-package modus-themes
  :init (modus-themes-load-themes)
  :config (modus-themes-load-vivendi))

(use-package racket-mode)

(use-package slime
  :init (setq inferior-lisp-program "ros run"))

(use-package tuareg)
(use-package merlin
  :hook ((caml-mode tuareg-mode) . merlin-mode))

(use-package js2-mode)

(use-package elpher)

;;;; EOF
