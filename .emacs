;;; .emacs --- primary Emacs configuration

;;; Commentary:

;; Y'know, try to follow the Elisp conventions appendix. (~C-h R elisp RET g Tips RET~).
;;
;; Use elisp-autofmt-buffer to make life easier
;;
;; ~use-package~ statements should follow this order:
;;
;; - ~:load_path~
;; - ~:ensure~
;; - ~:requires~
;; - ~:after~
;; - ~:demand~
;; - ~:defer~
;; - ~:init~
;; - ~:custom~
;; - ~:bind~
;; - ~:hook~
;; - ~:config~

;;; Code:

;;; Order-Explicit Global Stuff

;;;; Put ~debug-on-*~ Calls Here

;;;; Enable Packages and Package Repos

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;;; Package Configuration

(setq package-install-upgrade-built-in t)

(setq package-archive-priorities '(("gnu" . 30) ("nongnu" . 20) ("melpa" . 10)))

;;;; Initialize Packages

(package-initialize)

;;;; Misc. Customize

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;;; Misc. Global Requires

(require 'use-package)

;;; Appearance

;;;; Theme

(use-package custom :custom (custom-enabled-themes '(wombat)))

;;;; Make Emacs Background /Slightly/ Transparent

(set-frame-parameter (selected-frame) 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))

;;;; Dark Mode

(use-package emacs :custom (frame-background-mode 'dark))

;;;; Default Font

(add-to-list 'default-frame-alist '(font . "Source Code Pro-11"))

;;;; Alternate Default Font Just For Emoji

(let ((blobmoji-emoji
       '(?ℹ ; Letterlike Symbols (select sub-ranges)
         ?⌚ ?⌛ (?⏩ . ?⏳) (?⏸ . ?⏾) ; Miscellaneous Technical (select sub-ranges)
         (?☀ . ?⛿) ; Miscellaneous Symbols (entire block)
         (?✀ . ?➿) ; Dingbats (entire block)
         ?⭐ ; Miscellaneous Symbols and Arrows (select sub-ranges)
         (?🀀 . ?🀫) ; Mahjong Tiles (entire block)
         (?🀰 . ?🂓) ; Domino Tile (entire block)
         (?🂠 . ?🃵) ; Playing Cards (entire block)
         (?🅰 . ?🆚) ; Enclosed Alphanumeric Supplement (select sub-ranges)
         ?🈁 ?🈂 ?🈚 ?🈯 (?🈲 . ?🈺) ?🉐 ?🉑 ; Enclosed Ideograph Supplement (select sub-ranges)
         (?🌀 . ?🗿) ; Miscellaneous Symbols and Pictures (entire block)
         (?😀 . ?🙏) ; Emoticons (entire block)
         (?🚀 . ?🛿) ; Transport and Map Symbols (entire block)
         (?🟠 . ?🟫) ; Geometric Shapes Extended (select ranges)
         (?🤀 . ?🧿) ; Supplemental Symbols and Pictographs (entire block)
         (?🨀 . ?🩯) ; Chess Symbols (entire block)
         (?🩰 . ?🫿)))) ; Symbols and Pictographs Extended-A (entire block)
  (dolist (emoji blobmoji-emoji)
    (set-fontset-font t emoji "Blobmoji" nil)))

;;;; Disable Various Annoying UI Elements

(use-package
 emacs
 :custom (inhibit-startup-screen t) (scroll-bar-mode nil) (tool-bar-mode nil) (menu-bar-mode nil) (use-dialog-box nil))

;;; Configuring Editor Behaviors

;;;;; Enable *case-region

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;;;; Punctuation Style

(use-package
 emacs
 :custom (sentence-end-double-space nil) (require-final-newline t) (indent-tabs-mode nil) (fill-column 120))

;;;;; Delete Trailing Whitespace When Saving

(defvar mememe/delete-trailing-whitespace-exempt-modes '(python-mode yaml-mode)
  "Major modes to exempt from `mememe/delete-trailing-whitespace'")

;;;###autoload
(defun mememe/delete-trailing-whitespace-unless-exempt ()
  "Delete trailing whitespace, exempting any major modes defined in
`mememe/delete-trailing-whitespace-exempt-modes'"
  (interactive)
  (if (and (not (member major-mode mememe/delete-trailing-whitespace-exempt-modes)) (not (minibufferp)))
      (delete-trailing-whitespace)))

(use-package emacs :hook (before-save . mememe/delete-trailing-whitespace-unless-exempt))

;;;;; Trash Files

(use-package emacs :custom (delete-by-moving-to-trash t))

;;; Misc. Global Keybinds

;; 'H' here is the "hyper" key, the next in the logical progression from Meta to Super. It's defined in X for historical
;; reasons (Lisp machines, etc.) but AFAICT nobody has ever sold a PC keyboard with one. Not commercially, anyway.
;;
;; Though it isn't officially reserved for user keybinds -- only ~C-c letter~ is -- /I've/ never seen an Emacs package
;; that uses them, so it's pretty safe for user keybinds and (potentially) more ergonomic.
;;
;; If you aren't me, you'll need to modify your keyboard layout in order to bind something to it. Personally, as an X
;; user, I've got an ~.Xmodmap~ that sets my Caps Lock key to ~Control_L~ and my left Control key to ~Hyper_L~. Nobody
;; *needs* Caps Lock, that's what ~C-x C-u~ is for!
;;
;; (Actually, for the rare times that I *do* need to toggle Caps Lock -- like sometimes a script will mess up my
;; modifier state and I need to turn it back off -- I've bound the Menu key to Caps Lock. Nobody needs Caps Lock, but
;; *nobody* needs Menu.)

;;;;; Replace Yes or No With Y or N
;;
;; A little sketchy, I admit

(fset 'yes-or-no-p 'y-or-n-p)

;;;; General

;;;###autoload
(defun mememe/insert-shrug ()
  (interactive)
  (insert "¯\\_(ツ)_/¯"))

(use-package emacs :bind (("H-s" . mememe/insert-shrug) ("H-r" . query-replace-regexp)))

;;;; "Better" Versions of Defaults

;;;;; ibuffer Instead of list-buffers

(use-package ibuffer :bind ("C-x C-b" . ibuffer))

;;;; "Missing" Opposites for Defaults
;;
;; All generally just the previous keybind + Shift

;;;###autoload
(defun mememe/unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;;;###autoload
(defun mememe/yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))

;;;###autoload
(defun mememe/deactive-mark (arg)
  (interactive "p")
  (deactivate-mark arg))

(use-package
 emacs
 :bind (("M-Q" . mememe/unfill-paragraph) ("M-Y" . mememe/yank-pop-forwards) ("C-S-SPC" . mememe/deactive-mark)))

;;; Global Minor Modes, Global Configuration for Local Minor Modes

;;;; autorevert

(use-package autorevert :custom (global-auto-revert-mode t))

;;;; column-number-mode

(use-package simple :custom (column-number-mode t))

;;;; company

(use-package
 company
 :ensure t
 :custom (company-idle-delay 0.2)) ; how long to wait until popup

;;;; display-fill-column-indicator

(use-package display-fill-column-indicator :hook (prog-mode . display-fill-column-indicator-mode))

;;;; display-line-numbers-mode

(defvar mememe/display-line-numbers-exempt-modes
  '(vterm-mode
    eshell-mode
    shell-mode
    term-mode
    ansi-term-mode
    inferior-python-mode
    fireplace-mode
    image-mode
    dun-mode
    magit-status-mode
    dired-mode
    tetris-mode
    rustic-compilation-mode)
  "Major modes to exempt from `mememe/display-line-numbers-mode'")

(defun mememe/display-line-numbers-unless-exempt (original-fun &rest args)
  "Turn on line numbers, exempting any major modes defined in
`mememe/display-line-numbers-exempt-modes'"
  (if (and (not (member major-mode mememe/display-line-numbers-exempt-modes)) (not (minibufferp)))
      (apply original-fun args)))

(use-package
 display-line-numbers
 :custom (global-display-line-numbers-mode t)
 :config (advice-add 'display-line-numbers--turn-on :around 'mememe/display-line-numbers-unless-exempt))

;;;; flycheck

(use-package flycheck :ensure t :defer t)

;;;; flyspell

(use-package
 flyspell
 :ensure t
 :defer t
 :hook (((prog-mode conf-mode) . flyspell-prog-mode) (text-mode . flyspell-mode)))

;;;; hl-todo

(use-package hl-todo :ensure t :custom (global-hl-todo-mode t) (hl-todo-highlight-punctuation ":"))

;;;; hs-minor-mode

(use-package hideshow :hook (prog-mode . hs-minor-mode))

;;;; icomplete-mode

(use-package icomplete :custom (icomplete-mode t))

;;;; inheritenv

(use-package inheritenv :ensure t)

;;;; lsp-mode

(use-package lsp-mode :ensure t :custom (lsp-enable-on-type-formatting nil))

(use-package lsp-ui :ensure t)

;;;; recentf

(use-package
 recentf
 :custom
 (recentf-max-menu-items 25)
 (recentf-max-saved-items 25)
 (recentf-mode t)
 :bind ("C-x C-r" . recentf-open-files)
 :config (run-at-time nil (* 5 60) 'recentf-save-list))

;;;; savehist

(use-package savehist :custom (savehist-mode t))

;;;; sub-word-mode

(use-package subword :custom (global-subword-mode t))

;;;; rainbow-mode
(use-package rainbow-mode :ensure t)

;;;; text-scale-mode

(use-package face-remap :custom (global-text-scale-adjust-resizes-frames t))

;;;; yasnippet

(use-package yasnippet :ensure t :custom (yas-global-mode t) :hook ((prog-mode text-mode) . yas-minor-mode))

;;;; outline-minor-mode

(use-package outline :custom (outline-minor-mode-cycle t))

;;; Topic-Wide Major and Minor Mode Configuration

;;;; Ada

(use-package ada-mode :ensure t :custom (ada-indent-backend 'eglot) (ada-statement-backend 'eglot))

;;;; C

;;;;; C Header Files

;;;###autoload
(defun mememe/insert-c-header-include-guard ()
  "automatically insert a template include guard into an open C header file
if it's empty"
  (interactive)
  (if (and buffer-file-name (string-equal (file-name-extension buffer-file-name) "h") (eq (buffer-size) 0))
      (let ((header-file-symbol (concat (upcase (string-replace " " "_" (file-name-base buffer-file-name))) "_H")))
        (insert (concat "#ifndef " header-file-symbol "\n#define " header-file-symbol "\n\n\n\n#endif\n"))
        (previous-line 3))))

;;;;; C Mode

(use-package cc-mode :ensure t :custom (c-basic-offset 4) :hook (c-mode . mememe/insert-c-header-include-guard))

;;;; CUDA

(use-package cuda-mode :ensure t)

;;;; Dired / Filesystem More Generally

;; TODO: figure out how to add advice to dired-omit where it won't toggle on if a buffer doesn't contains any files
;; (files, not directories) that would be visible.

(use-package dired-x :after (dired) :hook (dired-mode . dired-extra-startup))

(use-package
 dired
 :custom (dired-use-ls-dired t) (dired-maybe-use-globstar t) (dired-vc-rename-file t)
 (dired-listing-switches
  (string-join '("-l" ; long output format, required
                 "--all" ; duh
                 "--si" ; same as ~--human-readable~, but powers of 10³ and not 2¹⁰
                 "--classify" ; "append indicator (one of */=>@|) to entries"
                 )
               " ")))

(use-package speedbar :bind ("H-f" . speedbar))

;;;; Elisp

(use-package elisp-mode :hook (emacs-lisp-mode . outline-minor-mode))

(use-package elisp-autofmt :ensure t)

;;;; git

(use-package git-modes :ensure t)

(use-package magit :ensure t :bind ("H-g" . magit-status))

;;;; GLSL

(use-package glsl-mode :ensure t)

;;;; gopher (as in the protocol)

(use-package elpher :ensure t)

;;;; gnuplot

(use-package gnuplot :ensure t)

;;;; HTML/XHTML

(use-package htmlize :ensure t)

;;;; Java

(use-package java-snippets :ensure t)

(use-package
 lsp-java
 :ensure t
 :custom
 (lsp-java-vmargs
  '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true"
    "-Xmx2G" ; locks up really easily if we don't raise this from the default
    "-Xms100m"))
 (lsp-java-configuration-runtimes
  '[(:name "JavaSE-17" :path "/usr/lib/jvm/java-17-openjdk/" :default t)
    (:name "JavaSE-21" :path "/usr/lib/jvm/java-21-openjdk/")]))

(use-package lsp-mode :ensure t :hook java-mode)

;;;; Kotlin

(use-package kotlin-mode :ensure t)

;;;; NASM / YASM

(use-package nasm-mode :ensure t)

;;;; Man / Help / Info / Other Documentation

(use-package man :custom (Man-notify-method 'pushy))

(use-package help-fns :custom (help-enable-symbol-autoload t))

;;;; Org

(use-package
 org
 :ensure t
 :custom
 (org-agenda-files nil)
 (org-agenda-loop-over-headlines-in-active-region nil)
 (org-babel-load-languages '((emacs-lisp . t) (C . t) (gnuplot . t) (shell . t)))
 (org-babel-tangle-lang-exts
  '(("awk" . "awk")
    ("clojurescript" . "cljs")
    ("clojure" . "clj")
    ("fortran" . "F90")
    ("groovy" . "groovy")
    ("haskell" . "hs")
    ("java" . "java")
    ("julia" . "jl")
    ("latex" . "tex")
    ("LilyPond" . "ly")
    ("lisp" . "lisp")
    ("lua" . "lua")
    ("maxima" . "max")
    ("ocaml" . "ml")
    ("perl" . "pl")
    ("processing" . "pde")
    ("python" . "py")
    ("ruby" . "rb")
    ("sed" . "sed")
    ("abc" . "abc")
    ("csharp" . "cs")
    ("io" . "io")
    ("mathomatic" . "math")
    ("picolisp" . "l")
    ("stata" . "do")
    ("tcl" . "tcl")
    ("vala" . "vala")
    ("vbnet" . "vb")
    ("D" . "d")
    ("C++" . "cpp")
    ("rustic" . "rs")
    ("emacs-lisp" . "el")
    ("elisp" . "el")
    ("nasm" . "s")))
 (org-enforce-todo-dependencies t)
 (org-export-with-smart-quotes t)
 (org-latex-default-packages-alist
  '(("AUTO" "inputenc" t ("pdflatex"))
    ("T1" "fontenc" t ("pdflatex"))
    ("" "graphicx" t nil)
    ("" "longtable" nil nil)
    ("" "wrapfig" nil nil)
    ("" "rotating" nil nil)
    ("normalem" "ulem" t nil)
    ("" "amsmath" t nil)
    ("" "amssymb" t nil)
    ("" "capt-of" nil nil)
    ("" "hyperref" nil nil)
    ("" "siunitx" nil nil)))
 (org-latex-hyperref-template
  "\\hypersetup{
 pdfauthor={%a},
 pdftitle={%t},
 pdfkeywords={%k},
 pdfsubject={%d},
 pdfcreator={%c},
 pdflang={%L},
 colorlinks = true,
 urlcolor = blue,
 linkcolor = blue,
 citecolor = red}
")
 (org-startup-indented t)
 :config (require 'ol-tel "~/.emacs.d/mememe/ol-tel.el"))

(use-package org-contrib :ensure t)

(use-package org-present :ensure t)

;;;; PHP

(use-package php-mode :ensure t)

;;;; Raku

(use-package raku-mode :ensure t :custom (raku-indent-offset 8))

;;;; Rust

(use-package rust-mode :ensure t :custom (rust-rustfmt-switches '("--edition" "2024")))

(use-package
 rustic
 :ensure t
 :custom (rustic-compile-backtrace "1")
 :config (add-to-list 'compilation-environment "RUST_BACKTRACE=1"))

(use-package lsp-mode :ensure t :custom (lsp-rust-analyzer-diagnostics-disabled ["inactive-code"]))

;;;; Shell

(use-package shell :custom (explicit-shell-file-name "/bin/bash"))

;;;; Slint

(use-package slint-mode :ensure t)

(use-package lsp-mode :ensure t :hook slint-mode)

;;;; YAML

(use-package yaml-mode :ensure t)

;;;; Other...

;;;;; Multiple Major Modes
;;
;; TODO: I intend to one day use this to allow me to explicitly use org-mode in program comments. I have yet to do so.

(use-package mmm-mode :ensure t)

;;; Misc. Unbound Convenience Functions

(defun mememe/revert-buffers-in-directory (dir &optional recursive)
  "Revert all file-visiting buffers under DIR.
If RECURSIVE is non-nil, include subdirectories."
  (interactive "DDirectory: \nP")
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (let ((fname (buffer-file-name buf)))
          (when (and fname (string-prefix-p dir (file-name-directory fname)))
            (when (or recursive (string= (file-name-directory fname) dir))
              (revert-buffer :ignore-auto :noconfirm))))))))

;;; .emacs ends here
