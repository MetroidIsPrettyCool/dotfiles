;;; .emacs --- primary Emacs configuration

;;; Commentary:

;; Y'know, try to follow the Elisp conventions appendix. (~C-h R elisp RET g Tips RET~).
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
;;
;; and the values for these symbols should start on the same line as the symbol if they're a single sexp, else on the
;; next line, the name of the package should be on the same line as use-package.

;;; Code:

;;; Order-Explicit Global Stuff

;;;; Put ~debug-on-*~ Calls Here

;;;; Enable Packages and Package Repos

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; init
(package-initialize)

;;;; Misc. Customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wombat))
 '(custom-safe-themes
   '("6819104c5f7d70485b32c10323aa396806d282fcee5b707e462bf3d156f44c39"
     "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4"
     "015a6f06366233b8f0d0a06af4aa7d8aadea126e2d70caa2422a3f173ee990ec"
     "e5494adf200eeff1505839672150dde6053e086869189c381b1ce9b792dda3a8" default))
 '(package-install-upgrade-built-in t)
 '(package-selected-packages
   '(ace-flyspell ada-mode circe color-theme-sanityinc-solarized company counsel cuda-mode editorconfig eglot elisp-autofmt
                  elpher erc ewal faceup fireplace flycheck-popup-tip flymd frameshot git-modes glsl-mode gnuplot
                  gnuplot-mode hl-todo htmlize idlwave image-dired+ inheritenv java-snippets kotlin-mode leetcode
                  lsp-java lsp-ui magit mmm-mode nasm-mode org-contrib org-present php-mode processing-mode python
                  rainbow-mode raku-mode rustic slint-mode soap-client tramp transient use-package verilog-mode
                  visual-fill-column wc-mode which-key window-tool-bar xresources-theme yaml-mode))
 '(safe-local-variable-directories
   '("/home/joseph/Documents/website/metroidisprettycool.github.io/"))
 '(safe-local-variable-values
   '((eval load-file (concat (car (dir-locals-find-file ".")) ".emacs/shortcuts.el")) (eval buffer-face-mode t)
     (eval setq buffer-face-mode-face '(:family "Mojang"))
     (eval remove-hook 'before-save-hook 'mememe/delete-trailing-whitespace-unless-exempt)
     (eval display-fill-column-indicator-mode t) (eval load-file "./shortcuts.el") (org-use-property-inheritance . t)
     (org-html-inline-images)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#aa322f") (40 . "#cb4b16") (60 . "#b58900") (80 . "#859900") (100 . "#2aa198") (120 . "#268bd2")
     (140 . "#d33682") (160 . "#6c71c4") (180 . "#dc322f") (200 . "#cb4b16") (220 . "#b58900") (240 . "#859900")
     (260 . "#2aa198") (280 . "#268bd2") (300 . "#d33682") (320 . "#6c71c4") (340 . "#dc322f") (360 . "#cb4b16")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#0A0F16" :foreground "#e0e0e0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 110 :width normal :foundry "ADBO" :family "Source Code Pro"))))
 '(fringe ((t (:background unspecified)))))

;;;; Misc. Global Requires

(require 'eglot)
(require 'lsp-mode)
(require 'use-package)

;;;; Appearance

;;;;; Make Emacs Background /Slightly/ Transparent

(set-frame-parameter (selected-frame) 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))

;;;;; Dark Mode

(use-package emacs
  :custom
  (frame-background-mode 'dark))

;;;;; Setup Default Font For Just Emoji

(let ((blobmoji-emoji '(;; Letterlike Symbols (select sub-ranges)
                        ?ℹ

                        ;; Miscellaneous Technical (select sub-ranges)
                        ?⌚
                        ?⌛
                        (?⏩ . ?⏳)
                        (?⏸ . ?⏾)

                        ;; Miscellaneous Symbols (entire block)
                        (?☀ . ?⛿)

                        ;; Dingbats (entire block)
                        (?✀ . ?➿)

                        ;; Miscellaneous Symbols and Arrows (select sub-ranges)
                        ?⭐

                        ;; Mahjong Tiles (entire block)
                        (?🀀 . ?🀫)

                        ;; Domino Tile (entire block)
                        (?🀰 . ?🂓)

                        ;; Playing Cards (entire block)
                        (?🂠 . ?🃵)

                        ;; Enclosed Alphanumeric Supplement (select sub-ranges)
                        (?🅰 . ?🆚)

                        ;; Enclosed Ideograph Supplement (select sub-ranges)
                        ?🈁
                        ?🈂
                        ?🈚
                        ?🈯
                        (?🈲 . ?🈺)
                        ?🉐
                        ?🉑

                        ;; Miscellaneous Symbols and Pictures (entire block)
                        (?🌀 . ?🗿)

                        ;; Emoticons (entire block)
                        (?😀 . ?🙏)

                        ;; Transport and Map Symbols (entire block)
                        (?🚀 . ?🛿)

                        ;; Geometric Shapes Extended (select ranges)
                        (?🟠 . ?🟫)

                        ;; Supplemental Symbols and Pictographs (entire block)
                        (?🤀 . ?🧿)

                        ;; Chess Symbols (entire block)
                        (?🨀 . ?🩯)

                        ;; Symbols and Pictographs Extended-A (entire block)
                        (?🩰 . ?🫿))))

  (dolist (emoji blobmoji-emoji) (set-fontset-font t emoji "Blobmoji" nil)))

;;;;; Disable Various UI Elements

(use-package emacs
  :custom
  (inhibit-startup-screen t)
  (scroll-bar-mode nil)
  (tool-bar-mode nil)
  (menu-bar-mode nil)
  (use-dialog-box nil))

;;; /Global/ Global Settings

;;;; Load Some Custom Elisp Files

(require 'temp-mode "~/Documents/elisp-progs/temp-mode.el")

;;;; Configuring Editor Behaviors

;;;;; Global Sub-Word Mode

(use-package emacs
  :custom (global-subword-mode t))

;;;;; Enable Case Region

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;;;; Punctuation Control

(use-package emacs
  :custom
  (sentence-end-double-space nil)
  (require-final-newline t)
  (indent-tabs-mode nil))

;;;;; Delete Trailing Whitespace When Saving

(defvar mememe/delete-trailing-whitespace-exempt-modes
  '(python-mode)
  "Major modes to exempt from `mememe/delete-trailing-whitespace'")

;;;###autoload
(defun mememe/delete-trailing-whitespace-unless-exempt ()
  "Delete trailing whitespace, exempting any major modes defined in
`mememe/delete-trailing-whitespace-exempt-modes'"
  (interactive)
  (if (and
       (not (member major-mode mememe/delete-trailing-whitespace-exempt-modes))
       (not (minibufferp)))
      (delete-trailing-whitespace)))

(add-hook 'before-save-hook 'mememe/delete-trailing-whitespace-unless-exempt)

;;;;; Trash Instead of Delete

(use-package emacs
  :custom (delete-by-moving-to-trash t))

;;;;; Auto-Revert

(use-package autorevert
  :custom (global-auto-revert-mode t))

;;;;; Minibuffer History

(use-package savehist
  :custom
  (savehist-mode t)
  (history-length 50))

;;;; Configure Minor Built-Ins

;;;;; fill-column

(use-package display-fill-column-indicator
  :hook (prog-mode . display-fill-column-indicator-mode)
  :custom (fill-column 120))

;;;;; column-number-mode

(use-package simple
  :custom (column-number-mode t))

;;;;; icomplete-mode

(use-package icomplete
  :custom (icomplete-mode t))

;;;;; help-mode

(use-package help-fns
  :custom (help-enable-symbol-autoload t))

;;;;; shell, etc

(use-package shell
  :custom (explicit-shell-file-name "/bin/bash"))

;;;;; text-scale-mode

(use-package face-remap
  :custom (global-text-scale-adjust-resizes-frames t))

;;;;; hs-minor-mode
(use-package hideshow
  :hook (prog-mode . hs-minor-mode))

;;;; Keybinds

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
(fset 'yes-or-no-p 'y-or-n-p)

;;;;; General

(keymap-global-set "H-f" 'speedbar)
(keymap-global-set "H-s" (lambda () (interactive) (insert "¯\\_(ツ)_/¯"))) ; ¯\_(ツ)_/¯
(keymap-global-set "H-r" 'query-replace-regexp)

;;;;; "Better" Versions of Defaults

;;;;;; ibuffer Instead of list-buffers

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

;;;;; "Missing" Opposites for Defaults
;;
;; All generally just the previous keybind + Shift

;;;;;; Opposite of M-q

;;;###autoload
(defun mememe/unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;;;###autoload
(defun mememe/unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(keymap-global-set "M-Q" 'mememe/unfill-paragraph)

;;;;;; Opposite of M-y

;;;###autoload
(defun mememe/yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))

(keymap-global-set "M-Y" 'mememe/yank-pop-forwards)

;;;;;; Opposite of C-SPC

;;;###autoload
(defun mememe/deactive-mark (arg)
  (interactive "p")
  (deactivate-mark arg))

(keymap-global-set "C-S-SPC" 'mememe/deactive-mark)

;;; Minor Mode Configuration

;;;; yasnippet

(use-package yasnippet
  :ensure t
  :custom (yas-global-mode t)
  :hook ((prog-mode text-mode) . yas-minor-mode)
  :config (yas-reload-all))

;;;; company

(use-package company
  :ensure t
  :custom
  (company-idle-delay 0.2)) ; how long to wait until popup

;;;; display-line-numbers-mode

(defvar mememe/display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode inferior-python-mode fireplace-mode image-mode dun-mode
               magit-status-mode dired-mode tetris-mode rustic-compilation-mode)
  "Major modes to exempt from `mememe/display-line-numbers-mode'")

(defun mememe/display-line-numbers-unless-exempt (original-fun &rest args)
  "Turn on line numbers, exempting any major modes defined in
`mememe/display-line-numbers-exempt-modes'"
  (if (and
       (not (member major-mode mememe/display-line-numbers-exempt-modes))
       (not (minibufferp)))
      (apply original-fun args)))

(use-package display-line-numbers
  :custom
  (global-display-line-numbers-mode t)
  :config
  (advice-add 'display-line-numbers--turn-on :around
              'mememe/display-line-numbers-unless-exempt))

;;;; recentf

(use-package recentf
  :ensure t
  :custom
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 25)
  (recentf-mode t)
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (run-at-time nil (* 5 60) 'recentf-save-list))

;;;; flyspell

(use-package flyspell
  :ensure t
  :defer t
  :hook
  (((prog-mode conf-mode) . flyspell-prog-mode)
   (text-mode . flyspell-mode)))

;;;; flycheck

(use-package flycheck
  :ensure t
  :defer t
  ;; :hook ((flycheck-mode . flycheck-popup-tip-mode))
  )

;;;; LSP

(use-package lsp-mode
  :ensure t
  :custom (lsp-enable-on-type-formatting nil))

;;;; hl-todo

(use-package hl-todo
  :ensure t
  :custom
  (global-hl-todo-mode t)
  (hl-todo-highlight-punctuation ":"))

;;;; outline-minor-mode

(use-package outline-mode
  :hook (emacs-lisp-mode . outline-minor-mode)
  :custom (outline-minor-mode-cycle t))

;;; Per-Topic (Language) Major/Minor Mode Configuration

;;;; Ada

(use-package ada-mode
  :ensure t
  :custom
  (ada-indent-backend 'eglot)
  (ada-statement-backend 'eglot))

;;;; C

;;;;; C Header Files

;;;###autoload
(defun mememe/insert-c-header-include-guard ()
  "automatically insert a template include guard into an open C header file
if it's empty"
  (interactive)
  (if (and
       buffer-file-name
       (string-equal (file-name-extension buffer-file-name) "h")
       (eq (buffer-size) 0))
      (let ((header-file-symbol (concat (upcase (string-replace " " "_" (file-name-base buffer-file-name))) "_H")))
        (insert (concat "#ifndef " header-file-symbol "\n#define " header-file-symbol "\n\n\n\n#endif\n"))
        (previous-line 3))))

;;;;; C Mode

(use-package cc-mode
  :ensure t
  :custom (c-basic-offset 4)
  :hook (c-mode . mememe/insert-c-header-include-guard))

;;;; Java

(use-package lsp-java
  :custom
  (lsp-java-vmargs '("-XX:+UseParallelGC"
                     "-XX:GCTimeRatio=4"
                     "-XX:AdaptiveSizePolicyWeight=90"
                     "-Dsun.zip.disableMemoryMapping=true"
                     "-Xmx2G" ; stutters and locks up real easy if we don't raise this from the default
                     "-Xms100m"))
  (lsp-java-configuration-runtimes '[;; (:name "JavaSE-1.5.0_22"
                                     ;;        :path "/usr/lib/jvm/jdk1.5.0_22/")
                                     (:name "JavaSE-8"
                                            :path "/usr/lib/jvm/java-8-openjdk/")
                                     (:name "JavaSE-11"
                                            :path "/usr/lib/jvm/java-11-openjdk/")
                                     (:name "JavaSE-17"
                                            :path "/usr/lib/jvm/java-17-openjdk/"
                                            :default t)
                                     (:name "JavaSE-21"
                                            :path "/usr/lib/jvm/java-21-openjdk/")
                                     ;; (:name "JavaSE-25"
                                     ;;        :path "/usr/lib/jvm/java-25-openjdk/")
                                     ]))

(use-package lsp-mode
  :ensure t
  :hook java-mode)

;;;; Org

(use-package org
  :ensure t
  :custom
  (org-agenda-files nil)
  (org-agenda-loop-over-headlines-in-active-region nil)
  (org-babel-load-languages '((emacs-lisp . t) (C . t) (gnuplot . t) (shell . t)))
  (org-babel-tangle-lang-exts
   '(("awk" . "awk") ("clojurescript" . "cljs") ("clojure" . "clj") ("fortran" . "F90") ("groovy" . "groovy")
     ("haskell" . "hs") ("java" . "java") ("julia" . "jl") ("latex" . "tex") ("LilyPond" . "ly") ("lisp" . "lisp")
     ("lua" . "lua") ("maxima" . "max") ("ocaml" . "ml") ("perl" . "pl") ("processing" . "pde") ("python" . "py")
     ("ruby" . "rb") ("sed" . "sed") ("abc" . "abc") ("csharp" . "cs") ("io" . "io") ("mathomatic" . "math")
     ("picolisp" . "l") ("stata" . "do") ("tcl" . "tcl") ("vala" . "vala") ("vbnet" . "vb") ("D" . "d") ("C++" . "cpp")
     ("rustic" . "rs") ("emacs-lisp" . "el") ("elisp" . "el") ("nasm" . "s")))
  (org-enforce-todo-dependencies t)
  (org-export-with-smart-quotes t)
  (org-latex-default-packages-alist
   '(("AUTO" "inputenc" t ("pdflatex")) ("T1" "fontenc" t ("pdflatex")) ("" "graphicx" t nil) ("" "longtable" nil nil)
     ("" "wrapfig" nil nil) ("" "rotating" nil nil) ("normalem" "ulem" t nil) ("" "amsmath" t nil) ("" "amssymb" t nil)
     ("" "capt-of" nil nil) ("" "hyperref" nil nil) ("" "siunitx" nil nil)))
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
  (org-startup-indented t))

(with-eval-after-load 'org
  (load-file "~/Documents/elisp-progs/ol-tel.el"))

;;;; Raku

(use-package raku-mode
  :ensure t
  :custom (raku-indent-offset 8))

;;;; Rust

(use-package rust-mode
  :ensure t
  :custom (rust-rustfmt-switches '("--edition" "2024")))

(use-package rustic
  :ensure t
  :custom (rustic-compile-backtrace "1")
  :config (add-to-list 'compilation-environment "RUST_BACKTRACE=1"))

(use-package lsp-mode
  :ensure t
  :custom (lsp-rust-analyzer-diagnostics-disabled ["inactive-code"]))

;;;; Slint

(use-package slint-mode
  :ensure t
  :hook (slint-mode . lsp-mode))

;;; Other Major Modes

;;;; dired

;; TODO: figure out how to add advice to dired-omit where it won't toggle on if a buffer doesn't contains any files
;; (files, not directories) that would be visible.

(use-package dired-x
  :after (dired)
  :hook
  (dired-mode . dired-extra-startup))

(use-package dired
  :custom
  (dired-use-ls-dired t)
  (dired-maybe-use-globstar t)
  (dired-vc-rename-file t)
  (dired-listing-switches (string-join '("-l" ; long output format, required
                                         "--all"
                                         ;; "--human-readable"
                                         "--si" ; same as ~--human-readable~, but powers of 10³ and not 2¹⁰
                                         "--classify" ; "append indicator (one of */=>@|) to entries"
                                         )
                                       " ")))

;;;; magit

(use-package magit
  :ensure t
  :bind ("H-g" . magit-status))

;;;; man

(use-package man
  :custom (Man-notify-method 'pushy))

;;; Misc. Unbound Convenience Functions

(defun mememe/revert-buffers-in-directory (dir &optional recursive)
  "Revert all file-visiting buffers under DIR.
If RECURSIVE is non-nil, include subdirectories."
  (interactive "DDirectory: \nP")
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (let ((fname (buffer-file-name buf)))
          (when (and fname
                     (string-prefix-p dir (file-name-directory fname)))
            (when (or recursive
                      (string= (file-name-directory fname) dir))
              (revert-buffer :ignore-auto :noconfirm))))))))

;;; .emacs ends here
