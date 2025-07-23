;; Notes on formatting:
;;
;; Comment headers...
;;
;; 1. Should be title-case
;;
;; 2. Should use the following sizes, starting from the top level and without skipping any sizes as they decrease:
;;
;;   - 20
;;
;;   - 15
;;
;;   - 10
;;
;;   - 5
;;
;; ~use-package~ statements should follow this order:
;;
;;   - ~:load_path~
;;
;;   - ~:requires~
;;
;;   - ~:ensure~
;;
;;   - ~:defer~
;;
;;   - ~:init~
;;
;;   - ~:custom~
;;
;;   - ~:bind~
;;
;;   - ~:hook~
;;
;;   - ~:config~
;;
;; The values for these symbols should start on the same line as the symbol if they're a single sexp, else on the next
;; line, the name of the package should be on the same line as use-package
;;
;; All sexps should have their final close-parens on the same line

;; ==================== Order-Explicit Global Stuff ====================

;; =============== Enable Packages and Package Repos ===============
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))

;; init
(package-initialize)

;; i have no memory of why this is set to nil. probably because i copy-pasted it without any real understanding
;;
;; leaving it commented in case there really was a good reason to do so

;; (setq package-enable-at-startup nil)

;; =============== Misc. Customize ===============
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wombat))
 '(custom-safe-themes
   '("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4"
     "015a6f06366233b8f0d0a06af4aa7d8aadea126e2d70caa2422a3f173ee990ec"
     "e5494adf200eeff1505839672150dde6053e086869189c381b1ce9b792dda3a8" default))
 '(delete-by-moving-to-trash t)
 '(explicit-shell-file-name "/bin/bash")
 '(global-text-scale-adjust-resizes-frames t)
 '(indent-tabs-mode nil)
 '(package-install-upgrade-built-in t)
 '(package-selected-packages
   '(ace-flyspell ada-mode circe color-theme-sanityinc-solarized company counsel cuda-mode editorconfig eglot elpher erc
                  ewal faceup fireplace flycheck-popup-tip flymd frameshot glsl-mode gnuplot gnuplot-mode htmlize
                  idlwave image-dired+ java-snippets leetcode lsp-ui magit mmm-mode nasm-mode org-contrib org-present
                  php-mode processing-mode python rainbow-mode raku-mode rustic soap-client tramp use-package
                  verilog-mode wc-mode which-key window-tool-bar xresources-theme yaml-mode))
 '(require-final-newline t)
 '(safe-local-variable-values
   '((eval load-file "./shortcuts.el")
     (org-use-property-inheritance . t) (org-html-inline-images)))
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
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#0A0F16" :foreground "#e0e0e0" :inverse-video nil
                         :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular
                         :height 110 :width normal :foundry "ADBO" :family "Source Code Pro"))))
 '(fringe ((t (:background unspecified)))))

;; =============== Appearance ===============

;; ========== Make Emacs /Slightly/ Transparent ==========
(set-frame-parameter (selected-frame) 'alpha 90)
(add-to-list 'default-frame-alist '(alpha . 90))

;; ========== Override Default Font Emoji ==========
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

;; ========== Dark Mode! ==========
(setq frame-background-mode 'dark)

;; ========== Load Da Theme ==========

;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions
;; 	      (lambda (frame)
;; 		(with-selected-frame frame
;; 		      (load-theme 'xresources t))))
;;   (if (window-system)
;;       (load-theme 'xresources t)))

;; ==================== /Global/ Global Settings ====================

;; =============== Misc. Global Requires ===============
(require 'eglot)

;; =============== Load All My Various Custom Elisp Files ===============
(require 'temp-mode "~/Documents/elisp-progs/temp-mode.el")

;; =============== Enable Some Stuff That's Disabled By Default ===============
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; =============== Configuring Editor Behaviors ===============

;; ========== Delete Trailing Whitespace When Saving ==========
(defvar delete-trailing-whitespace-exempt-modes
  '(python-mode)
  "Major modes to exempt from `delete-trailing-whitespace'")

;;;###autoload
(defun delete-trailing-whitespace-unless-exempt ()
  "Delete trailing whitespace, exempting any major modes defined in
`delete-trailing-whitespace-exempt-modes'"
  (interactive)
  (if (and
       (not (member major-mode delete-trailing-whitespace-exempt-modes))
       (not (minibufferp)))
      (delete-trailing-whitespace)))

(add-hook 'before-save-hook 'delete-trailing-whitespace-unless-exempt)

;; ========== Enable and Disable Built-In Minor Modes ==========

;; ===== fill-column =====
(setq-default fill-column 120)

(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;; ===== Disable Various Kiddy UI Elements =====
(setq inhibit-startup-screen t)
(set-scroll-bar-mode nil)
(setq tool-bar-mode nil)
(setq menu-bar-mode nil)

;; =============== Keybinds ===============
;;
;; 'H' is the "hyper" key, not present on basically every PC keyboard ever made. and unused by everything i've ever seen
;; in Emacs, so it's pretty safe for user keybinds. you'll need to modify your keyboard layout, personally, i have left
;; control bound to ~Hyper_L~ (and capslock bound to ~Control_L~)

;; ========== Misc ==========

;; (global-set-key (kbd "H-l") 'goto-line)
(global-set-key (kbd "H-u") 'undo-only)
(global-set-key (kbd "H-c") 'comment-region)
(global-set-key (kbd "H-s") (lambda () (interactive) (insert "¯\\_(ツ)_/¯")))
(global-set-key (kbd "H-r") 'query-replace-regexp)

;; ========== "Missing" Opposites for Defaults ==========
;;
;; all generally just the previous keybind+shift

;; ===== M-q =====

;;;###autoload
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;;;###autoload
(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(global-set-key (kbd "M-Q") 'unfill-paragraph)

;; ===== M-y =====

;;;###autoload
(defun yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))

(global-set-key (kbd "M-Y") 'yank-pop-forwards)

;; ==================== Global Mode-Specific Config ====================

;; =============== Minor Modes ===============

;; =========== yasnippet ===========
(use-package yasnippet
  :ensure t
  :hook (((prog-mode text-mode) . yas-minor-mode))
  :config
  (yas-global-mode t)
  (yas-reload-all))

;; =========== company ===========
(use-package company
  :ensure t
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind (:map company-mode-map
	      ("<tab>". tab-indent-or-complete)
	      ("TAB". tab-indent-or-complete)))

;; =========== display-line-numbers-mode ===========
(defvar display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode inferior-python-mode fireplace-mode image-mode dun-mode
               magit-status-mode dired-mode tetris-mode rustic-compilation-mode)
  "Major modes to exempt from `display-line-numbers-mode'")

(defun display-line-numbers-unless-exempt (original-fun &rest args)
  "Turn on line numbers, exempting any major modes defined in `display-line-numbers-exempt-modes'"
  (if (and
       (not (member major-mode display-line-numbers-exempt-modes))
       (not (minibufferp)))
      (apply original-fun args)))

(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode t)
  (advice-add 'display-line-numbers--turn-on :around 'display-line-numbers-unless-exempt))

;; =========== recentf ===========
(use-package recentf
  :ensure t
  :custom
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 25)
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (recentf-mode t)
  (run-at-time nil (* 5 60) 'recentf-save-list))

;; =========== ibuffer ===========
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

;; =========== flyspell ===========
(use-package flycheck
  :ensure t
  :defer t
  :hook (((prog-mode conf-mode) . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

;; =========== flycheck ===========
(use-package flycheck
  :ensure t
  :defer t
  :hook ((flycheck-mode . flycheck-popup-tip-mode)))

;; ========== column-number-mode ==========
(column-number-mode t)

;; =========== cua ===========

;; (cua-selection-mode t)

;; ========== LSP ==========
(use-package lsp-mode
  :custom
  (lsp-enable-on-type-formatting nil))

;; =============== Minor-Minor Mode Inter-Configuration ===============

;; =============== Per-Topic Major/Minor Mode Configuration ===============

;; ========== C ==========

;; ===== Header Files =====

;;;###autoload
(defun insert-c-header-include-guard ()
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

;; ===== Finally... =====
(use-package cc-mode
  :ensure t
  :custom (c-basic-offset 4)
  ;; :bind (:map c-mode-map ("C-c h" . insert-c-header-include-guard))
  :hook (c-mode . insert-c-header-include-guard))

;; ========== Ada ==========
(use-package ada-mode
  :ensure t
  :custom
  (ada-indent-backend 'eglot)
  (ada-statement-backend 'eglot))

;; ========== Rust ==========
(use-package rust-mode
  :ensure t
  :custom (rust-rustfmt-switches '("--edition" "2024")))

(use-package rustic
  :ensure t
  :custom (rustic-compile-backtrace "")
  :config (add-to-list 'compilation-environment "RUST_BACKTRACE=1"))

(use-package lsp-mode
  :custom (lsp-rust-analyzer-diagnostics-disabled ["inactive-code"]))

;; ========== Org ==========
(use-package org
  :load-path "~/Documents/elisp-progs"
  :requires ol-tel
  :custom
  (org-agenda-files nil)
  (org-agenda-loop-over-headlines-in-active-region nil)
  (org-babel-load-languages '((emacs-lisp . t) (C . t) (gnuplot . t) (shell . t)))
  (org-enforce-todo-dependencies t)
  (org-export-with-smart-quotes t)
  (org-latex-default-packages-alist
   '(("AUTO" "inputenc" t ("pdflatex")) ("T1" "fontenc" t ("pdflatex")) ("" "graphicx" t nil) ("" "longtable" nil nil)
    ("" "wrapfig" nil nil) ("" "rotating" nil nil) ("normalem" "ulem" t nil) ("" "amsmath" t nil) ("" "amssymb" t nil)
    ("" "capt-of" nil nil) ("" "hyperref" nil nil) ("" "siunitx" nil nil)))
  (org-latex-hyperref-template
   "\\hypersetup{\12 pdfauthor={%a},\12 pdftitle={%t},\12
pdfkeywords={%k},\12 pdfsubject={%d},\12 pdfcreator={%c}, \12
pdflang={%L},\12 colorlinks = true,\12 urlcolor = blue,\12 linkcolor =
blue,\12 citecolor = red\12}\12")
  (org-startup-indented t))

;; ========== Raku... ==========
(use-package raku-mode
  :custom (raku-indent-offset 8))

;; =============== Other Major Modes ===============

;; ========== dired ==========
(use-package dired
  :custom (dired-listing-switches "-al --human-readable")
  :hook (dired-mode . dired-omit-mode))

;; ========== magit ==========
(use-package magit
  :ensure t
  :bind ("H-g" . magit-status))

;; ========== man ==========
(use-package man
  :custom (Man-notify-method 'pushy))
