(with-eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(require 'csharp-mode)

(add-hook 'csharp-mode-hook 'omnisharp-mode)
(add-hook 'csharp-mode-hook #'flycheck-mode)

(eval-after-load
    'company
  '(add-to-list 'company-backends 'company-omnisharp))

(add-hook 'csharp-mode-hook #'company-mode)

(add-to-list 'auto-mode-alist '("\\.csproj\\'" . xml-mode))

(require 'display-line-numbers)

(defvar display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode inferior-python-mode fireplace-mode image-mode dun-mode magit-status-mode dired-mode tetris-mode rustic-compilation-mode)
  "Major modes to exempt from display-line-numbers-mode")

(defun display-line-numbers--turn-on ()
  "Turn on line numbers, exempting any major modes defined in `display-line-numbers-exempt-modes'"
  (if (and
       (not (member major-mode display-line-numbers-exempt-modes))
       (not (minibufferp)))
      (display-line-numbers-mode)))

(global-display-line-numbers-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(ansi-color-faces-vector
    [default bold shadow italic underline bold bold-italic bold])
   '(ansi-color-names-vector
     (vector "#839496" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#fdf6e3"))
    '(column-number-mode t)
 '(compilation-environment '("RUST_BACKTRACE=1"))
 '(custom-enabled-themes '(wombat))
 '(custom-safe-themes
   '("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4"
     "015a6f06366233b8f0d0a06af4aa7d8aadea126e2d70caa2422a3f173ee990ec"
     "e5494adf200eeff1505839672150dde6053e086869189c381b1ce9b792dda3a8" default))
 '(delete-by-moving-to-trash t)
 '(dired-listing-switches "-al --human-readable")
 '(explicit-shell-file-name "/bin/bash")
 '(fci-rule-color "#073642")
 '(fill-column 120)
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode nil)
 '(global-display-line-numbers-mode t)
 '(global-text-scale-adjust-resizes-frames t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(lsp-enable-on-type-formatting nil)
 '(lsp-rust-analyzer-diagnostics-disabled ["inactive-code"])
 '(menu-bar-mode nil)
 '(org-agenda-files nil)
 '(org-agenda-loop-over-headlines-in-active-region nil)
 '(org-babel-load-languages '((emacs-lisp . t) (C . t) (gnuplot . t) (shell . t)))
 '(org-enforce-todo-dependencies t)
 '(org-export-with-smart-quotes t)
 '(org-latex-default-packages-alist
   '(("AUTO" "inputenc" t ("pdflatex")) ("T1" "fontenc" t ("pdflatex")) ("" "graphicx" t nil) ("" "longtable" nil nil)
     ("" "wrapfig" nil nil) ("" "rotating" nil nil) ("normalem" "ulem" t nil) ("" "amsmath" t nil) ("" "amssymb" t nil)
     ("" "capt-of" nil nil) ("" "hyperref" nil nil) ("" "siunitx" nil nil)))
 '(org-latex-hyperref-template
   "\\hypersetup{\12 pdfauthor={%a},\12 pdftitle={%t},\12 pdfkeywords={%k},\12 pdfsubject={%d},\12 pdfcreator={%c}, \12 pdflang={%L},\12 colorlinks = true,\12 urlcolor = blue,\12 linkcolor = blue,\12 citecolor = red\12}\12")
 '(org-startup-indented t)
 '(package-install-upgrade-built-in t)
 '(package-selected-packages
   '(ace-flyspell ada-mode bind-key circe cl-lib color-theme-sanityinc-solarized company counsel cuda-mode editorconfig
                  eglot eldoc elpher erc ewal faceup fireplace flycheck-popup-tip flymake flymd frameshot glsl-mode
                  gnuplot gnuplot-mode htmlize idlwave image-dired+ java-snippets jsonrpc leetcode lsp-mode lsp-ui magit
                  mmm-mode nasm-mode org org-contrib org-present php-mode processing-mode project python racket-mode
                  rainbow-mode raku-mode rustic soap-client track-changes tramp use-package verilog-mode wc-mode
                  which-key window-tool-bar xref xresources-theme yaml-mode yasnippet))
 '(raku-indent-offset 8)
 '(require-final-newline t)
 '(rust-rustfmt-switches '("--edition" "2021"))
 '(rustic-compile-backtrace "")
 '(safe-local-variable-values
   '((eval load-file "./minecraft-research/shortcuts.el")
     (eval local-set-key (kbd "H-m RET") (lambda nil (interactive) (insert "/Minecraft/")))
     (eval define-key temp-mode-map (kbd "H-m") (lambda nil (interactive) (insert "/Minecraft/"))) (temp-mode . t)
     (org-use-property-inheritance . t) (org-html-inline-images)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#aa322f") (40 . "#cb4b16") (60 . "#b58900") (80 . "#859900") (100 . "#2aa198") (120 . "#268bd2")
     (140 . "#d33682") (160 . "#6c71c4") (180 . "#dc322f") (200 . "#cb4b16") (220 . "#b58900") (240 . "#859900")
     (260 . "#2aa198") (280 . "#268bd2") (300 . "#d33682") (320 . "#6c71c4") (340 . "#dc322f") (360 . "#cb4b16")))
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-types '(((undo discard-info)) ((undo discard-info)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#0A0F16" :foreground "#e0e0e0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 110 :width normal :foundry "ADBO" :family "Source Code Pro"))))
 '(fringe ((t (:background "unspecified"))))))
