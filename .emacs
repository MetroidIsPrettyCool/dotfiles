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
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#839496" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#fdf6e3"))
 '(beacon-color "#d33682")
 '(custom-enabled-themes '(xresources))
 '(custom-safe-themes
   '("86704574d397606ee1433af037c46611fb0a2787e8b6fd1d6c96361575be72d2" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default))
 '(fci-rule-color "#073642")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(lsp-ui lsp-mode rustic magit company cl-lib flycheck-popup-tip omnisharp glsl-mode csharp-mode ace-flyspell flymd image-dired+ yaml-mode circe frameshot ewal xresources-theme theme-magic java-snippets yasnippet color-theme-sanityinc-solarized))
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#aa322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :foreground "#e0e0e0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 96 :width normal :foundry "ADBO" :family "Source Code Pro")))))

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

(scroll-bar-mode -1)

(require 'display-line-numbers)
(defcustom display-line-numbers-exempt-modes '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode)
  "Major modes on which to disable the linum mode, exempts them from global requirement"
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "turn on line numbers but excempting certain majore modes defined in `display-line-numbers-exempt-modes'"
  (if (and
       (not (member major-mode display-line-numbers-exempt-modes))
       (not (minibufferp)))
      (display-line-numbers-mode)))

(global-display-line-numbers-mode)

(yas-global-mode)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(require 'zone)
(zone-when-idle 180)

(with-eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))

(add-to-list 'auto-mode-alist '("\\.csproj\\'" . xml-mode))

(add-hook 'csharp-mode-hook 'omnisharp-mode)
(add-hook 'csharp-mode-hook #'flycheck-mode)

(eval-after-load
 'company
 '(add-to-list 'company-backends 'company-omnisharp))

(add-hook 'csharp-mode-hook #'company-mode)

(global-set-key (kbd "C-x g") 'magit-status)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq rustic-lsp-server 'rust-analyzer)
(setq lsp-rust-analyzer-server-display-inlay-hints t)

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; (use-package lsp-ui
;;   :ensure
;;   :commands lsp-ui-mode
;;   :custom
;;   (lsp-ui-peek-always-show t)
;;   (lsp-ui-sideline-show-hover t)
;;   (lsp-ui-doc-enable nil)
;;  )

(use-package company
  :ensure
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-mode-map
	("<tab>". tab-indent-or-complete)
	("TAB". tab-indent-or-complete)))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

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

(set-fontset-font "fontset-default"
		  (cons (decode-char 'ucs #x1f680)
                        (decode-char 'ucs #x1f6ff))
                  "NotoColorEmoji")
(set-fontset-font "fontset-default"
		  (cons (decode-char 'ucs #x2700)
                        (decode-char 'ucs #x27bf))
		  "NotoColorEmoji")
(set-fontset-font "fontset-default"
		  (cons (decode-char 'ucs #x2600)
                        (decode-char 'ucs #x26ff))
                  "NotoColorEmoji")
(set-fontset-font "fontset-default"
		  (cons (decode-char 'ucs #x1f300)
                        (decode-char 'ucs #x1f5ff))
                  "NotoColorEmoji")
(set-fontset-font "fontset-default"
		  (cons (decode-char 'ucs #x1f600)
                        (decode-char 'ucs #x1f64f))
                  "NotoColorEmoji")
(set-fontset-font "fontset-default"
		  (cons (decode-char 'ucs #x1f900)
                        (decode-char 'ucs #x1f9ff))
                  "NotoColorEmoji")
(set-fontset-font "fontset-default"
		  (cons (decode-char 'ucs #x1fa70)
                        (decode-char 'ucs #x1faff))
                  "NotoColorEmoji")

