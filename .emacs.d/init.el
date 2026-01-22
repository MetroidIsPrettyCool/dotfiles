;;; init.el --- primary Emacs configuration -*- fill-column: 80; lexical-binding: t; -*-

;;; Commentary:

;;;; Targets

;; I intend this configuration work well without modification for the latest
;; release version of GNU Emacs, on both my Desktop, with GUI frames and a
;; server-client setup; and on my Android smartphone, with one TUI frame inside
;; Termux.

;; You may encounter problems trying to use this config in any other way, for
;; any other purpose, in any other context. I can only test so much...

;;;; General Conventions

;; Follow advice at C-h R elisp RET g Tips RET

;; Use `elisp-autofmt-buffer' to make life easier.

;; ~use-package~ statements should follow this order:
;; 1.  `:load_path'
;; 2.  `:vc'
;; 3.  `:ensure'
;; 4.  `:requires'
;; 5.  `:after'
;; 6.  `:demand'
;; 7.  `:defer'
;; 8.  `:init'
;; 9.  `:custom'
;; 10. `:bind'
;; 11. `:hook'
;; 12. `:config'

;;;; Key Bindings

;; Throughout this file I use a lot of bindings that involve the "H" modifier.

;; This is the "hyper" key, the next in the logical progression from Meta to
;; Super. It's defined in X for historical reasons (Lisp machines, etc.) but
;; AFAICT nobody has ever sold a PC keyboard with one. Not /commercially,/
;; anyway.

;; Though it isn't officially reserved for user keybinds -- only ~C-c letter~ is
;; -- /I've/ never seen an Emacs package that uses them, so it's pretty safe for
;; user keybinds and (potentially) more ergonomic.

;; If you aren't me, you'll need to modify your keyboard layout in order to bind
;; something to it. Personally, as an X user, I've got an ~.Xmodmap~ that sets
;; my Caps Lock key to ~Control_L~ and my left Control key to ~Hyper_L~. Nobody
;; *needs* Caps Lock, that's what ~C-x C-u~ is for!

;; (Actually, for the rare times that I *do* need to toggle Caps Lock -- like
;; sometimes a script will mess up my modifier state and I need to turn it back
;; off -- I've bound the Menu key to Caps Lock. Nobody needs Caps Lock, but
;; *nobody* needs Menu.)

;;; Code:

;;; Put ~debug-on-*~ Calls Here

;; (debug-on-entry 'get-scratch-buffer-create)

;;; Meta-Configuration

;; Rather than spread everything out into a billion little if-feature-detected
;; guards with no coordination, I've chosen to define a number of named feature
;; flags right up top here, so that later parts of the configuration may
;; reference them as they please.

;;;; Keys and Input

(defconst mememe--commands-to-remember
  '((align-regexp)
    (back-to-indentation)
    (down-list)
    (exchange-point-and-mark)
    (global-text-scale-adjust)
    (ibuffer-filter-by-used-mode ibuffer-mode-map ibuffer)
    (isearch-forward-word)
    (isearch-repeat-forward isearch-mode-map)
    (isearch-toggle-case-fold isearch-mode-map)
    (up-list)
    (xref-find-definitions)
    (speedbar))
  "Alist of commands I use rarely but don't want to forget.

Each entry takes the form (COMMAND &optional MAP REQUIRE), where MAP
will be passed to `where-is-internal' as the key map(s) to look in, and
REQUIRE will be `require'-ed to load the keymap if non-nil.")

(defconst mememe--key-prefix
  (if (or (daemonp) (display-graphic-p)) "H-" "<home> ")
  "Literal string to prefix to every non-shadow key binding.")

(defconst mememe--swap-backspace-and-del
  (not (or (daemonp) (display-graphic-p)))
  "Should we swap <Backspace> and <DEL>?

This will resolve the \"Backspace invokes help\" problem.")

;;;; Appearance

(defconst mememe--assume-small-screen (eq system-type 'android)
  "Should we should assume our frame is going to be very small?")

(defconst mememe--configure-fonts (or (daemonp) (display-graphic-p))
  "Should we configure fonts?")

(defconst mememe--configure-graphical-background
  (or (daemonp) (display-graphic-p))
  "Should we configure the frame background (transparency and hints)?")

;;;; Packages

(defconst mememe--external-dependencies
  (pcase system-type
    ('gnu/linux '(ls
                  bash
                  git
                  html-tidy
                  libvterm
                  rust-analyzer
                  slint-lsp
                  (jdks . [(:name
                            "JavaSE-17"
                            :path
                            "/usr/lib/jvm/java-17-openjdk/"
                            :default
                            t)
                           (:name
                            "JavaSE-21"
                            :path
                            "/usr/lib/jvm/java-21-openjdk/")])
                  eclipse-jdt-ls
                  xdg-open))
    ('android   '(ls bash git)))
  "Pseudo-alist of external (outside Emacs) dependencies we'll rely on.

See `assoc-default' for what I mean by \"pseudo-alist.\" Values will be
picked out of this list with a call like:

(assoc-default 'foo mememe--external-dependencies #'eq t)

Expected formats of associated values are key-specific, I'll document them here.

 'jdks   should be usable as the value of `lsp-java-configuration-runtimes'

Some of these dependencies are fetched or created automatically by
relevant tools, like libvterm, others are system-wide. Either way: if
they aren't listed here, we shouldn't do anything that needs them. (If
we can help it.)")

(defconst mememe--use-lsp (not (eq system-type 'android))
  "Should we install and configure LSP-related packages?")

(defconst mememe--default-assembly-language
  (pcase (car (split-string system-configuration "-" nil nil))
    ("m68k"                             ; my beloved
     '68000)

    ((or "aarch64"   "aarch64_be"  "arm"           "arm64_32"
         "armeb"     "armebv7r"    "armv4t"        "armv5te"
         "armv6"     "armv6k"      "armv7"         "armv7a"
         "armv7k"    "armv7r"      "armv7s"        "thumbv4t"
         "thumbv5te" "thumbv6m"    "thumbv7a"      "thumbv7em"
         "thumbv7m"  "thumbv7neon" "thumbv8m.base" "thumbv8m.main")
     'arm)

    ((or "mips"        "mips64"        "mips64el"    "mipsel"
         "mipsisa32r6" "mipsisa32r6el" "mipsisa64r6" "mipsisa64r6el")
     'mips)

    ((or "riscv32gc"   "riscv32i"   "riscv32im"
         "riscv32imac" "riscv32imc" "riscv64gc" "riscv64imac")
     'riscv)

    ((or "powerpc" "powerpc64" "powerpc64le") ; would love to own one day...
     'power)

    ("s390x"
     'system/390)

    ((or "sparc" "sparc64" "sparcv9")
     'sparc)

    ((or "wasm32" "wasm64")
     'wasm)

    ((or "i386" "i586" "i686" "x86_64") ; my beloathed
     'x86))
  "What kind of assembler should we configure?")

;;;; Behavior

(defconst mememe--force-single-width-characters nil
  "Should we force Emacs to never treat any character wider than one cell?

Not a very good idea. Appealing, though...")

;;; Set Up Sources

;;;; Load Paths

;; I like to put my manually-installed or self-written Elisp under the "lisp"
;; directory of my user's Emacs data dir.

(let* ((my-elisp  (expand-file-name "lisp"   user-emacs-directory))
       (my-themes (expand-file-name "themes" my-elisp)))
  (add-to-list 'load-path my-elisp)
  (add-to-list 'custom-theme-load-path my-themes))

;;;; Packages

;; I like up-to-date packages, and I like having lots of packages. I don't,
;; however, completely /trust/ MELPA. So let's prefer gnu and nongnu, even if it
;; means slightly older packages.

;; We're also going to use the use-package macro for as much configuration as
;; possible.

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setopt package-install-upgrade-built-in t)

(setopt package-archive-priorities '(("gnu"    . 30)
                                     ("nongnu" . 20)
                                     ("melpa"  . 10)))

(package-initialize)

(require 'use-package)

;;;; Custom File

;; I don't dislike Customize as much as a lot of people seem to, but I also
;; don't really want to just commit its contents to my dotfiles repo. Nobody
;; needs to know my local safe variable lists, do they? /I/ don't even really
;; need to know that.

;; So we'll stick that gobbledygook somewhere else.

(use-package cus-edit
  :demand t
  :custom (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :config (load custom-file))

;;; Appearance

;;;; Theme

;; Use my custom theme.

(use-package custom :custom (custom-enabled-themes '(adora)))

;;;; Background

;; Thank god we don't just have to make the whole frame transparent anymore.
;; Thank you, Po Lu and Emacs 29!

;; Also, dark mode hint.

(when mememe--configure-graphical-background
  (set-frame-parameter (selected-frame) 'alpha-background 90)
  (add-to-list 'default-frame-alist '(alpha-background . 90))

  (use-package frame :custom (frame-background-mode 'dark)))

;;;; Font / Text Settings

;; I'm not particular about very many things, but I really, really despise
;; inconsistent character widths and heights in otherwise monospace contexts.
;; Here lie my vain, hacky attempts to force this issue.

;;;;; Default Font

(set-face-attribute 'default nil :family "SauceCode Pro NFM" :height 104)

;;;;; Fallback Font

(when mememe--configure-fonts
  (setopt inhibit-compacting-font-caches t)
  (setopt use-default-font-for-symbols t)

  (defun mememe-force-default-fontset (frame)
    (with-selected-frame frame
      (set-fontset-font t 'unicode (font-spec :name "Sarasa Mono J")
                        nil
                        nil)
      (set-fontset-font t 'unicode (font-spec :name "Noto Sans Mono")
                        nil
                        'append)
      (set-fontset-font t 'unicode (font-spec :name "Symbola")
                        nil
                        'append)
      (message "Forced fontset for frame %s" (frame-parameter frame 'name))))

  (if (daemonp)
      (add-hook 'after-make-frame-functions 'mememe-force-default-fontset)
    (mememe-force-default-fontset (selected-frame))))

;;;;; Make All Characters <= 1 Wide

;; XXXXX HACK DONT FAIL KLUDGE XXXXX

(when mememe--force-single-width-characters
  (setopt cjk-ambiguous-chars-are-wide nil)

  (map-char-table
   (lambda (range width)
     (when (> width 1)
       (set-char-table-range char-width-table range 1)))
   char-width-table))

;;;;; Make Zero-Width Characters /Actually/ Display Zero-Width

;; I suppose it depends on what you're doing, but `org-mode' uses ZWSP as an
;; escape character all the time. I do not want that to fuck over my column
;; alignment.

;; Later, we'll hook `whitespace-mode' to make these more visible when we want
;; that.

(defconst mememe-zero-width-characters
  '((#x200B . ("ZWSP" . 'empty-box))  ; ZERO WIDTH SPACE
    (#x200C . ("ZWNJ" . 'empty-box))  ; ZERO WIDTH NON-JOINER
    (#x200D . ("ZWJ"  . 'empty-box))  ; ZERO WIDTH JOINER
    (#xFEFF . ("ZBOM" . 'empty-box))) ; ZERO WIDTH NO-BREAK SPACE (BOM)
  "Alist of characters we want to display as zero-width.

Association of (CODEPOINT-OR-RANGE . DISPLAY) pairs, where DISPLAY is
some \"element\" as described in the help page for
`glyphless-char-display'.")

(dolist (char mememe-zero-width-characters)
  (set-char-table-range glyphless-char-display (car char) 'zero-width))

;;;; Initial Buffer

;; *scratch* is my friend :)

(setopt initial-buffer-choice t)
(setopt inhibit-startup-screen t)

;;;; Mode Line

;; NOTE: Emacs 31 is going to bring mode-line-collapse-minor-modes, apply that
;; once it has landed. I'm tired of not having a built-in way to disable minor
;; mode lighters! (Mucking around with alists notwithstanding.) I love me some
;; minor modes, but I do NOT need visual confirmation of all eight hundred of
;; them at once. hideshow and sub-word, you're gonna get it...

;; On Android, we just want to compress the mode line. There's not a lot of
;; screen space on that thing!

;; On desktop, I want to split up the default mode bar to separate my major mode
;; from my minor modes. The proper way to do this is to write your own
;; functions. I have not done this.

;; Don't do what I'm doing. It's bad. There's no reason to go rummaging around
;; in these internal data structures like this. It works (for now) but you
;; shouldn't do it. I'm just lazy.

(when mememe--assume-small-screen
  (setopt mode-line-compact          t)
  (setopt mode-line-percent-position nil)) ; Who ever needs this?

(unless mememe--assume-small-screen
  (setopt mode-line-compact 'long)      ; `list-packages' wigs out otherwise,
                                        ; IDK why. INREQ!

  (setopt mode-line-percent-position nil)

  (setopt
   mode-line-format
   '("%e"
     mode-line-front-space
     (:propertize
      (""
       mode-line-mule-info
       mode-line-client
       mode-line-modified
       mode-line-remote
       mode-line-window-dedicated)
      display (min-width (6.0)))

     mode-line-frame-identification
     mode-line-buffer-identification

     " "                                       ; Reduced from 3 spaces.

     mode-line-position

     " "                                       ; Spacer I've kept

     ;; Ugly HACK To Pull Out Compilation / Recursive Edit Status & Major Mode

     ;; I tried using backquote, but it didn't work. I clearly messed it up
     ;; somehow. :/ Really should just write this all explicitly...
     (:eval
      (format-mode-line
       (list
        (nth 0 mode-line-modes)                ; Compilation status
        (nth 1 mode-line-modes)                ; Recursive edit open brackets
        ;; ...                                 ; Literal ")"
        (nth 3 mode-line-modes)                ; Major mode
        (nth 4 mode-line-modes)                ; Process status
        (car (last mode-line-modes 2)))))      ; Recursive edit closing brackets

     " "                                       ; Spacer I've added.

     (project-mode-line project-mode-line-format)
     (vc-mode vc-mode)

     mode-line-format-right-align

     ;; Ugly HACK To Pull Out Minor Modes
     (:eval
      (format-mode-line
       (append
        (butlast (nthcdr 5 mode-line-modes) 3) ; List of minor modes
        ;; ...                                 ; Literal ")"
        ;; ...                                 ; Recursive edit closing brackets
        (last mode-line-modes))))              ; Literal spacer " "

     mode-line-misc-info
     " "                                       ; Extra spacer I've added.
     mode-line-end-spaces)))

;;; Configuring Editor Behaviors

;;;; Disable GUI Dialog Boxes

(setopt use-dialog-box nil)

;;;; Window-Splitting

;; I'd rather see a vertical split than a horizontal one.

(unless mememe--assume-small-screen
  (use-package window
    :custom (split-width-threshold 160) (split-height-threshold 90)))

;;;; Enable *case-region

;; Who needs Caps Lock?

(defconst mememe-disabled-to-enable
  '(upcase-region downcase-region narrow-to-region)
  "Disabled commands to enable on startup.")

(dolist (command mememe-disabled-to-enable)
  (put command 'disabled nil))

;;;; Formatting Style

;; One space after a period, fill to about half a frame.

(setopt sentence-end-double-space nil)

(setopt fill-column (if mememe--assume-small-screen 50 120))

;;;; Trash, Don't Delete

;; Just to be safe. I always delete things I don't mean to...

(setopt delete-by-moving-to-trash t)

;;; Global Commands and Keys

;; Global commands and key definitions that don't have anything to do with any
;; specific modes.

;; I want easy access to `query-replace-regexp', since it's the most powerful of
;; the *-replace-* functions. Follow it up with !, and it's the same as
;; `replace-regexp'; escape your regexp syntax, and it's the same as
;; `query-replace-string' or `replace-string';

;; I want some convenient opposites of my commonly-used commands,

;; and I also want a few extra convenience helpers.

;;;; But First, Some Housekeeping

(when mememe--swap-backspace-and-del
  ;; (keyboard-translate ?\C-h ?\C-?)
  (key-translate "C-h" "<DEL>")
  (keymap-set global-map "C-x ?" 'help-command))

(when (key-valid-p (string-trim mememe--key-prefix))
  (unbind-key (string-trim mememe--key-prefix) global-map))

;;;; Global Map

(defun mememe-unfill-paragraph ()
  "Un-fill a paragraph at point.

Essentially the opposite of `fill-paragraph'"
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(keymap-set global-map "M-Q" #'mememe-unfill-paragraph)

(defun mememe-yank-pop-forwards (uarg)
  "`yank-pop' with a reversed understanding of the prefix uargument."
  (interactive "p")
  (yank-pop (- uarg)))
(keymap-set global-map "M-Y" #'mememe-yank-pop-forwards)

(defun mememe-deactivate-mark (uarg)
  "Interactive wrapper around `deactivate-mark'."
  (interactive "p")
  (deactivate-mark uarg))
(keymap-set global-map "C-S-SPC" #'mememe-deactivate-mark)

(keymap-set global-map "<mouse-9>" #'next-buffer)
(keymap-set global-map "<mouse-8>" #'previous-buffer)

(when (or (daemonp) (display-graphic-p))
  (defun mememe-yank-with-target (target)
    "Yank system clipboard contents using a specific ICCCM TARGET atom."
    (interactive
     (progn
       (when buffer-read-only
         (error "Buffer is read-only: %S" (current-buffer)))
       (list (intern (completing-read
                      "Target Atom: "
                      (list 'UTF8_STRING 'COMPOUND_TEXT
                            'STRING      'text/plain\;charset=utf-8
                            'text/plain  'text/html
                            'TIMESTAMP   'TARGETS))))))

    (let ((interprogram-paste-function
           (lambda ()
             (let ((s (gui-get-selection 'CLIPBOARD target)))
               (cond ((stringp s) s)
                     (s           (format "%S" s))
                     (t           (error "No data for atom!")))))))
      (yank)))

  (keymap-set global-map (concat mememe--key-prefix "y")
              #'mememe-yank-with-target)
  (keymap-set global-map "C-c y" #'mememe-yank-with-target))

(defun mememe-de-fancy-unicode-buffer (&optional buffer)
  (interactive)
  "Replace fancy unicode characters in BUFFER with ASCII equivalents.

I just use it to clean up copy-pasted text for when I'm working in Org
mode.

BUFFER may be nil, in which case it will operate on the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (save-mark-and-excursion
      (save-match-data
        (replace-string "—"             ; Em Dash
                        "--" nil (point-min) (point-max))
        (replace-regexp (rx (| "–"      ; En Dash
                               "¬"))    ; Not Sign, I've seen OCR mix these up
                        "-" nil (point-min) (point-max))
        (replace-regexp (rx (| "“"      ; Left Double Quotation Mark
                               "”"))    ; Right Double Quotation Mark
                        "\"" nil (point-min) (point-max))
        (replace-regexp (rx (| "‘"      ; Left Single Quotation Mark
                               "’"))    ; Right Single Quotation Mark
                        "'" nil (point-min) (point-max))))))

;;;; Toggles Map

(defvar-keymap mememe-toggle-map)
(keymap-set global-map (concat mememe--key-prefix "t") mememe-toggle-map)
(keymap-set global-map "C-c t" mememe-toggle-map)

;;;; Search and Replace Map

(defvar-keymap mememe-search-and-replace-map :repeat t)
(keymap-set global-map
            (concat mememe--key-prefix "s")
            mememe-search-and-replace-map)
(keymap-set global-map "C-c s" mememe-search-and-replace-map)

(keymap-set mememe-search-and-replace-map "H-s" #'search-forward-regexp)
(keymap-set mememe-search-and-replace-map "s"   #'search-forward-regexp)
(keymap-set mememe-search-and-replace-map "H-r" #'query-replace-regexp)
(keymap-set mememe-search-and-replace-map "r"   #'query-replace-regexp)

;;;; Insert Map

(defvar-keymap mememe-insert-map :repeat t)
(keymap-set global-map (concat mememe--key-prefix "i") mememe-insert-map)
(keymap-set global-map "C-c i" mememe-insert-map)

(keymap-set mememe-insert-map "H-s" "¯ \\ _ ( ツ ) _ / ¯")
(keymap-set mememe-insert-map "s"   "¯ \\ _ ( ツ ) _ / ¯")

(keymap-set mememe-insert-map "H-#" "█")
(keymap-set mememe-insert-map "#"   "█")

(keymap-set mememe-insert-map "H-," "‚")
(keymap-set mememe-insert-map ","   "‚")

(defun mememe-insert-zwsp (uarg)
  "Insert zero-width spaces according to prefix argument."
  (interactive "p")
  (insert-char #x200b uarg))
(keymap-set mememe-insert-map "H-z" #'mememe-insert-zwsp)
(keymap-set mememe-insert-map "z"   #'mememe-insert-zwsp)

;;;; Miscellaneous Map

(defvar-keymap mememe-misc-map)
(keymap-set global-map (concat mememe--key-prefix "x") mememe-misc-map)
(keymap-set global-map "C-c x" mememe-misc-map)

(defun mememe-dump-buffer-local-variables ()
  (interactive)
  (with-temp-buffer-window "*Local Variables Dump*"
      #'display-buffer-reuse-window
      nil
    (prin1 (buffer-local-variables))))
(keymap-set mememe-misc-map "H-<home>" #'mememe-dump-buffer-local-variables)
(keymap-set mememe-misc-map "<home>"   #'mememe-dump-buffer-local-variables)

(defun mememe-cdmktempdir ()
  "Create a temporary directory and visit it with `dired'."
  (interactive)
  (when-let* ((tempdir (make-temp-file "cdmktmpdir" t)))
    (message tempdir)
    (dired tempdir)))

(when (assoc-default 'xdg-open mememe--external-dependencies #'eq t)
  (require 'url)

  (defun mememe--region-or-word-at-point-no-properties ()
    "Return the region as a string, or the word at point as a string.

Properties are stripped, non-contiguous regions are concatenated."
    (if (region-active-p)
        (let ((region-text (funcall region-extract-function nil)))
          (substring-no-properties (if (listp region-text)
                                       (apply #'concat region-text)
                                     region-text)))
      (word-at-point t)))

  (defun mememe-wiktionary-region-or-word ()
    "Search for the region or word at point on the English Wiktionary."
    (interactive)
    (browse-url-xdg-open
     (concat
      "https://en.wiktionary.org/wiki/Special:Search?go=Try+exact+match&search="
      (url-hexify-string (mememe--region-or-word-at-point-no-properties)
                         url-query-key-value-allowed-chars))))
  (keymap-set mememe-misc-map "H-w H-t"  #'mememe-wiktionary-region-or-word)
  (keymap-set mememe-misc-map "w t"      #'mememe-wiktionary-region-or-word)

  (defun mememe-wikipedia-region-or-word ()
    "Search for the region or word at point on the English Wikipedia."
    (interactive)
    (browse-url-xdg-open
     (concat
      "https://en.wikipedia.org/wiki/Special:Search?go=Try+exact+match&search="
      (url-hexify-string (mememe--region-or-word-at-point-no-properties)
                         url-query-key-value-allowed-chars)
      "&ns0=1")))
  (keymap-set mememe-misc-map "H-w H-p"  #'mememe-wikipedia-region-or-word)
  (keymap-set mememe-misc-map "w p"      #'mememe-wikipedia-region-or-word))

;; Maybe this ↓ should be a minor mode? Using font-lock and all that?

(defvar mememe--displaying-zero-width-chars nil)

(defun mememe-toggle-display-zero-width-chars ()
  "Toggle drawing zero-width characters."
  (interactive)
  (dolist (char mememe-zero-width-characters)
    (set-char-table-range
     glyphless-char-display (car char)
     (if mememe--displaying-zero-width-chars 'zero-width (cdr char))))
  (setq mememe--displaying-zero-width-chars
        (not mememe--displaying-zero-width-chars)))

;;; Global Mode / Package Configuration

;; That is, configuration for global minor modes, global configuration for
;; /local/ minor modes, global configuration for heavily-derived major modes
;; (y'know, like special-mode or comint-mode), and configuration for packages
;; that don't provide any modes.

;;;; `align-mode' (built-in)

(use-package align
  :defer t
  :bind
  (:map mememe-misc-map
        ("H-a" . align-regexp)
        ("a" . align-regexp)))

;;;; `auto-revert-mode' (built-in)

;; Globally automatically revert any buffer associated with a file when that
;; file changes on disk.

(use-package autorevert :custom (global-auto-revert-mode t))

;;;; `column-number-mode' (built-in)

;; Display column number in the mode line, next to the line number. Looks like
;; this: (920,14)

(use-package simple :custom (column-number-mode t))

;;;; `company-mode'

;; Completion.

(use-package company
  :ensure t
  :custom (company-idle-delay 0.1)      ; how long to wait until popup
  :bind (:map mememe-toggle-map
              ("H-c" . company-mode)
              ("c" . company-mode)))

;;;; `conf-mode'

(defconst mememe-equals-header-regexp
  (rx (not "=")                         ; no leading =s
      (group-n 1
        (group-n 2 (>= 2 "="))          ; some # of =s
        " "
        (minimal-match (one-or-more (any upper digit punct ?\s)))
        " "
        (backref 2))                    ; same # of =s as the left side
      (not "="))                        ; and no more
  "Match strings of uppercase letters surrounded by balanced \"=\" signs.

Actual match is contained within capture group 1.")

(require 'cl-macs)

(defun mememe-comment-header-p (bound)
  "Comment header matcher for `font-lock-keywords'.

Returns non-nil and updates `match-data' if it matches, as prescribed by
font-lock-keywords."
  (let ((case-fold-search nil))
    (cl-loop while (re-search-forward mememe-equals-header-regexp bound t)
             ;; See `parse-partial-sexp' value 4. Non-nil if in comment.
             if (save-excursion (nth 4 (syntax-ppss (match-beginning 1))))
             return t)))

(defun mememe-conf-mode-font-lock-add-keywords ()
  (font-lock-add-keywords nil
                          '((mememe-comment-header-p
                             1
                             '(:weight bold :foreground "#60afef")
                             prepend))))

(use-package conf-mode
  :defer t
  :hook ((conf-mode . mememe-conf-mode-font-lock-add-keywords)))

;;;; `display-fill-column-indicator' (built-in)

;; Displays a visual indication of where the fill column is. We're gonna enable
;; it for modes where we write a lot besides prose text.

(use-package display-fill-column-indicator
  :defer t
  :hook
  ((conf-mode
    prog-mode
    ;; Derives from `text-mode' instead of either of the previous two, for
    ;; whatever reason.
    gitattributes-mode)
   .
   display-fill-column-indicator-mode))

;;;; `display-line-numbers-mode' (built-in)

;; Enable line numbers (just the usual style) in modes where it makes sense.

(defvar mememe-display-line-numbers-exempt-modes
  '(comint-mode
    compilation-mode
    dired-mode
    dun-mode
    eshell-mode
    image-mode
    magit-status-mode
    special-mode
    speedbar-mode
    term-mode
    tetris-mode
    vterm-mode)
  "Major modes to exempt from `global-display-line-numbers-mode'.

Modes which are derived (parented by) from listed modes will also be
exempted. For example, if we specify `comint-mode', then `shell-mode'
will be implied, as it is derived from the latter. A list of a given
mode's parents can be found with the function
`derived-mode-all-parents'.")

(defun mememe-display-line-numbers-unless-exempt (original-fun &rest args)
  "Advice for `display-line-numbers--turn-on'.

Turn on line numbers unless the current major-mode (or one of its
parents) is listed in `mememe-display-line-numbers-exempt-modes'."
  (if (not
       (or (memq major-mode mememe-display-line-numbers-exempt-modes)
           (derived-mode-p mememe-display-line-numbers-exempt-modes)
           (minibufferp)))
      (apply original-fun args)))

(use-package display-line-numbers
  :custom (global-display-line-numbers-mode t)
  :config
  (advice-add
   'display-line-numbers--turn-on
   :around 'mememe-display-line-numbers-unless-exempt))

;;;; `edit-indirect'

(use-package edit-indirect
  :ensure t
  :bind (:map
         mememe-misc-map
         ("H-e H-r" . edit-indirect-region)
         ("e r" . edit-indirect-region)))

;;;; `editorconfig-mode'

(use-package editorconfig :custom (editorconfig-mode t))

;;;; The `electric-*-mode's (built-in))

;; Don't want to change whether any of these are enable from default, but I'd
;; like to be able to easily toggle them on or off.

(use-package elec-pair
  :defer t
  :bind (:map
         mememe-toggle-map
         ("H-e H-p" . electric-pair-mode)
         ("e p" . electric-pair-mode)))

(use-package electric
  :defer t
  :bind (:map
         mememe-toggle-map
         ("H-e H-i" . electric-indent-mode)
         ("e i" . electric-indent-mode)
         ("H-e H-l" . electric-layout-mode)
         ("e l" . electric-layout-mode)))

;;;; `files' (built-in)

;; Low-level file saving and loading stuff. I want trailing newlines but NO
;; other trailing whitespace.

(defvar mememe-delete-trailing-whitespace-exempt-modes '()
  "Major modes to exempt from `mememe-delete-trailing-whitespace'.

Modes which are derived (parented by) from listed modes will also be
exempted. For example, if we specify `comint-mode', then `shell-mode'
will be implied, as it is derived from the latter. A list of a given
mode's parents can be found with the function
`derived-mode-all-parents'.")

(defun mememe-delete-trailing-whitespace-unless-exempt ()
  "Wrapper around `delete-trailing-whitespace'.

Delete trailing whitespace unless the current major-mode is a member of,
or is derived from a member of,
`mememe-delete-trailing-whitespace-exempt-modes'."
  (interactive)
  (if (not
       (or (memq major-mode mememe-delete-trailing-whitespace-exempt-modes)
           (derived-mode-p mememe-delete-trailing-whitespace-exempt-modes)
           (minibufferp)))
      (delete-trailing-whitespace)))

(use-package files
  :custom (require-final-newline t)
  :hook (before-save . mememe-delete-trailing-whitespace-unless-exempt))

;;;; `flycheck-mode'

;; Automatic syntax checking.

(use-package flycheck :ensure t :defer t)

;;;; `flyspell-mode' (built-in)

;; Automatic spell-checking. We'll just let it default to whatever backend it
;; wants, 'cause I'm lazy.

;; Oh, and turn it on for comments and strings in programming modes, too!

(use-package flyspell
  :defer t
  :hook
  (((prog-mode conf-mode) . flyspell-prog-mode) (text-mode . flyspell-mode)))

;;;; `frameshot-mode'

;; Screenshots!

(use-package frameshot :ensure t :defer t)

;;;; `hl-todo-mode'

;; Highlight keywords like TODO and FAIL and FIXME and HACK and so forth.
;; Enabling globally enables for prog- and text-mode (besides org) derived major
;; modes.

(use-package hl-todo
  :ensure t
  :custom
  (global-hl-todo-mode t)
  (hl-todo-keyword-faces                 ; default definitions + a few of my own
   '(("HOLD"       . "#d0bf8f")
     ("TODO"       . "#cc9393")
     ("NEXT"       . "#dca3a3")
     ("THEM"       . "#dc8cc3")
     ("PROG"       . "#7cb8bb")
     ("OKAY"       . "#7cb8bb")
     ("DONT"       . "#5f7f5f")
     ("FAIL"       . "#8c5353")
     ("DONE"       . "#afd8af")
     ("NOTE"       . "#d0bf8f")
     ("MAYBE"      . "#d0bf8f")
     ("KLUDGE"     . "#d0bf8f")
     ("HACK"       . "#d0bf8f")
     ("TEMP"       . "#d0bf8f")
     ("FIXME"      . "#cc9393")
     ("XXXX*"      . "#cc9393")
     ;; my own additions begin here
     ("INREQ"      . "#9beeef")
     ("NEEDS INFO" . "#9beeef")
     ("REQUIRES"   . "#9eef00")
     ("PROVENANCE" . "#9eef00")))
  (hl-todo-highlight-punctuation "!:"))

;;;; `hs-minor-mode' (built-in)

;; Hide and Show blocks within curly braces (or whatever a major mode defines).

;; More specifically: if you place the point inside something like this:
;; function foobar { # does something... }
;; and then you C-c @ C-c or S-<middle mouse>, it becomes this:
;; function foobar {...}
;; Just do it again to toggle back.

(use-package hideshow :defer t ;; :hook (prog-mode . hs-minor-mode)
  :bind (:map
         mememe-toggle-map
         ("H-h" . hs-minor-mode)
         ("H-h" . hs-minor-mode)))

;;;; `ibuffer-mode' (built-in)

;; `list-buffers' but strictly more powerful.

(use-package ibuffer :defer t :bind ("C-x C-b" . ibuffer))

;;;; `icomplete-mode' (built-in)

;; Automatic completion of things in the minibuffer.

(use-package icomplete :defer t :custom (icomplete-mode t))

;;;; `indent-tabs-mode' (built-in)

(use-package simple :defer t :custom (indent-tabs-mode nil))

;;;; `inheritenv'

;; Allows background processes to inherit environment variables from their
;; calling buffers.

(use-package inheritenv :ensure t :defer t)

;;;; `kill-ring-deindent-mode'

;; Exactly what it sounds like. De-indent the kill ring.

(use-package indent-aux :custom (kill-ring-deindent-mode t))

;;;; `lsp-mode'

;; The other Language Server Protocol implementation for Emacs. Arguably the
;; better one.

;; I really don't like it when automatic punctuation matching is on all the
;; time, so I've disabled this LSP "feature".

(when mememe--use-lsp (use-package lsp-mode
                        :ensure t
                        :defer t
                        :custom
                        (lsp-enable-on-type-formatting nil)
                        (lsp-file-watch-ignored-directories
                         '("[/\\\\]\\.git\\'"
                           "[/\\\\]\\.github\\'"
                           "[/\\\\]\\.gitlab\\'"
                           "[/\\\\]\\.circleci\\'"
                           "[/\\\\]\\.hg\\'"
                           "[/\\\\]\\.bzr\\'"
                           "[/\\\\]_darcs\\'"
                           "[/\\\\]\\.svn\\'"
                           "[/\\\\]_FOSSIL_\\'"
                           "[/\\\\]\\.jj\\'"
                           "[/\\\\]\\.idea\\'"
                           "[/\\\\]\\.ensime_cache\\'"
                           "[/\\\\]\\.eunit\\'"
                           "[/\\\\]node_modules"
                           "[/\\\\]\\.yarn\\'"
                           "[/\\\\]\\.turbo\\'"
                           "[/\\\\]\\.fslckout\\'"
                           "[/\\\\]\\.tox\\'"
                           "[/\\\\]\\.nox\\'"
                           "[/\\\\]dist\\'"
                           "[/\\\\]dist-newstyle\\'"
                           "[/\\\\]\\.hifiles\\'"
                           "[/\\\\]\\.hiefiles\\'"
                           "[/\\\\]\\.stack-work\\'"
                           "[/\\\\]\\.bloop\\'"
                           "[/\\\\]\\.bsp\\'"
                           "[/\\\\]\\.metals\\'"
                           "[/\\\\]target\\'"
                           "[/\\\\]\\.ccls-cache\\'"
                           "[/\\\\]\\.vs\\'"
                           "[/\\\\]\\.vscode\\'"
                           "[/\\\\]\\.venv\\'"
                           "[/\\\\]\\.mypy_cache\\'"
                           "[/\\\\]\\.pytest_cache\\'"
                           "[/\\\\]\\.build\\'"
                           "[/\\\\]__pycache__\\'"
                           "[/\\\\]site-packages\\'"
                           "[/\\\\].pyenv\\'"
                           "[/\\\\]\\.deps\\'"
                           "[/\\\\]build-aux\\'"
                           "[/\\\\]autom4te.cache\\'"
                           "[/\\\\]\\.reference\\'"
                           "[/\\\\]bazel-[^/\\\\]+\\'"
                           "[/\\\\]\\.cache[/\\\\]lsp-csharp\\'"
                           "[/\\\\]\\.meta\\'"
                           "[/\\\\]\\.nuget\\'"
                           "[/\\\\]Library\\'"
                           "[/\\\\]\\.lsp\\'"
                           "[/\\\\]\\.clj-kondo\\'"
                           "[/\\\\]\\.shadow-cljs\\'"
                           "[/\\\\]\\.babel_cache\\'"
                           "[/\\\\]\\.cpcache\\'"
                           "[/\\\\]\\checkouts\\'"
                           "[/\\\\]\\.gradle\\'"
                           "[/\\\\]\\.m2\\'"
                           "[/\\\\]bin/Debug\\'"
                           "[/\\\\]obj\\'"
                           "[/\\\\]_opam\\'"
                           "[/\\\\]_build\\'"
                           "[/\\\\]\\.elixir_ls\\'"
                           "[/\\\\]\\.elixir-tools\\'"
                           "[/\\\\]\\.terraform\\'"
                           "[/\\\\]\\.terragrunt-cache\\'"
                           "[/\\\\]\\result"
                           "[/\\\\]\\result-bin"
                           "[/\\\\]\\.direnv\\'"
                           "[/\\\\]\\.devenv\\'"
                           "[/\\\\]fetched\\'"))))

(when mememe--use-lsp (use-package lsp-ui :ensure t :after lsp-mode))

;;;; `menu-bar-mode' (built-in)

;; I don't hate the menu bar. /But/ I do want an easy-to-remember way to turn it
;; off.

(use-package menu-bar
  :defer t
  :custom (menu-bar-mode nil)
  :bind
  (:map mememe-toggle-map
        ("H-m H-b" . menu-bar-mode)
        ("m b"     . menu-bar-mode)))

;;;; `outline-minor-mode' (built-in)

;; That which spawned our savior org-mode, useful in its own right for
;; headline-izing Elisp code. Let's turn on backtab cycling!

(use-package outline :defer t :custom (outline-minor-mode-cycle t))

;;;; `prog-mode' (built-in)

;; Highlight markdown-style equals sign headers in comments, just like
;; we did in `conf-mode'.

(defun mememe-prog-mode-font-lock-add-keywords ()
  (font-lock-add-keywords nil
                          '((mememe-comment-header-p
                             1
                             '(:weight bold :foreground "#60afef")
                             prepend))))

(use-package prog-mode
  :defer t
  :hook ((prog-mode . mememe-prog-mode-font-lock-add-keywords)))

;;;; `project' (built-in)

;; I like Emacs' new lightweight project system. I don't fully understand it,
;; but I like it.

(use-package project :defer t :custom (project-mode-line t))

;;;; `rainbow-mode'

;; Colorize RGB hex strings like #b51a43 or #667a76 with that color as the
;; background face.

;; For whatever reason, this package does not obey the typical minor-mode
;; conventions. You can't toggle it off with numeric arguments, for example.
;; PITA! One day I'll wrap or fork it.

(use-package rainbow-mode :defer t :ensure t)

;;;; `savehist-mode' (built-in)

;; Saves your minibuffer history. `recentf-mode' always gave me headaches, but
;; this is great!

(use-package savehist :custom (savehist-mode t))

;;;; `scroll-bar-mode' (built-in)

(use-package scroll-bar :custom (scroll-bar-mode nil))

;;;; `subword-mode' (built-in)

;; Treat CamelCase strings as multiple words for the purpose of `forward-word',
;; and `backward-word' etc.

(use-package subword :custom (global-subword-mode t))

;;;; `tab-bar-mode' (built-in)

;; Tabs!

(use-package tab-bar
  :defer t
  :custom (tab-bar-new-tab-to 'rightmost)
  :bind
  (("C-x t n" . tab-bar-switch-to-next-tab) ; Was `tab-duplicate'.
   ("C-x t p" . tab-bar-switch-to-prev-tab) ; Was `project-other-tab-command'.
   :map mememe-toggle-map
   ("H-t H-b" . tab-bar-mode)
   ("t b"     . tab-bar-mode)))

;;;; `text-scale-mode' (built-in)

;; Not the best for scaling text, but not the worst.

(use-package face-remap :custom (global-text-scale-adjust-resizes-frames t))

;;;; `titlecase'

;; I'm sick of doing `capitalize-region' and then manually adjusting. This is,
;; as it's author states, a best-effort attempt to do it right. It hasn't given
;; me any problems, so I appreciate that.

(use-package titlecase
  :vc (:url "https://codeberg.org/acdw/titlecase.el.git"
            :rev :newest)
  :ensure t
  :custom
  ;; Wikipedia-style is actually already the default, but I respect it enough
  ;; that I want to explicitly agree.
  ((titlecase-style 'wikipedia)
   (titlecase-downcase-sentences t))
  :bind
  (:map mememe-misc-map
        ("H-t" . titlecase-dwim)
        ("t"   . titlecase-dwim)))

;;;; `tool-bar-mode' (built-in)

(use-package tool-bar :custom (tool-bar-mode nil))

;;;; `vlf-mode'

;; Specialized mode for opening Very Large Files. Loading `vlf-setup'

(use-package vlf :ensure t :config (require 'vlf-setup))

;;;; `yas-minor-mode'

;; Snippets. We want them in the minibuffer, and we want them now!

(use-package yasnippet
  :ensure t
  :custom (yas-global-mode t)
  :hook (minibuffer-setup . yas-minor-mode)

  ;; We have to do our keybinding here because `use-package' isn't smart enough
  ;; to understand this constant.
  :config (keymap-set minibuffer-local-map "<tab>" yas-maybe-expand))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;;; Topic-Wide Major and Minor Mode Configuration

;;;; Ada

;; The Ada programming language. I usually prefer `lsp-mode' over `eglot', but
;; I'm not touching this until I remember what these settings mean.

(when mememe--use-lsp (use-package ada-mode
                        :ensure t
                        :custom
                        (ada-indent-backend 'eglot)
                        (ada-statement-backend 'eglot)))

;;;; Assembly

;; Always use `nasm-mode' over `asm-mode' on x86. Not touching gas with a
;; ten-meter pole...

(when (eq mememe--default-assembly-language 'x86)

  (dolist (mode-association auto-mode-alist)
    (when (eq (cdr mode-association) 'asm-mode)
      (setcdr mode-association 'nasm-mode)))

  (use-package nasm-mode :ensure t :defer t))

;;;; C

(use-package cc-mode :ensure t :custom (c-basic-offset 4))

;;;; CUDA

;; We'll just inherit from our `cc-mode' settings.

(use-package cuda-mode :ensure t)

;;;; Dired / Filesystem More Generally

;; TODO: figure out how to add advice to dired-omit where it won't toggle on if
;; a buffer doesn't contains any files (files, not directories) that would be
;; visible.

(use-package dired-x :after (dired) :hook (dired-mode . dired-extra-startup))

(when (assoc-default 'ls mememe--external-dependencies #'eq t)
  (use-package dired
    :defer t
    :custom
    (dired-use-ls-dired t)
    (dired-maybe-use-globstar t)
    (dired-vc-rename-file t)
    (dired-listing-switches
     (string-join
      '("-l"          ; long output format, required
        "--all"       ; duh
        "--si"        ; same as ~--human-readable~, but powers of 10 not 2
        "--classify") ; "append indicator (one of */=>@|) to entries"
      " "))))

(use-package speedbar
  :defer t
  :bind (:map mememe-toggle-map ("H-s H-b" . speedbar) ("s b" . speedbar)))

;;;; Emacs Lisp

;; Turn on outlines for Elisp, bring in an auto-formatter package.

(use-package elisp-mode :hook (emacs-lisp-mode . outline-minor-mode))

(use-package elisp-autofmt
  :ensure t
  :defer t
  :hook emacs-lisp-mode)

;;;; git

;; Gotta love `magit'. Let's pull in git-modes too, to give us some major-modes
;; for .gitattributes and .gitignore and so forth.

(use-package git-modes :ensure t :defer t)

(when (assoc-default 'git mememe--external-dependencies #'eq t)
  (use-package magit
    :ensure t
    :defer t
    :bind (:map mememe-toggle-map ("H-g" . magit-status) ("g" . magit-status))))

;;;; GLSL

(use-package glsl-mode :ensure t :defer t :mode ("\\.vsh" "\\.fsh"))

;;;; Gopher

;; As in the application protocol.

(use-package elpher :ensure t :defer t)

;;;; gnuplot

;; There's a gnuplot package and a gnuplot-mode package, I'm not sure what the
;; difference is. This one seems reasonable?

(use-package gnuplot :ensure t :defer t)

;;;; HTML/XHTML

;; HTMLize is a neat package that "renders" an HTML buffer in-Emacs.

(when (or (daemonp) (display-graphic-p))
  (defun mememe-yank-with-html-target ()
    (interactive)
    (mememe-yank-with-target 'text/html))
  (use-package mhtml-mode :bind (:map mhtml-mode-map
                                 ("C-M-y" . mememe-yank-with-html-target))))

(use-package htmlize :ensure t :demand t)

;;;; Java

;; Java is not a language I want to write without a language server.

(when (and mememe--use-lsp
           (assoc-default 'eclipse-jdt-ls
                          mememe--external-dependencies
                          #'eq
                          t)
           (assoc-default 'jdks
                          mememe--external-dependencies
                          #'eq
                          t))
  (use-package lsp-java
    :ensure t
    :after lsp-mode
    :custom
    (lsp-java-vmargs
     '("-XX:+UseParallelGC"
       "-XX:GCTimeRatio=4"
       "-XX:AdaptiveSizePolicyWeight=90"
       "-Dsun.zip.disableMemoryMapping=true"
       "-Xmx2G"                         ; locks up really easily without this
       "-Xms100m"))
    (lsp-java-configuration-runtimes (assoc-default 'jdks
                                                    mememe--external-dependencies
                                                    #'eq
                                                    nil)))

  (use-package lsp-mode :ensure t :defer t :hook java-mode))

;;;; JSON

(use-package js
  :mode ("\\.mcmeta" . js-json-mode)
  :custom (js-indent-level 2))

;;;; Kotlin

;; I may or may not set up LSP here too at some point. For now, all I want is
;; syntax highlighting.

(use-package kotlin-mode :ensure t :defer t)

;;;; LaTeX

(use-package auctex :ensure t)

(use-package reftex :hook LaTeX-mode)

;;;; Man / Help / Info / Other Documentation

;; Unless I choose otherwise, I want all documentation to share one window.

;; TODO: make Man behave how I want it to, rewrite all of this to use groups
;; because I don't like how it's behaving.

(defun mememe-display-buffer-maybe-same-window-by-mode (buffer alist)
  "Conditionally display BUFFER in the selected window.

If the major-mode of the selected window is either equal to, or a member
of, the mode entry of the action alist, display BUFFER in the selected
window. Sort of like `display-buffer--maybe-same-window', but broader.

See Info node `(elisp) Buffer Display Action Alists' for more about the
mode entry."
  (when-let* ((mode (alist-get 'mode alist))
              (_    (with-selected-window (selected-window)
                      (memq major-mode mode))))

    (display-buffer-same-window buffer alist)))

(defvar mememe-doc-buffer-modes
  '(help-mode apropos-mode finder-mode Man-mode Info-mode shortdoc-mode)
  "Major-modes we'll consider \"Documentation\".")

(defun mememe-doc-buffer-p (buffer-or-name &rest _args)
  "Return non-nil if BUFFER-NAME is a documentation buffer."
  (let ((buffer-name (if (stringp buffer-or-name)
                         buffer-or-name
                       (buffer-name buffer-or-name))))
    (and
     (not (when (bufferp buffer-or-name) (minibufferp buffer-or-name)))
     (with-current-buffer buffer-or-name
       (memq major-mode mememe-doc-buffer-modes)))))

(add-to-list
 'display-buffer-alist
 `(mememe-doc-buffer-p (display-buffer--maybe-same-window
                        mememe-display-buffer-maybe-same-window-by-mode
                        display-buffer-reuse-window
                        display-buffer-reuse-mode-window
                        display-buffer--maybe-pop-up-frame-or-window
                        display-buffer-in-previous-window
                        display-buffer-use-some-window
                        display-buffer-pop-up-frame)

                       (mode                 . ,mememe-doc-buffer-modes)
                       (inhibit-switch-frame . t)))

(use-package help-mode
  :custom
  ((describe-bindings-show-prefix-commands t)
   (help-enable-symbol-autoload t)
   (help-enable-symbol-autoload t)
   (help-enable-symbol-autoload t)
   (help-window-keep-selected t)
   (help-window-select t)))

(use-package man :custom (Man-notify-method 'thrifty))

;;;; Org

;; Good 'ol `org-mode'. Here's what I think are some reasonable defaults, plus
;; some other stuff, and a little package I wrote to enable telephone links.

;; TODO: clean up all of these initial implementations, maybe merge some of the
;; simpler ones. Fix some of the bugs.

(defun mememe-ox-filter-html-tidy (string backend comm-channel)
  "Clean Org mode (X)HTML export with the html-tidy CLI tool.
Add this function to `org-export-filter-final-output-functions' to use
it. This can be done on a per-file basis with the #+BIND keyword if you
enable the `org-export-allow-bind-keywords' user option."
  (when (org-export-derived-backend-p backend 'html)
    (when-let* ((stderr-tmp-file (make-temp-file "mememe-ox-filter-html-tidy"))
                (output-file (plist-get comm-channel :output-file))
                (input-file (plist-get comm-channel :input-file))
                (lang (plist-get comm-channel :language)))
      (message "mememe-ox-filter-html-tidy: processing %S" output-file)
      (with-temp-buffer
        (insert string)
        (let ((exit-status
               (call-process-region nil nil
                                    "tidy"
                                    t (list t stderr-tmp-file) nil
                                    "-language"                lang
                                    "--add-xml-decl"           "yes"
                                    "--output-xhtml"           "yes"
                                    "--char-encoding"          "utf8"
                                    "--output-bom"             "no"
                                    "--clean"                  "yes"
                                    "--drop-empty-elements"    "yes"
                                    "--drop-empty-paras"       "yes"
                                    "--merge-divs"             "auto"
                                    "--merge-spans"            "auto"
                                    "--numeric-entities"       "yes"
                                    "--fix-style-tags"         "yes"
                                    "--fix-uri"                "yes"
                                    "--lower-literals"         "yes"
                                    "--repeated-attributes"    "keep-last"
                                    "--strict-tags-attributes" "yes"
                                    "--escape-cdata"           "no"
                                    "--join-styles"            "yes"
                                    "--merge-emphasis"         "yes"
                                    "--replace-color"          "yes"
                                    "--indent"                 "yes"
                                    "--indent-with-tabs"       "no"
                                    "--keep-tabs"              "no"
                                    "--tidy-mark"              "yes")))
          (with-temp-buffer
            (insert-file-contents stderr-tmp-file)
            (save-match-data
              (while (re-search-forward
                      (rx "line" (*? space) (group-n 1 (+ digit))
                          (*? space)
                          "column" (*? space) (group-n 2 (+ digit))
                          (*? space) ?- (*? space)
                          (group-n 3 (or "Warning" "Error")) ?:
                          (group-n 4 (*? anything))
                          eol)
                      nil t)

                (message "mememe-ox-filter-html-tidy: %s:%s:%s - %s:%s"
                         output-file
                         (match-string-no-properties 1)
                         (match-string-no-properties 2)
                         (match-string-no-properties 3)
                         (match-string-no-properties 4)))))

          (delete-file stderr-tmp-file nil)
          (when (= exit-status 2)
            (error
             "mememe-ox-filter-html-tidy: tidy exited with code 2 indicating errors!")))
        (buffer-substring-no-properties (point-min) (point-max))))))

(require 'xmltok)

(defun mememe--get-xmltok-attr (attr-name)
  (catch 'found
    (dolist (attr xmltok-attributes)
      (when (string-equal
             (buffer-substring-no-properties (xmltok-attribute-name-start attr)
                                             (xmltok-attribute-name-end   attr))
             attr-name)
        (throw 'found attr)))))

(defun mememe-ox-filter-html-trim-example-blocks (string backend comm-channel)
  "Trim excess newlines from example blocks for Org (X)HTML export.
Add this function to `org-export-filter-example-block-functions' to use
it. This can be done on a per-file basis with the #+BIND keyword if you
enable the `org-export-allow-bind-keywords' user option."
  (when (org-export-derived-backend-p backend 'html)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (xmltok-forward)
      (message
       "mememe-ox-filter-html-trim-example-blocks: trimming example %S in %S"
       (xmltok-attribute-value (mememe--get-xmltok-attr "id"))
       (plist-get comm-channel :output-file))
      (when (= (char-after) ?\C-j) (delete-char 1))
      (xmltok-forward)
      (when (= (char-before) ?\C-j) (backward-delete-char 1))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun mememe--recurring-fmt-id-p (id)
  "Return (PREFIX NUMBER COUNT) for PREFIX(.NUMBER+) ids, else nil
where
    NUMBER    is the string form of NUMBER, and
    COUNT     is the number of times the .NUMBER appears."
  (when-let* ((i (string-match-p (rx "." (+ digit) eos) id)))
    (let* ((count 1)
           (number (substring id (1+ i)))
           (suffix (substring id i))
           (prefix
            (substring id nil
                       (cl-loop for j from i downto 0 by (- (length id) i)
                                while (string-suffix-p suffix
                                                       (substring id nil j))
                                do (setq count (1+ count))
                                finally return j))))
      (list prefix number count))))

(defun mememe--underscore-fmt-id-parse (id)
  "Return (PREFIX . NUMBER-OR-NIL) for PREFIX(_NUMBER?) ids
where
    NUMBER-OR-NIL    is either the number (as an integer) after the
                     underscore, or nil if the underscore was omitted."
  (let ((i (string-match-p (rx "_" (+ digit) eos) id)))
    (cons (substring id nil i)
          (when i (string-to-number (substring id (1+ i)))))))

(defun mememe-ox-filter-html-dedup-ids (string backend comm-channel)
  "Fix duplicate element ids in Org (X)HTML export.
Just naïvely looks for elements with the qname id and notes down if it
finds multiple examples of the same thing. The first element (from the
top of the document) with a given id is assumed to be the \"correct\"
element for that id, all subsequent elements with the same id are
assumed to be the duplicates.

If the id follows the format PREFIX(.NUMBER+), e.g. \"fnr.1.1\", then
the rewriting engine will continue this format to de-duplicate.
Otherwise, they will be suffixed _NUMBER, with NUMBER starting at 1 for
the first encountered duplicate and increasing by one for every
subsequent duplicate -- skipping any values that were found in the
document.

For example, running this filter on the document

<foo id=\"alice\">
  <bar id=\"bob\">
    <baz id=\"bob\"></baz>
  </bar>
  <bar id=\"fnr.1\"></bar>
  <bar id=\"fnr.1.1\"></bar>
  <bar id=\"fnr.1\">
    <baz></baz>
  </bar>
  <bar id=\"fnr.1\">
    <baz></baz>
  </bar>
  <qux id=\"bob_1\"/>
  <qux id=\"bob_1\"/>
  <qux id=\"bob_2\"/>
</foo>

produces the result

<foo id=\"alice\">
  <bar id=\"bob\">
    <baz id=\"bob_3\"></baz>
  </bar>
  <bar id=\"fnr.1\"></bar>
  <bar id=\"fnr.1.1\"></bar>
  <bar id=\"fnr.1.1.1\">
    <baz></baz>
  </bar>
  <bar id=\"fnr.1.1.1.1\">
    <baz></baz>
  </bar>
  <qux id=\"bob_1\"/>
  <qux id=\"bob_3\"/>
  <qux id=\"bob_2\"/>
</foo>

Does not perform any hyperlink rewriting, it is assumed that all links
to a given id were meant to point to the first such instance.

Add this function to `org-export-filter-final-output-functions' to use
it. This can be done on a per-file basis with the #+BIND keyword if you
enable the `org-export-allow-bind-keywords' user option."
  (when (org-export-derived-backend-p backend 'html)
    (message "mememe-ox-filter-html-dedup-ids: processing %S"
             (plist-get comm-channel :output-file))
    (with-temp-buffer
      (insert string)
      (xmltok-save
        (goto-char (point-min))
        (xmltok-forward-prolog)
        ;; same format as `dup-ids', but for the rewritten ids.
        (let ((new-ids))
          (let (;; set of encountered attribute values for the id attribute.
                (seen-ids (make-hash-table :test 'equal))
                ;; list of (ATTR-VAL ATTR-VAL-START ATTR-VAL-END) lists, one
                ;; entry for every id value that already existed in
                ;; first-occurrence when we encountered it. The list is in
                ;; reverse order -- i.e. from the bottom of the file to the top.
                (dup-ids))
            ;; step 1: get a list of all the ids in the document.
            (while-let ((kind (xmltok-forward)))
              (when (or (eq kind 'start-tag) (eq kind 'empty-element))
                (when-let* ((id-attr (mememe--get-xmltok-attr "id"))
                            (value   (xmltok-attribute-value id-attr)))
                  (if (not (gethash value seen-ids))
                      (puthash value t seen-ids)
                    (push (list value
                                (xmltok-attribute-value-start id-attr)
                                (xmltok-attribute-value-end   id-attr))
                          dup-ids)))))
            ;; step 2: come up with the new ids
            (let (;; map of the values of PREFIX for id values of the format
                  ;; PREFIX(.NUMBER+) we've seen to the associated suffix for
                  ;; the number of duplicates so far.
                  (recurring-dups (make-hash-table :test 'equal))
                  ;; map of the values of PREFIX for id values of the format
                  ;; PREFIX_NUMBER we've seen to the number of duplicates so
                  ;; far.
                  (underscore-dups (make-hash-table :test 'equal)))
              (dolist (id (nreverse dup-ids))
                (cond
                 ((when-let* ((id-value (car id))
                              (parsed (mememe--recurring-fmt-id-p id-value))
                              (prefix       (nth 0 parsed))
                              (number       (nth 1 parsed))
                              (suffix (concat (gethash prefix recurring-dups "")
                                              "." number))
                              (new-id (concat prefix suffix)))
                    (while (gethash new-id seen-ids)
                      (setq suffix (concat suffix "." number))
                      (setq new-id (concat new-id "." number)))
                    (setcar id new-id)
                    (push id new-ids)
                    (puthash prefix suffix recurring-dups)
                    t))
                 ((let* ((id-value (car id))
                         (parsed (mememe--underscore-fmt-id-parse id-value))
                         (prefix (car parsed))
                         (number (1+ (or (cdr parsed) 0)))
                         (new-id (format "%s_%d" prefix number)))
                    (while (gethash new-id seen-ids)
                      (setq number (1+ number))
                      (setq new-id (format "%s_%d" prefix number)))
                    (setcar id new-id)
                    (push id new-ids)
                    (puthash prefix number underscore-dups)
                    t))))))
          ;; step 3: write them back into the document
          (dolist (id new-ids)
            (goto-char (nth 1 id))
            (delete-region (nth 1 id) (nth 2 id))
            (insert (nth 0 id)))))
      (buffer-substring (point-min) (point-max)))))

(defun mememe-ox-filter-zwsp (string backend comm-channel)
  "Remove Zero-Width Space characters from Org mode (X)HTML export.
Add this function to `org-export-filter-final-output-functions' to use
it. This can be done on a per-file basis with the #+BIND keyword if you
enable the `org-export-allow-bind-keywords' user option."
  (message "mememe-ox-filter-zwsp: processing %S"
           (plist-get comm-channel :output-file))
  (string-replace "​" "" string))

(defun mememe-ox-filter-html-extra-style (string backend comm-channel)
  "Add extra style tag to Org (X)HTML output if the default style is found.
Add this function to `org-export-filter-final-output-functions' to use
it. This can be done on a per-file basis with the #+BIND keyword if you
enable the `org-export-allow-bind-keywords' user option."
  (when (org-export-derived-backend-p backend 'html)
    (message "mememe-ox-filter-html-extra-style: processing %S"
             (plist-get comm-channel :output-file))
    (let ((default-style-content (string-trim org-html-style-default
                                              (rx "<style type=\"text/css\">")
                                              (rx "</style>"))))
      (with-temp-buffer
        (insert string)
        (xmltok-save
          (goto-char (point-min))
          (xmltok-forward-prolog)
          (catch 'done
            (while (xmltok-forward)
              (cond
               ((and (eq xmltok-type 'start-tag)
                     (string-equal (xmltok-start-tag-qname) "style"))
                (let ((content-start (point)))
                  (cl-loop until (eq (xmltok-forward) 'end-tag)
                           unless xmltok-type do (throw 'done nil))
                  (when (string-equal default-style-content
                                      (buffer-substring content-start
                                                        xmltok-start))
                    (insert "<style>/*<![CDATA[*/@namespace\"http://www.w3.org/1999/xhtml\";body{background:snow}details{padding:0 1ch}details:not([open]) > summary .summary-text::after{content:\"…\"}summary h2{display:inline}#postamble summary,#preamble summary{font-size:90%;margin:.2em}details[open] > :last-child,summary{margin-block-end:0.25em;padding-block-end:0.25em;border-block-end:inset 2px gray}.org-center img,.org-center table{margin-inline-start:auto;margin-inline-end:auto}.org-center > figure:only-child > img,.org-center > img:only-child{max-width:50%}img{max-width:100%}pre.src{background-color:#0D0D0D;color:white}pre,table{border:inset medium gray;border-radius:0}table{min-width:30%}td,th{padding-inline-start:1ch;padding-inline-end:1ch}colgroup{border-right:solid thin black}col:not(:first-child){border-left:dashed thin gray}col:not(:last-child){border-right:dashed thin gray}thead > tr{border-bottom:solid thin black}tbody > tr:not(:last-child){border-bottom:dashed thin gray}tbody:not(:last-child) > tr:last-child{border-bottom:solid thin black}dl p,ol p,ul p,#export-details dl{margin-block-start:0;margin-block-end:0}dl:has( > dd > p + p) > dt{margin-block-start:1em}dl:has( > dd > p + p) > dd{margin-block-end:1em}dl:has( > dd > p + p) > * > p,ol:has( > li > p + p) > * > p,ol:has( > li > p + p) > li,ul:has( > li > p + p) > * > p,ul:has( > li > p + p) > li{margin-block-start:1em;margin-block-end:1em}aside{margin-left:50%;text-align:right;font-size:smaller}pre.src-c::before{content:\"C\"}pre.src-C\\+\\+::before,pre.src-c\\+\\+::before{content:\"C++\"}pre.src-json::before{content:\"JSON\"}pre.src-rust::before{content:\"Rust\"}/*]]>*/</style>")
                    (throw 'done
                           (buffer-substring-no-properties (point-min)
                                                           (point-max))))))
               ((and (eq xmltok-type 'end-tag)
                     (string-equal (xmltok-end-tag-qname) "head"))
                (throw 'done nil))))))))))

(defun mememe-ox-filter-html-foreign-lang-clean (string backend comm-channel)
  "Clean up style and script elements in Org (X)HTML export.
This means removing type attributes, and escaping <![CDATA[ ]]> marked
sections with the comment delimiters /**/, if they were not already
present.

Add this function to `org-export-filter-final-output-functions' to use
it. This can be done on a per-file basis with the #+BIND keyword if you
enable the `org-export-allow-bind-keywords' user option."
  (when (org-export-derived-backend-p backend 'html)
    (message "mememe-ox-filter-html-style-type-attrs: processing %S"
             (plist-get comm-channel :output-file))
    (catch 'early-return
      (with-temp-buffer
        (insert string)
        (xmltok-save
          (goto-char (point-min))
          (xmltok-forward-prolog)
          (while (xmltok-forward)
            (when (and (eq xmltok-type 'start-tag)
                       (or (string-equal (xmltok-start-tag-qname) "style")
                           (string-equal (xmltok-start-tag-qname) "script")))
              (when-let* ((type-attr (mememe--get-xmltok-attr "type")))
                (delete-region (- (xmltok-attribute-name-start type-attr)
                                  (if (= ? (char-before
                                            (xmltok-attribute-name-start
                                             type-attr)))
                                      1 0))
                               (+ (xmltok-attribute-value-end type-attr)
                                  (if (and
                                       (= ?\" (char-after
                                               (xmltok-attribute-value-end
                                                type-attr)))
                                       (= ?\" (char-before
                                               (xmltok-attribute-value-start
                                                type-attr))))
                                      1 0)))
                (goto-char xmltok-start)
                (xmltok-forward))
              (let ((content-start (point)))
                (while (not (eq (xmltok-forward) 'end-tag))
                  (unless xmltok-type (throw 'early-return nil)))
                (goto-char xmltok-start)
                (let ((tag-name (xmltok-end-tag-qname))
                      (content (delete-and-extract-region content-start
                                                          xmltok-start)))
                  (insert
                   (with-temp-buffer
                     (insert content)
                     (goto-char (point-min))
                     (if (string-equal tag-name "style")
                         (css-mode) (js-mode))
                     (when (search-forward "<![CDATA[" nil t)
                       (unless (nth 4 (syntax-ppss))
                         (insert "*/")
                         (backward-char 11)
                         (insert "/*"))
                       (search-forward "]]>")
                       (unless (nth 4 (syntax-ppss))
                         (insert "*/")
                         (backward-char 5)
                         (insert "/*")))
                     (buffer-substring-no-properties (point-min)
                                                     (point-max)))))))))
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun mememe-ox-filter-html-details-toc (string backend comm-channel)
  "Reformat table of contents in Org (X)HTML export as a <details>.
Add this function to `org-export-filter-final-output-functions' to use
it. This can be done on a per-file basis with the #+BIND keyword if you
enable the `org-export-allow-bind-keywords' user option."
  (when (org-export-derived-backend-p backend 'html)
    (message "mememe-ox-filter-html-details-toc: processing %S"
             (plist-get comm-channel :output-file))
    (catch 'early-return
      (with-temp-buffer
        (insert string)
        (xmltok-save
          (goto-char (point-min))
          (xmltok-forward-prolog)
          (while (not (and (eq (xmltok-forward) 'start-tag)
                           (when-let* ((id (mememe--get-xmltok-attr "id")))
                             (string-equal
                              (xmltok-attribute-value id)
                              "table-of-contents"))))
            (unless xmltok-type (throw 'early-return)))
          (insert "<details><summary>")
          (while (not (and (eq (xmltok-forward) 'start-tag)
                           (string-equal (xmltok-start-tag-qname) "h2")))
            (unless xmltok-type (throw 'early-return)))
          (insert "<span class=\"summary-text\">")
          (while (not (and (eq (xmltok-forward) 'end-tag)
                           (string-equal (xmltok-end-tag-qname) "h2")))
            (unless xmltok-type (throw 'early-return)))
          (insert "</summary>")
          (goto-char xmltok-start)
          (insert "</span>")
          (while (not (and (eq (xmltok-forward) 'end-tag)
                           (string-equal (xmltok-end-tag-qname) "nav")))
            (unless xmltok-type (throw 'early-return)))
          (goto-char xmltok-start)
          (insert "</details>"))
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun mememe-ox-filter-html-postamble (string backend comm-channel)
  "Inject a better postamble into Org (X)HTML export.
Add this function to `org-export-filter-final-output-functions' and set
`org-html-postamble-format' to \"<postamble-here />\" in order to use
it. This can be done on a per-file basis with the #+BIND keyword if you
enable the `org-export-allow-bind-keywords' user option."

  (when (org-export-derived-backend-p backend 'html)
    (message "mememe-ox-filter-html-postamble: processing %S"
             (plist-get comm-channel :output-file))
    (with-temp-buffer
      (insert string)
      (xmltok-save
        (goto-char (point-min))
        (xmltok-forward-prolog)
        (while (xmltok-forward)
          (when (and (eq xmltok-type 'empty-element)
                     (string-equal (xmltok-start-tag-qname)
                                   "postamble-here"))
            (delete-region xmltok-start (point))
            (insert
             "<details id=\"export-details\">
<summary><span class=\"summary-text\">Export Details</span></summary>
<dl>"
             (if (string-empty-p user-full-name) ""
               (concat "<dt>Author:</dt><dd>"
                       user-full-name
                       "</dd>"))
             "<dt>Last Exported:</dt><dd>"
             (format-time-string "%Y-%m-%d %H:%M:%S")
             "</dd>
<dt>Created with:</dt><dd>
<dl style=\"margin: 0\"><dt><a href=\"https://www.gnu.org/software/emacs/\">GNU Emacs</a></dt>
<dd>version "
             (string-trim-left (emacs-version) "GNU Emacs ")
             "</dd><dt><a href=\"https://orgmode.org\">Org</a> mode</dt>
<dd>version "
             (if (fboundp 'org-version) (org-version) "unknown")
             "</dd><dt><a href=\"https://github.com/andras-simonyi/citeproc-el\">citeproc-el</a></dt>
<dd>version "
             (or (when-let* ((pkg (cadr (assq 'citeproc package-alist))))
                   (package-version-join
                    (package-desc-version
                     pkg)))
                 "unknown")
             "</dd><dt><a href=\"https://elpa.nongnu.org/nongnu/htmlize.html\">htmlize</a></dt>
<dd>version "
             (or (and (boundp 'htmlize-version) htmlize-version) "unknown")

             (if (assoc-default 'html-tidy mememe--external-dependencies
                                #'eq t)
                 (with-temp-buffer
                   (call-process "tidy" nil t nil "--version")
                   (goto-char 0)
                   (insert "</dd><dt><a href=\"https://www.html-tidy.org/\">")
                   (search-forward "HTML Tidy")
                   (insert "</a>")
                   (search-forward " version")
                   (backward-char 8)
                   (delete-char 1)
                   (insert "</dt>\n<dd>")
                   (buffer-substring-no-properties
                    (point-min) (1- (point-max))))
               "")

             "</dd><dt>and the following Emacs themes:</dt><dd><ul>\n"
             (mapconcat (lambda (e) (format "<li><code>%s</code></li>\n" e))
                        custom-enabled-themes)
             "</ul></dd></dl></dd></dl><div class=\"org-center\">
<p><a href=\"https://validator.w3.org/check?uri=referer\">Validate me</a>!
  <a href=\"https://jigsaw.w3.org/css-validator/check/referer\">
      <img style=\"border:0;width:88px;height:31px\"
          src=\"https://jigsaw.w3.org/css-validator/images/vcss\"
          alt=\"Valid CSS!\" />
  </a>
</p>
</div></details>"))))
        (buffer-substring-no-properties (point-min) (point-max)))))

(use-package org
  :ensure t
  :defer t
  :custom
  (org-agenda-files nil)
  (org-agenda-loop-over-headlines-in-active-region nil)
  (org-confirm-babel-evaluate nil)
  (org-descriptive-links nil)
  (org-enforce-todo-dependencies t)
  (org-export-allow-bind-keywords t)
  (org-export-with-smart-quotes t)
  (org-html-doctype "xhtml5")
  (org-html-extension "xhtml")
  (org-html-html5-fancy t)
  (org-html-postamble t)
  (org-html-postamble-format '(("en" "<postamble-here />")))
  (org-html-xml-declaration
   '(("xhtml" .
      "<?xml version=\"1.0\" encoding=\"%s\"?>")
     ("html" .
      "<?xml version=\"1.0\" encoding=\"%s\"?>")
     ("php" .
      "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>")))
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
  (org-list-allow-alphabetical t)
  (org-startup-indented t)
  ;; Moved down here to prevent some weird loading dependency issues
  :config
  (progn
    (add-to-list 'org-src-lang-modes '("json" . js-json-mode))
    (add-to-list 'org-src-lang-modes '("JSON" . js-json-mode))

    (add-to-list 'org-babel-tangle-lang-exts '("nasm" . "s"))

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t) (C . t) (gnuplot . t) (shell . t) (latex . t)))

    (setq org-export-filter-final-output-functions
          (append
           (list #'mememe-ox-filter-zwsp
                 #'mememe-ox-filter-html-dedup-ids
                 #'mememe-ox-filter-html-extra-style
                 #'mememe-ox-filter-html-details-toc
                 #'mememe-ox-filter-html-postamble)
           (when (assoc-default 'html-tidy mememe--external-dependencies
                                #'eq t)
             (list #'mememe-ox-filter-html-tidy))
           (list #'mememe-ox-filter-html-foreign-lang-clean)))

    (setq org-export-filter-example-block-functions
          '(mememe-ox-filter-html-trim-example-blocks))))

(use-package citeproc :ensure t)

(use-package ol-tel :after org-mode)

;; (use-package org-contrib :ensure t :after org-mode)

(use-package org-present :ensure t :after org-mode)

;;;; PHP

;; I don't like PHP whatsoever, but I'd rather have the mode than lack it.

(use-package php-mode :ensure t :defer t)

;;;; Python

;; All we need is to avoid clobbering our whitespace.

(add-to-list 'mememe-delete-trailing-whitespace-exempt-modes 'python-mode)

;;;; Raku

;; I don't much like Ruby either

(use-package raku-mode :ensure t :defer t :custom (raku-indent-offset 8))

;;;; Rust

;; Oh baby gimmie that LSP functionality.

(use-package rust-mode
  :ensure t
  :defer t
  :custom (rust-rustfmt-switches '("--edition" "2024")))

(when (and mememe--use-lsp
           (assoc-default 'rust-analyzer mememe--external-dependencies #'eq t))

  (use-package rustic
    :ensure t
    :after (lsp-mode rust-mode)
    :custom (rustic-compile-backtrace "1")
    :config (add-to-list 'compilation-environment "RUST_BACKTRACE=1"))

  (use-package lsp-mode
    :ensure t
    :defer t
    :custom (lsp-rust-analyzer-diagnostics-disabled ["inactive-code"])))

;;;; Shell

;; TODO: Lots to improve here, haven't gotten around to any of it. I'd like to
;; get Emacs to understand my aliases, at the very least.

(defconst mememe-eshell-prompt-chars-to-lead 5
  "How many characters to lead the eshell prompt with.")

(defconst mememe-eshell-prompt-dir-levels-to-follow 2
  "How many directory levels to list in eshell prompt.")

(defun mememe-eshell-prompt-function ()
  (let* ((ps1 '())
         (cwd (abbreviate-file-name (eshell/pwd)))
         (cwd-leading-len (min (string-width cwd)
                               mememe-eshell-prompt-chars-to-lead))

         (cwd-leading (substring cwd
                                 0
                                 cwd-leading-len))
         (cwd-parts (split-string cwd "/" t))

         (cwd-last-parts (last cwd-parts
                               mememe-eshell-prompt-dir-levels-to-follow))
         (cwd-last-parts-joined (string-join cwd-last-parts "/"))
         (cwd-rejoined (concat cwd-leading "../" cwd-last-parts-joined)))

    (push (if (>= (string-width cwd-rejoined) (string-width cwd))
              cwd cwd-rejoined)
          ps1)

    (unless (eshell-exit-success-p)
      (push (format " [%d]" eshell-last-command-status) ps1))

    (push (cond ((= (file-user-uid) 0) " Y ") (" ,\\ ")) ps1)

    (apply #'concat (nreverse ps1))))

(use-package eshell
  :defer t
  :custom (eshell-prompt-function #'mememe-eshell-prompt-function))

(when (assoc-default 'bash mememe--external-dependencies #'eq t)
  (use-package shell
    :defer t
    :custom (explicit-shell-file-name "/usr/bin/bash")))

(when (assoc-default 'libvterm mememe--external-dependencies #'eq t)
  (use-package vterm :ensure t :defer t))

;;;; Slint

;; I have no need to write slint macros in-line, so we'll just leave it for
;; slint source files only.

;; Oh, and we'll turn on LSP + rainbow modes.

;; TODO: Hook and binding for the slint LSP's format-file functionality.

(use-package slint-mode :defer t :ensure t)

(when (and mememe--use-lsp
           (assoc-default 'slint-lsp mememe--external-dependencies #'eq t))
  (use-package lsp-mode :ensure t :defer t :hook slint-mode))

(use-package rainbow-mode
  :ensure t
  :defer t
  :hook (slint-mode . (lambda () (unless rainbow-mode (rainbow-mode)))))

;;;; YAML

;; Friggin' YAML. Well, we want a major mode and we don't want any significant
;; whitespace getting merked.

(use-package yaml-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'mememe-delete-trailing-whitespace-exempt-modes 'yaml-mode))

;;;; Other

;;;;; Multiple Major Modes

;; TODO: I intend to one day use this to allow me to explicitly use org-mode in
;; program comments. I have yet to do so.

;; (use-package mmm-mode :ensure t)

;;; init.el ends here
