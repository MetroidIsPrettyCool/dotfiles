;;; init.el --- primary Emacs configuration -*- fill-column: 80; -*-

;;; Commentary:

;;;; Targets

;; I intend this configuration work well without modification on both my
;; Desktop, with GUI frames and a server-client setup; and on my Android
;; smartphone, with one TUI frame inside Termux. I am unconcerned with any
;; problems this config might have in any other contexts.

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

;; Throughout this file I use a lot of bindings that involve the 'H' modifier.
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

;;; Meta-Configuration

;; Rather than spread everything out into a billion little if-feature-detected
;; guards with no coordination, I've chosen to define a number of named feature
;; flags right up top here, so that later parts of the configuration may
;; reference them as they please.

;;;; Keys and Input

(defconst mememe/commands-to-remember
  '((align-regexp)
    (back-to-indentation)
    (down-list)
    (exchange-point-and-mark)
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

(defconst mememe/key-prefix
  (if (or (daemonp) (display-graphic-p)) "H-" "<home> ")
  "Literal string to prefix to every non-shadow key binding.")

(defconst mememe/swap-backspace-and-del
  (not (or (daemonp) (display-graphic-p)))
  "Should we swap <Backspace> and <DEL>?

This will resolve the \"Backspace invokes help\" problem.")

;;;; Appearance

(defconst mememe/assume-small-screen (eq system-type 'android)
  "Should we should assume our frame is going to be very small?")

(defconst mememe/configure-fonts (or (daemonp) (display-graphic-p))
  "Should we configure fonts?")

(defconst mememe/configure-graphical-background
  (or (daemonp) (display-graphic-p))
  "Should we configure the frame background (transparency and hints)?")

;;;; Packages

(defconst mememe/external-dependencies
  (pcase system-type
    ('gnu/linux '(ls
                  bash
                  git
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
                  eclipse-jdt-ls))
    ('android   '(ls bash git)))
  "Pseudo-alist of external (outside Emacs) dependencies we'll rely on.

See `assoc-default' for what I mean by \"pseudo-alist.\" Values will be
picked out of this list with a call like:

(assoc-default 'foo mememe/external-dependencies #'eq t)

Expected formats of associated values are key-specific, I'll document them here.

 'jdks   should be usable as the value of `lsp-java-configuration-runtimes'

Some of these dependencies are fetched or created automatically by
relevant tools, like libvterm, others are system-wide. Either way: if
they aren't listed here, we shouldn't do anything that needs them. (If
we can help it.)")

(defconst mememe/use-lsp (not (eq system-type 'android))
  "Should we install and configure LSP-related packages?")

(defconst mememe/default-assembly-language
  (pcase (car (split-string system-configuration "-" nil nil))
    ("m68k"                             ; my beloved
     '68000)

    ((or "aarch64"
         "aarch64_be"
         "arm"
         "arm64_32"
         "armeb"
         "armebv7r"
         "armv4t"
         "armv5te"
         "armv6"
         "armv6k"
         "armv7"
         "armv7a"
         "armv7k"
         "armv7r"
         "armv7s"
         "thumbv4t"
         "thumbv5te"
         "thumbv6m"
         "thumbv7a"
         "thumbv7em"
         "thumbv7m"
         "thumbv7neon"
         "thumbv8m.base"
         "thumbv8m.main")
     'arm)

    ((or "mips"
         "mips64"
         "mips64el"
         "mipsel"
         "mipsisa32r6"
         "mipsisa32r6el"
         "mipsisa64r6"
         "mipsisa64r6el")
     'mips)

    ((or "riscv32gc"
         "riscv32i"
         "riscv32im"
         "riscv32imac"
         "riscv32imc"
         "riscv64gc"
         "riscv64imac")
     'riscv)

    ((or "powerpc"                      ; one day...
         "powerpc64"
         "powerpc64le")
     'power)

    ((or "s390x")
     'system/390)

    ((or "sparc"
         "sparc64"
         "sparcv9")
     'sparc)

    ((or "wasm32"
         "wasm64")
     'wasm)

    ((or "i386"                         ; my beloathed
         "i586"
         "i686"
         "x86_64")
     'x86))
  "What kind of assembler should we configure?")

;;;; Behavior

(defconst mememe/additional-scratch-text t
  "Should we insert some additional text into the *scratch* buffer?

Involves some advice kludgery.")

(defconst mememe/force-single-width-characters nil
  "Should we force Emacs to never treat any character wider than one cell?")

;;; Order-Explicit Global Stuff

;;;; Put ~debug-on-*~ Calls Here

;; (debug-on-entry 'get-scratch-buffer-create)

;;;; Load Path

;; I like to put my manually-installed or self-written Elisp under the "lisp"
;; directory of my user's Emacs data dir.

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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

(setopt custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; Appearance

;; Dark mode: Fira Mono, an alpha background and Wombat theme.
;; <https://knowyourmeme.com/memes/shower-time-diesel-jeans>

;;;; Theme

;; Use the slightly tweaked version of wombat in my .emacs.d. I want a darker
;; background, I want the fringe to be colored the same as the background, and I
;; want a better-looking tab bar.

(use-package custom :custom (custom-enabled-themes '(wombat)))

;;;; Background

;; Thank god we don't just have to make the whole frame transparent anymore.
;; Thank you, Po Lu and Emacs 29!

;; Also, dark mode hint.

(when mememe/configure-graphical-background
  (set-frame-parameter (selected-frame) 'alpha-background 90)
  (add-to-list 'default-frame-alist '(alpha-background . 90))

  (use-package frame :custom (frame-background-mode 'dark)))

;;;; Font / Text Settings

;; I'm not particular about very many things, but I really, really despise
;; inconsistent character widths and heights in otherwise monospace contexts.
;; Here lie my vain, hacky attempts to force this issue.

;;;;; Default Font

(set-face-attribute 'default nil :family "SauceCode Pro NFM" :height 110)

;;;;; Fallback Font

(when mememe/configure-fonts
  (setopt inhibit-compacting-font-caches t)
  (setopt use-default-font-for-symbols t)

  (defun mememe/force-default-fontset (frame)
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
      ;; (set-fontset-font t 'unicode (font-spec :name "Unifont")
      ;;                   nil
      ;;                   'append)
      ;; (set-fontset-font t 'unicode (font-spec :name "Unifont Upper")
      ;;                   nil
      ;;                   'append)
      (message "Forced fontset for frame %s" (frame-parameter frame 'name))))

  (if (daemonp)
      (add-hook 'after-make-frame-functions 'mememe/force-default-fontset)
    (mememe/force-default-fontset (selected-frame))))

;;;;; Make All Characters <= 1 Wide

;; XXXXX HACK DONT FAIL KLUDGE XXXXX

(when mememe/force-single-width-characters
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

;; Later we'll hook `whitespace-mode' to make these more visible when we want
;; that.

(defconst mememe/zero-width-characters
  '((#x200B . ("ZWSP" . 'empty-box))    ; ZERO WIDTH SPACE
    (#x200C . ("ZWNJ" . 'empty-box))    ; ZERO WIDTH NON-JOINER
    (#x200D . ("ZWJ" . 'empty-box))     ; ZERO WIDTH JOINER
    (#xFEFF . ("ZWNBSP" . 'empty-box))) ; ZERO WIDTH NO-BREAK SPACE (BOM)
  "Alist of characters we want to display as zero-width.

Association of (CODEPOINT-OR-RANGE . DISPLAY) pairs, where DISPLAY is
some \"element\" as described in the help page for
`glyphless-char-display'.")

(dolist (char mememe/zero-width-characters)
  (set-char-table-range glyphless-char-display (car char) 'zero-width))

;;;; Initial Buffer

;; *scratch* is my friend :)

(setopt initial-buffer-choice t)
(setopt inhibit-startup-screen t)

;; But let's make it do a little more. Rather than redefine the scratch message,
;; we'll advise (really, Emacs? No hook?) the scratch buffer creation function
;; to insert some additional text right after. That lets us evaluate arbitrary
;; lisp forms at /insertion/ rather than message definition time.

(when mememe/additional-scratch-text
  (defun mememe/display-propertize-string (string properties)
    "`propertize'-es each character of STRING as `'display' string, where the
displayed string has PROPERTIES, and concatenates those characters together.

This allows us to embed propertized strings into otherwise font-locked
contexts, like comments; and, because we're splitting up into
characters, we can kill and yank like it's normal text AND the point
even behaves normally."
    (apply #'concat
           (seq-map (lambda (c)
                      (let ((c (string c)))
                        (propertize c
                                    'display
                                    (apply #'propertize c properties))))
                    string)))

  (defun mememe/format-list-for-scratch (strings-or-formats)
    "Format a list of strings / formats for
`mememe/my-additional-startup-scratch-text'.

Each STRING-OR-FORMAT should take one of the following forms:

  ('string STRING)                   a literal string.
  ('datetime TIME FORMAT PROPERTIES) a time, time format string, and a
                                     text properties list.
  ('propertize STRING PROPERTIES)    a string to `'display' `propertize'."
    (apply #'concat
           (mapcar (lambda (string-or-format)
                     (pcase                           (nth 0 string-or-format)
                       ('string                       (nth 1 string-or-format))

                       ('propertize (let ((string     (nth 1 string-or-format))
                                          (properties (nth 2 string-or-format)))

                                      (mememe/display-propertize-string
                                       string
                                       properties)))

                       ('datetime   (let ((time       (nth 1 string-or-format))
                                          (format     (nth 2 string-or-format))
                                          (properties (nth 3 string-or-format)))

                                      (mememe/display-propertize-string
                                       (format-time-string format time)
                                       properties)))))
                   strings-or-formats)))

  (defun mememe/my-additional-startup-scratch-text ()
    "Insert some extra (formatted!) text into the scratch buffer.

That is: 1.) the time Emacs started, the time it finished loading, and
the current time, 2.) The current date, 3.) the contents of
commands-to-remember + their bindings, and 4.) a nice welcome back.

This function returns that same scratch buffer, for easy integration
into advice."
    (let ((scratch (get-buffer "*scratch*")))
      (when scratch
        (with-current-buffer scratch
          (insert
           (mememe/format-list-for-scratch
            (let ((strings-or-formats '()))
              (push '(string ";; The time at startup was: ")
                    strings-or-formats)
              (push `(datetime ,before-init-time "%H:%M:%S.%N" (face bold))
                    strings-or-formats)
              (push '(string "\n")
                    strings-or-formats)

              (push '(string ";; The time after init was: ")

                    strings-or-formats)
              (push `(datetime ,after-init-time  "%H:%M:%S.%N" (face bold))

                    strings-or-formats)
              (push '(string "\n")

                    strings-or-formats)

              (push '(string ";; The time is now:         ")

                    strings-or-formats)
              (push `(datetime nil               "%H:%M:%S.%N" (face bold))
                    strings-or-formats)
              (push '(string "\n\n")
                    strings-or-formats)

              (push '(string ";; Today is ")
                    strings-or-formats)
              (push '(datetime nil "%A"        (face
                                                (:weight
                                                 bold
                                                 :foreground
                                                 "#DFA510")))
                    strings-or-formats)
              (push '(string ", ")
                    strings-or-formats)
              (push '(datetime nil "1%Y-%m-%d" (face
                                                (:weight
                                                 bold
                                                 :foreground
                                                 "#DFA510")))
                    strings-or-formats)
              (push '(string ".\n\n")
                    strings-or-formats)

              (push '(string ";; You wanted to remember the following:\n")
                    strings-or-formats)

              (let ((longest-cmd
                     (cl-loop for entry in mememe/commands-to-remember
                              maximize (string-width
                                        (prin1-to-string (car entry))))))

                (dolist (entry mememe/commands-to-remember)
                  (let* ((cmd      (nth 0 entry))
                         (in-map   (nth 1 entry))
                         (requires (nth 2 entry))
                         (cmd-str (prin1-to-string cmd)))

                    (when requires (require requires))

                    (push '(string ";; ")
                          strings-or-formats)
                    (push `(propertize ,cmd-str (face bold))
                          strings-or-formats)
                    (push `(string
                            ,(concat ":"
                                     (make-string (- (+ 1 longest-cmd)
                                                     (string-width cmd-str))
                                                  ?\s)))
                          strings-or-formats)

                    (dolist (key (or
                                  (mapcar #'key-description
                                          (ensure-list
                                           (where-is-internal
                                            cmd
                                            (eval in-map)
                                            mememe/assume-small-screen)))
                                 (if (commandp cmd)
                                     (list (concat "M-x " cmd-str))
                                   '("Non-Interactive"))))

                      (push `(propertize ,key (face underline))
                            strings-or-formats)

                      (push '(string ", ")
                            strings-or-formats))

                    (setcar strings-or-formats '(string "\n")))))
              (push '(string "\n")
                    strings-or-formats)

              (push '(string ";; ")
                    strings-or-formats)
              (push '(propertize "Welcome home." (face
                                                  (:weight
                                                   bold
                                                   :foreground
                                                   "#00FF95")))
                    strings-or-formats)
              (push '(string "\n\n")
                    strings-or-formats)

              (nreverse strings-or-formats))))
          (set-buffer-modified-p nil)))
      scratch))

  ;; There are two functions in the default Emacs distribution that are
  ;; responsible for inserting the contents of `initial-scratch-message' into
  ;; the *scratch* buffer: `get-scratch-buffer-create' in simple.el, which
  ;; handles M-x scratch buffer and co., and `command-line-1' in startup.el.

  ;; They have almost identical implementations of the insertion logic: "If the
  ;; initial scratch message exists, `get-buffer' the scratch buffer and
  ;; `with-current-buffer' `insert' `initial-scratch-message' after we
  ;; `substitute-command-keys' + mark it as 'not modified'".

  ;; The difference is that the latter only does it because the former's logic
  ;; isn't quite right. See, command-line-1 invokes get-scratch-buffer-create,
  ;; but only after /already/ creating an empty scratch buffer.
  ;; get-scratch-buffer-create sees that the scratch buffer already exists, and
  ;; thus doesn't /do/ anything. command-line-1 then checks if the buffer exists
  ;; or is *empty,* and if it is, inserts the initial-scratch-message like we'd
  ;; want.

  ;; Isn't that stupid?

  ;; So we need to run our insertion code in two places. First, as advice to
  ;; get-scratch-buffer-create whenever the scratch buffer doesn't exist; and
  ;; second, right after everything initializes, since we're following up on
  ;; command-line-1's buffer creation and insertion and not our advisee's.

  (add-hook 'emacs-startup-hook 'mememe/my-additional-startup-scratch-text)

  (define-advice get-scratch-buffer-create
      (:around (orig &rest args) mememe/more-scratch-text)
    "Advice for `get-scratch-buffer-create' to `insert' more text.

Checks before running the original function if *scratch* exists, and, if
so, what size it is and whether it has been modified.

If it didn't exist, or did and was zero size AND unmodified, we call
`mememe/my-additional-startup-scratch-text' after calling the original
function.

Tested working with this configuration in \"GNU Emacs 30.2 (build 2,
x86_64-pc-linux-gnu, GTK+ Version 3.24.50, cairo version 1.18.4)\". If
it stops working, check the diffs."

    (let* ((maybe-scratch (get-buffer "*scratch*")))
      (apply orig args)
      (if maybe-scratch maybe-scratch
        (mememe/my-additional-startup-scratch-text)))))

;; I can think of 3 cleaner ways to do this:

;; 1. simply hook into `buffer-list-update-hook', check if a *scratch* buffer
;; has been created, and if so, insert our text.

;; 2. Use override advice instead of around, and simply insert and format the
;; default message ourselves whenever the scratch buffer doesn't exist OR exists
;; but is empty and unmodified. (Perhaps even consulting a global flag to see if
;; this is the first scratch buffer we've created before looking at its size?)

;; 3. Give up on tweaking the default scratch buffer and write our own, major
;; mode and all, and use it for `initial-buffer-choice'.

;; Number 3 is clearly the cleanest, of course. Something to think about.

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

(when mememe/assume-small-screen
      (setopt mode-line-compact          t)
      (setopt mode-line-percent-position nil)) ; Who ever needs this?

(unless mememe/assume-small-screen
  (setopt mode-line-compact 'long)      ; `list-packages' wigs out otherwise,
                                        ; IDK why. INREQ.

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

     " "                              ; Reduced from 3 spaces.

     mode-line-position

     ;; " "                           ; Spacer I've removed

     ;; Ugly HACK To Pull Out Compilation / Recursive Edit Status & Major Mode

     ;; I tried using backquote, but it didn't work. I clearly messed it up
     ;; somehow. :/ Really should just write this all explicitly...
     (:eval
      (format-mode-line
       (list
        (nth 0 mode-line-modes)           ; Compilation status
        (nth 1 mode-line-modes)           ; Recursive edit open brackets
        ;; ...                            ; Literal ")"
        (nth 3 mode-line-modes)           ; Major mode
        (nth 4 mode-line-modes)           ; Process status
        (car (last mode-line-modes 2))))) ; Recursive edit closing brackets

     " "                                ; Spacer I've added.

     (project-mode-line project-mode-line-format)
     (vc-mode vc-mode)

     mode-line-format-right-align

     ;; mode-line-modes

     ;; Ugly HACK To Pull Out Minor Modes
     (:eval
      (format-mode-line
       (append
        (butlast (nthcdr 5 mode-line-modes) 3) ; List of minor modes
        ;; ...                                 ; Literal ")"
        ;; ...                                 ; Recursive edit closing brackets
        (last mode-line-modes))))              ; Literal spacer " "

     mode-line-misc-info
     " "                              ; Extra spacer I've added.
     mode-line-end-spaces)))

;;; Configuring Editor Behaviors

;;;; Disable GUI Dialog Boxes

(setopt use-dialog-box nil)

;;;; Window-Splitting

;; Only matters on my 16:10 desktop monitor. I'd rather see a vertical split
;; than a horizontal one.

(unless mememe/assume-small-screen
  (use-package emacs
    :custom (split-width-threshold 160) (split-height-threshold 90)))

;;;; Enable *case-region

;; Who needs Caps Lock?

(defconst mememe/disabled-to-enable '(upcase-region downcase-region)
  "Disabled commands to enable on startup.")

(dolist (command mememe/disabled-to-enable)
  (put command 'disabled nil))

;;;; Punctuation Style

;; One space after a period, spaces over tabs (don't want to bother configuring
;; tab stops), put a newline at the end of a file and fill to 120.

(use-package emacs
  :custom
  (sentence-end-double-space nil)
  (require-final-newline t)
  (indent-tabs-mode nil)
  (fill-column (if mememe/assume-small-screen 50 120)))

;;;; Delete Trailing Whitespace When Saving

(defvar mememe/delete-trailing-whitespace-exempt-modes '()
  "Major modes to exempt from `mememe/delete-trailing-whitespace'.

Modes which are derived (parented by) from listed modes will also be
exempted. For example, if we specify `comint-mode', then `shell-mode'
will be implied, as it is derived from the latter. A list of a given
mode's parents can be found with the function
`derived-mode-all-parents'.")

(defun mememe/delete-trailing-whitespace-unless-exempt ()
  "Wrapper around `delete-trailing-whitespace'.

Delete trailing whitespace unless the current major-mode (or one of its
parents) is listed in `mememe/delete-trailing-whitespace-exempt-modes'."
  (interactive)
  (if (not
       (or (member major-mode mememe/delete-trailing-whitespace-exempt-modes)
           (derived-mode-p mememe/delete-trailing-whitespace-exempt-modes)
           (minibufferp)))
      (delete-trailing-whitespace)))

(use-package emacs
  :hook (before-save . mememe/delete-trailing-whitespace-unless-exempt))

;;;; Trash, Don't Delete

;; Just to be safe. I always delete things I don't mean to...

(use-package emacs :custom (delete-by-moving-to-trash t))

;;;; Projects

;; I like Emacs' new lightweight project system. I don't fully understand it,
;; but I like it.

(use-package project :custom (project-mode-line t))

;;; Global Commands and Keys

;; Global commands and key definitions that don't have anything to do with any
;; specific modes.

;; I want easy access to `query-replace-regexp', since it's the most powerful of
;; the *-replace-* functions. Follow it up with !, and it's the same as
;; `replace-regexp'; escape your regexp syntax, and it's the same as
;; `query-replace-string' or `replace-string'.

;; I want some convenient opposites of my commonly-used commands.

;; and I also want a few extra convenience helpers.

;;;; Functions & Commands

(defun mememe/unfill-paragraph ()
  "Un-fill a paragraph at point.

Essentially the opposite of `fill-paragraph'"
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(keymap-set global-map "M-Q" #'mememe/unfill-paragraph)

(defun mememe/yank-pop-forwards (uarg)
  "`yank-pop' with a reversed understanding of the prefix uargument."
  (interactive "p")
  (yank-pop (- uarg)))
(keymap-set global-map "M-Y" #'mememe/yank-pop-forwards)

(defun mememe/deactivate-mark (uarg)
  "Interactive wrapper around `deactivate-mark'."
  (interactive "p")
  (deactivate-mark uarg))
(keymap-set global-map "C-S-SPC" #'mememe/deactivate-mark)

(defun mememe/dump-buffer-local-variables ()
  (interactive)
  (with-temp-buffer-window "*Local Variables Dump*"
      #'display-buffer-reuse-window
      nil
    (prin1 (buffer-local-variables))))

(defun mememe/insert-zwsp (uarg)
  "Insert zero-width spaces according to prefix argument."
  (interactive "p")
  (insert-char #x200b uarg))

(defun mememe/cdmktempdir ()
  "Create a temporary directory with mktemp and visit it with `dired'."
  (interactive)
  (when-let* ((tempdir (condition-case nil
                           (with-temp-buffer
                             (call-process "mktemp" nil t nil "-d")
                             (string-trim (buffer-string)))
                         (file-missing
                          (message "Unable to call mktemp, is it installed?")
                          nil))))
    (message tempdir)
    (dired tempdir)))

(require 'url)

(defun mememe/region-or-word-at-point-no-properties ()
  "Return the region as a string, or the word at point as a string.

Properties are stripped, non-coniiguous regions are concatenated."
  (if (region-active-p)
      (let ((region-text (funcall region-extract-function)))
        (substring-no-properties (if (listp region-text)
                                     (apply #'concat region-text)
                                   region-text)))
    (word-at-point t)))

(defun mememe/wiktionary-region-or-word ()
  "Search for the region or word at point on the English Wiktionary."
  (interactive)
  (browse-url-xdg-open
   (concat
    "https://en.wiktionary.org/wiki/Special:Search?go=Try+exact+match&search="
    (url-hexify-string (mememe/region-or-word-at-point-no-properties)
                       url-query-key-value-allowed-chars))))

(defun mememe/wikipedia-region-or-word ()
  "Search for the region or word at point on the English Wiktionary."
  (interactive)
  (browse-url-xdg-open
   (concat
    "https://en.wikipedia.org/wiki/Special:Search?go=Try+exact+match&search="
    (url-hexify-string (mememe/region-or-word-at-point-no-properties)
                       url-query-key-value-allowed-chars)
    "&ns0=1")))

(defun mememe/wiktionary-region-or-word ()
  "Search for the region or word at point on the Wikipedia."
  (interactive)
  (browse-url-xdg-open
   (concat
    "https://en.wiktionary.org/wiki/Special:Search?go=Try+exact+match&search="
    (url-hexify-string (mememe/region-or-word-at-point-no-properties)
                       url-query-key-value-allowed-chars)
    "&ns0=1")))

;;;; Keys

(when mememe/swap-backspace-and-del
  ;; (keyboard-translate ?\C-h ?\C-?)
  (key-translate "C-h" "<DEL>")
  (keymap-set global-map "C-x ?" 'help-command))

(when (key-valid-p (string-trim mememe/key-prefix))
  (unbind-key (string-trim mememe/key-prefix) global-map))

(defvar-keymap mememe/insert-map :repeat t)
(keymap-set mememe/insert-map "H-s" "¯ \\ _ ( ツ ) _ / ¯") ; ¯\_(ツ)_/¯
(keymap-set mememe/insert-map "s"   "¯ \\ _ ( ツ ) _ / ¯") ; ¯\_(ツ)_/¯
(keymap-set mememe/insert-map "H-z" #'mememe/insert-zwsp)  ; ZWSP
(keymap-set mememe/insert-map "z"   #'mememe/insert-zwsp)  ; ZWSP

(keymap-set global-map (format "%si" mememe/key-prefix) mememe/insert-map)
(keymap-set global-map "C-c i" mememe/insert-map)

(defvar-keymap mememe/toggle-map)
(keymap-set global-map (format "%st" mememe/key-prefix) mememe/toggle-map)
(keymap-set global-map "C-c t" mememe/toggle-map)

(defvar-keymap mememe/search-and-replace-map
  :repeat t
  "H-s" #'search-forward-regexp
  "s"   #'search-forward-regexp
  "H-r" #'query-replace-regexp
  "r"   #'query-replace-regexp)
(keymap-set global-map
            (format "%ss" mememe/key-prefix)
            mememe/search-and-replace-map)
(keymap-set global-map "C-c s" mememe/search-and-replace-map)

(defvar-keymap mememe/misc-map
  "H-<home>" #'mememe/dump-buffer-local-variables
  "<home>"   #'mememe/dump-buffer-local-variables
  "H-a"      #'align-regexp
  "a"        #'align-regexp
  "H-w H-p"  #'mememe/wikipedia-region-or-word
  "w p"      #'mememe/wikipedia-region-or-word
  "H-w H-t"  #'mememe/wiktionary-region-or-word
  "w t"      #'mememe/wiktionary-region-or-word)

(keymap-set global-map (format "%sx" mememe/key-prefix) mememe/misc-map)
(keymap-set global-map "C-c x" mememe/misc-map)

;;; Global Mode Configuration

;; That is, configuration for global minor modes, global configuration for
;; /local/ minor modes, and global configuration for heavily-derived major modes
;; (y'know, like special-mode or comint-mode).

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
  :custom (company-idle-delay 0.1)) ; how long to wait until popup

;;;; `conf-mode'

(defconst mememe/equals-header-regexp
  "[^=]\\(\\(=\\{2,\\}\\) [A-Z0-9\\.!?:;()\" -]+ \\2\\)[^=]"
  "Match strings of uppercase letters surrounded by balanced \"=\" signs.

Actual match is contained within capture group 1.")

(defun mememe/comment-header-p (bound)
  "Comment header matcher for `font-lock-keywords'.

Returns non-nil and updates `match-data' if it matches, as prescribed by
font-lock-keywords."
  (cl-loop while (re-search-forward mememe/equals-header-regexp bound t)
           ;; See `parse-partial-sexp' value 4. Non-nil if inside comment.
           if (save-excursion (nth 4 (syntax-ppss (match-beginning 1))))
           return t))

(defun mememe/conf-mode-font-lock-add-keywords ()
  (font-lock-add-keywords nil
                          '((mememe/comment-header-p
                             1
                             '(:weight bold :foreground "#60afef")
                             prepend))))

(use-package conf-mode
  :defer t
  :hook ((conf-mode . mememe/conf-mode-font-lock-add-keywords)))

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

(defvar mememe/display-line-numbers-exempt-modes
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

(defun mememe/display-line-numbers-unless-exempt (original-fun &rest args)
  "Advice for `display-line-numbers--turn-on'.

Turn on line numbers unless the current major-mode (or one of its
parents) is listed in `mememe/display-line-numbers-exempt-modes'."
  (if (not
       (or (member major-mode mememe/display-line-numbers-exempt-modes)
           (derived-mode-p mememe/display-line-numbers-exempt-modes)
           (minibufferp)))
      (apply original-fun args)))

(use-package display-line-numbers
  :custom (global-display-line-numbers-mode t)
  :config
  (advice-add
   'display-line-numbers--turn-on
   :around 'mememe/display-line-numbers-unless-exempt))

;;;; `editorconfig-mode'

(use-package editorconfig :custom (editorconfig-mode t))

;;;; The `electric-*-mode's (built-in))

;; Don't want to change whether any of these are enable from default, but I'd
;; like to be able to easily toggle them on or off.

(use-package elec-pair
  :defer t
  :bind (:map
         mememe/toggle-map
         ("H-e H-p" . electric-pair-mode)
         ("e p" . electric-pair-mode)))

(use-package electric
  :defer t
  :bind (:map
         mememe/toggle-map
         ("H-e H-i" . electric-indent-mode)
         ("e i" . electric-indent-mode)
         ("H-e H-l" . electric-layout-mode)
         ("e l" . electric-layout-mode)))

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
   '(("HOLD" . "#d0bf8f")
     ("TODO" . "#cc9393")
     ("NEXT" . "#dca3a3")
     ("THEM" . "#dc8cc3")
     ("PROG" . "#7cb8bb")
     ("OKAY" . "#7cb8bb")
     ("DONT" . "#5f7f5f")
     ("FAIL" . "#8c5353")
     ("DONE" . "#afd8af")
     ("NOTE" . "#d0bf8f")
     ("MAYBE" . "#d0bf8f")
     ("KLUDGE" . "#d0bf8f")
     ("HACK" . "#d0bf8f")
     ("TEMP" . "#d0bf8f")
     ("FIXME" . "#cc9393")
     ("XXXX*" . "#cc9393")
     ;; my own additions begin here
     ("INREQ" . "#9beeef")
     ("NEEDS INFO" . "#9beeef")
     ("REQUIRES" . "#9eef00")))
  (hl-todo-highlight-punctuation "!:"))

;;;; `hs-minor-mode' (built-in)

;; Hide and Show blocks within curly braces (or whatever a major mode defines).

;; More specifically: if you place the point inside something like this:
;; function foobar { # does something... }
;; and then you C-c @ C-c or S-<middle mouse>, it becomes this:
;; function foobar {...}
;; Just do it again to toggle back.

(use-package hideshow :defer t :hook (prog-mode . hs-minor-mode))

;;;; `ibuffer-mode' (built-in)

;; `list-buffers' but strictly more powerful.

(use-package ibuffer :defer t :bind ("C-x C-b" . ibuffer))

;;;; `icomplete-mode' (built-in)

;; Automatic completion of things in the minibuffer.

(use-package icomplete :defer t :custom (icomplete-mode t))

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

(when mememe/use-lsp (use-package lsp-mode
                       :ensure t
                       :defer t
                       :custom (lsp-enable-on-type-formatting nil)))

(when mememe/use-lsp (use-package lsp-ui :ensure t :after lsp-mode))

;;;; `menu-bar-mode' (built-in)

;; I don't hate the menu bar. /But/ I do want an easy-to-remember way to turn it
;; off.

(use-package menu-bar
  :defer t
  :custom (menu-bar-mode nil)
  :bind
  (:map mememe/toggle-map
        ("H-m H-b" . menu-bar-mode)
        ("m b"     . menu-bar-mode)))

;;;; `outline-minor-mode' (built-in)

;; That which spawned our savior org-mode, useful in its own right for
;; headline-izing Elisp code. Let's turn on backtab cycling!

(use-package outline :defer t :custom (outline-minor-mode-cycle t))

;;;; `prog-mode' (built-in)

;; Highlight markdown-style equals sign headers in comments, just like
;; we did in `conf-mode'.

(defun mememe/prog-mode-font-lock-add-keywords ()
  (font-lock-add-keywords nil
                          '((mememe/comment-header-p
                             1
                             '(:weight bold :foreground "#60afef")
                             prepend))))

(use-package prog-mode
  :defer t
  :hook ((prog-mode . mememe/prog-mode-font-lock-add-keywords)))

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
   :map mememe/toggle-map
   ("H-t H-b" . tab-bar-mode)
   ("t b"     . tab-bar-mode)))

;;;; `text-scale-mode' (built-in)

;; Not the best for scaling text, but not the worst.

(use-package face-remap :custom (global-text-scale-adjust-resizes-frames t))

;;;; `tool-bar-mode' (built-in)

(use-package tool-bar :custom (tool-bar-mode nil))

;;;; `vlf-mode'

;; Specialized mode for opening Very Large Files. Loading `vlf-setup'

(use-package vlf :ensure t :config (require 'vlf-setup))

;;;; `whitespace-mode' (built-in)

;; Since we've told Emacs it should draw zero-width characters with zero width,
;; we won't be able to tell if they're present in the buffer! This function
;; hooks into the built-in whitespace mode to toggle drawing these characters as
;; hex boxes whenever we toggle the mode.

(defun mememe/whitespace-mode-hook ()
  "Toggle drawing zero-width characters with `whitespace-mode'."
  (dolist (char mememe/zero-width-characters)
    (set-char-table-range
     glyphless-char-display (car char)
     (if whitespace-mode
         (cdr char)
       'zero-width))))

(use-package whitespace :hook (whitespace-mode . mememe/whitespace-mode-hook))

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

(when mememe/use-lsp (use-package ada-mode
                       :ensure t
                       :custom
                       (ada-indent-backend 'eglot)
                       (ada-statement-backend 'eglot)))

;;;; Assembly

;; Always use `'nasm-mode' over `asm-mode' on x86. Not touching gas with a
;; ten-meter pole.

(when (eq mememe/default-assembly-language 'x86)

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

(when (assoc-default 'ls mememe/external-dependencies #'eq t)
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
  :bind (:map mememe/toggle-map ("H-s H-b" . speedbar) ("s b" . speedbar)))

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

(when (assoc-default 'git mememe/external-dependencies #'eq t)
  (use-package magit
    :ensure t
    :defer t
    :bind (:map mememe/toggle-map ("H-g" . magit-status) ("g" . magit-status))))

;;;; GLSL

(use-package glsl-mode :ensure t :defer t)

;;;; Gopher

;; As in the application protocol.

(use-package elpher :ensure t :defer t)

;;;; gnuplot

;; There's a gnuplot package and a gnuplot-mode package, I'm not sure what the
;; difference is. This one seems reasonable?

(use-package gnuplot :ensure t :defer t)

;;;; HTML/XHTML

;; HTMLize is a neat package that "renders" an HTML buffer in-Emacs.

(use-package htmlize :ensure t :defer t)

;;;; Java

;; Java is not a language I want to write without a language server.

(when (and mememe/use-lsp
           (assoc-default 'eclipse-jdt-ls
                          mememe/external-dependencies
                          #'eq
                          t)
           (assoc-default 'jdks
                          mememe/external-dependencies
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
                                                    mememe/external-dependencies
                                                    #'eq
                                                    nil)))

  (use-package lsp-mode :ensure t :defer t :hook java-mode))

;;;; Kotlin

;; I may or may not set up LSP here too at some point. For now, all I want is
;; syntax highlighting.

(use-package kotlin-mode :ensure t :defer t)

;;;; Man / Help / Info / Other Documentation

;; Unless I choose otherwise, I want all documentation to share one window.

(defvar mememe/doc-buffer-modes
  '(help-mode apropos-mode finder-mode Man-mode Info-mode shortdoc-mode)
  "Major-modes we'll consider \"Documentation\".")

(defun mememe/doc-buffer-p (buffer-or-name &rest _args)
  "Return non-nil if BUFFER-NAME is a documentation buffer."
  (let ((buffer-name (if (stringp buffer-or-name)
                         buffer-or-name
                       (buffer-name buffer-or-name))))
    (and
     (not (when (bufferp buffer-or-name) (minibufferp buffer-or-name)))
     (with-current-buffer buffer-or-name
       (memq major-mode mememe/doc-buffer-modes)))))

(add-to-list
 'display-buffer-alist
 `(mememe/doc-buffer-p (display-buffer-reuse-window
                        display-buffer-reuse-mode-window
                        display-buffer-same-window)
                       (mode                       . ,mememe/doc-buffer-modes)
                       (inhibit-switch-frame       . t)
                       (post-command-select-window . nil)))
(use-package
 help-mode
 :custom
 ((describe-bindings-show-prefix-commands t)
  (help-enable-symbol-autoload t)
  (help-enable-symbol-autoload t)
  (help-enable-symbol-autoload t)
  (help-window-keep-selected t)
  (help-window-select t)))

;;;; Org

;; Good 'ol `org-mode'. Here's what I think are some reasonable defaults, plus
;; some other stuff, and a little package I wrote to enable telephone links.

(use-package org
  :ensure t
  :defer t
  :custom
  (org-agenda-files nil)
  (org-agenda-loop-over-headlines-in-active-region nil)
  (org-babel-load-languages
   '((emacs-lisp . t) (C . t) (gnuplot . t) (shell . t)))
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
  (org-startup-indented t))

(use-package org-contrib :ensure t :after org-mode)

(use-package org-present :ensure t :after org-mode)

(use-package ol-tel :after org-mode)

;;;; PHP

;; I don't like PHP whatsoever, but I'd rather have the mode than lack it.

(use-package php-mode :ensure t :defer t)

;;;; Python

;; All we need is to avoid clobbering our whitespace.

(add-to-list 'mememe/delete-trailing-whitespace-exempt-modes 'python-mode)

;;;; Raku

;; I don't much like Ruby either

(use-package raku-mode :ensure t :defer t :custom (raku-indent-offset 8))

;;;; Rust

;; Oh baby gimmie that LSP functionality.

(use-package rust-mode
  :ensure t
  :defer t
  :custom (rust-rustfmt-switches '("--edition" "2024")))

(when (and mememe/use-lsp
           (assoc-default 'rust-analyzer mememe/external-dependencies #'eq t))

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

(defconst mememe/eshell-prompt-chars-to-lead 5
  "How many characters to lead the eshell prompt with.")

(defconst mememe/eshell-prompt-dir-levels-to-follow 2
  "How many directory levels to list in eshell prompt.")

(defun mememe/eshell-prompt-function ()
  (let* ((ps1 '())
         (cwd (abbreviate-file-name (eshell/pwd)))
         (cwd-leading-len (min (string-width cwd)
                               mememe/eshell-prompt-chars-to-lead))

         (cwd-leading (substring cwd
                                 0
                                 cwd-leading-len))
         (cwd-parts (split-string cwd "/" t))

         (cwd-last-parts (last cwd-parts
                               mememe/eshell-prompt-dir-levels-to-follow))
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
  :custom (eshell-prompt-function #'mememe/eshell-prompt-function))

(when (assoc-default 'bash mememe/external-dependencies #'eq t)
  (use-package shell
    :defer t
    :custom (explicit-shell-file-name "/usr/bin/bash")))

(when (assoc-default 'libvterm mememe/external-dependencies #'eq t)
  (use-package vterm :ensure t :defer t))

;;;; Slint

;; I have no need to write slint macros in-line, so we'll just leave it for
;; slint source files only.

;; Oh, and we'll turn on LSP + rainbow modes.

;; TODO: Hook and binding for the slint LSP's format-file functionality.

(use-package slint-mode :defer t :ensure t)

(when (and mememe/use-lsp
           (assoc-default 'slint-lsp mememe/external-dependencies #'eq t))
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
  (add-to-list 'mememe/delete-trailing-whitespace-exempt-modes 'yaml-mode))

;;;; Other...

;;;;; Multiple Major Modes

;; TODO: I intend to one day use this to allow me to explicitly use org-mode in
;; program comments. I have yet to do so.

;; (use-package mmm-mode :ensure t)

;;; init.el ends here
