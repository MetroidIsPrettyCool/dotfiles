;;; adora-theme.el --- personal Emacs theme  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Joseph Burke

;; Author: Joseph Burke
;; Maintainer: Joseph Burke
;; Created:  3 Nov 2025
;; Version: 0.1.0
;; Package-Requires: ((Emacs "24") color)
;; Keywords: theme

;; This file is not part of GNU Emacs.

;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;; Commentary:

;; Hacked up from the built-in Wombat theme.

;; TODO: Look into the Elisp manual chapter 42.12.2 Defining Faces, and think about display specs.

;;; Code:

(require 'color)

(defun adora-theme--color-to-hex (color)
  "Take a list of three 0-255 ints, return a hex-formatted string."
    (apply #'format "#%02x%02x%02x" color))

(defun adora-theme--mult-lightness-color-to-hex (color lightness-multiple)
  "Take a list of three 0-255 ints, multiply their L*a*b* lightness by
COEFFICIENT, return a hex-formatted string."
  (let* ((color-floats  (mapcar (lambda (x) (/ (float x) 255.0)) color))
         (color-lab     (apply #'color-srgb-to-lab color-floats))
         (old-lightness (nth 0 color-lab))
         (new-lightness (* old-lightness lightness-multiple)))

    (setcar color-lab new-lightness)
    (apply #'format "#%02x%02x%02x" (mapcar (lambda (x) (min 255 (max 0 (floor (* x 255.0)))))
                                            (apply #'color-lab-to-srgb color-lab)))))

(defun adora-theme--mult-hsl-color-to-hex (color hsl-multiples)
  (let* ((color-floats   (mapcar (lambda (x) (/ (float x) 255.0)) color))
         (color-hsl      (apply #'color-rgb-to-hsl color-floats))
         (new-hue        (* (nth 0 color-hsl) (nth 0 hsl-multiples)))
         (new-saturation (* (nth 1 color-hsl) (nth 1 hsl-multiples)))
         (new-lightness  (* (nth 2 color-hsl) (nth 2 hsl-multiples)))
         (new-color-rgb  (color-hsl-to-rgb new-hue new-saturation new-lightness)))

    (apply #'format "#%02x%02x%02x" (mapcar (lambda (x) (min 255 (max 0 (floor (* x 255.0)))))
                                            new-color-rgb))))

(defun adora-theme--discretize-color-to-hex (color discretize-to)
  "Take a list of three 0-255 ints, discretize them, return a hex-formatted
string."
  (apply #'format "#%02x%02x%02x"
         (mapcar (lambda (channel)
                   (floor (* (floor (/ (float channel) (/ 256.0 (float discretize-to))))
                             (/ 255.0 (float (- discretize-to 1))))))
                 color)))

(deftheme adora
  "Personal theme color-picked from screenshots of She-Ra.

That's \"Princesses of Power\", not the original. Say what you will,
that show looks gorgeous."
  :background-mode 'dark)

(let* (;; Base Colors, Sorted by Color-pick Provenance

       ;; Holding the Sword for the First Time
       (base-light-gold          '(#xeb #xd8 #x9d)) ; #ebd89d
       (base-gold                '(#xd4 #xb3 #x52)) ; #d4b352
       (base-red                 '(#xdc #x38 #x53)) ; #dc3853

       ;; Catra and Adora Sitting on a Ledge in the Fright Zone
       (base-orange              '(#xee #x5b #x26)) ; #ee5b26
       (base-light-teal          '(#x86 #xf1 #xd0)) ; #86f1d0
       (base-yellow              '(#xe7 #xe9 #x60)) ; #e7e960

       ;; The Death of Light Hope
       (base-desaturated-red     '(#xb5 #x3e #x5c)) ; #b53e5c
       (base-dark-gold           '(#xa8 #x7f #x64)) ; #a87f64
       (base-black               '(#x06 #x04 #x18)) ; #060418
       (base-gray                '(#x68 #x65 #x6c)) ; #68656c
       (base-light-gray          '(#xd4 #xd8 #xdd)) ; #d4d8dd
       (base-white               '(#xfe #xf9 #xfe)) ; #fef9fe

       ;; Catra Moping on Horde Prime's Windowsill
       (base-green               '(#x8e #xe5 #x97)) ; #8ee597
       (base-light-orange        '(#xff #xa7 #x54)) ; #ffa754
       (base-dark-orange         '(#x89 #x29 #x02)) ; #892902

       ;; She-ra Reawakens, Closeup on Eyes
       (base-light-blue          '(#xc2 #xfb #xfb)) ; #c2fbfb

       ;; Bow, Entrapta and Wrong Hordak Find the Computer Room
       (base-dark-gray           '(#x3e #x4a #x41)) ; #3e4a41
       (base-cyan                '(#x4f #xda #xcb)) ; #4fdacb
       (base-desaturated-purple  '(#x95 #x6d #x92)) ; #956d92
       (base-desaturated-blue    '(#x4b #x66 #x96)) ; #4b6696

       ;; Happy Ending!
       (base-purple              '(#xc0 #x75 #xe8)) ; #c075e8
       (base-sand                '(#xdc #xb5 #x8e)) ; #dcb58e

       ;; Derived & Other Colors
       (final-black          (adora-theme--color-to-hex base-black))                                   ; #060418
       (final-dark-gray      (adora-theme--color-to-hex base-dark-gray))                               ; #3e4a41
       (final-mid-dark-gray  (adora-theme--mult-lightness-color-to-hex base-gray (/ 2.0 3.0)))         ; #454249
       (final-gray           (adora-theme--color-to-hex base-gray))                                    ; #68656c
       (final-mid-light-gray (adora-theme--mult-lightness-color-to-hex base-light-gray (/ 2.0 3.0)))   ; #868a8e
       (final-light-gray     (adora-theme--color-to-hex base-light-gray))                              ; #d4d8dd
       (final-off-white      (adora-theme--color-to-hex base-white))                                   ; #fef9fe
       (final-white          "#ffffff")                                                                ; #ffffff

       (final-red            (adora-theme--color-to-hex base-desaturated-red))                         ; #b53e5c
       (final-orange         (adora-theme--color-to-hex base-orange))                                  ; #ee5b26
       (final-yellow         (adora-theme--color-to-hex base-gold))                                    ; #d4b352
       (final-green          (adora-theme--mult-lightness-color-to-hex base-green (/ 4.0 5.0)))        ; #5fb56b
       (final-cyan           (adora-theme--mult-hsl-color-to-hex base-cyan '(1.0 0.5 1.0)))            ; #71b7af
       (final-blue           (adora-theme--color-to-hex base-desaturated-blue))                        ; #4b6696
       (final-purple         (adora-theme--color-to-hex base-desaturated-purple))                      ; #956d92

       (final-light-red      (adora-theme--mult-lightness-color-to-hex base-red 1.2))                  ; #fb566b
       (final-light-orange   (adora-theme--color-to-hex base-light-orange))                            ; #ffa754
       (final-light-yellow   (adora-theme--color-to-hex base-light-gold))                              ; #ebd89d
       (final-light-green    (adora-theme--color-to-hex base-green))                                   ; #8ee597
       (final-light-cyan     (adora-theme--mult-lightness-color-to-hex base-cyan 1.1))                 ; #69f0e1
       (final-light-blue     (adora-theme--color-to-hex base-light-blue))                              ; #c2fbfb
       (final-light-purple   (adora-theme--mult-lightness-color-to-hex base-purple 1.2))               ; #e395ff

       (final-sand           (adora-theme--color-to-hex base-sand))                                    ; #dcb58e

       (final-dark-red       (adora-theme--mult-lightness-color-to-hex base-desaturated-red 0.7))      ; #8e153d
       (final-dark-orange    (adora-theme--color-to-hex base-dark-orange))                             ; #892902

       (final-dark-sand      (adora-theme--color-to-hex base-dark-gold)))                              ; #a87f64

       (custom-theme-set-faces
        'adora
        ;;                                   :foreground ttttttttttttttttttttt :background tttttttttttttttttttt :weight tttt :underline t ))))
        `(cursor                       ((t (                                   :background ,final-gray                                    ))))
        `(highlight                    ((t ( :foreground ,final-white          :background ,final-mid-dark-gray              :underline t ))))
        `(region                       ((t (                                   :background ,final-mid-dark-gray                           ))))
        `(secondary-selection          ((t ( :foreground ,final-off-white      :background ,final-dark-red                                ))))
        `(isearch                      ((t ( :foreground ,final-mid-light-gray :background ,final-dark-gray                               ))))
        `(lazy-highlight               ((t ( :foreground ,final-light-gray     :background ,final-dark-gray                               ))))
        `(mode-line                    ((t ( :foreground ,final-off-white      :background ,final-mid-dark-gray                           ))))
        `(mode-line-inactive           ((t ( :foreground ,final-mid-light-gray :background ,final-mid-dark-gray                           ))))
        `(minibuffer-prompt            ((t ( :foreground ,final-light-red                                                                 ))))

        `(escape-glyph                 ((t ( :foreground ,final-dark-sand                                       :weight bold              ))))
        `(homoglyph                    ((t ( :foreground ,final-dark-sand                                       :weight bold              ))))

        `(font-lock-builtin-face       ((t ( :foreground ,final-light-orange                                                              ))))
        `(font-lock-comment-face       ((t ( :foreground ,final-yellow                                                                    ))))
        `(font-lock-constant-face      ((t ( :foreground ,final-light-blue                                                                ))))
        `(font-lock-function-name-face ((t ( :foreground ,final-sand                                                                      ))))
        `(font-lock-keyword-face       ((t ( :foreground ,final-light-orange                                    :weight bold              ))))
        `(font-lock-string-face        ((t ( :foreground ,final-light-yellow                                                              ))))
        `(font-lock-type-face          ((t ( :foreground ,final-light-red                                       :weight bold              ))))
        `(font-lock-variable-name-face ((t ( :foreground ,final-light-yellow                                                              ))))
        `(font-lock-warning-face       ((t ( :foreground ,final-red                                                                       ))))

        `(help-key-binding             ((t ( :foreground ,final-off-white      :background ,final-dark-gray                               ))))

        `(link                         ((t ( :foreground ,final-light-blue                                                   :underline t ))))
        `(link-visited                 ((t ( :foreground ,final-light-red                                                    :underline t ))))
        `(button                       ((t ( :foreground ,final-off-white      :background ,final-dark-gray                               ))))
        `(header-line                  ((t ( :foreground ,final-sand           :background ,final-dark-gray                               ))))

        `(gnus-group-news-1            ((t ( :foreground ,final-light-green                                     :weight bold              ))))
        `(gnus-group-news-1-low        ((t ( :foreground ,final-light-green                                                               ))))
        `(gnus-group-news-2            ((t ( :foreground ,final-dark-sand                                       :weight bold              ))))
        `(gnus-group-news-2-low        ((t ( :foreground ,final-dark-sand                                                                 ))))
        `(gnus-group-news-3            ((t ( :foreground ,final-yellow                                          :weight bold              ))))
        `(gnus-group-news-3-low        ((t ( :foreground ,final-yellow                                                                    ))))
        `(gnus-group-news-4            ((t ( :foreground ,final-mid-light-gray                                  :weight bold              ))))
        `(gnus-group-news-4-low        ((t ( :foreground ,final-mid-light-gray                                                            ))))
        `(gnus-group-news-5            ((t ( :foreground ,final-dark-sand                                       :weight bold              ))))
        `(gnus-group-news-5-low        ((t ( :foreground ,final-dark-sand                                                                 ))))
        `(gnus-group-news-low          ((t ( :foreground ,final-mid-light-gray                                                            ))))
        `(gnus-group-mail-1            ((t ( :foreground ,final-light-green                                     :weight bold              ))))
        `(gnus-group-mail-1-low        ((t ( :foreground ,final-light-green                                                               ))))
        `(gnus-group-mail-2            ((t ( :foreground ,final-dark-sand                                       :weight bold              ))))
        `(gnus-group-mail-2-low        ((t ( :foreground ,final-dark-sand                                                                 ))))
        `(gnus-group-mail-3            ((t ( :foreground ,final-yellow                                          :weight bold              ))))
        `(gnus-group-mail-3-low        ((t ( :foreground ,final-yellow                                                                    ))))
        `(gnus-group-mail-low          ((t ( :foreground ,final-mid-light-gray                                                            ))))
        `(gnus-header-content          ((t ( :foreground ,final-light-blue                                                                ))))
        `(gnus-header-from             ((t ( :foreground ,final-light-green                                     :weight bold              ))))
        `(gnus-header-subject          ((t ( :foreground ,final-dark-sand                                                                 ))))
        `(gnus-header-name             ((t ( :foreground ,final-light-blue                                                                ))))
        `(gnus-header-newsgroups       ((t ( :foreground ,final-dark-sand                                                                 ))))

        `(message-header-name          ((t ( :foreground ,final-light-blue                                      :weight bold              ))))
        `(message-header-cc            ((t ( :foreground ,final-light-green                                                               ))))
        `(message-header-other         ((t ( :foreground ,final-light-green                                                               ))))
        `(message-header-subject       ((t ( :foreground ,final-dark-sand                                                                 ))))
        `(message-header-to            ((t ( :foreground ,final-dark-sand                                                                 ))))
        `(message-cited-text-1         ((t ( :foreground ,final-mid-light-gray                                                            ))))
        `(message-separator            ((t ( :foreground ,final-light-red                                       :weight bold              ))))

        `(ansi-color-black             ((t ( :foreground ,final-black          :background ,final-black                                   ))))
        `(ansi-color-red               ((t ( :foreground ,final-red            :background ,final-red                                     ))))
        `(ansi-color-green             ((t ( :foreground ,final-green          :background ,final-green                                   ))))
        `(ansi-color-yellow            ((t ( :foreground ,final-yellow         :background ,final-yellow                                  ))))
        `(ansi-color-blue              ((t ( :foreground ,final-blue           :background ,final-blue                                    ))))
        `(ansi-color-magenta           ((t ( :foreground ,final-purple         :background ,final-purple                                  ))))
        `(ansi-color-cyan              ((t ( :foreground ,final-cyan           :background ,final-cyan                                    ))))
        `(ansi-color-white             ((t ( :foreground ,final-off-white      :background ,final-off-white                               ))))

        `(ansi-color-bright-black      ((t ( :foreground ,final-mid-dark-gray  :background ,final-mid-dark-gray                           ))))
        `(ansi-color-bright-red        ((t ( :foreground ,final-light-red      :background ,final-light-red                               ))))
        `(ansi-color-bright-green      ((t ( :foreground ,final-light-green    :background ,final-light-green                             ))))
        `(ansi-color-bright-yellow     ((t ( :foreground ,final-light-yellow   :background ,final-light-yellow                            ))))
        `(ansi-color-bright-blue       ((t ( :foreground ,final-light-blue     :background ,final-light-blue                              ))))
        `(ansi-color-bright-magenta    ((t ( :foreground ,final-light-purple   :background ,final-light-purple                            ))))
        `(ansi-color-bright-cyan       ((t ( :foreground ,final-light-cyan     :background ,final-light-cyan                              ))))
        `(ansi-color-bright-white      ((t ( :foreground ,final-white          :background ,final-white                                   ))))

        `(tab-bar                      ((t ( :foreground ,final-black          :background ,final-light-yellow                            ))))
        `(tab-bar-tab                  ((t ( :foreground ,final-black          :background ,final-off-white                               ))))
        `(tab-bar-tab-inactive         ((t ( :foreground ,final-black          :background ,final-yellow                                  ))))

        `(fringe                       ((t (                                   :background unspecified                                    ))))

        `(default                      ((t ( :foreground ,final-off-white      :background ,final-black                                   ))))))

  (provide-theme 'adora)

;;; adora-theme.el ends here
