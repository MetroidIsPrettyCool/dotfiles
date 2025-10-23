;;; temp-mode.el --- General-purpose temporary minor mode to enable in local variables for custom keymaps

;; copied from: https://emacs.stackexchange.com/questions/519/key-bindings-specific-to-a-buffer

(defvar temp-mode-map (make-sparse-keymap)
  "Keymap while temp-mode is active.")

;;;###autoload
(define-minor-mode temp-mode
  "A temporary minor mode to be activated only specific to a buffer, for
custom keybinds and such."
  :init-value nil
  :lighter " Temp"
  temp-mode-map)

(provide 'temp-mode)
;;; temp-mode.el ends here
