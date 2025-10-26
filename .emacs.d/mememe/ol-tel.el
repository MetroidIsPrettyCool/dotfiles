;;; ol-tel.el --- support for links to telephone numbers in org-mode

;;; Commentary:

;;; Code:

(require 'ol)

;;;###autoload
(defun org-tel-open (path _)
  "Open the telephone number with xdg-open"
  (let ((url (format "tel:%s" path)))
    (browse-url-xdg-open url)))

;;;###autoload
(defun org-tel-export (path description backend _)
  "Export a telephone number link from Org files."
  (let ((url (format "tel:%s" path))
        (desc (or description path)))
    (pcase backend
      (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" url desc))
      (`latex (format "\\href{%s}{%s}" url desc))
      (`texinfo (format "@uref{%s,%s}" url desc))
      (`ascii (format "%s <%s>" desc url))
      (_ url))))

(org-link-set-parameters "tel" :follow 'org-tel-open :export 'org-tel-export)

(provide 'ol-tel)

;;; ol-tel.el ends here
