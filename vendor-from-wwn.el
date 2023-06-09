;;; vendor-from-wwn.el --- Extract vendor information from WWNs

;; Copyright (C) 2013 Tom Koelman

;; Author: Tom Koelman <tkoelman@xs4all.nl>
;; Created: July 2013
;; Version: 0.1.0
;; Keywords: extensions
;; Homepage: http://github.com/tomkoelman/vendor-from-wwn
;; Package-Requires: ((dash "1.5.0")
;;                    (cl-lib "1.0"))

;; This file is not part of GNU Emacs
;; Standard GPL v3 or higher license applies.

;;; Commentary:

;; This package provides functionality to extract vendor information from a WWN.
;; 
;; Key defun is vendor-from-wwn/nice-wwn, which takes a WWN and returns a
;; nicely formatted string with all vendor-specific information recognizable.
;;
;;; Code:

(require 'dash)
(require 'cl-lib)

(defvar vendor-from-wwn/oui-list nil)

(defun vendor-from-wwn/oui-filename()
  "Returns the filename that's used as datasource."
  (concat (file-name-directory (symbol-file 'vendor-from-wwn/oui-list-from-file)) "oui.txt"))

(defun vendor-from-wwn/oui-url()
  "Returns the url that's used as a datasource."
  "http://standards.ieee.org/develop/regauth/oui/oui.txt")

(defun vendor-from-wwn/oui-buffer()
  "Returns a buffer containing oui, gotten from internet. Returns nil if that fails."
  (url-retrieve-synchronously (vendor-from-wwn/oui-url)))

(defun vendor-from-wwn/oui-list-from-buffer ()
  "Parses the current buffer and returns an assoc list from vendor id to vendor string."
  (let (id-to-vendor)
    (goto-char (point-min))
    (while (re-search-forward "\\(..\\)-\\(..\\)-\\(..\\) +([^)]*)\\([^\r]+\\)" nil t)
      (let ((id (downcase (concat (match-string 1)
                                  (match-string 2)
                                  (match-string 3))))
            (vendor (substring (match-string 4) 2)))
        (push (cons id vendor) id-to-vendor)))
    id-to-vendor))

(defun vendor-from-wwn/oui-list-from-file ()
  "Parses the oui.txt file and returns an assoc list from vendor id to vendor string. Returns nil on some fail."
  (when (file-exists-p (vendor-from-wwn/oui-filename))
    (with-temp-buffer
      (insert-file-contents (vendor-from-wwn/oui-filename))
      (vendor-from-wwn/oui-list-from-buffer)
      )))

(defun vendor-from-wwn/oui-list-from-url ()
  "Retrieves the oui.txt file and returns an assoc list from vendor id to vendor string."
  (let ((buffer (vendor-from-wwn/oui-buffer))
        oui-list)
    (when buffer
      (save-excursion
        (pop-to-buffer buffer)
        (write-region (point-min) (point-max) (vendor-from-wwn/oui-filename))
        (setq oui-list (vendor-from-wwn/oui-list-from-buffer)))
      (kill-buffer buffer)
      )
    oui-list)
  )

(defun vendor-from-wwn/oui-list ()
  "Returns an assoc list of vendor id to vendor string. Does caching on first call."
  (setq vendor-from-wwn/oui-list (or vendor-from-wwn/oui-list
                                     (vendor-from-wwn/oui-list-from-file)
                                     (vendor-from-wwn/oui-list-from-url)
                                     ))
  vendor-from-wwn/oui-list)

(defun vendor-from-wwn/normalize-wwn (wwn)
  "Returns the normalized form of WWN."
  (mapconcat 'identity (split-string (downcase wwn) ":") ""))

(defun vendor-from-wwn/pairs (str)
  "Returns a list of strings of length 2. E.g. \"aabbcc\" would yield
 (list \"aa\" \"bb\" \"cc\")."
  (mapcar (lambda(arg) (concat (car arg) (cadr arg)))
            (-partition-all 2 (split-string str "" t))))

(defun vendor-from-wwn/colon-separated-pairs (str)
  "Returns STR, split into pairs, separated by :'s."
  (mapconcat 'identity (vendor-from-wwn/pairs (vendor-from-wwn/normalize-wwn str)) ":"))

(defun vendor-from-wwn/vendor-specific-nice-wwn (wwn)
  "Converts WWN to nicely formatted string with vendor-specific information."
  (let ((vendor-specific-extension (vendor-specific-extension-from-wwn wwn)))
    (concat "[" (vendor-from-wwn/colon-separated-pairs (vendor-sequence-from-wwn wwn)) "]"
            (when vendor-specific-extension
              (concat "[" (vendor-from-wwn/colon-separated-pairs vendor-specific-extension) "]")))))
  
(defun vendor-from-wwn/nice-wwn (wwn)
  "Return a nicely formatted version of WWN."
  (let ((vendor-specific-extension (vendor-specific-extension-from-wwn wwn)))
    (concat "[" (network-address-authority-from-wwn wwn) "]" 
            "[" (vendor-from-wwn/colon-separated-pairs (oui-from-wwn wwn)) "]"
            (vendor-from-wwn/vendor-specific-nice-wwn wwn))))
  
(defun vendor-from-wwn/valid-wwn (wwn)
  "Checks the validity of a WWN. Retuns nil when invalid."
  (let ((wwn (vendor-from-wwn/normalize-wwn wwn)))
    (and (or (= (length wwn) 16)
             (= (length wwn) 32))
         (string-match "^[[:xdigit:]]+$" wwn))))

(defun vendor-sequence-from-wwn (wwn)
  "Returns the vendor sequence or serial number from WWN."
  (let ((wwn (vendor-from-wwn/normalize-wwn wwn))
        (naa (network-address-authority-from-wwn wwn)))
    (cond ((or (equal naa "1")
               (equal naa "2"))
           (substring wwn 10))
          ((equal naa "5")
           (substring wwn 7))
          ((equal naa "6")
           (substring wwn 7 16))
)))

(defun vendor-specific-extension-from-wwn (wwn)
  "Returns the vendor specific extension from WWN. Not every WWN has one, returns nil when not."
  (when (equal (network-address-authority-from-wwn wwn) "6")
    (substring (vendor-from-wwn/normalize-wwn wwn) 16)))

(defun oui-from-wwn (wwn)
  "Returns the Organizationally Unique Identifier or OUI from WWN."
  (let ((wwn (vendor-from-wwn/normalize-wwn wwn))
        (naa (network-address-authority-from-wwn wwn)))
    (cond ((or (equal naa "1")
               (equal naa "2"))
           (substring wwn 4 10))
          ((or (equal naa "5")
               (equal naa "6"))
           (substring wwn 1 7))
)))


(defun network-address-authority-from-wwn (wwn)
  "Returns the Network Address Authority or NAA from WWN."
  (let ((wwn (vendor-from-wwn/normalize-wwn wwn)))
    (cond ((equal (substring wwn 0 1) "1")
           "1")
          ((equal (substring wwn 0 1) "2")
           "2")
          ((equal (substring wwn 0 1) "5")
           "5")
          ((equal (substring wwn 0 1) "6")
           "6")
)))

(defun vendor-from-wwn (wwn)
  "Returns the vendor for WWN."
  (let ((oui-list (vendor-from-wwn/oui-list))
        (oui (oui-from-wwn wwn)))
    (assoc-default oui oui-list)))

(provide 'vendor-from-wwn)
;;; vendor-from-wwn.el ends here
