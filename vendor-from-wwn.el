;;; vendor-from-wwn.el --- Extract vendor information from WWNs  -*- lexical-binding: t; -*-

;; Copyright (C) 2013,2026 Tom Koelman

;; Author: Tom Koelman
;; Created: July 2013
;; Version: 0.7.0
;; Keywords: extensions
;; Homepage: http://github.com/tomkoelman/vendor-from-wwn
;; Package-Requires: ()

;; This file is not part of GNU Emacs
;; Standard GPL v3 or higher license applies.

;;; Commentary:

;; This package provides functionality to extract vendor information from a WWN.
;; 
;; Key defun is vendor-from-wwn/nice-wwn, which takes a WWN and returns a
;; nicely formatted string with all vendor-specific information recognizable.
;;
;; Another exported defun is vendor-from-wwn, which takes a WWN and returns
;; the vendor (if known).
;;
;;; Code:

(defconst vendor-from-wwn/version "0.6.0")

(defvar vendor-from-wwn/oui-table nil)

(defun vendor-from-wwn/oui-filename()
  "Returns the filename that's used as datasource."
  (expand-file-name "vendor-from-wwn.cache" user-emacs-directory))

(defconst vendor-from-wwn/oui-url "https://standards.ieee.org/develop/regauth/oui/oui.txt" "URL that's used as a datasource.")

(defun vendor-from-wwn/oui-buffer()
  "Returns a buffer containing oui, gotten from internet. Returns nil if that fails."
  (url-retrieve-synchronously vendor-from-wwn/oui-url))

(defun vendor-from-wwn/oui-table-from-ieee-buffer ()
  "Parses the current buffer and returns a hash table from vendor id to vendor string."
  (let ((entry-re (rx (group (= 2 anychar)) "-"
                         (group (= 2 anychar)) "-"
                         (group (= 2 anychar))
                         (+ " ") "(" (* (not ")")) ")"
                         (group (+ (not (any "\r" "\n")))))))
    (goto-char (point-min))
    (let ((table (make-hash-table :test 'equal
                                  :size (how-many entry-re (point-min) (point-max)))))
      (goto-char (point-min))
      (while (re-search-forward entry-re nil t)
        (let ((id (downcase (concat (match-string 1)
                                    (match-string 2)
                                    (match-string 3))))
              (vendor (substring (match-string 4) 2)))
          (puthash id vendor table)))
      table)))

(defun vendor-from-wwn/oui-table-from-cache-file ()
  "Reads the cached tab-separated file into a hash table. Returns nil on some fail."
  (when (file-exists-p (vendor-from-wwn/oui-filename))
    (let ((coding-system-for-read 'raw-text))
      (with-temp-buffer
        (insert-file-contents (vendor-from-wwn/oui-filename))
        (goto-char (point-min))
        (forward-line 1)
        (let ((table (make-hash-table :test 'equal
                                      :size (count-lines (point) (point-max)))))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
              (when (string-match "\\`\\([^\t]+\\)\t\\(.+\\)\\'" line)
                (puthash (match-string 1 line) (match-string 2 line) table)))
            (forward-line 1))
          (when (> (hash-table-count table) 0)
            table))))))

(defun vendor-from-wwn/oui-table-from-url ()
  "Retrieves the oui.txt file and returns a hash table from vendor id to vendor string."
  (let ((buffer (vendor-from-wwn/oui-buffer))
        oui-table)
    (when buffer
      (with-current-buffer buffer
        (setq oui-table (vendor-from-wwn/oui-table-from-ieee-buffer)))
      (kill-buffer buffer)
      (let ((coding-system-for-write 'raw-text))
        (with-temp-buffer
          (insert (format "# version %s, retrieved %s\n"
                          vendor-from-wwn/version
                          (format-time-string "%Y-%m-%d")))
          (maphash (lambda (id vendor)
                     (insert id "\t" vendor "\n"))
                   oui-table)
          (write-region (point-min) (point-max) (vendor-from-wwn/oui-filename)))))
    oui-table))

(defun vendor-from-wwn/oui-table ()
  "Returns a hash table of vendor id to vendor string. Does caching on first call."
  (setq vendor-from-wwn/oui-table (or vendor-from-wwn/oui-table
                                      (vendor-from-wwn/oui-table-from-cache-file)
                                      (vendor-from-wwn/oui-table-from-url))))

(defun vendor-from-wwn/normalize-wwn (wwn)
  "Returns the normalized form of WWN."
  (replace-regexp-in-string ":" "" (downcase wwn)))

(defun vendor-from-wwn/pairs (str)
  "Returns a list of strings of length 2. E.g. \"aabbcc\" would yield
 (list \"aa\" \"bb\" \"cc\")."
  (let ((chars (split-string str "" t))
        result)
    (while chars
      (push (concat (pop chars) (or (pop chars) "")) result))
    (nreverse result)))

(defun vendor-from-wwn/colon-separated-pairs (str)
  "Returns STR, split into pairs, separated by :'s."
  (string-join (vendor-from-wwn/pairs str) ":"))

(defun vendor-from-wwn/parse-wwn (wwn)
  "Parse WWN into its components. Returns (naa oui vendor-seq vendor-ext)."
  (let* ((wwn (vendor-from-wwn/normalize-wwn wwn))
         (naa (substring wwn 0 1)))
    (unless (member naa '("1" "2" "5" "6"))
      (error "Unknown NAA value: %s" naa))
    (pcase naa
      ((or "1" "2") (list naa (substring wwn 4 10) (substring wwn 10) nil))
      ("5"          (list naa (substring wwn 1 7) (substring wwn 7) nil))
      ("6"          (list naa (substring wwn 1 7) (substring wwn 7 16) (substring wwn 16))))))

;;;###autoload
(defun vendor-from-wwn/nice-wwn (wwn)
  "Return a nicely formatted version of WWN."
  (pcase-let ((`(,naa ,oui ,seq ,ext) (vendor-from-wwn/parse-wwn wwn)))
    (concat "[" naa "]"
            "[" (vendor-from-wwn/colon-separated-pairs oui) "]"
            "[" (vendor-from-wwn/colon-separated-pairs seq) "]"
            (when ext
              (concat "[" (vendor-from-wwn/colon-separated-pairs ext) "]")))))
  
(defun vendor-from-wwn/valid-wwn (wwn)
  "Checks the validity of a WWN. Returns nil when invalid."
  (let ((wwn (vendor-from-wwn/normalize-wwn wwn)))
    (and (or (= (length wwn) 16)
             (= (length wwn) 32))
         (string-match "^[[:xdigit:]]+$" wwn))))

(defun vendor-sequence-from-wwn (wwn)
  "Returns the vendor sequence or serial number from WWN."
  (nth 2 (vendor-from-wwn/parse-wwn wwn)))

(defun vendor-specific-extension-from-wwn (wwn)
  "Returns the vendor specific extension from WWN. Not every WWN has one, returns nil when not."
  (nth 3 (vendor-from-wwn/parse-wwn wwn)))

(defun oui-from-wwn (wwn)
  "Returns the Organizationally Unique Identifier or OUI from WWN."
  (nth 1 (vendor-from-wwn/parse-wwn wwn)))

(defun network-address-authority-from-wwn (wwn)
  "Returns the Network Address Authority or NAA from WWN."
  (nth 0 (vendor-from-wwn/parse-wwn wwn)))

;;;###autoload
(defun vendor-from-wwn (wwn)
  "Returns the vendor for WWN."
  (gethash (nth 1 (vendor-from-wwn/parse-wwn wwn))
           (vendor-from-wwn/oui-table)))

(provide 'vendor-from-wwn)
;;; vendor-from-wwn.el ends here
