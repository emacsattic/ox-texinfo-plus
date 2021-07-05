;;; ox-texinfo+.el --- Extensions for Org's Texinfo exporter  -*- lexical-binding: t -*-

;; Copyright (C) 2012-2017  Free Software Foundation, Inc.
;; Copyright (C) 2015-2021  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Package-Requires: ((org "9.4"))
;; Homepage: https://github.com/tarsius/ox-texinfo-plus
;; Keywords: outlines, hypermedia, calendar, wp

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides some extensions for Org's `texinfo' exporter
;; defined in `ox-texinfo'.

;; 1. Create `@deffn' and similar definition items by writing list
;;    items in Org that look similar to what they will look like in
;;    Info.  To enable this, add:
;;
;;      #+TEXINFO_DEFFN: t
;;
;;    to your Org file.  After doing that, you can create definition
;;    items like so:
;;
;;      - Command: magit-section-show
;;
;;        Show the body of the current section.
;;
;;      - Function: magit-git-exit-code &rest args
;;      - Macro: magit-insert-section &rest args
;;      - Variable: magit-display-buffer-noselect
;;      - User Option: magit-display-buffer-function
;;      - Key: q, magit-mode-bury-buffer

;; 2. Optionally modify the Org file before exporting it.  This is
;;    implemented using a hook that can be set using the `BIND'
;;    property:
;;
;;      #+BIND: ox-texinfo+-before-export-hook some-function
;;      #+BIND: ox-texinfo+-before-export-hook another-function
;;
;;    The function `ox-texinfo+-update-version-strings' is provided
;;    as an example.  It makes some assumptions that might not be
;;    appropriate for your manuals, so you might have to define your
;;    own variant.

;; 3. Fully respect the local value of `indent-tabs-mode' from the Org
;;    file when editing source blocks and exporting.  This affects all
;;    source blocks and all exporters.
;;
;;    I recommend you add this at the end of Org files to avoid
;;    strange indentation, at least with the `texinfo' exporter:
;;
;;      # Local Variables:
;;      # indent-tabs-mode: nil
;;      # End:

;;; Code:

(require 'cl-lib)
(require 'ox-texinfo)

;;; Definition Items

(let* ((exporter (org-export-get-backend 'texinfo))
       (options (org-export-backend-options exporter)))
  (unless (assoc :texinfo-deffn options)
    (setf (org-export-backend-options exporter)
          (cons '(:texinfo-deffn "TEXINFO_DEFFN" nil nil t)
                options))))

(defun org-texinfo-plain-list--ox-texinfo+ (fn plain-list contents info)
  "If TEXINFO_DEFFN is t, then maybe use a @deffn or similar item."
  (if (equal (plist-get info :texinfo-deffn) "t")
      (org-texinfo+-plain-list plain-list contents info)
    (funcall fn plain-list contents info)))
(advice-add 'org-texinfo-plain-list :around
            'org-texinfo-plain-list--ox-texinfo+)

(defun org-texinfo-item--ox-texinfo+ (fn item contents info)
  "If TEXINFO_DEFFN is t, then maybe use a @deffn or similar item."
  (if (equal (plist-get info :texinfo-deffn) "t")
      (org-texinfo+-item item contents info)
    (funcall fn item contents info)))
(advice-add 'org-texinfo-item :around
            'org-texinfo-item--ox-texinfo+)

(defconst org-texinfo+-item-regexp
  (format "\\`%s: \\(.*\\)\n"
          (regexp-opt '("deffn"        ; CATEGORY NAME ARGUMENTS
                        "Command" ; deffn Command NAME ARGUMENTS
                        "defun"   "Function"    ; NAME ARGUMENTS
                        "defmac"  "Macro"       ; NAME ARGUMENTS
                        "defspec"               ; NAME ARGUMENTS
                        "defvr"        ; CATEGORY NAME
                        "defvar"  "Variable"    ; NAME
                        "defopt"  "User Option" ; NAME
                        "Face"                  ; NAME
                        "Key"                   ; KEY COMMAND
                        ) t)))

(defun org-texinfo+-get-list-type (item)
  (plist-get (cadr (plist-get (cadr item) :parent)) :previous-list-type))

(defun org-texinfo+-set-list-type (item value)
  (let ((parent (plist-get (cadr item) :parent)))
    (setf (cadr parent)
          (plist-put (cadr parent) :previous-list-type value))))

(defun org-texinfo+-maybe-begin-list (this type)
  (prog1 (pcase (list (org-texinfo+-get-list-type this) type)
           (`(list               table) "@end itemize\n\n@table @asis\n")
           (`(,(or `nil `single) table) "@table @asis\n")
           (`(table               list) "@end table\n\n@itemize\n")
           (`(,(or `nil `single)  list) "@itemize\n"))
    (org-texinfo+-set-list-type this type)))

(defun org-texinfo+-maybe-end-list (this type)
  (prog1 (pcase (list (if (eq (car this) 'item)
                          (org-texinfo+-get-list-type this)
                        (plist-get (cadr this) :previous-list-type))
                      type)
           (`(list  ,_) "@end itemize\n\n")
           (`(table ,_) "@end table\n\n"))
    (org-texinfo+-set-list-type this type)))

(defun org-texinfo+-plain-list (plain-list contents _info)
  (concat contents (org-texinfo+-maybe-end-list plain-list nil)))

(defun org-texinfo+-item (item contents info)
  (if (let ((case-fold-search nil))
        (string-match org-texinfo+-item-regexp contents))
      (pcase (match-string 1 contents)
        ("Face" (org-texinfo+-face-item item contents info))
        ("Key"  (org-texinfo+-key-item  item contents info))
        (_      (org-texinfo+-def-item  item contents info)))
    (let* ((plain-list (plist-get (cadr item) :parent))
           (attr (org-export-read-attribute :attr_texinfo plain-list))
           (indic (or (plist-get attr :indic)
                      (plist-get info :texinfo-def-table-markup)))
           (type (org-element-property :type plain-list)))
      (concat (let ((str (org-texinfo+-maybe-begin-list
                          item (if (equal type "table") 'table 'list))))
                (if str
                    (concat str (and (eq type 'descriptive)
                                     (concat " " indic)))
                  "\n"))
              "@item\n"
              (let ((tag (org-element-property :tag item)))
                (and tag (concat " " (org-export-data tag info))))
              contents))))

(defun org-texinfo+-face-item (item contents _info)
  (concat (org-texinfo+-maybe-begin-list item 'table)
          (format "@item @w{ }--- Face: %s\n%s"
                  (match-string 2 contents)
                  (substring contents (match-end 0)))))

(defun org-texinfo+-key-item (item contents _info)
  (concat (org-texinfo+-maybe-begin-list item 'table)
          (let ((head (match-string 2 contents))
                (body (substring contents (match-end 0))))
            (if (string-match ", " head)
                (let ((key (substring head 0 (match-beginning 0)))
                      (cmd (substring head (match-end 0))))
                  (format "\
@kindex %s
@cindex %s
@item @kbd{%s} @tie{}@tie{}@tie{}@tie{}(@code{%s})
%s" key cmd key cmd body))
              (error "Bad Key item %s" head)))))

(defun org-texinfo+-def-item (item contents _info)
  (let ((type (match-string 1 contents))
        (head (match-string 2 contents))
        (body (substring contents (match-end 0)))
        (prefix ""))
    (pcase type
      ("Command"
       (setq prefix (format "@cindex %s\n" head))
       (setq type "deffn")
       (setq head (concat "Command " head)))
      ("Function"    (setq type "defun"))
      ("Macro"       (setq type "defmac"))
      ("Variable"    (setq type "defvar"))
      ("User Option" (setq type "defopt")))
    (format "%s%s@%s %s\n%s@end %s\n\n"
            (or (org-texinfo+-maybe-end-list item 'single) "")
            prefix type head body type)))

;;; Before Export Hook

(defvar ox-texinfo+--before-export-hook nil)

(defun ox-texinfo+--before-export-hook (&rest _ignored)
  "Run the hook `ox-texinfo+--before-export-hook'."
  (let ((ox-texinfo+--before-export-hook
         (cl-mapcan (pcase-lambda (`(,var ,val))
                      (and (eq var 'ox-texinfo+-before-export-hook)
                           (list val)))
                    (let ((org-export-allow-bind-keywords t))
                      (org-export--list-bound-variables)))))
    (run-hooks 'ox-texinfo+--before-export-hook)))

(advice-add 'org-texinfo-export-to-info    :before 'ox-texinfo+--before-export-hook)
(advice-add 'org-texinfo-export-to-texinfo :before 'ox-texinfo+--before-export-hook)

(defun ox-texinfo+-update-copyright-years ()
  "Update copyright years in the current buffer.
How the copyright years are located and formatted is hard-coded,
so you might have to write your own version of this function."
  (let ((year (format-time-string "%Y"))
        (years "\\(\\([0-9]\\{4\\}\\)\\(?:-\\([0-9]\\{4\\}\\)\\)?\\)"))
    (cl-flet ((bump (re)
                    (goto-char (point-min))
                    (while (re-search-forward (concat re years) nil t)
                      (if (match-end 3)
                          (replace-match year t t nil 3)
                        (replace-match (concat (match-string 2) "-" year)
                                       t t nil 1)))))
      (save-excursion
        (bump "^#\\+DATE: ")
        (bump "^Copyright (C) "))))
  (save-buffer))

(defun ox-texinfo+-update-version-strings ()
  "Update version strings in the current buffer.
How the version strings are located and formatted is hard-coded,
so you might have to write your own version of this function."
  (interactive)
  (let ((desc (ox-texinfo+-get-version t)))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^#\\+SUBTITLE: for version \\(.+\\)" nil t)
        (replace-match desc t t nil 1))
      (when (re-search-forward "^This manual is for [^ ]+ version \\(.+\\)" nil t)
        (replace-match (concat desc ".") t t nil 1)))
    (save-buffer)
    (when noninteractive
      (message "Generating %s.texi"
               (file-name-sans-extension
                (file-name-nondirectory buffer-file-name))))))

(defun ox-texinfo+-get-version (&optional verbose rev)
  (let* ((release (and noninteractive (getenv "VERSION")))
         (amend   (and noninteractive (getenv "AMEND")))
         (rev     (or rev (if amend "HEAD~" "HEAD")))
         (version
          (or release
              (ox-texinfo+--describe-revision rev "--abbrev=0")))
         (version
          (if (string-prefix-p "v" version)
              (substring version 1)
            version))
         (version (or release
                      (format "%s (%s+1)" version
                              (ox-texinfo+--describe-revision rev)))))
    (when verbose
      (message "Setting version in %s to %s%s"
               (file-name-nondirectory buffer-file-name)
               version
               (cond (release " [for release]")
                     (amend   " [for amend]")
                     (t       ""))))
    version))

(defun ox-texinfo+--describe-revision (rev &rest args)
  (with-temp-buffer
    (and (zerop (apply #'call-process "git" nil (current-buffer) nil
                       "describe" "--tags" (append args (list rev))))
         (progn (goto-char (point-min))
                (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position))))))

;;; Untabify

(defun org-export-to--ox-texinfo+-disable-indent-tabs-mode
    (fn backend file-or-buffer
        &optional async subtreep visible-only body-only ext-plist post-process)
  "Fix handling of `indent-tabs-mode'."
  (let ((saved-indent-tabs-mode (default-value 'indent-tabs-mode)))
    (setq-default indent-tabs-mode indent-tabs-mode)
    (unwind-protect
        (funcall fn backend file-or-buffer
                 async subtreep visible-only body-only ext-plist post-process)
      (setq-default indent-tabs-mode saved-indent-tabs-mode))))

(advice-add 'org-export-to-file :around
            'org-export-to--ox-texinfo+-disable-indent-tabs-mode)
(advice-add 'org-export-to-buffer :around
            'org-export-to--ox-texinfo+-disable-indent-tabs-mode)

(defun org-src-mode--ox-texinfo+-maybe-disable-indent-tabs-mode ()
  "Fix handling of `indent-tabs-mode'."
  (when (= org-src--tab-width 0)
    (setq indent-tabs-mode nil)))

(add-hook 'org-src-mode-hook
          'org-src-mode--ox-texinfo+-maybe-disable-indent-tabs-mode)

;;; ox-texinfo+.el ends soon
(provide 'ox-texinfo+)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; ox-texinfo+.el ends here
