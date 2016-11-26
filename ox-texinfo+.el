;;; ox-texinfo+.el --- extended Texinfo Back-End for Org Export Engine

;; Copyright (C) 2012-2015  Free Software Foundation, Inc.
;; Copyright (C) 2015-2016  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Package-Requires: ((dash "2.10.0") (org "8.3"))
;; Homepage: https://github.com/tarsius/ox-texinfo-plus
;; Keywords: outlines, hypermedia, calendar, wp

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library fixes some bugs in `ox-texinfo' by redefining the
;; faulty functions.  These fixes should eventually be proposed to
;; upstream.

;; This library also provides an extended exporter `texinfo+' with
;; adds some additional features needed by the Magit manual.  I won't
;; document these features/hacks until I am done documenting Magit.

;;; Code:

(eval-when-compile (require 'cl))
(require 'dash)
(require 'ox-texinfo)

(setq org-texinfo-info-process '("makeinfo --no-split %f"))

;;; Patching `ox-texinfo'
;;;; Bugfixes

;; This also fixes the bugs for the regular `ox-texinfo' exporter,
;; which is a good thing.

;; If the description part of the link is missing then also omit it in
;; the export instead of using the section number.  Just copying the
;; section name as description in the org source is not an option
;; because texinfo is to stupid to realize that the string duplicated
;; in the texi file will appear only once in the info output and
;; therefore would justify wrong.
;;
;; For links to `gitman', use ifFORMATs to hardcode how they are
;; exported in the various output formats.  Unfortunately we cannot
;; do anything about the extra space that texinfo injects before the
;; period after the link.
;;
(defun org-texinfo-link (link desc info)
  "Transcode a LINK object from Org to Texinfo.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((type (org-element-property :type link))
         (raw-path (org-element-property :path link))
         ;; Ensure DESC really exists, or set it to nil.
         (desc (and (not (string= desc "")) desc))
         (path (cond
                ((member type '("http" "https" "ftp"))
                 (concat type ":" raw-path))
                ((string= type "file") (org-export-file-uri raw-path))
                (t raw-path))))
    (cond
     ((org-export-custom-protocol-maybe link desc 'texinfo))
     ((org-export-inline-image-p link org-texinfo-inline-image-rules)
      (org-texinfo--inline-image link info))
     ((equal type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
        (if (not destination) desc
          (format "@ref{%s,,%s}"
                  (org-texinfo--get-node destination info)
                  desc))))
     ((member type '("custom-id" "id" "fuzzy"))
      (let ((destination
             (if (equal type "fuzzy")
                 (org-export-resolve-fuzzy-link link info)
               (org-export-resolve-id-link link info))))
        (case (org-element-type destination)
          ((nil)
           (format org-texinfo-link-with-unknown-path-format
                   (org-texinfo--sanitize-content path)))
          ;; Id link points to an external file.
          (plain-text
           (if desc (format "@uref{file://%s,%s}" destination desc)
             (format "@uref{file://%s}" destination)))
          (headline
           (format "@ref{%s,%s}"
                   (org-texinfo--get-node destination info)
                   (cond
                    (desc)
                    ;; -((org-export-numbered-headline-p destination info)
                    ;; - (mapconcat
                    ;; -  #'number-to-string
                    ;; -  (org-export-get-headline-number destination info) "."))
                    (t (org-export-data
                        (org-element-property :title destination) info)))))
          (otherwise
           (format "@ref{%s,,%s}"
                   (org-texinfo--get-node destination info)
                   (cond
                    (desc)
                    ;; No description is provided: first try to
                    ;; associate destination to a number.
                    ((let ((n (org-export-get-ordinal destination info)))
                       (cond ((not n) nil)
                             ((integerp n) n)
                             (t (mapconcat #'number-to-string n ".")))))
                    ;; Then grab title of headline containing
                    ;; DESTINATION.
                    ((let ((h (org-element-lineage destination '(headline) t)))
                       (and h
                            (org-export-data
                             (org-element-property :title destination) info))))
                    ;; Eventually, just return "Top" to refer to the
                    ;; beginning of the info file.
                    (t "Top")))))))
     ((equal type "info")
      (let* ((info-path (split-string path "[:#]"))
             (info-manual (car info-path))
             (info-node (or (cadr info-path) "Top"))
             (title (or desc "")))
        (if (equal info-manual "gitman")
            (format "
@ifinfo
@ref{%s,,,%s,}
@end ifinfo
@ifhtml
@html
the <a href=\"http://git-scm.com/docs/%s\">%s(1)</a> manpage
@end html
@end ifhtml
@iftex
the %s(1) manpage
@end iftex
"
                    info-node info-manual ; info
                    info-node info-node   ; html
                    info-node)            ; pdf
          (format "@ref{%s,%s,,%s,}" info-node title info-manual))))
     ((string= type "mailto")
      (format "@email{%s}"
              (concat (org-texinfo--sanitize-content path)
                      (and desc (concat "," desc)))))
     ;; External link with a description part.
     ((and path desc) (format "@uref{%s,%s}" path desc))
     ;; External link without a description part.
     (path (format "@uref{%s}" path))
     ;; No path, only description.  Try to do something useful.
     (t
      (format (plist-get info :texinfo-link-with-unknown-path-format) desc)))))

;;;; NONODE sections

;; To much would have to be redefined to make sure this doesn't affect
;; the regular `texinfo' exporter.  As long as users don't use the new
;; `NONODE' property, that exporter behaves as before - so we are good.

(defun org-texinfo--menu-entries (scope info)
  "List direct children in SCOPE needing a menu entry.
SCOPE is a headline or a full parse tree.  INFO is a plist
holding contextual information."
  (let* ((cache (or (plist-get info :texinfo-entries-cache)
                    (plist-get (plist-put info :texinfo-entries-cache
                                          (make-hash-table :test #'eq))
                               :texinfo-entries-cache)))
         (cached-entries (gethash scope cache 'no-cache)))
    (if (not (eq cached-entries 'no-cache)) cached-entries
      (puthash scope
               (org-element-map (org-element-contents scope) 'headline
                 (lambda (h)
                   (and (not (org-element-property :NONODE h))
                        (not (org-not-nil (org-element-property :COPYING h)))
                        (not (org-element-property :footnote-section-p h))
                        (not (org-export-low-level-p h info))
                        h))
                 info nil 'headline)
               cache))))

(defun org-texinfo-headline (headline contents info)
  "Transcode a HEADLINE element from Org to Texinfo.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((class (plist-get info :texinfo-class))
         (level (org-export-get-relative-level headline info))
         (nonode (org-element-property :NONODE headline))
         (numberedp (and (not nonode)
                         (org-export-numbered-headline-p headline info)))
         (class-sectioning (assoc class (plist-get info :texinfo-classes)))
         ;; Find the index type, if any.
         (index (org-element-property :INDEX headline))
         ;; Create node info, to insert it before section formatting.
         ;; Use custom menu title if present.
         (node (and (not nonode)
                    (format "@node %s\n" (org-texinfo--get-node headline info))))
         ;; Section formatting will set two placeholders: one for the
         ;; title and the other for the contents.
         (section-fmt
          (if (org-not-nil (org-element-property :APPENDIX headline))
              "@appendix %s\n%s"
            (let ((sec (if (and (symbolp (nth 2 class-sectioning))
                                (fboundp (nth 2 class-sectioning)))
                           (funcall (nth 2 class-sectioning) level numberedp)
                         (nth (1+ level) class-sectioning))))
              (cond
               ;; No section available for that LEVEL.
               ((not sec) nil)
               ;; Section format directly returned by a function.
               ((stringp sec) sec)
               ;; (numbered-section . unnumbered-section)
               ((not (consp (cdr sec)))
                (concat (if (or index (not numberedp)) (cdr sec) (car sec))
                        "\n%s"))))))
         (todo
          (and (plist-get info :with-todo-keywords)
               (let ((todo (org-element-property :todo-keyword headline)))
                 (and todo (org-export-data todo info)))))
         (todo-type (and todo (org-element-property :todo-type headline)))
         (tags (and (plist-get info :with-tags)
                    (org-export-get-tags headline info)))
         (priority (and (plist-get info :with-priority)
                        (org-element-property :priority headline)))
         (text (org-export-data (org-element-property :title headline) info))
         (full-text (funcall (plist-get info :texinfo-format-headline-function)
                             todo todo-type priority text tags))
         (contents (if (org-string-nw-p contents) (concat "\n" contents) "")))
    (cond
     ;; Case 1: This is a footnote section: ignore it.
     ((org-element-property :footnote-section-p headline) nil)
     ;; Case 2: This is the `copying' section: ignore it
     ;;         This is used elsewhere.
     ((org-not-nil (org-element-property :COPYING headline)) nil)
     ;; Case 3: An index.  If it matches one of the known indexes,
     ;;         print it as such following the contents, otherwise
     ;;         print the contents and leave the index up to the user.
     (index
      (concat node
              (format
               section-fmt
               full-text
               (concat contents
                       (and (member index '("cp" "fn" "ky" "pg" "tp" "vr"))
                            (concat "\n@printindex " index))))))
     ;; Case 4: This is a deep sub-tree: export it as a list item.
     ;;         Also export as items headlines for which no section
     ;;         format has been found.
     ((or (not section-fmt) (org-export-low-level-p headline info))
      ;; Build the real contents of the sub-tree.
      (concat (and (org-export-first-sibling-p headline info)
                   (format "@%s\n" (if numberedp 'enumerate 'itemize)))
              "@item\n" full-text "\n"
              contents
              (if (org-export-last-sibling-p headline info)
                  (format "@end %s" (if numberedp 'enumerate 'itemize))
                "\n")))
     ;; Case 5: Standard headline.  Export it as a section.
     (t (concat node (format section-fmt full-text contents))))))

;;; Extending `ox-texinfo'
;;;; Define `texinfo+' exporter

(org-export-define-derived-backend 'texinfo+ 'texinfo
  :translate-alist '((item . org-texinfo+item)
                     (plain-list . org-texinfo+plain-list))
  :menu-entry '(?x "Export to Texinfo+"
                   ((?t "As TEXI file" org-texinfo+export-to-texinfo)
                    (?i "As INFO file" org-texinfo+export-to-info))))

(defun org-texinfo+export-to-texinfo
  (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let ((outfile (org-export-output-file-name ".texi" subtreep))
        (org-export-coding-system org-texinfo-coding-system))
    (org-export-to-file 'texinfo+ outfile
      async subtreep visible-only body-only ext-plist)))

(defun org-texinfo+export-to-info
  (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let ((outfile (org-export-output-file-name ".texi" subtreep))
        (org-export-coding-system org-texinfo-coding-system))
    (org-export-to-file 'texinfo+ outfile
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-texinfo-compile file)))))


;;;; Definition items

(defconst org-texinfo+item-regexp
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

(defun org-texinfo+get-list-type (item)
  (plist-get (cadr (plist-get (cadr item) :parent)) :previous-list-type))

(defun org-texinfo+set-list-type (item value)
  (let ((parent (plist-get (cadr item) :parent)))
    (setf (cadr parent)
          (plist-put (cadr parent) :previous-list-type value))))

(defun org-texinfo+maybe-begin-list (this type)
  (prog1 (pcase (list (org-texinfo+get-list-type this) type)
           (`(list               table) "@end itemize\n\n@table @asis\n")
           (`(,(or `nil `single) table) "@table @asis\n")
           (`(table               list) "@end table\n\n@itemize\n")
           (`(,(or `nil `single)  list) "@itemize\n"))
    (org-texinfo+set-list-type this type)))

(defun org-texinfo+maybe-end-list (this type)
  (prog1 (pcase (list (if (eq (car this) 'item)
                          (org-texinfo+get-list-type this)
                        (plist-get (cadr this) :previous-list-type))
                      type)
           (`(list  ,_) "@end itemize\n\n")
           (`(table ,_) "@end table\n\n"))
    (org-texinfo+set-list-type this type)))

(defun org-texinfo+plain-list (plain-list contents info)
  (concat contents (org-texinfo+maybe-end-list plain-list nil)))

(defun org-texinfo+item (item contents info)
  (if (let ((case-fold-search nil))
        (string-match org-texinfo+item-regexp contents))
      (pcase (match-string 1 contents)
        ("Face" (org-texinfo+face-item item contents info))
        ("Key"  (org-texinfo+key-item  item contents info))
        (_      (org-texinfo+def-item  item contents info)))
    (let* ((plain-list (plist-get (cadr item) :parent))
           (attr (org-export-read-attribute :attr_texinfo plain-list))
           (indic (or (plist-get attr :indic)
                      (plist-get info :texinfo-def-table-markup)))
           (table-type (plist-get attr :table-type))
           (type (org-element-property :type plain-list))
           (list-type (cond
                       ((eq type 'ordered) "enumerate")
                       ((eq type 'unordered) "itemize")
                       ((member table-type '("ftable" "vtable")) table-type)
                       (t "table"))))
      (concat (--when-let (org-texinfo+maybe-begin-list
                           item (if (equal type "table") 'table 'list))
                (concat (substring it 0 -1)
                        (and (eq type 'descriptive) (concat " " indic))))
              "\n@item\n"
              (--when-let (org-element-property :tag item)
                (concat " " (org-export-data it info)))
              contents))))

(defun org-texinfo+face-item (item contents info)
  (concat (org-texinfo+maybe-begin-list item 'table)
          (format "@item @w{ }--- Face: %s\n%s"
                  (match-string 2 contents)
                  (substring contents (match-end 0)))))

(defun org-texinfo+key-item (item contents info)
  (concat (org-texinfo+maybe-begin-list item 'table)
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

(defun org-texinfo+def-item (item contents info)
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
            (or (org-texinfo+maybe-end-list item 'single) "")
            prefix type head body type)))

;;; ox-texinfo+.el ends soon
(provide 'ox-texinfo+)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; ox-texinfo+.el ends here
