;;; embark-org.el --- Embark targets and actions for Org Mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Omar Antolín Camarena

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package configures the Embark package for use in Org Mode
;; buffers. It teaches Embark a number of Org related targets and
;; appropriate actions. Currently it has table cells, whole tables,
;; and links. Targets to add: headings (Embark already has generic
;; support for outlines, so we just nee to add Org specific actions),
;; dates, source blocks, etc.

;;; Code:

(require 'embark)
(require 'org)

;;; Tables

;; We define both cell and table targets

(defun embark-org-target-cell ()
  "Target contents of Org table cell at point."
  (when (and (derived-mode-p 'org-mode) (org-at-table-p))
    `(org-table-cell
      ,(save-excursion (string-trim (org-table-get-field)))
      . (,(save-excursion (skip-chars-backward "^|") (point))
         . ,(save-excursion (skip-chars-forward "^|") (point))))))

(defun embark-org-target-table ()
  "Target entire Org table at point."
  (when (and (derived-mode-p 'org-mode) (org-at-table-p))
    `(org-table
      ,(buffer-substring (org-table-begin) (org-table-end))
      . (,(org-table-begin) . ,(org-table-end)))))

(dolist (motion '(org-table-move-cell-up org-table-move-cell-down
                  org-table-move-cell-left org-table-move-cell-right))
  (add-to-list 'embark-repeat-actions motion))

(push 'embark--ignore-target
      (alist-get 'org-table-edit-field embark-target-injection-hooks))

(embark-define-keymap embark-org-table-cell-map
  "Keymap for actions the current cells, column or row of an Org table."
  ("<up>"    org-table-move-cell-up)
  ("<down>"  org-table-move-cell-down)
  ("<left>"  org-table-move-cell-left)
  ("<right>" org-table-move-cell-right)
  ("=" org-table-eval-formula)
  ("e" org-table-edit-field)
  ("g" org-table-recalculate))

(embark-define-keymap embark-org-table-map
  "Keymap for actions on entire Org table."
  ("=" org-table-edit-formulas)
  ("c" org-table-convert)
  ("t" org-table-transpose-table-at-point)
  ("f" org-table-follow-field-mode)
  ("y" org-table-paste-rectangle)
  ("d" org-table-toggle-formula-debugger)
  ("i" org-table-iterate)
  ("e" org-table-export))

(add-to-list 'embark-target-finders #'embark-org-target-table)
(add-to-list 'embark-keymap-alist '(org-table . embark-org-table-map))

(add-to-list 'embark-target-finders #'embark-org-target-cell)
(add-to-list 'embark-keymap-alist '(org-table-cell . embark-org-table-cell-map))

;;; Links

;; The link support has a slightly complicated design in order to
;; achieve the following goals:

;; 1. RET should simply be org-open-at-point

;; 2. When the link is to a file, URL, email address or elisp
;;    expression or command, we want to offer the user actions for
;;    that underlying type.

;; 3. Even in those cases, we still want some actions to apply to the
;;    entire link including description: actions to copy the link as
;;    markdown, or just the link description or target.

;; So the strategy is as follows (illustrated with file links):

;; - The target will be just the file, without the description and
;;   also without the "file:" prefix nor the "::line-number or search"
;;   suffix.  That way, file actions will correctly apply to it.

;; - The type will not be 'file, but 'org-file-link that way we can
;;   register a keymap for 'org-file-link that inherits from both
;;   embark-org-link-map (with RET bound to org-open-at-point and a
;;   few other generic link actions) and embark-file-map.

;; - The commands to copy the link at point in some format will be
;;   written as commands that act on the Org link at point.  This way
;;   they are independently (plausibly) useful, and we circumvent the
;;   problem that the whole Org link is not actually the target (just
;;   the inner file is!).

;; Alternative design I considered: separate each target into two, a
;; whole link target which includes the description and brackets and
;; what not; and an "inner target" which is just the file or URL or
;; whatever.  Cons of this approach: much target cycling is required!
;; First of all, an unadorned embark-dwim definitely should be
;; org-open-at-point, which means the whole link target would need
;; priority. That means that any file, URL, etc. actions would require
;; you to cycle first.  This sounds very inconvenient, the above
;; slightly more complex design allows both whole-link and inner
;; target actions to work without cycling.

(defun embark-org-target-link ()
  "Target destination of Org link."
  (when (and (derived-mode-p 'org-mode 'org-agenda-mode)
             (org-in-regexp org-link-any-re))
    (let ((target (or (match-string-no-properties 2)
                      (match-string-no-properties 0))))
      (append
       (cond
        ((string-prefix-p "http" target)
         (list 'org-url-link target))
        ((string-prefix-p "mailto:" target)
         (list 'org-email-link (string-remove-prefix "mailto:" target)))
        ((string-prefix-p "file:" target)
         (list 'org-file-link
               (replace-regexp-in-string
                "::.*" "" (string-remove-prefix "file:" target))))
        ((string-match-p "^[./]" target)
         (list 'org-file-link (abbreviate-file-name (expand-file-name target))))
        ((string-prefix-p "elisp:(" target)
         (list 'org-expression-link (string-remove-prefix "elisp:" target)))
        ((string-prefix-p "elisp:" target)
         (list 'command (string-remove-prefix "elisp:" target)))
        (t (list 'org-link target)))
       (cons (match-beginning 0) (match-end 0))))))

(add-to-list 'embark-target-finders #'embark-org-target-link)

(defmacro embark-org-define-link-copier (name formula description)
  "Define a command that copies the Org link at point according to FORMULA.
The command's name is formed by appending NAME to
embark-org-copy-link.  The docstring includes the DESCRIPTION of
what part or in what format the link is copied."
  `(defun ,(intern (format "embark-org-copy-link-%s" name)) ()
     ,(format "Copy to the kill-ring the Org link at point%s." description)
     (interactive)
     (when (org-in-regexp org-link-any-re)
       (let* ((full (match-string-no-properties 0))
              (target (or (match-string-no-properties 2)
                          (match-string-no-properties 0)))
              (description (match-string-no-properties 3))
              (kill ,formula))
         (ignore full target description)
         (when kill
           (message "Saved '%s'" kill)
           (kill-new kill))))))

(embark-org-define-link-copier in-full full " in full")
(embark-org-define-link-copier description description "'s description")
(embark-org-define-link-copier target target "'s target")
(embark-org-define-link-copier
 as-markdown (format "[%s](%s)" description target) "as Markdown")

(fset 'embark-org-copy-link-inner-target 'kill-new)
(put 'embark-org-copy-link-inner-target 'function-documentation
      "Copy 'inner part' of the Org link at point's target.
For mailto and elisp links, the inner part is the portion of the
target after 'mailto:' or 'elisp:'.

For file links the inner part is the file name, without the
'file:' prefix and without '::' suffix (used for line numbers,
IDs or search terms).

For URLs the inner part is the whole target including the 'http:'
or 'https:' prefix.  For any other type of link the inner part is
also the whole target.")

(embark-define-keymap embark-org-copy-map
  "Keymap for different ways to copy Org links to the kill-ring."
  :parent nil
  ("w" embark-org-copy-link-in-full)
  ("d" embark-org-copy-link-description)
  ("t" embark-org-copy-link-target)
  ("i" embark-org-copy-link-inner-target)
  ("m" embark-org-copy-link-as-markdown))

(fset 'embark-org-copy-map embark-org-copy-map)

(embark-define-keymap embark-org-link-map
  "Keymap for actions on Org links"
  ("RET" org-open-at-point)
  ("TAB" org-insert-link)
  ("w" 'embark-org-copy-map))

(defmacro embark-org--define-link-keymap (type)
  "Define a keymap for Org link of given TYPE.
The keymap will inherit from `embark-org-link-map' and from
`embark-TYPE-map' in that order."
  `(defvar ,(intern (format "embark-org-%s-link-map" type))
     (make-composed-keymap embark-org-link-map
                           ,(intern (format "embark-%s-map" type)))
     ,(format "Keymap for Embark actions on Org %s links" type)))

;; The reason for this is left as an exercise to the reader.
;; Solution: Na ryvfc gnetrg znl cebzcg gur hfre sbe fbzrguvat!
(push 'embark--ignore-target
      (alist-get 'org-open-at-point embark-target-injection-hooks))

(push 'embark--ignore-target
      (alist-get 'org-insert-link embark-target-injection-hooks))

(embark-org--define-link-keymap url)
(embark-org--define-link-keymap file)
(embark-org--define-link-keymap email)
(embark-org--define-link-keymap expression)

(add-to-list 'embark-keymap-alist '(org-link . embark-org-link-map))
(add-to-list 'embark-keymap-alist '(org-url-link . embark-org-url-link-map))
(add-to-list 'embark-keymap-alist '(org-email-link . embark-org-email-link-map))
(add-to-list 'embark-keymap-alist '(org-file-link . embark-org-file-link-map))
(add-to-list 'embark-keymap-alist
             '(org-expression-link . embark-org-expression-link-map))

(provide 'embark-org)
;;; embark-org.el ends here
