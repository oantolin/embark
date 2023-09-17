;;; embark-org.el --- Embark targets and actions for Org Mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

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
;; buffers.  It teaches Embark a number of Org related targets and
;; appropriate actions.  Currently it has table cells, whole tables,
;; source blocks and links.  Targets to add: headings (Embark already
;; has generic support for outlines, so we just nee to add Org
;; specific actions), timestamps, etc.

;;; Code:

(require 'embark)
(require 'org)
(require 'org-element)

;;; Basic target finder for Org

;; There are very many org element and objects types, we'll only
;; recognize those for which there are specific actions we can put in
;; a keymap, or even if there aren't any specific actions, if it's
;; important to be able to kill, delete or duplicate (embark-insert)
;; them conveniently.  I'll start conservatively and we can add more
;; later

(defconst embark-org--types
  '(
    babel-call
    ;; bold
    ;; center-block
    ;; citation
    ;; citation-reference
    ;; clock
    ;; code
    ;; comment
    ;; comment-block
    ;; diary-sexp
    ;; drawer
    ;; dynamic-block
    ;; entity
    ;; example-block
    ;; export-block
    ;; export-snippet
    ;; fixed-width
    footnote-definition
    footnote-reference
    ;; headline ; the bounds include the entire subtree!
    ;; horizontal-rule
    ;; inline-babel-call
    inline-src-block
    ;; inlinetask
    ;; italic
    item
    ;; keyword
    ;; latex-environment
    ;; latex-fragment
    ;; line-break
    link
    ;; macro
    ;; node-property
    ;; paragraph ; the existing general support seems fine
    plain-list
    ;; planning
    ;; property-drawer
    ;; quote-block
    ;; radio-target
    ;; section
    ;; special-block
    src-block
    ;; statistics-cookie
    ;; strike-through
    ;; subscript
    ;; superscript
    table ; supported via a specific target finder
    table-cell
    ;; table-row ; we'll put row & column actions in the cell map
    ;; target ; I think there are no useful actions for radio targets
    timestamp
    ;; underline
    ;; verbatim
    ;; verse-block
    )
  "Supported Org object and element types.")

(defun embark-org-target-element-context ()
  "Target all Org elements or objects around point."
  (when (derived-mode-p 'org-mode 'org-agenda-mode)
    (cl-loop
     for elt = (org-element-lineage (org-element-context) embark-org--types t)
     then (org-element-lineage elt embark-org--types)
     while elt
     ;; clip bounds to narrowed portion of buffer
     for begin = (max (org-element-property :begin elt) (point-min))
     for end = (min (org-element-property :end elt) (point-max))
     for target = (buffer-substring begin end)
      ;; Adjust table-cell to exclude final |. (Why is that there?)
      ;; Note: We are not doing this is an embark transformer because we
      ;; want to adjust the bounds too.
      ;; TODO? If more adjustments like this become necessary, add a
      ;; nice mechanism for doing them.
      when (and (eq (car elt) 'table-cell) (string-suffix-p "|" target))
      do (setq target (string-trim (string-remove-suffix "|" target))
               end (1- end))
      collect `(,(intern (format "org-%s" (car elt))) ,target ,begin . ,end))))

(if-let (((not (memq 'embark-org-target-element-context embark-target-finders)))
         (tail (memq 'embark-target-active-region embark-target-finders)))
    (push 'embark-org-target-element-context (cdr tail))
  (push 'embark-org-target-element-context embark-target-finders))

;;; Custom Org actions

(defvar org-export-with-toc)

(defun embark-org-copy-as-markdown (start end)
  "Export the region from START to END to markdown and save on the `kill-ring'."
  (interactive "r")
  (require 'ox)
  (kill-new
   (let (org-export-with-toc)
     (string-trim
      (org-export-string-as (buffer-substring-no-properties start end) 'md t))))
  (deactivate-mark))

(add-to-list 'embark-pre-action-hooks
             '(embark-org-copy-as-markdown embark--mark-target))

(keymap-set embark-region-map "M" #'embark-org-copy-as-markdown) ; good idea?

;;; Tables

(dolist (motion '(org-table-move-cell-up org-table-move-cell-down
                  org-table-move-cell-left org-table-move-cell-right
                  org-table-move-row org-table-move-column
                  org-table-move-row-up org-table-move-row-down
                  org-table-move-column-left org-table-move-column-right))
  (add-to-list 'embark-repeat-actions motion))

(dolist (cmd '(org-table-eval-formula org-table-edit-field))
  (push 'embark--ignore-target (alist-get cmd embark-target-injection-hooks)))

(defvar-keymap embark-org-table-cell-map
  :doc "Keymap for actions the current cells, column or row of an Org table."
  :parent embark-general-map
  "RET" #'org-table-align ; harmless default
  "<up>"    #'org-table-move-cell-up
  "<down>"  #'org-table-move-cell-down
  "<left>"  #'org-table-move-cell-left
  "<right>" #'org-table-move-cell-right
  "d" #'org-table-kill-row
  "c" #'org-table-copy-down
  "D" #'org-table-delete-column ; capital = column
  "^" #'org-table-move-row-up
  "v" #'org-table-move-row-down
  "<" #'org-table-move-column-left
  ">" #'org-table-move-column-right
  "o" #'org-table-insert-row
  "O" #'org-table-insert-column ; capital = column
  "h" #'org-table-insert-hline
  "=" #'org-table-eval-formula
  "e" #'org-table-edit-field
  "g" #'org-table-recalculate)

(defvar-keymap embark-org-table-map
  :doc "Keymap for actions on entire Org table."
  :parent embark-general-map
  "RET" #'org-table-align ; harmless default
  "=" #'org-table-edit-formulas
  "s" #'org-table-sort-lines
  "t" #'org-table-transpose-table-at-point
  "c" #'org-table-convert
  "f" #'org-table-follow-field-mode
  "y" #'org-table-paste-rectangle
  "d" #'org-table-toggle-formula-debugger
  "o" #'org-table-toggle-coordinate-overlays
  "g" #'org-table-iterate
  "e" #'org-table-export)

(push 'embark--ignore-target            ; prompts for file name
      (alist-get 'org-table-export embark-target-injection-hooks))

(add-to-list 'embark-keymap-alist '(org-table embark-org-table-map))

(add-to-list 'embark-keymap-alist '(org-table-cell embark-org-table-cell-map))

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

(autoload 'org-attach-dir "org-attach")

(defun embark-org--refine-link-type (_type target)
  "Refine type of link TARGET if we have more specific actions available."
  (when (string-match org-link-any-re target)
    (let ((target (or (match-string-no-properties 2 target)
                      (match-string-no-properties 0 target))))
      (cond
       ((string-prefix-p "http" target)
        (cons 'org-url-link target))
       ((string-prefix-p "mailto:" target)
        (cons 'org-email-link (string-remove-prefix "mailto:" target)))
       ((string-prefix-p "file:" target)
        (cons 'org-file-link
              (replace-regexp-in-string
               "::.*" "" (string-remove-prefix "file:" target))))
       ((string-prefix-p "attachment:" target)
        (cons 'org-file-link
              (expand-file-name
               (replace-regexp-in-string
                "::.*" "" (string-remove-prefix "attachment:" target))
               (org-attach-dir))))
       ((string-match-p "^[./]" target)
        (cons 'org-file-link (abbreviate-file-name (expand-file-name target))))
       ((string-prefix-p "elisp:(" target)
        (cons 'org-expression-link (string-remove-prefix "elisp:" target)))
       ((string-prefix-p "elisp:" target)
        (cons 'command (string-remove-prefix "elisp:" target)))
       (t (cons 'org-link target))))))

(add-to-list 'embark-transformer-alist
             '(org-link . embark-org--refine-link-type))

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

(defalias 'embark-org-copy-link-inner-target #'kill-new
  "Copy inner part of the Org link at point's target.
For mailto and elisp links, the inner part is the portion of the
target after `mailto:' or `elisp:'.

For file links the inner part is the file name, without the
`file:' prefix and without `::' suffix (used for line numbers,
IDs or search terms).

For URLs the inner part is the whole target including the `http:'
or `https:' prefix.  For any other type of link the inner part is
also the whole target.")

(defvar-keymap embark-org-link-copy-map
  :doc "Keymap for different ways to copy Org links to the kill-ring.

You should bind w in this map to your most frequently used link
copying function.  The default is for w to copy the \"inner
target\" (see `embark-org-copy-link-inner-target'); which is also
bound to i."
  :parent nil
  "w" #'embark-org-copy-link-inner-target
  "f" #'embark-org-copy-link-in-full
  "d" #'embark-org-copy-link-description
  "t" #'embark-org-copy-link-target
  "i" #'embark-org-copy-link-inner-target
  "m" #'embark-org-copy-as-markdown)

(fset 'embark-org-link-copy-map embark-org-link-copy-map)

(defvar-keymap embark-org-link-map
  :doc "Keymap for actions on Org links."
  :parent embark-general-map
  "RET" #'org-open-at-point
  "'" #'org-insert-link
  "w" #'embark-org-link-copy-map)

;; The reason for this is left as an exercise to the reader.
;; Solution: Na ryvfc gnetrg znl cebzcg gur hfre sbe fbzrguvat!
(cl-pushnew 'embark--ignore-target
            (alist-get 'org-open-at-point embark-target-injection-hooks))
(cl-pushnew 'embark--ignore-target
            (alist-get 'org-insert-link embark-target-injection-hooks))

(add-to-list 'embark-keymap-alist
             '(org-link embark-org-link-map))
(add-to-list 'embark-keymap-alist
             '(org-url-link embark-org-link-map embark-url-map))
(add-to-list 'embark-keymap-alist
             '(org-email-link embark-org-link-map embark-email-map))
(add-to-list 'embark-keymap-alist
             '(org-file-link embark-org-link-map embark-file-map))
(add-to-list 'embark-keymap-alist
             '(org-expression-link embark-org-link-map embark-expression-map))

;;; Org headings

(defun embark-org--refine-heading (type target)
  "Refine TYPE of heading TARGET in Org buffers."
  (cons
   (if (derived-mode-p 'org-mode) 'org-heading type)
   target))

(add-to-list 'embark-transformer-alist '(heading . embark-org--refine-heading))

(defvar-keymap embark-org-heading-map
  :doc "Keymap for actions on Org headings."
  :parent embark-heading-map
  "RET" #'org-todo
  "TAB" #'org-cycle
  "t" #'org-todo
  "s" #'org-schedule
  "d" #'org-deadline
  "," #'org-priority
  ":" #'org-set-tags-command
  "P" #'org-set-property
  "D" #'org-delete-property
  "k" #'org-cut-subtree
  "N" #'org-narrow-to-subtree
  "T" #'org-tree-to-indirect-buffer
  "<left>" #'org-do-promote
  "<right>" #'org-do-demote
  "^" #'org-sort
  "r" #'org-refile
  "R" #'embark-org-refile-here
  "I" #'org-clock-in
  "O" #'org-clock-out
  "a" #'org-archive-subtree-default-with-confirmation
  "h" #'org-insert-heading-respect-content
  "H" #'org-insert-todo-heading-respect-content
  "l" #'org-store-link
  "j" #'embark-org-insert-link-to)

(dolist (cmd '(org-todo org-metaright org-metaleft org-metaup org-metadown
               org-shiftmetaleft org-shiftmetaright org-cycle org-shifttab))
  (cl-pushnew cmd embark-repeat-actions))

(dolist (cmd '(org-set-tags-command org-set-property
               org-delete-property org-refile org-schedule))
  (cl-pushnew 'embark--ignore-target
              (alist-get cmd embark-target-injection-hooks)))

(add-to-list 'embark-keymap-alist '(org-heading embark-org-heading-map))

;;; Source blocks

(defun embark-org-copy-block-contents ()
  "Save contents of source block at point to the `kill-ring'."
  (interactive)
  (when (org-in-src-block-p)
    (let ((contents (nth 2 (org-src--contents-area (org-element-at-point)))))
    (with-temp-buffer
      (insert contents)
      (org-do-remove-indentation)
      (kill-new (buffer-substring (point-min) (point-max)))))))

(defvar-keymap embark-org-src-block-map
  :doc "Keymap for actions on Org source blocks."
  :parent embark-general-map
  "RET" #'org-babel-execute-src-block
  "C-SPC" #'org-babel-mark-block
  "TAB" #'org-indent-block
  "c" #'embark-org-copy-block-contents
  "h" #'org-babel-check-src-block
  "k" #'org-babel-remove-result-one-or-many
  "p" #'org-babel-previous-src-block
  "n" #'org-babel-next-src-block
  "t" #'org-babel-tangle
  "s" #'org-babel-switch-to-session
  "l" #'org-babel-load-in-session
  "'" #'org-edit-special
  "/" #'org-babel-demarcate-block
  "N" #'org-narrow-to-block)

(cl-defun embark-org--at-block-head
    (&rest rest &key run bounds &allow-other-keys)
  "Save excursion and RUN the action at the head of the current block.
If BOUNDS are given, use them to locate the block (useful for
when acting on a selection of blocks).  Applies RUN to the REST
of the arguments."
  (save-excursion
    (when bounds (goto-char (car bounds)))
    (org-babel-goto-src-block-head)
    (apply run rest)))

(cl-pushnew #'embark-org--at-block-head
            (alist-get 'org-indent-block embark-around-action-hooks))

(dolist (motion '(org-babel-next-src-block org-babel-previous-src-block))
  (add-to-list 'embark-repeat-actions motion))

(dolist (cmd '(org-babel-execute-maybe
               org-babel-lob-execute-maybe
               org-babel-execute-src-block
               org-babel-execute-src-block-maybe
               org-babel-execute-buffer
               org-babel-execute-subtree))
  (cl-pushnew #'embark--ignore-target
              (alist-get cmd embark-target-injection-hooks)))

(add-to-list 'embark-keymap-alist '(org-src-block embark-org-src-block-map))

;;; Inline source blocks

(defvar-keymap embark-org-inline-src-block-map
  :doc "Keymap for actions on Org inline source blocks."
  :parent embark-general-map
  "RET" #'org-babel-execute-src-block
  "'" #'org-edit-inline-src-code
  "k" #'org-babel-remove-inline-result)

(add-to-list 'embark-keymap-alist
             '(org-inline-src-block embark-org-inline-src-block-map))

;;; Babel calls

(defvar-keymap embark-org-babel-call-map
  :doc "Keymap for actions on Org babel calls."
  :parent embark-general-map
  "RET" #'org-babel-lob-execute-maybe
  "k" #'org-babel-remove-result)

(add-to-list 'embark-keymap-alist
             '(org-babel-call embark-org-babel-call-map))

;;; List items

(defvar-keymap embark-org-item-map
  :doc "Keymap for actions on Org list items."
  :parent embark-general-map
  "RET" #'org-toggle-checkbox
  "c" #'org-toggle-checkbox
  "t" #'org-toggle-item
  "n" #'org-next-item
  "p" #'org-previous-item
  "<left>" #'org-outdent-item
  "<right>" #'org-indent-item
  "<up>" #'org-move-item-up
  "<down>" #'org-move-item-down
  ">" #'org-indent-item-tree
  "<" #'org-outdent-item-tree)

(dolist (cmd '(org-toggle-checkbox
               org-toggle-item
               org-next-item
               org-previous-item
               org-outdent-item
               org-indent-item
               org-move-item-up
               org-move-item-down
               org-indent-item-tree
               org-outdent-item-tree))
  (add-to-list 'embark-repeat-actions cmd))

(add-to-list 'embark-keymap-alist '(org-item embark-org-item-map))

;;; Org plain lists

(defvar-keymap embark-org-plain-list-map
  :doc "Keymap for actions on plain Org lists."
  :parent embark-general-map
  "RET" #'org-list-repair
  "r" #'org-list-repair
  "s" #'org-sort-list
  "b" #'org-cycle-list-bullet
  "t" #'org-list-make-subtree
  "c" #'org-toggle-checkbox)

(add-to-list 'embark-repeat-actions 'org-cycle-list-bullet)

(add-to-list 'embark-keymap-alist '(org-plain-list embark-org-plain-list-map))

(cl-defun embark-org--toggle-checkboxes
    (&rest rest &key run type &allow-other-keys)
  "Around action hook for `org-toggle-checkbox'.
See `embark-around-action-hooks' for the keyword arguments RUN and TYPE.
REST are the remaining arguments."
  (apply (if (eq type 'org-plain-list) #'embark--mark-target run)
         :type type
         rest))

(cl-pushnew #'embark-org--toggle-checkboxes
            (alist-get 'org-toggle-checkbox embark-around-action-hooks))

;;; "Encode" region using Org export in place

(defvar-keymap embark-org-export-in-place-map
  :doc "Keymap for actions which replace the region by an exported version."
  :parent embark-general-map
  "m" #'org-md-convert-region-to-md
  "h" #'org-html-convert-region-to-html
  "a" #'org-ascii-convert-region-to-ascii
  "l" #'org-latex-convert-region-to-latex)

(fset 'embark-org-export-in-place-map embark-org-export-in-place-map)

(keymap-set embark-encode-map "o" 'embark-org-export-in-place-map)

;;; References to Org headings, such as agenda items

;; These are targets that represent an org heading but not in the
;; current buffer, instead they have a text property named
;; `org-marker' that points to the actual heading.

(defun embark-org-target-agenda-item ()
  "Target Org agenda item at point."
  (when (and (derived-mode-p 'org-agenda-mode)
             (get-text-property (line-beginning-position) 'org-marker))
    (let ((start (+ (line-beginning-position) (current-indentation)))
          (end (line-end-position)))
      `(org-heading ,(buffer-substring start end) ,start . ,end))))

(let ((tail (memq 'embark-org-target-element-context embark-target-finders)))
  (cl-pushnew 'embark-org-target-agenda-item (cdr tail)))

(cl-defun embark-org--at-heading
    (&rest rest &key run target &allow-other-keys)
  "RUN the action at the location of the heading TARGET refers to.
The location is given by the `org-marker' text property of
target.  Applies RUN to the REST of the arguments."
  (if-let ((marker (get-text-property 0 'org-marker target)))
      (org-with-point-at marker
        (apply run :target target rest))
    (apply run :target target rest)))

(cl-defun embark-org-goto-heading (&key target &allow-other-keys)
  "Jump to the org heading TARGET refers to."
  (when-let ((marker (get-text-property 0 'org-marker target)))
    (pop-to-buffer (marker-buffer marker))
    (widen)
    (goto-char marker)
    (org-fold-reveal)
    (pulse-momentary-highlight-one-line)))

(defun embark-org-heading-default-action (target)
  "Default action for Org headings.
There are two types of heading TARGETs: the heading at point in a
normal org buffer, and references to org headings in some other
buffer (for example, org agenda items).  For references the
default action is to jump to the reference, and for the heading
at point, the default action is whatever is bound to RET in
`embark-org-heading-map', or `org-todo' if RET is unbound."
  (if (get-text-property 0 'org-marker target)
      (embark-org-goto-heading :target target)
    (command-execute
     (or (keymap-lookup embark-org-heading-map "RET") #'org-todo))))

(defconst embark-org--invisible-jump-to-heading
  '(org-tree-to-indirect-buffer
    org-refile
    org-clock-in
    org-clock-out
    org-archive-subtree-default-with-confirmation
    org-store-link)
  "Org heading actions which won't display the heading's buffer.")

(setf (alist-get 'org-heading embark-default-action-overrides)
      #'embark-org-heading-default-action)

(map-keymap
 (lambda (_key cmd)
   (unless (or (where-is-internal cmd (list embark-general-map))
               (memq cmd embark-org--invisible-jump-to-heading))
     (cl-pushnew 'embark-org-goto-heading
                 (alist-get cmd embark-pre-action-hooks))))
 embark-org-heading-map)

(dolist (cmd embark-org--invisible-jump-to-heading)
  (cl-pushnew 'embark-org--at-heading
              (alist-get cmd embark-around-action-hooks)))

(defun embark-org--in-source-window (target function)
  "Call FUNCTION, in the source window, on TARGET's `org-marker'.

If TARGET does not have an `org-marker' property a `user-error'
is signaled.  In case the TARGET comes from an org agenda buffer
and the `other-window-for-scrolling' is an org mode buffer, then
the FUNCTION is called with that other window temporarily
selected; otherwise the FUNCTION is called in the selected
window."
  (if-let ((marker (get-text-property 0 'org-marker target)))
      (with-selected-window
          (or (and (derived-mode-p 'org-agenda-mode)
                   (let ((window (ignore-errors (other-window-for-scrolling))))
                     (with-current-buffer (window-buffer window)
                       (when (derived-mode-p 'org-mode) window))))
              (selected-window))
        (funcall function marker))
    (user-error "The target is an org heading rather than a reference to one")))

(defun embark-org-refile-here (target)
  "Refile the heading at point in the source window to TARGET.

If TARGET is an agenda item and `other-window-for-scrolling' is
displaying an org mode buffer, then that is the source window.
If TARGET is a minibuffer completion candidate, then the source
window is the window selected before the command that opened the
minibuffer ran."
  (embark-org--in-source-window target
    (lambda (marker)
      (org-refile nil nil
                  ;; The RFLOC argument:
                  (list
                   ;; Name
                   (org-with-point-at marker
                     (nth 4 (org-heading-components)))
                   ;; File
                   (buffer-file-name (marker-buffer marker))
                   ;; nil
                   nil
                   ;; Position
                   marker)))))

(defun embark-org-insert-link-to (target)
  "Insert a link to the TARGET in the source window.

If TARGET is an agenda item and `other-window-for-scrolling' is
displaying an org mode buffer, then that is the source window.
If TARGET is a minibuffer completion candidate, then the source
window is the window selected before the command that opened the
minibuffer ran."
  (embark-org--in-source-window target
    (lambda (marker)
      (org-with-point-at marker (org-store-link nil t))
      (org-insert-all-links 1 "" ""))))

(provide 'embark-org)
;;; embark-org.el ends here
