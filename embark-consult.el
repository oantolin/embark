;;; embark-consult.el --- Consult integration for Embark -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023  Free Software Foundation, Inc.

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Maintainer: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: convenience
;; Version: 1.1
;; Homepage: https://github.com/oantolin/embark
;; Package-Requires: ((emacs "27.1") (compat "30") (embark "1.0") (consult "1.7"))

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

;; This package provides integration between Embark and Consult.  The package
;; will be loaded automatically by Embark.

;; Some of the functionality here was previously contained in Embark
;; itself:

;; - Support for consult-buffer, so that you get the correct actions
;; for each type of entry in consult-buffer's list.

;; - Support for consult-line, consult-outline, consult-mark and
;; consult-global-mark, so that the insert and save actions don't
;; include a weird unicode character at the start of the line, and so
;; you can export from them to an occur buffer (where occur-edit-mode
;; works!).

;; Just load this package to get the above functionality, no further
;; configuration is necessary.

;; Additionally this package contains some functionality that has
;; never been in Embark: access to Consult preview from auto-updating
;; Embark Collect buffer that is associated to an active minibuffer
;; for a Consult command.  For information on Consult preview, see
;; Consult's info manual or its readme on GitHub.

;; If you always want the minor mode enabled whenever it possible use:

;; (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

;; If you don't want the minor mode automatically on and prefer to
;; trigger the consult previews manually use this instead:

;; (keymap-set embark-collect-mode-map "C-j"
;;   #'consult-preview-at-point)

;;; Code:

(require 'embark)
(require 'consult)

(eval-when-compile
  (require 'cl-lib))

;;; Consult preview from Embark Collect buffers

(defun embark-consult--collect-candidate ()
  "Return candidate at point in collect buffer."
  (cadr (embark-target-collect-candidate)))

(add-hook 'consult--completion-candidate-hook #'embark-consult--collect-candidate)

;;; Support for consult-location

(defun embark-consult--strip (string)
  "Strip substrings marked with the `consult-strip' property from STRING."
  (if (text-property-not-all 0 (length string) 'consult-strip nil string)
      (let ((end (length string)) (pos 0) (chunks))
        (while (< pos end)
          (let ((next (next-single-property-change pos 'consult-strip string end)))
            (unless (get-text-property pos 'consult-strip string)
              (push (substring string pos next) chunks))
            (setq pos next)))
        (apply #'concat (nreverse chunks)))
    string))

(defun embark-consult--target-strip (type target)
  "Remove the unicode suffix character from a TARGET of TYPE."
  (cons type (embark-consult--strip target)))

(setf (alist-get 'consult-location embark-transformer-alist)
      #'embark-consult--target-strip)

(defun embark-consult-goto-location (target)
  "Jump to consult location TARGET."
  (consult--jump (car (consult--get-location target)))
  (pulse-momentary-highlight-one-line (point)))

(setf (alist-get 'consult-location embark-default-action-overrides)
      #'embark-consult-goto-location)

(defun embark-consult-export-location-occur (lines)
  "Create an occur mode buffer listing LINES.
The elements of LINES should be completion candidates with
category `consult-line'."
  (let ((buf (generate-new-buffer "*Embark Export Occur*"))
        (mouse-msg "mouse-2: go to this occurrence")
        (inhibit-read-only t)
        last-buf)
    (with-current-buffer buf
      (dolist (line lines)
        (pcase-let*
            ((`(,loc . ,num) (consult--get-location line))
             ;; the text properties added to the following strings are
             ;; taken from occur-engine
             (lineno (propertize
                      (format "%7d:" num)
                      'occur-prefix t
                      ;; Allow insertion of text at the end
                      ;; of the prefix (for Occur Edit mode).
                      'front-sticky t
                      'rear-nonsticky t
                      'read-only t
                      'occur-target loc
                      'follow-link t
                      'help-echo mouse-msg
                      'font-lock-face list-matching-lines-prefix-face
                      'mouse-face 'highlight))
             (contents (propertize (embark-consult--strip line)
                                   'occur-target loc
                                   'occur-match t
                                   'follow-link t
                                   'help-echo mouse-msg
                                   'mouse-face 'highlight))
             (nl (propertize "\n" 'occur-target loc))
             (this-buf (marker-buffer loc)))
          (unless (eq this-buf last-buf)
            (insert (propertize
                     (format "lines from buffer: %s\n" this-buf)
                     'face list-matching-lines-buffer-name-face
                     'read-only t))
            (setq last-buf this-buf))
          (insert lineno contents nl)))
      (goto-char (point-min))
      (occur-mode))
    (pop-to-buffer buf)))

(defun embark-consult-export-location-grep (lines)
  "Create a grep mode buffer listing LINES.
Any LINES that come from a buffer which is not visiting a file
will be excluded from the grep buffer, since grep mode only works
with files.  The elements of LINES should be completion
candidates with category `consult-location'.  No matches will be
highlighted in the exported buffer, since the `consult-location'
candidates do not carry that information."
  (let (non-file-buffers)
    (embark-consult--export-grep
     :header "Exported line search results (file-backed buffers only):\n\n"
     :lines lines
     :insert
     (lambda (lines)
       (let ((count 0))
         (dolist (line lines)
           (pcase-let* ((`(,loc . ,num) (consult--get-location line))
                        (lineno (format "%d" num))
                        (contents (embark-consult--strip line))
                        (buffer (marker-buffer loc))
                        (file (buffer-file-name buffer)))
             (if (null file)
                 (cl-pushnew buffer non-file-buffers)
               (insert (file-relative-name file) ":" lineno ":" contents "\n")
               (cl-incf count))))
         count))
     :footer
     (lambda ()
       (when non-file-buffers
         (let ((start (goto-char (point-max))))
           (insert "\nSome results were in buffers with no associated file"
                   " and are missing\nfrom the exported result:\n")
           (dolist (buf non-file-buffers)
             (insert "- " (buffer-name buf) "\n"))
           (insert "\nEither save the buffers or use the"
                   " `embark-consult-export-location-occur'\nexporter.")
           (message "This exporter does not support non-file buffers: %s"
                    non-file-buffers)
           (add-text-properties
            start (point-max)
            '(read-only t wgrep-footer t front-sticky t))))))))

(defun embark-consult--upgrade-markers ()
  "Upgrade consult-location cheap markers to real markers.
This function is meant to be added to `embark-collect-mode-hook'."
  (when (eq embark--type 'consult-location)
    (dolist (entry tabulated-list-entries)
      (when (car entry)
        (consult--get-location (car entry))))))

;; Set default `occur-mode' based exporter for consult-line,
;; consult-line-multi, consult-outline and alike Another option is
;; using grep-mode by using `embark-consult-export-location-grep'
(setf (alist-get 'consult-location embark-exporters-alist)
      #'embark-consult-export-location-occur)
(cl-pushnew #'embark-consult--upgrade-markers embark-collect-mode-hook)

;;; Support for consult-grep

(defvar grep-mode-line-matches)
(defvar grep-num-matches-found)
(declare-function wgrep-setup "ext:wgrep")

(defvar-keymap embark-consult-rerun-map
  :doc "A keymap with a binding for `embark-rerun-collect-or-export'."
  :parent nil
  "g" #'embark-rerun-collect-or-export)

(cl-defun embark-consult--export-grep (&key header lines insert footer)
  "Create a grep mode buffer listing LINES.
The HEADER string is inserted at the top of the buffer.  The
function INSERT is called to insert the LINES and should return a
count of the matches (there may be more than one match per line).
The function FOOTER is called to insert a footer."
  (let ((buf (generate-new-buffer "*Embark Export Grep*")))
    (with-current-buffer buf
      (insert (propertize header 'wgrep-header t 'front-sticky t))
      (let ((count (funcall insert lines)))
        (funcall footer)
        (goto-char (point-min))
        (grep-mode)
        (setq-local grep-num-matches-found count
                    mode-line-process grep-mode-line-matches))
      ;; Make this buffer current for next/previous-error
      (setq next-error-last-buffer buf)
      ;; Set up keymap before possible wgrep-setup, so that wgrep
      ;; restores our binding too when the user finishes editing.
      (use-local-map (make-composed-keymap
                      embark-consult-rerun-map
                      (current-local-map)))
      ;; TODO Wgrep 3.0 and development versions use different names for the
      ;; parser variable.
      (defvar wgrep-header/footer-parser)
      (defvar wgrep-header&footer-parser)
      (setq-local wgrep-header/footer-parser #'ignore
                  wgrep-header&footer-parser #'ignore)
      (when (fboundp 'wgrep-setup) (wgrep-setup)))
    (pop-to-buffer buf)))

(defun embark-consult-export-grep (lines)
  "Create a grep mode buffer listing LINES.
The elements of LINES should be completion candidates with
category `consult-grep'."
  (embark-consult--export-grep
   :header "Exported grep results:\n\n"
   :lines lines
   :insert
   (lambda (lines)
     (dolist (line lines) (insert line "\n"))
     (goto-char (point-min))
     (let ((count 0) prop)
       (while (setq prop (text-property-search-forward
                          'face 'consult-highlight-match t))
         (cl-incf count)
         (put-text-property (prop-match-beginning prop)
                            (prop-match-end prop)
                            'font-lock-face
                            'match))
       count))
   :footer #'ignore))

(defun embark-consult-goto-grep (location)
  "Go to LOCATION, which should be a string with a grep match."
  (consult--jump (consult--grep-position location))
  (pulse-momentary-highlight-one-line (point)))

(setf (alist-get 'consult-grep embark-default-action-overrides)
      #'embark-consult-goto-grep)
(setf (alist-get 'consult-grep embark-exporters-alist)
      #'embark-consult-export-grep)

;;; Support for consult-xref

(declare-function consult-xref "ext:consult-xref")
(declare-function xref--show-xref-buffer "xref")
(declare-function xref-pop-to-location "xref")
(defvar xref-auto-jump-to-first-xref)
(defvar consult-xref--fetcher)

(defun embark-consult-export-xref (items)
  "Create an xref buffer listing ITEMS."
  (cl-flet ((xref-items (items)
              (mapcar (lambda (item) (get-text-property 0 'consult-xref item))
                      items)))
    (let ((fetcher consult-xref--fetcher)
          (input (minibuffer-contents)))
      (set-buffer
       (xref--show-xref-buffer
        (lambda ()
          (let ((candidates (funcall fetcher)))
            (if (null (cdr candidates))
                candidates
              (catch 'xref-items
                (minibuffer-with-setup-hook
                    (lambda ()
                      (insert input)
                      (add-hook
                       'minibuffer-exit-hook
                       (lambda ()
                         (throw 'xref-items
                                (xref-items
                                 (or
                                  (plist-get
                                   (embark--maybe-transform-candidates)
                                   :candidates)
                                  (user-error "No candidates for export")))))
                       nil t))
                  (consult-xref fetcher))))))
        `((fetched-xrefs . ,(xref-items items))
          (window . ,(embark--target-window))
          (auto-jump . ,xref-auto-jump-to-first-xref)
          (display-action)))))))

(setf (alist-get 'consult-xref embark-exporters-alist)
      #'embark-consult-export-xref)

(defun embark-consult-xref (cand)
  "Default action override for `consult-xref', open CAND xref location."
  (xref-pop-to-location (get-text-property 0 'consult-xref cand)))

(setf (alist-get 'consult-xref embark-default-action-overrides)
      #'embark-consult-xref)

;;; Support for consult-find and consult-locate

(setf (alist-get '(file . consult-find) embark-default-action-overrides
                 nil nil #'equal)
      #'find-file)

(setf (alist-get '(file . consult-locate) embark-default-action-overrides
                 nil nil #'equal)
      #'find-file)

;;; Support for consult-isearch-history

(setf (alist-get 'consult-isearch-history embark-transformer-alist)
      #'embark-consult--target-strip)

;;; Support for consult-man and consult-info

(defun embark-consult-man (cand)
  "Default action override for `consult-man', open CAND man page."
  (man (get-text-property 0 'consult-man cand)))

(setf (alist-get 'consult-man embark-default-action-overrides)
      #'embark-consult-man)

(declare-function consult-info--action "ext:consult-info")

(defun embark-consult-info (cand)
  "Default action override for `consult-info', open CAND info manual."
  (consult-info--action cand)
  (pulse-momentary-highlight-one-line (point)))

(setf (alist-get 'consult-info embark-default-action-overrides)
      #'embark-consult-info)

(setf (alist-get 'consult-info embark-transformer-alist)
      #'embark-consult--target-strip)

;;; Bindings for consult commands in embark keymaps

(keymap-set embark-become-file+buffer-map "C b" #'consult-buffer)
(keymap-set embark-become-file+buffer-map "C 4 b" #'consult-buffer-other-window)

;;; Support for Consult search commands

(defvar-keymap embark-consult-sync-search-map
  :doc "Keymap for Consult sync search commands"
  :parent nil
  "o" #'consult-outline
  "i" 'consult-imenu
  "I" 'consult-imenu-multi
  "l" #'consult-line
  "L" #'consult-line-multi)

(defvar-keymap embark-consult-async-search-map
  :doc "Keymap for Consult async search commands"
  :parent nil
  "g" #'consult-grep
  "r" #'consult-ripgrep
  "G" #'consult-git-grep
  "f" #'consult-find
  "F" #'consult-locate)

(defvar embark-consult-search-map
  (keymap-canonicalize
   (make-composed-keymap embark-consult-sync-search-map
                         embark-consult-async-search-map))
  "Keymap for all Consult search commands.")

(fset 'embark-consult-sync-search-map embark-consult-sync-search-map)
(keymap-set embark-become-match-map "C" 'embark-consult-sync-search-map)

(cl-pushnew 'embark-consult-async-search-map embark-become-keymaps)

(fset 'embark-consult-search-map embark-consult-search-map)
(keymap-set embark-general-map "C" 'embark-consult-search-map)

(map-keymap
 (lambda (_key cmd)
   (cl-pushnew 'embark--unmark-target
               (alist-get cmd embark-pre-action-hooks))
   (cl-pushnew 'embark--allow-edit
               (alist-get cmd embark-target-injection-hooks)))
 embark-consult-search-map)

(defun embark-consult--unique-match (&rest _)
  "If there is a unique matching candidate, accept it.
This is intended to be used in `embark-target-injection-hooks'."
  (let ((candidates (cdr (embark-minibuffer-candidates))))
    (if (or (null candidates) (cdr candidates))
        (embark--allow-edit)
      (delete-minibuffer-contents)
      (insert (car candidates)))))

(dolist (cmd '(consult-outline consult-imenu consult-imenu-multi))
  (setf (alist-get cmd embark-target-injection-hooks)
        (remq 'embark--allow-edit
              (alist-get cmd embark-target-injection-hooks)))
  (cl-pushnew #'embark-consult--unique-match
              (alist-get cmd embark-target-injection-hooks)))

(cl-defun embark-consult--async-search-dwim
    (&key action type target candidates &allow-other-keys)
  "DWIM when using a Consult async search command as an ACTION.
If the TYPE of the target(s) has a notion of associated
file (files, buffers, libraries and some bookmarks do), then run
the ACTION with `consult-project-function' set to nil, and search
only the files associated to the TARGET or CANDIDATES.  For other
types, run the ACTION with TARGET or CANDIDATES as initial input."
  (if-let ((file-fn (cdr (assq type embark--associated-file-fn-alist))))
      (let (consult-project-function)
        (funcall action
                 (delq nil (mapcar file-fn (or candidates (list target))))))
    (funcall action nil (or target (string-join candidates " ")))))

(map-keymap
 (lambda (_key cmd)
   (unless (eq cmd #'consult-locate)
     (cl-pushnew cmd embark-multitarget-actions)
     (cl-pushnew #'embark-consult--async-search-dwim
                 (alist-get cmd embark-around-action-hooks))))
 embark-consult-async-search-map)

;;; Tables of contents for buffers: imenu and outline candidate collectors

(defun embark-consult-outline-candidates ()
  "Collect all outline headings in the current buffer."
  (cons 'consult-location (consult--outline-candidates)))

(autoload 'consult-imenu--items "consult-imenu")

(defun embark-consult-imenu-candidates ()
  "Collect all imenu items in the current buffer."
  (cons 'imenu (mapcar #'car (consult-imenu--items))))

(declare-function consult-imenu--group "ext:consult-imenu")

(defun embark-consult--imenu-group-function (type prop)
  "Return a suitable group-function for imenu.
TYPE is the completion category.
PROP is the metadata property.
Meant as :after-until advice for `embark-collect--metadatum'."
  (when (and (eq type 'imenu) (eq prop 'group-function))
    (consult-imenu--group)))

(advice-add #'embark-collect--metadatum :after-until
            #'embark-consult--imenu-group-function)

(defun embark-consult-imenu-or-outline-candidates ()
  "Collect imenu items in prog modes buffer or outline headings otherwise."
  (if (derived-mode-p 'prog-mode)
      (embark-consult-imenu-candidates)
    (embark-consult-outline-candidates)))

(setf (alist-get 'imenu embark-default-action-overrides) 'consult-imenu)

(add-to-list 'embark-candidate-collectors
             #'embark-consult-imenu-or-outline-candidates
             'append)

(provide 'embark-consult)
;;; embark-consult.el ends here
