;;; embark-consult.el --- Consult integration for Embark -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Omar Antolín Camarena

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: convenience
;; Version: 0.1
;; Homepage: https://github.com/oantolin/embark
;; Package-Requires: ((emacs "25.1") (embark "0.9") (consult "0.1"))

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

;; This package provides integration between Embark and Consult. To
;; use it, arrange for it to be loaded once both of those are loaded:

;; (with-eval-after-load 'consult
;;   (with-eval-after-load 'embark
;;     (require 'embark-consult)))

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
;; for a Consult command. For information on Consult preview, see
;; Consult's info manual or its readme on GitHub.

;; - `embark-consult-preview-at-point', a command to trigger Consult's
;; preview for the entry at point.

;; - `embark-consult-preview-minor-mode', a minor mode for Embark
;; Collect buffers that automatically previews the entry at point as
;; you move around.

;; If you always want the minor mode enabled whenever it possible use:

;; (add-hook 'embark-collect-mode-hook #'embark-consult-preview-minor-mode)

;; If you don't want the minor mode automatically on and prefer to
;; trigger the consult previews manually use this instead:

;; (define-key embark-collect-mode-map (kbd "C-j")
;;   #'embark-consult-preview-at-point)

;;; Code:

(require 'embark)
(require 'consult)

(eval-when-compile
  (require 'cl-lib))

;;; Consult preview from Embark Collect buffers

(defvar-local embark-consult-preview--last-entry nil
  "Stores last entry previewed.")

(defun embark-consult-preview--preconditions ()
  "Check if Consult preview for Embark can be used in current buffer.
Signal an error unless current buffer is an auto-updating Embark
Collect buffer that is associated to an active minibuffer for a
Consult command."
  (unless (derived-mode-p 'embark-collect-mode)
    (user-error "Not in an Embark Collect buffer"))
  (unless (and (active-minibuffer-window)
               (eq (window-buffer (active-minibuffer-window))
                   embark-collect-from))
    (user-error
     "This Embark Collect buffer is not associated to an active minibuffer"))
  (unless (buffer-local-value 'consult--preview-function embark-collect-from)
    (user-error "No Consult preview function found")))

(defun embark-consult-preview--trigger ()
  "Trigger Consult preview for entry at point if different from previous."
  (let ((entry (ignore-errors (button-label (point))))) ; error at eob
    (unless (equal entry embark-consult-preview--last-entry)
      (setq embark-consult-preview--last-entry entry)
      (with-selected-window (active-minibuffer-window)
        (funcall consult--preview-function (minibuffer-contents) entry)))))

(defun embark-consult-preview-at-point ()
  "Trigger Consult preview for Embark Collect entry at point.
Must be run from an auto-updating Embark Collect buffer that is
associated to an active minibuffer for a Consult command."
  (interactive)
  (condition-case err
      (progn
        (embark-consult-preview--preconditions)
        (embark-consult-preview--trigger))
    (user-error
     (embark-consult-preview-minor-mode -1)
     (message "Turning off preview: %s" (cadr err)))))

(define-minor-mode embark-consult-preview-minor-mode
  "Minor mode to use Consult preview as you move around.
Must be used in an auto-updating Embark Collect buffer that is
associated to an active minibuffer for a Consult command."
  :init-value nil
  :lighter " Preview"
  (remove-hook 'post-command-hook #'embark-consult-preview-at-point t)
  (condition-case nil
      (progn
        (embark-consult-preview--preconditions)
        (when embark-consult-preview-minor-mode
          (add-hook 'post-command-hook
                    #'embark-consult-preview-at-point nil t)))
    (user-error (setq embark-consult-preview-minor-mode nil))))

;;; Support for consult-location

(defun embark-consult--strip-prefix (string property)
  "Remove the initial characters of STRING with the given PROPERTY.
The prefix is stored as the `embark-consult-prefix' property of
the first remaining character."
  (when (get-text-property 0 property string)
    (let* ((i (next-single-property-change 0 property string))
           (stripped (substring string i)))
      (put-text-property 0 1
                         'embark-consult-prefix (substring string 0 i)
                         stripped)
      stripped)))

(defun embark-consult-location-strip-prefix (target)
  "Remove the unicode prefix character from a `consult-location' TARGET."
  (cons 'consult-location
        (embark-consult--strip-prefix target 'consult-location)))

(setf (alist-get 'consult-location embark-transformer-alist)
      #'embark-consult-location-strip-prefix)

(defun embark-consult-export-occur (lines)
  "Create an occur mode buffer listing LINES.
The elements of LINES are assumed to be values of category `consult-line'."
  (let ((buf (generate-new-buffer "*Embark Export Occur*"))
        (mouse-msg "mouse-2: go to this occurrence")
        last-buf)
    (with-current-buffer buf
      (dolist (line lines)
        (pcase-let*
            ((`(,loc . ,num) (get-text-property 0 'consult-location line))
             (prefix-len (next-single-property-change 0 'consult-location line))
             ;; the text properties added to the following strings are
             ;; taken from occur-engine
             (lineno (propertize (format "%7d:" num)
                                 'occur-prefix t
				 ;; Allow insertion of text at the end
                                 ;; of the prefix (for Occur Edit mode).
				 'front-sticky t
				 'rear-nonsticky t
				 'occur-target loc
				 'follow-link t
				 'help-echo mouse-msg))
             (contents (propertize (substring line prefix-len)
				   'occur-target loc
                                   'occur-match t
				   'follow-link t
				   'help-echo mouse-msg))
             (nl (propertize "\n" 'occur-target loc))
             (this-buf (marker-buffer loc)))
          (unless (eq this-buf last-buf)
            (insert (propertize
                     (format "lines from buffer: %s\n" this-buf)
                     'face list-matching-lines-buffer-name-face))
            (setq last-buf this-buf))
          (insert (concat lineno contents nl))))
      (goto-char (point-min))
      (occur-mode))
    (switch-to-buffer buf)))

(setf (alist-get 'consult-location embark-collect-initial-view-alist)
      'list)
(setf (alist-get 'consult-location embark-exporters-alist)
      #'embark-consult-export-occur)

;;; Support for consult-grep

(defvar wgrep-header/footer-parser)
(declare-function wgrep-setup "wgrep")

(defun embark-consult-export-grep (lines)
  "Create a grep mode buffer listing LINES."
  (let ((buf (generate-new-buffer "*Embark Export Grep*")))
    (with-current-buffer buf
      (insert (propertize "Exported grep results:\n\n" 'wgrep-header t))
      (dolist (line lines) (insert line "\n"))
      (goto-char (point-min))
      (grep-mode)
      (setq-local wgrep-header/footer-parser #'ignore)
      (when (fboundp 'wgrep-setup) (wgrep-setup)))
    (switch-to-buffer buf)))

(autoload 'compile-goto-error "compile")

(defun embark-consult-goto-location (location)
  "Go to LOCATION, which should be a string with a grep match."
  (interactive "sLocation: ")
  ;; Actions are run in the target window, so in this case whatever
  ;; window was selected when the command that produced the
  ;; xref-location candidates ran.  In particular, we inherit the
  ;; default-directory of the buffer in that window, but we really
  ;; want the default-directory of the minibuffer or collect window we
  ;; call the action from, which is the previous window, since the
  ;; location is given relative to that directory.
  (with-temp-buffer
    (setq default-directory (with-selected-window (previous-window)
                              default-directory))
    (insert location "\n")
    (grep-mode)
    (goto-char (point-min))
    (let ((display-buffer-overriding-action '(display-buffer-same-window)))
      (compile-goto-error))))

(setf (alist-get 'consult-grep embark-default-action-overrides)
      #'embark-consult-goto-location)
(setf (alist-get 'consult-grep embark-exporters-alist)
      #'embark-export-grep)
(setf (alist-get 'consult-grep embark-collect-initial-view-alist)
      'list)

;;; Support for consult-multi

(defun embark-consult-refine-multi-type (target)
  "Refine `consult-multi' TARGET to its real type.
This function takes a target of type `consult-multi' (from
Consult's `consult-multi' category) and transforms it to its
actual type."
  (pcase (get-text-property 0 'consult-multi target)
    (`(,category . ,stripped)
     (put-text-property 0 1 'embark-consult-prefix (substring target 0 1)
                        stripped)
     (cons category stripped))
    ('nil (cons 'general target))))

(setf (alist-get 'consult-multi embark-transformer-alist)
      #'embark-consult-refine-multi-type)

;;; Support for consult-isearch

(defun embark-consult-isearch-strip-prefix (target)
  "Remove the unicode prefix character from a `consult-isearch' TARGET."
  (cons 'consult-isearch (embark-consult--strip-prefix target 'invisible)))

(setf (alist-get 'consult-isearch embark-transformer-alist)
      #'embark-consult-isearch-strip-prefix)

;;; Support for consult-register

(setf (alist-get 'consult-register embark-collect-initial-view-alist)
      'zebra)

;;; Support for consult-yank*

(setf (alist-get 'consult-yank embark-collect-initial-view-alist)
      'zebra)

;;; bindings for consult commands in embark keymaps

(define-key embark-file-map "x" #'consult-file-externally)

(define-key embark-become-file+buffer-map "Cb" #'consult-buffer)

;;; Support for Consult search commands

(embark-define-keymap embark-consult-non-async-search-map
  "Keymap for Consult non-async search commands"
  ("o" consult-outline)
  ("i" consult-imenu)
  ("p" consult-project-imenu)
  ("l" consult-line))

(embark-define-keymap embark-consult-async-search-map
  "Keymap for Consult async search commands"
  ("g" consult-grep)
  ("r" consult-ripgrep)
  ("G" consult-git-grep)
  ("f" consult-find)
  ("L" consult-locate))

(defvar embark-consult-search-map
  (keymap-canonicalize
   (make-composed-keymap embark-consult-non-async-search-map
                         embark-consult-async-search-map))
  "Keymap for Consult async search commands.")

(define-key embark-become-match-map "C" embark-consult-non-async-search-map)

(add-to-list 'embark-become-keymaps 'embark-consult-async-search-map)

(define-key embark-general-map "C" embark-consult-search-map)

(dolist (bind (cdr embark-consult-search-map))
  (add-to-list 'embark-allow-edit-commands (cdr bind)))

(defun embark-consult-unique-match ()
  "If there is a unique matching candidate, accept it.
This is intended to be used in `embark-setup-overrides' for some
actions that are on `embark-allow-edit-commands'."
  ;; I couldn't quickly get this to work for ivy, so just skip ivy
  (unless (eq mwheel-scroll-up-function 'ivy-next-line)
    (let ((candidates (embark-minibuffer-candidates)))
      (unless (or (null (cdr candidates)) (cddr candidates))
        (delete-minibuffer-contents)
        (insert (cadr candidates))
        (add-hook 'post-command-hook #'exit-minibuffer nil t)))))

(dolist (cmd '(consult-outline consult-imenu consult-project-imenu))
  (cl-pushnew #'embark-consult-unique-match
              (alist-get cmd embark-setup-overrides)))

(defun embark-consult-accept-tofu ()
  "Accept input if it already has the unicode prefix.
This is intended to be used in `embark-setup-overrides' for the
`consult-line' and `consult-outline' actions."
  (let ((input (minibuffer-contents)))
    (when (and (> (length input) 0)
               (<= consult--tofu-char
                   (aref input 0)
                   (+ consult--tofu-char consult--tofu-range -1)))
      (add-hook 'post-command-hook #'exit-minibuffer nil t))))

(dolist (cmd '(consult-line consult-outline))
  (cl-pushnew #'embark-consult-accept-tofu
              (alist-get cmd embark-setup-overrides)))

(defun embark-consult-add-async-separator ()
  "Add Consult's async separator at the beginning.
This is intended to be used in `embark-setup-hook' for any action
that is a Consult async command."
  (when consult-async-default-split
    (goto-char (minibuffer-prompt-end))
    (insert consult-async-default-split)
    (goto-char (point-max))))

(dolist (bind (cdr embark-consult-async-search-map))
  (cl-pushnew #'embark-consult-add-async-separator
              (alist-get (cdr bind) embark-setup-overrides)))

;; fix default action for tofu-prefixing commands

(defun embark-consult-restore-prefix ()
  "Replace the minibuffer contents with the untransformed target.
This is used for default actions for types that have a
transformer that removes a unicode prefix from the target."
  (when-let ((pos (minibuffer-prompt-end))
             (prefix (get-text-property pos 'embark-consult-prefix)))
    (goto-char pos)
    (insert prefix)))

(dolist (cmd '(consult-line consult-buffer consult-isearch
               consult-outline consult-mark consult-global-mark))
  (cl-pushnew #'embark-consult-restore-prefix
              (alist-get cmd embark-setup-overrides)))

(provide 'embark-consult)
;;; embark-consult.el ends here
