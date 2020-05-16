;;; embark.el --- Conveniently act on minibuffer completions   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Omar Antolín Camarena

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

;; 

;;; Code:

(defgroup embark nil
  "Emacs Mini-Buffer Actions Rooted in Keymaps"
  :group 'minibuffer)

(defcustom embark-keymap-alist
  '((general . embark-general-map)
    (file . embark-file-map)
    (buffer . embark-buffer-map)
    (symbol . embark-symbol-map))
  "Alist of action types and corresponding keymaps."
  :type '(alist :key-type symbol :value-type variable)
  :group 'embark)

(defcustom embark-classifiers
  '(embark-category embark-symbol)
  "List of functions to classify the current completion session.
Each function should take no arguments and return a symbol
classifying the current minibuffer completion session, or nil to
indicate it could not determine the type of completion."
  :type 'hook
  :group 'embark)

(defun embark-category ()
  "Return minibuffer completion category per metadata."
  (completion-metadata-get
   (completion-metadata (minibuffer-contents)
                        minibuffer-completion-table
                        minibuffer-completion-predicate)
   'category))

(defun embark-symbol ()
  "Determine if currently completing symbols."
  (let ((mct minibuffer-completion-table))
    (when (or (eq mct 'help--symbol-completion-table)
              (vectorp mct)
              (and (consp mct) (symbolp (car mct))))
      'symbol)))

(defun embark-classify ()
  "Classify current minibuffer completion session."
  (or (run-hook-with-args-until-success 'embark-classifiers)
      'general))

(defvar embark--target nil
  "String the next action will operate on.")

(defvar embark--old-erm nil
  "Stores value of `enable-recursive-minibuffers'.")

(defun embark-target ()
  "Return the target for the current action.
Save the result somewhere if you need it more than once: calling
this function again before the next action is initiating will
return nil."
  (prog1 embark--target
    (setq embark--target nil)))

(defun embark--inject ()
  "Inject `embark-string' into next minibuffer prompt."
  (unless (eq this-command 'execute-extended-command)
    (delete-minibuffer-contents)
    (insert (embark-target))))

(defun embark--cleanup ()
  "Remove all hooks and modifications."
  (unless embark--target
    (setq enable-recursive-minibuffers embark--old-erm)
    (remove-hook 'minibuffer-setup-hook #'embark--inject)
    (remove-hook 'post-command-hook #'embark--cleanup)))

(defun embark--set-target ()
  "Set the top completion candidate as target."
  (let ((old-contents (minibuffer-contents)))
    (minibuffer-force-complete nil nil t)
    (setq embark--target (minibuffer-contents))
    (delete-minibuffer-contents)
    (insert old-contents)))

(defun embark-act ()
  "Embark upon a minibuffer action.
Bind this command to a key in `minibuffer-local-completion-map'."
  (interactive)
  (let* ((kind (embark-classify))
         (keymap (cdr (assq kind embark-keymap-alist))))
    (setq embark--old-erm enable-recursive-minibuffers
          enable-recursive-minibuffers t)
    (embark--set-target)
    (message "Act on %s %s" kind embark--target)
    (add-hook 'minibuffer-setup-hook #'embark--inject)
    (add-hook 'post-command-hook #'embark--cleanup)
    (set-transient-map (symbol-value keymap))))

(defun embark-insert ()
  "Insert `embark-target' into the previously selected buffer at point."
  (interactive)
  (with-minibuffer-selected-window (insert (embark-target))))

(defun embark-save ()
  "Save `embark-target' in the kill ring."
  (interactive)
  (kill-new (embark-target)))

(defun embark-describe-symbol ()
  "Describe `embark-target' as a symbol"
  (interactive)
  (describe-symbol (intern (embark-target))))

(defvar embark-general-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" #'embark-insert)
    (define-key map "w" #'embark-save)
    map))

(defvar embark-file-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map embark-general-map)
    (define-key map "d" #'delete-file)
    (define-key map "r" #'rename-file)
    (define-key map "c" #'copy-file)
    map))

(defvar embark-buffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map embark-general-map)
    (define-key map "k" #'kill-buffer)
    map))

(defvar embark-symbol-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map embark-general-map)
    (define-key map "o" #'embark-describe-symbol)
    map))

(provide 'embark)
;;; embark.el ends here
