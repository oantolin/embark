;;; embark.el --- Conveniently act on minibuffer completions   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Omar Antolín Camarena

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: convenience
;; Version: 0.1
;; Homepage: https://github.com/oantolin/embark
;; Package-Requires: ((emacs "25.1"))

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

;; embark - Emacs Mini-Buffer Actions Rooted in Keymaps.

;; This package provides a command `embark-act' to execute actions on
;; the top minibuffer completion canidate (the one that would be
;; chosen by minibuffer-force-complete).  You should bind `embark-act'
;; to some key in `minibuffer-local-completion-map'.

;; The actions are arranged into keymaps separated by the type of
;; completion currently taking place.  By default `embark' recognizes
;; the following types of completion: file names, buffers and symbols.
;; The classification is configurable, see the variable
;; `embark-classifiers'.

;; For any given type there is a corresponding keymap as noted in
;; `embark-keymap-alist'.  For example, for the completion category
;; `file', by default the corresponding keymap is `embark-file-map'.
;; In this keymap you can bind normal commands you might want to use
;; on file names.  For example, by default `embark-file-map' binds
;; `delete-file' to "d", `rename-file' to "r" and `copy-file' to "c".

;; The default keymaps that come with `embark' all set
;; `embark-general-map' as their parent, so that the actions bound
;; there are available no matter what type of completion you are in
;; the middle of.  By default this includes bindings to save the
;; current candidate in the kill ring and to insert the current
;; candidate in the previously selected buffer (the buffer that was
;; current when you executed a command that opened up the minibuffer).

;; You can use any command that reads from the minibuffer as an action
;; and the target of the action will be inserted at the first
;; minibuffer prompt.  You don't even have to bind a command in one of
;; the keymaps listed in `embark-keymap-alist' to use it!  After
;; running `embark-act' all of your keybindings and even
;; `execute-extended-command' can be used to run a command.

;; Additionally you can write your own commands that do not read from
;; the minibuffer but act on the current target anyway: just use the
;; `embark-target' function (exactly once!: it "self-destructs") to
;; retrieve the current target.  See the definitions of
;; `embark-insert' or `embark-save' for examples.

;; If you wish to see a reminder of which actions are available, for
;; now, I recommend installing which-key and using `which-key-mode'
;; with the `which-key-show-transient-maps' variable set to t.

;;; Code:

(defgroup embark nil
  "Emacs Mini-Buffer Actions Rooted in Keymaps"
  :group 'minibuffer)

(defcustom embark-general-map-alist
  '(("i" . embark-insert)
    ("w" . embark-save))
  "Base keymap alist for general actions."
  :type '(alist :key-type symbol :value-type function)
  :group 'embark)

(defcustom embark-file-map-alist
  '(("d" . delete-file)
    ("r" . rename-file)
    ("c" . copy-file))
  "Base keymap alist for file actions."
  :type '(alist :key-type symbol :value-type function)
  :group 'embark)

(defcustom embark-buffer-map-alist
  '(("k" . kill-buffer))
  "Base keymap alist for buffer actions."
  :type '(alist :key-type symbol :value-type function)
  :group 'embark)

(defcustom embark-symbol-map-alist
  '(("o" . embark-describe-symbol))
  "Base keymap alist for symbol actions."
  :type '(alist :key-type symbol :value-type function)
  :group 'embark)

(defcustom embark-keymap-alist
  '((general . embark-general-map)
    (file . embark-file-map)
    (buffer . embark-buffer-map)
    (command . embark-symbol-map)
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

(defvar embark--abortp nil
  "Whether to abort all minibuffers after the action.")

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
    (remove-hook 'post-command-hook #'embark--cleanup)
    (when embark--abortp
      (setq embark--abortp nil)
      (abort-recursive-edit))))

(defun embark--set-target ()
  "Set the top completion candidate as target."
  (let ((old-contents (minibuffer-contents)))
    (minibuffer-force-complete nil nil t)
    (setq embark--target (minibuffer-contents))
    (delete-minibuffer-contents)
    (insert old-contents)))

(defun embark-act (arg)
  "Embark upon a minibuffer action.
With a prefix ARG, exit minibuffer after the action.
Bind this command to a key in `minibuffer-local-completion-map'."
  (interactive "P")
  (let* ((kind (embark-classify))
         (keymap (cdr (assq kind embark-keymap-alist))))
    (setq embark--old-erm enable-recursive-minibuffers
          enable-recursive-minibuffers t
          embark--abortp arg)
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
  "Describe `embark-target' as a symbol."
  (interactive)
  (describe-symbol (intern (embark-target))))


(defun embark--generate-map (keymap-alist &optional parent-map)
  "Generage keymap based on KEYMAP-ALIST if PARENT-MAP is non-nil, set it as the parent."
  (let ((map (make-sparse-keymap)))
    (mapc (lambda (keypair)
            (pcase keypair
              (`(,key . ,fn)
               (define-key map (kbd key) fn))))
          keymap-alist)
    (when parent-map
      (set-keymap-parent map parent-map))
    map))

(defvar embark-general-map
  (embark--generate-map embark-general-map-alist))

(defvar embark-file-map
  (embark--generate-map embark-file-map-alist embark-general-map))

(defvar embark-buffer-map
  (embark--generate-map embark-buffer-map-alist embark-general-map))

(defvar embark-symbol-map
  (embark--generate-map embark-symbol-map-alist embark-general-map))

(provide 'embark)
;;; embark.el ends here
