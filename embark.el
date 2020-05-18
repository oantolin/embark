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
;; chosen by minibuffer-force-complete) or on the completion candidate
;; at point in the completions buffer.  You should bind `embark-act'
;; to some key in `minibuffer-local-completion-map' and in
;; `completion-list-mode-map'.

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

;; By default embark just inserts the target of the action into the
;; next minibuffer prompt but doesn't "press RET" for you.  This is a
;; prudent default, to let you examine the minibuffer before commiting
;; to execute the action.  But if you want to live dangerously you can
;; put the `embark-ratify' property on a command for which you want
;; embark to "Automatically press RET".  For example, to have embark
;; kill buffers without confirmation use:

;; (put 'kill-buffer 'embark-ratify t)

;; Additionally you can write your own commands that do not read from
;; the minibuffer but act on the current target anyway: just use the
;; `embark-target' function (exactly once!: it "self-destructs") to
;; retrieve the current target.  See the definitions of
;; `embark-insert' or `embark-save' for examples.

;; If you wish to see a reminder of which actions are available, I
;; recommend installing which-key and using `which-key-mode' with the
;; `which-key-show-transient-maps' variable set to t.

;;; Code:

(require 'subr-x)

(defgroup embark nil
  "Emacs Mini-Buffer Actions Rooted in Keymaps"
  :group 'minibuffer)

(defcustom embark-keymap-alist
  '((general . embark-general-map)
    (file . embark-file-map)
    (buffer . embark-buffer-map)
    (command . embark-symbol-map)
    (symbol . embark-symbol-map)
    (package . embark-package-map))
  "Alist of action types and corresponding keymaps."
  :type '(alist :key-type symbol :value-type variable)
  :group 'embark)

(defcustom embark-classifiers
  '(embark-category embark-package embark-symbol)
  "List of functions to classify the current completion session.
Each function should take no arguments and return a symbol
classifying the current minibuffer completion session, or nil to
indicate it could not determine the type of completion."
  :type 'hook
  :group 'embark)

(defun embark--metadata ()
  "Return current minibuffer completion metadata."
  (completion-metadata (minibuffer-contents)
                       minibuffer-completion-table
                       minibuffer-completion-predicate))

(defun embark-category ()
  "Return minibuffer completion category per metadata."
  (completion-metadata-get (embark--metadata) 'category))

(defun embark-symbol ()
  "Determine if currently completing symbols."
  (let ((mct minibuffer-completion-table))
    (when (or (eq mct 'help--symbol-completion-table)
              (vectorp mct)
              (and (consp mct) (symbolp (car mct)))
              (completion-metadata-get (embark--metadata) 'symbolsp))
      'symbol)))

(defun embark-package ()
  "Determine if currently completing package names."
  (when (string-suffix-p "package: " (or (minibuffer-prompt) ""))
    'package))

(defun embark-classify ()
  "Classify current minibuffer completion session."
  (or (run-hook-with-args-until-success 'embark-classifiers)
      'general))

(defvar embark--target nil
  "String the next action will operate on.")

(defvar embark--old-erm nil
  "Stores value of `enable-recursive-minibuffers'.")

(defvar embark--abort nil
  "Command to abort 0, 1 or all recursive minibuffers after action.")

(defvar embark--overlay nil
  "Overlay to communicate embarking on an action to the user.")

(defun embark-target ()
  "Return the target for the current action.
Save the result somewhere if you need it more than once: calling
this function again before the next action is initiating will
return nil."
  (prog1 embark--target
    (setq embark--target nil)))

(defun embark--inject ()
  "Inject embark target into minibuffer prompt."
  (when-let ((target (embark-target)))
    (unless (eq this-command 'execute-extended-command)
      (delete-minibuffer-contents)
      (insert target)
      (when (get this-command 'embark-ratify)
        (setq unread-command-events '(13))))))

(defun embark--cleanup ()
  "Remove all hooks and modifications."
  (unless embark--target
    (setq enable-recursive-minibuffers embark--old-erm)
    (remove-hook 'minibuffer-setup-hook #'embark--inject)
    (remove-hook 'post-command-hook #'embark--cleanup)
    (delete-overlay embark--overlay)
    (setq embark--overlay nil)
    (funcall embark--abort)))

(defun embark--set-target ()
  "Set the top completion candidate as target."
  (cond
   ((minibufferp)
    (let ((completions (completion-all-sorted-completions)))
      (if (null completions)
          (minibuffer-contents)
        (setq embark--target
              (concat
               (substring (minibuffer-contents)
                          0 (or (cdr (last completions)) 0))
               (car completions))))))
   ((eq major-mode 'completion-list-mode)
    (if (not (get-text-property (point) 'mouse-face))
        (user-error "No completion here")
      ;; this fairly delicate logic is taken from `choose-completion'
      (let (beg end)
        (cond
         ((and (not (eobp)) (get-text-property (point) 'mouse-face))
          (setq end (point) beg (1+ (point))))
         ((and (not (bobp))
               (get-text-property (1- (point)) 'mouse-face))
          (setq end (1- (point)) beg (point)))
         (t (user-error "No completion here")))
        (setq beg (previous-single-property-change beg 'mouse-face))
        (setq end (or (next-single-property-change end 'mouse-face)
                      (point-max)))
        (setq embark--target (buffer-substring-no-properties beg end)))))))

(defun embark-act (arg)
  "Embark upon a minibuffer action.
With a universal prefix ARG (\\[universal-argument]), exit minibuffer after the
action.  With two prefix arguments (\\[universal-argument] \\[universal-argument]) exit all recursive
minibuffers.  Bind this command to a key in
`minibuffer-local-completion-map'."
  (interactive "P")
  (let* ((kind (embark-classify))
         (keymap (alist-get kind embark-keymap-alist)))
    (setq embark--old-erm enable-recursive-minibuffers
          enable-recursive-minibuffers t
          embark--abort (pcase arg
                          ('(4) #'abort-recursive-edit)
                          ('(16) #'top-level)
                          (_ #'ignore)))
    (embark--set-target)
    (when-let ((mini (active-minibuffer-window)))
      (setq embark--overlay
            (make-overlay (point-min)
                          (minibuffer-prompt-end)
                          (window-buffer mini)))
      (overlay-put embark--overlay 'before-string
                   (concat (propertize "Act" 'face 'highlight) " ")))
    (add-hook 'minibuffer-setup-hook #'embark--inject)
    (add-hook 'post-command-hook #'embark--cleanup)
    (set-transient-map (symbol-value keymap))))

(defun embark-keymap (keymap-alist &optional parent-map)
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

;;; custom actions

(defun embark-insert ()
  "Insert embark target into the previously selected buffer at point."
  (interactive)
  (with-selected-window (active-minibuffer-window)
    (with-selected-window (minibuffer-selected-window)
      (insert (embark-target)))))

(defun embark-save ()
  "Save embark target in the kill ring."
  (interactive)
  (kill-new (embark-target)))

(defun embark-cancel ()
  "Cancel current action."
  (interactive)
  (ignore (embark-target)))

(defun embark-eshell-in-directory ()
  "Run eshell in directory of embark target."
  (interactive)
  (let ((default-directory
          (file-name-directory
           (expand-file-name
            (embark-target)))))
    (eshell '(4))))

(defun embark-describe-symbol ()
  "Describe embark target as a symbol."
  (interactive)
  (describe-symbol (intern (embark-target))))

(defun embark-find-definition ()
  "Find definition of embark target."
  (interactive)
  (let ((symbol (intern (embark-target))))
    (cond
     ((fboundp symbol) (find-function symbol))
     ((boundp symbol) (find-variable symbol)))))

(defun embark-info-emacs-command ()
  "Go to the Info node in the Emacs manual for embark target."
  (interactive)
  (Info-goto-emacs-command-node (embark-target)))

(defun embark-info-lookup-symbol ()
  "Display the definition of embark target, from the relevant manual."
  (info-lookup-symbol (intern (embark-target)) 'emacs-lisp-mode))

(defun embark-rename-buffer ()
  "Rename embark target buffer."
  (interactive)
  (with-current-buffer (embark-target)
    (call-interactively #'rename-buffer)))

(declare-function #'package-desc-p "package")
(declare-function #'package--from-builtin "package")
(declare-function #'package-desc-extras "package")
(defvar package--builtins)
(defvar package-alist)
(defvar package-archive-contents)

(defun embark-browse-package-url ()
  "Open homepage for embark target package with `browse-url'."
  (interactive)
  (if-let ((pkg (intern (embark-target)))
           (desc (or ; found this in `describe-package-1' 
                  (if (package-desc-p pkg) pkg)
                  (car (alist-get pkg package-alist))
                  (if-let ((built-in (assq pkg package--builtins)))
                      (package--from-builtin built-in)
                    (car (alist-get pkg package-archive-contents)))))
           (url (alist-get :url (package-desc-extras desc))))
      (browse-url url)
    (message "No homepage found for `%s'" pkg)))

;;; keymaps

(defvar embark-general-map
  (embark-keymap
   '(("i" . embark-insert)
     ("w" . embark-save)
     ("C-g" . embark-cancel))))

(defvar embark-file-map
  (embark-keymap
   '(("f" . find-file)
     ("o" . find-file-other-window)
     ("d" . delete-file)
     ("r" . rename-file)
     ("c" . copy-file)
     ("!" . shell-command)
     ("&" . async-shell-command)
     ("=" . ediff-files)
     ("e" . embark-eshell-in-directory))
   embark-general-map))

(defvar embark-buffer-map
  (embark-keymap
   '(("k" . kill-buffer)
     ("b" . switch-to-buffer)
     ("o" . switch-to-buffer-other-window)
     ("q" . bury-buffer)
     ("r" . embark-rename-buffer)
     ("=" . ediff-buffers))
   embark-general-map))

(defvar embark-symbol-map
  (embark-keymap
   '(("h" . embark-describe-symbol)
     ("c" . embark-info-emacs-command)
     ("s" . embark-info-lookup-symbol)
     ("d" . embark-find-definition))
   embark-general-map))

(defvar embark-package-map
  (embark-keymap
   '(("h" . describe-package)
     ("i" . package-install)
     ("d" . package-delete)
     ("r" . package-reinstall)
     ("u" . embark-browse-package-url))
   embark-general-map))

(provide 'embark)
;;; embark.el ends here
