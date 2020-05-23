;;; embark.el --- Conveniently act on minibuffer completions   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Omar Antolín Camarena

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: convenience
;; Version: 0.2
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

;; This package provides a pair of commands, `embark-act' and
;; `embark-exit-and-act', to execute actions on the top minibuffer
;; completion canidate: the one that would be chosen by
;; minibuffer-force-complete. Additionally `embark-act' can act on the
;; completion candidate at point in the completions buffer. You should
;; bind both of them in `minibuffer-local-completion-map' and also
;; bind `embark-act' in `completion-list-mode-map'.

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

;; By default, for most commands `embark' inserts the target of the
;; action into the next minibuffer prompt and "presses RET" for you,
;; accepting the target as is.  You can add commands for which you
;; want the chance to edit the target before acting upon it to the
;; list `embark-allow-edit-commands'.

;; If you want the default to be to allowing editing the target for
;; all commands, set `embark-allow-edit-default' to t and list
;; exceptions in `embark-skip-edit-commands'.

;; If you want to customize what happens after the target is inserted
;; at the minibuffer prompt of an action, you can use the global
;; `embark-setup-hook' or override it in the `embark-setup-overrides'
;; alist.  See the default value of `embark-setup-overrides' for an
;; example.

;; You can also write your own commands that do not read from the
;; minibuffer but act on the current target anyway: just use the
;; `embark-target' function (exactly once!: it "self-destructs") to
;; retrieve the current target.  See the definitions of
;; `embark-insert' or `embark-save' for examples.

;; If you wish to see a reminder of which actions are available, I
;; recommend installing which-key and using `which-key-mode' with the
;; `which-key-show-transient-maps' variable set to t.

;;; Code:

(eval-when-compile (require 'subr-x))

;;; user facing options

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
  '(embark-cached-type
    embark-category
    embark-package
    embark-symbol)
  "List of functions to classify the current completion session.
Each function should take no arguments and return a symbol
classifying the current minibuffer completion session, or nil to
indicate it could not determine the type of completion."
  :type 'hook
  :group 'embark)

(defcustom embark-target-finders
  '(embark-top-minibuffer-completion
    embark-completion-at-point)
  "List of functions to pick the target for actions.
Each function should take no arguments and return either a target
string or nil (to indicate it found no target)."
  :type 'hook
  :group 'embark)

(defcustom embark-indicator (propertize "Act" 'face 'highlight)
  "Echo area indicator the user is embarking upon an action."
  :type 'string
  :group 'embark)

(defcustom embark-setup-hook nil
  "Hook to run after injecting target into minibuffer.
It can be overriden by the `embark-setup-overrides' alist."
  :type 'hook
  :group 'embark)

(defcustom embark-setup-overrides
  '((async-shell-command embark--shell-prep)
    (shell-command embark--shell-prep)
    (eval-expression embark--eval-prep))
  "Alist associating commands with post-injection setup hooks.
For commands appearing as keys in this alist, run the
corresponding value as a setup hook (instead of
`embark-setup-hook') after injecting the target into in the
minibuffer and before acting on it."
  :type '(alist :key-type function :value-type hook)
  :group 'embark)

(defcustom embark-allow-edit-default nil
  "Is the user allowed to edit the target before acting on it?
This variable sets the default policy, and can be overidden.
When this variable is nil, it is overridden by
`embark-allow-edit-commands'; when it is t, it is overidden by
`embark-skip-edit-commands'."
  :type 'boolean
  :group 'embark)

(defcustom embark-allow-edit-commands
  '(delete-file
    delete-directory
    kill-buffer
    shell-command
    async-shell-command
    embark-kill-buffer-and-window
    eval-expression)
  "Allowing editing of target prior to acting for these commands.
This list is used only when `embark-allow-edit-default' is nil."
  :type 'hook
  :group 'embark)

(defcustom embark-skip-edit-commands nil
  "Skip editing of target prior to acting for these commands.
This list is used only when `embark-allow-edit-default' is t."
  :type 'hook
  :group 'embark)

(defcustom embark-occur-bind-actions nil
  "Bind actions without a prefix in the Embark Occur buffer.
When this variable is non-nil, Embark Occur buffers will have the
usual actions for that type of candidate bound directly, without
needing to run `embark-act' to use them."
  :type 'boolean
  :group 'embark)

(defcustom embark-pre-action-hook nil
  "Hook run right before an action is embarked upon."
  :type 'hook
  :group 'embark)

;;; stashing the type of completions for a *Completions* buffer

(defvar embark--type nil
  "Cache for the completion type, meant to be set buffer-locally.
Always keep the non-local value equal to nil.")

(defvar embark--previous-buffer nil
  "Cache for the previous buffer, meant to be set buffer-locally.
Always keep the non-local value equal to nil.")

(defvar embark--command nil
  "Command that started the completion session.")

(defun embark--record-command ()
  "Record the command that opened the minibuffer."
  (setq-local embark--command this-command))

(add-hook 'minibuffer-setup-hook #'embark--record-command)

(defun embark-cached-type ()
  "Return buffer local cached completion type if available."
  embark--type)

(defun embark--completions-info (&optional _start _end)
  "Cache the completion type when popping up the completions buffer."
  (let ((type (embark-classify))
        (cmd embark--command))
    (with-current-buffer "*Completions*"
      (setq-local embark--command cmd)
      (setq-local embark--type type)
      (setq-local embark--previous-buffer
                  (if (minibufferp completion-reference-buffer)
                      (with-current-buffer completion-reference-buffer
                        (window-buffer (minibuffer-selected-window)))
                    completion-reference-buffer)))))

(advice-add 'minibuffer-completion-help :after #'embark--completions-info)

;;; better guess for default-directory in *Completions* buffers

(defun embark-completions-default-directory ()
  "Guess a reasonable default directory for the completions buffer.
Meant to be added to `completion-setup-hook'."
  (when (and minibuffer-completing-file-name
             (minibufferp))
    (let ((dir (file-name-directory
                (expand-file-name
                 (buffer-substring (minibuffer-prompt-end) (point))))))
      (with-current-buffer standard-output
        (setq-local default-directory dir)))))

(add-hook 'completion-setup-hook #'embark-completions-default-directory t)

;;; core functionality

(defvar embark--target nil "String the next action will operate on.")
(defvar embark--keymap nil "Keymap to activate for next action.")

(defvar embark--old-erm nil "Stores value of `enable-recursive-minibuffers'.")
(defvar embark--overlay nil
  "Overlay to communicate embarking on an action to the user.")

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
              (completion-metadata-get (embark--metadata) 'symbolsp)
              ;; before Emacs 27, M-x does not have command category
              (string-match-p "M-x" (or (minibuffer-prompt) "")))
      'symbol)))

(defun embark-package ()
  "Determine if currently completing package names."
  (when (string-suffix-p "package: " (or (minibuffer-prompt) ""))
    'package))

(defun embark-classify ()
  "Classify current minibuffer completion session."
  (or (run-hook-with-args-until-success 'embark-classifiers)
      'general))

(defun embark-target ()
  "Return the target for the current action.
Save the result somewhere if you need it more than once: calling
this function again before the next action is initiating will
return nil."
  (prog1 embark--target
    (setq embark--target nil)))

(defun embark--inject ()
  "Inject embark target into minibuffer prompt."
  (when (or (not (string-match-p "M-x" (minibuffer-prompt)))
            (eq real-this-command 'embark-default-action)
            (eq real-this-command 'embark-action<embark-default-action>))
    (when-let ((target (embark-target)))
      (delete-minibuffer-contents)
      (insert target)
      (let ((embark-setup-hook
             (or (alist-get this-command embark-setup-overrides)
                 embark-setup-hook)))
        (run-hooks 'embark-setup-hook)
        (when (if embark-allow-edit-default
                  (memq this-command embark-skip-edit-commands)
                (not (memq this-command embark-allow-edit-commands)))
          (setq unread-command-events '(13)))))))

(defun embark--cleanup ()
  "Remove all hooks and modifications."
  (unless embark--target
    (setq enable-recursive-minibuffers embark--old-erm)
    (remove-hook 'minibuffer-setup-hook #'embark--inject)
    (remove-hook 'post-command-hook #'embark--cleanup)
    (when embark--overlay
      (delete-overlay embark--overlay)
      (setq embark--overlay nil))))

(defun embark-top-minibuffer-completion ()
  "Return the top completion candidate in the minibuffer."
  (when (minibufferp)
    (let ((contents (minibuffer-contents)))
      (if (test-completion contents
                           minibuffer-completion-table
                           minibuffer-completion-predicate)
          contents
        (let ((completions (completion-all-sorted-completions)))
          (if (null completions)
              contents
            (concat
             (substring contents 0 (or (cdr (last completions)) 0))
             (car completions))))))))

(defun embark-completion-at-point ()
  "Return the completion candidate at point in a completions buffer."
  (when (eq major-mode 'completion-list-mode)
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
        (let ((raw (buffer-substring-no-properties beg end)))
          (if (eq embark--type 'file)
              (abbreviate-file-name (expand-file-name raw))
            raw))))))

(defun embark--keymap-for-type (type)
  "Return the keymap for the given completion type."
  (symbol-value (alist-get type embark-keymap-alist)))

(defun embark--setup ()
  "Setup for next action."
  (setq embark--keymap (embark--keymap-for-type (embark-classify)))
  (setq embark--target
        (run-hook-with-args-until-success 'embark-target-finders))
  (when (minibufferp)
    (setq embark--previous-buffer (window-buffer (minibuffer-selected-window))))
  (setq embark--old-erm enable-recursive-minibuffers)
  (add-hook 'minibuffer-setup-hook #'embark--inject)
  (add-hook 'post-command-hook #'embark--cleanup))

(defun embark--prefix-argument-p ()
  "Is this command a prefix argument setter?
This is used to keep the transient keymap active."
  (memq this-command
        '(universal-argument
          universal-argument-more
          digit-argument
          negative-argument)))

(defun embark--start ()
  "Start an action: show indicator and setup keymap."
  (let ((mini (active-minibuffer-window)))
    (if (not mini)
        (message "%s on '%s'" embark-indicator embark--target)
      (setq embark--overlay
            (make-overlay (point-min)
                          (minibuffer-prompt-end)
                          (window-buffer mini)))
      (overlay-put embark--overlay 'before-string
                   (concat embark-indicator " "))))
  (set-transient-map embark--keymap #'embark--prefix-argument-p
                     (lambda ()
                       (setq embark--keymap nil)
                       (run-hooks 'embark-pre-action-hook))))

(defun embark-act ()
  "Embark upon a minibuffer action.
Bind this command to a key in `minibuffer-local-completion-map'."
  (interactive)
  (embark--setup)
  (setq enable-recursive-minibuffers t)
  (embark--start))

(defun embark-exit-and-act ()
  "Exit the minibuffer and embark upon an action."
  (interactive)
  (embark--setup)                       ; setup now
  (run-at-time 0 nil #'embark--start)   ; start action later
  (top-level))

(defun embark-keymap (binding-alist &optional parent-map)
  "Return keymap with bindings given by BINDING-ALIST.
If PARENT-MAP is non-nil, set it as the parent keymap."
  (let ((map (make-sparse-keymap)))
    (dolist (key-fn binding-alist)
      (pcase-let ((`(,key . ,fn) key-fn))
        (when (stringp key) (setq key (kbd key)))
        (define-key map key fn)))
    (when parent-map
      (set-keymap-parent map parent-map))
    map))

(defun embark--action-command (action)
  "Turn an action into a command that performs the action."
  (let ((name (intern (format "embark-action<%s>" action)))
        (fn (lambda ()
              (interactive)
              (setq this-command action)
              (embark--setup)
              (call-interactively action))))
    (fset name fn)
    (when (symbolp action)
      (put name 'function-documentation
           (documentation action)))
    name))

(defun embark-occur ()
  "Create a buffer with current candidates for further action."
  (interactive)
  (ignore (embark-target)) ; allow use from embark-act
  (when (minibufferp) (switch-to-completions))
  (when (derived-mode-p 'completion-list-mode)
    (let ((occur-buffer (current-buffer)))
      (rename-buffer "*Embark Occur*" t)
      (when embark-occur-bind-actions
        (let ((occur-map (keymap-canonicalize
                          (embark--keymap-for-type embark--type))))
          (dolist (binding (cdr occur-map))
            (setcdr binding (embark--action-command (cdr binding))))
          (push (cons t occur-map) minor-mode-overriding-map-alist)))
      (run-at-time 0 nil (lambda () (pop-to-buffer occur-buffer)))
      (top-level))))

;;; custom actions

(defun embark-insert ()
  "Insert embark target at point into the previously selected buffer."
  (interactive)
  (with-current-buffer embark--previous-buffer
    (insert (substring-no-properties (embark-target)))
    (setq embark--previous-buffer nil)))

(defun embark-save ()
  "Save embark target in the kill ring."
  (interactive)
  (kill-new (substring-no-properties (embark-target))))

(defun embark-cancel ()
  "Cancel current action."
  (interactive)
  (ignore (embark-target)))

(defun embark-undefined ()
  "Cancel action and show an error message."
  (interactive)
  (ignore (embark-target))
  (embark--cleanup) ; remove overlay immediately
  (minibuffer-message "Unknown embark action"))

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

(declare-function package-desc-p "package")
(declare-function package--from-builtin "package")
(declare-function package-desc-extras "package")
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

(defun embark-insert-relative-path ()
  "Insert relative path to embark target.
The insert path is relative to the previously selected buffer's
`default-directory'."
  (interactive)
  (with-current-buffer embark--previous-buffer
    (insert (file-relative-name (embark-target)))
    (setq embark--previous-buffer nil)))

(defun embark-save-relative-path ()
  "Save the relative path to embark target to kill ring.
The insert path is relative to the previously selected buffer's
`default-directory'."
  (interactive)
  (kill-new (file-relative-name (embark-target))))

(defun embark-shell-command-on-buffer (buffer command &optional replace)
  "Run shell COMMAND on contents of BUFFER.
Called with \\[universal-argument], replace contents of buffer
with command output."
  (interactive
   (list
    (read-buffer "Buffer: ")
    (read-shell-command "Shell command: ")
    current-prefix-arg))
  (with-current-buffer buffer
    (shell-command-on-region (point-min) (point-max) command replace)))

(defun embark-bury-buffer ()
  "Bury embark target buffer."
  (interactive)
  (if-let ((buf (embark-target))
           (win (get-buffer-window buf)))
      (with-selected-window win
        (bury-buffer))
    (bury-buffer)))

(defun embark-kill-buffer-and-window ()
  "Kill embark target buffer and delete its window."
  (interactive)
  (when-let ((win (get-buffer-window (embark-target))))
    (with-selected-window win
      (kill-buffer-and-window))))

(defun embark-default-action ()
  "Default action.
This is whatever command opened the minibuffer in the first place."
  (interactive)
  (setq this-command embark--command)   ; so the proper hooks apply
  (call-interactively embark--command))

;;; setup hooks for actions

(defun embark--shell-prep ()
  "Prepare target for use as argument for a shell command.
This quotes the spaces, inserts an extra space at the beginning
and leaves the point to the left of it."
  (let ((contents (minibuffer-contents)))
    (delete-minibuffer-contents)
    (insert " " (replace-regexp-in-string "\\s-" "\\\\\\&" contents))
    (beginning-of-line)))

(defun embark--eval-prep ()
  "If target is: a variable, skip edit; a function, wrap in parens."
  (if (not (fboundp (intern (minibuffer-contents))))
      (setq unread-command-events '(13))
    (beginning-of-line)
    (insert "(")
    (end-of-line)
    (insert ")")
    (backward-char)))

;;; keymaps

(defvar embark-general-map
  (embark-keymap
   '(("i" . embark-insert)
     ("w" . embark-save)
     ("C-g" . embark-cancel)
     ("RET" . embark-default-action)
     ([remap self-insert-command] . embark-undefined)
     ("C-u" . universal-argument))
   universal-argument-map))

(defvar embark-file-map
  (embark-keymap
   '(("f" . find-file)
     ("o" . find-file-other-window)
     ("d" . delete-file)
     ("D" . delete-directory)
     ("r" . rename-file)
     ("c" . copy-file)
     ("!" . shell-command)
     ("&" . async-shell-command)
     ("=" . ediff-files)
     ("e" . embark-eshell-in-directory)
     ("+" . make-directory)
     ("I" . embark-insert-relative-path)
     ("W" . embark-save-relative-path))
   embark-general-map))

(defvar embark-buffer-map
  (embark-keymap
   '(("k" . kill-buffer)
     ("b" . switch-to-buffer)
     ("o" . switch-to-buffer-other-window)
     ("z" . embark-bury-buffer)
     ("q" . embark-kill-buffer-and-window)
     ("r" . embark-rename-buffer)
     ("=" . ediff-buffers)
     ("|" . embark-shell-command-on-buffer))
   embark-general-map))

(defvar embark-symbol-map
  (embark-keymap
   '(("h" . embark-describe-symbol)
     ("c" . embark-info-emacs-command)
     ("s" . embark-info-lookup-symbol)
     ("d" . embark-find-definition)
     ("e" . eval-expression))
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
