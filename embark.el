;;; embark.el --- Conveniently act on minibuffer completions   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Omar Antolín Camarena

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: convenience
;; Version: 0.8
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

;; This package provides a sort of right-click contextual menu for Emacs,
;; accessed through the `embark-act' command (which you should bind to a
;; convenient key), offering you relevant /actions/ to use on a /target/
;; determined by the context:

;; - In the minibuffer, the target is the current best completion
;;  candidate.
;; - In the `*Completions*' buffer the target is the completion at point.
;; - In a regular buffer, the target is the region if active, or else the
;;  file, symbol or url at point.

;; The type of actions offered depend on the type of the target:

;; - For files you get offered actions like deleting, copying,
;;  renaming, visiting in another window, running a shell command on the
;;  file, etc.
;; - For buffers the actions include switching to or killing the buffer.
;; - For package names the actions include installing, removing or
;;  visiting the homepage.

;; Everything is easily configurable: determining the current target,
;; classifying it, and deciding with actions are offered for each type
;; in the classification. The above introduction just mentions part of
;; the default configuration.

;; Configuring which act are offered for a type is particularly easy and
;; requires no programming: the variable `embark-keymap-alist' associates
;; target types with variable containing keymaps, and those keymaps
;; containing binds for the actions. For example, in the default
;; configuration the type `file' is associated with the symbol
;; `embark-file-keymap'. That symbol names a keymap with single-letter
;; keybindings for common Emacs file commands, for instance `c' is bound
;; to `copy-file'. This means that if while you are in the minibuffer
;; after running a command that prompts for a file, such as `find-file' or
;; `rename-file', you can copy a file by running `embark-act' and then
;; pressing `c'.

;; These action keymaps are very convenient but not strictly necessary
;; when using `embark-act': you can use any command that reads from the
;; minibuffer as an action and the target of the action will be inserted
;; at the first minibuffer prompt. After running `embark-act' all of your
;; keybindings and even `execute-extended-command' can be used to run a
;; command.

;; The actions in `embark-general-map' are available no matter what
;; type of completion you are in the middle of. By default this
;; includes bindings to save the current candidate in the kill ring
;; and to insert the current candidate in the previously selected
;; buffer (the buffer that was current when you executed a command
;; that opened up the minibuffer).

;; You can read about the Embark GitHub project wiki:
;; https://github.com/oantolin/embark/wiki/Default-Actions

;; Besides acting individually on targets, Embark lets you work
;; collectively on a set of target /candidates/. For example, while you are
;; in the minibuffer the candidates are simply the possible completions
;; of your input. Embark provides two commands to work on candidate sets:

;; - The `embark-occur' command produces a buffer listing all candidates,
;;  for you to peruse and run actions on at your leisure. The
;;  candidates can be viewed in a grid or as a list showing additional
;;  annotations. The `embark-live-occur' variant produces "live" Embark
;;  Occur buffer, meaning they autoupdate as the set of candidates
;;  changes.

;; - The `embark-export' command tries to open a buffer in an appropriate
;;  major mode for the set of candidates. If the candidates are files
;;  export produces a Dired buffer; if they are buffers, you get an
;;  Ibuffer buffer; and if they are packages you get a buffer in
;;  package menu mode.

;; These are always available as "actions" (although they do not act
;; on just the current target but on all candidates) for embark-act and
;; are bound to O, L and E, respectively, in embark-general-map. This
;; means that you do not have to bind your own key bindings for these
;; (although you can, of course), just a key binding for `embark-act'
;; or `embark-act-noexit'.


;;; Code:

(eval-when-compile (require 'subr-x))

(require 'ffap) ; used it to recognize file and url targets

;;; user facing options

(defgroup embark nil
  "Emacs Mini-Buffer Actions Rooted in Keymaps"
  :group 'minibuffer)

(defcustom embark-keymap-alist
  '((file . embark-file-map)
    (url . embark-url-map)
    (buffer . embark-buffer-map)
    (command . embark-symbol-map)
    (unicode-name . embark-unicode-name-map)
    (symbol . embark-symbol-map)
    (package . embark-package-map)
    (bookmark . embark-bookmark-map)
    (region . embark-region-map))
  "Alist of action types and corresponding keymaps.
For any type not listed here, `embark-act' will use
`embark-general-map'."
  :type '(alist :key-type symbol :value-type variable)
  :group 'embark)

(defvar embark-overriding-keymap nil
  "Can be bound to short circuit `embark-keymap-alist'.
Embark will set the parent of this map to `embark-general-map'.")

(defcustom embark-classifiers
  '(embark-category-type
    embark-dired-type
    embark-ibuffer-type)
  "List of functions to classify current buffer context.
Each function should take no arguments and return the type
symbol, or nil to indicate it could not determine the type in
current context.  If the type is not determined by current buffer
context fallback to `embark-target-classifiers'."
  :type 'hook
  :group 'embark)

(defcustom embark-target-classifiers
  '(embark-url-target-type
    embark-file-target-type
    embark-symbol-target-type
    embark-buffer-target-type)
  "List of functions to classify current target.
Each function takes the target as argument and returns the type
symbol, or nil to indicate it could not determine the type of
current target.  If the type isn't determined by current target
fallback to the `general' type."
  :type 'hook
  :group 'embark)

(defcustom embark-target-finders
  '(embark-top-minibuffer-completion
    embark-button-label
    embark-completion-at-point
    ffap-url-at-point
    ffap-file-at-point
    embark-symbol-at-point)
  "List of functions to determine the target in current context.
Each function should take no arguments and return either a target
string or nil (to indicate it found no target).  If the region is
active the region content is used as current target."
  :type 'hook
  :group 'embark)

(defcustom embark-become-keymaps
  '(embark-become-help-map
    embark-become-file+buffer-map
    embark-become-shell-command-map
    embark-become-match-map)
  "List of keymaps for `embark-become'.
Each keymap groups a set of related commands that can
conveniently become one another."
  :type '(repeat variable)
  :group 'embark)

(defcustom embark-input-getters '(embark-minibuffer-input)
  "List of functions to get current input for `embark-become'.
Each function should take no arguments and return either the
current input string or nil (to indicate it is not applicable)."
  :type 'hook
  :group 'embark)

(defcustom embark-prompter 'embark-keymap-prompter
  "Function used to prompt the user for actions.

This should be set to a function that prompts the use for an
action and returns the symbol naming the action command.  The
default value, `embark-keymap-prompter' activates the type
specific action keymap given in `embark-keymap-alist'.
There is also `embark-completing-read-prompter' which
prompts for an action with completion."
  :type '(choice (const :tag "Use action keymaps" embark-keymap-prompter)
                 (const :tag "Read action with completion"
                        embark-completing-read-prompter)
                 (function :tag "Other"))
  :group 'embark)

(defcustom embark-action-indicator (propertize "Act" 'face 'highlight)
  "Indicator to use when embarking upon an action.

If set to a string prepend it to the minibuffer prompt or to the
message in the echo area when outside of the minibuffer.  When set
to a function it is called with no arguments to indicate the
pending action itself.  For nil no indication is shown."
  :type '(choice function string nil)
  :group 'embark)

(defcustom embark-become-indicator (propertize "Become" 'face 'highlight)
  "Indicator to use when using `embark-become'.

If set to a string prepend it to the minibuffer prompt or to the
message in the echo area when outside of the minibuffer.  When set
to a function it is called with no arguments to indicate the
pending action itself.  For nil no indication is shown."
  :type '(choice function string nil)
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

(defcustom embark-pre-action-hook nil
  "Hook run right before an action is embarked upon."
  :type 'hook
  :group 'embark)

(defcustom embark-post-action-hook nil
  "Hook run after an embarked upon action concludes."
  :type 'hook
  :group 'embark)

(defcustom embark-candidate-collectors
  '(embark-minibuffer-candidates
    embark-completions-buffer-candidates
    embark-dired-candidates
    embark-ibuffer-candidates
    embark-embark-occur-candidates)
  "List of functions that collect all candidates in a given context.
These are used to fill an Embark Occur buffer."
  :type 'hook
  :group 'embark)

(defcustom embark-occur-initial-view-alist
  '((file . grid)
    (buffer . grid)
    (symbol . list)
    (line . list)
    (kill-ring . zebra)
    (t . list))
  "Initial views for Embark Occur buffers by type.
This is an alist associating completion types to either `list',
`grid' or `zebra' (which means list view together with
`embark-occur-zebra-minor-mode').  Additionally you can associate
t to a default initial view for types not mentioned separately."
  :type '(alist :key-type symbol
                :value-type (choice (const :tag "List view" list)
                                    (const :tag "Grid view" grid)))
  :group 'embark)

(defcustom embark-exporters-alist
  '((buffer . embark-export-ibuffer)
    (file . embark-export-dired)
    (package . embark-export-list-packages)
    (t . embark-occur))
  "Alist associating completion types to export functions.
Each function should take a list of strings which are candidates
for actions and make a buffer appropriate to manage them.  For
example, the default is to make a dired buffer for files, and an
ibuffer for buffers.

The key t is also allowed in the alist, and the corresponding
value indicates the default function to use for other types.  The
default is `embark-occur'."
  :type '(alist :key-type symbol :value-type function)
  :group 'embark)

(defvar embark-overriding-export-function nil
  "Can be bound to short circuit `embark-exporters-alist'.
The expected format is the same as for functions in
`embark-exporters-alist'.")

(defcustom embark-live-occur-update-delay 0.15
  "Wait this long for more input before updating Embark Live Occur buffer."
  :type 'number
  :group 'embark)

(defcustom embark-live-occur-initial-delay 0.3
  "Wait this long for input before popping up Embark Live Occur buffer."
  :type 'number
  :group 'embark)

(defcustom embark-occur-minibuffer-completion nil
  "Should RET on an Embark Occur entry do minibuffer completion?
By default, pressing RET or clicking the mouse on an entry in an
Embark Occur buffer runs the default action on the entry.  If this
variable is non-nil, then when the Embark Occur buffer is
associated to the active minibuffer and is live updating,
pressing RET or clicking the mouse instead completes the
minibuffer input to the chosen entry and, unless this leads to
new completion candidates (for example, when entering a directory
in `find-file'), exits the minibuffer.

If you are using `embark-completing-read' as your
`completing-read-function' you might want to set
`embark-occur-minibuffer-completion' to t."
  :type 'boolean
  :group 'embark)

;;; stashing information for actions in buffer local variables

(defvar-local embark--type nil
  "Cache for the completion type, meant to be set buffer-locally.")

(defvar-local embark--target-window nil
  "Cache for the previous window, meant to be set buffer-locally.")

(defvar-local embark--command nil
  "Command that started the completion session.")

(defun embark--record-command ()
  "Record the command that opened the minibuffer."
  (setq embark--command this-command))

(add-hook 'minibuffer-setup-hook #'embark--record-command)

(defun embark--default-directory ()
  "Guess a reasonable default directory for the current candidates."
  (if (and (minibufferp) minibuffer-completing-file-name)
      (file-name-directory
       (expand-file-name
        (buffer-substring (minibuffer-prompt-end) (point))))
    default-directory))

(defun embark--target-window ()
  "Get target buffer for insert actions."
  (cond
   (embark--target-window embark--target-window)
   ((minibufferp) (minibuffer-selected-window))
   ((derived-mode-p 'completion-list-mode)
    (if (minibufferp completion-reference-buffer)
        (with-current-buffer completion-reference-buffer
          (minibuffer-selected-window))
      (get-buffer-window completion-reference-buffer)))
   (t (selected-window))))

(defun embark--cache-info (&optional buffer)
  "Cache information needed for actions in variables local to BUFFER."
  (let ((type (embark-classify))
        (cmd embark--command)
        (dir (embark--default-directory))
        (target-window (embark--target-window)))
    (with-current-buffer (or buffer standard-output)
      (setq embark--command cmd)
      (setq embark--type type)
      (setq-local default-directory dir)
      (setq embark--target-window target-window))))

(add-hook 'completion-setup-hook #'embark--cache-info t)

;;; internal variables

(defvar embark--target-region nil
  "Should the active region's contents be the embark target?")

(defvar-local embark-occur-candidates nil
  "List of candidates in current occur buffer.")

(defvar-local embark-occur-view 'list
  "Type of view in occur buffer: `list' or `grid'.")

(defvar-local embark-occur-from nil
  "The buffer `embark-occur' was called from.")

(defvar-local embark-occur-linked-buffer nil
  "Buffer local variable indicating which Embark Buffer to update.")

(defvar-local embark-occur-annotator nil
  "Annotation function of minibuffer session for this occur.")

(defvar-local embark--live-occur--timer nil
  "Timer scheduled to update Embark Live Occur buffer.")

;;; core functionality

(defun embark--metadata ()
  "Return current minibuffer completion metadata."
  (completion-metadata
   (buffer-substring-no-properties (field-beginning) (point))
   minibuffer-completion-table
   minibuffer-completion-predicate))

(defun embark-category-type ()
  "Return minibuffer completion category metadatum."
  (completion-metadata-get (embark--metadata) 'category))

(defun embark-dired-type ()
  "Report that dired buffers yield files."
  (when (derived-mode-p 'dired-mode) 'file))

(defun embark-ibuffer-type ()
  "Report that ibuffer buffers yield buffers."
  (when (derived-mode-p 'ibuffer-mode) 'buffer))

(defun embark-target-type ()
  "Report type determined by target."
  (when-let ((target
              (run-hook-with-args-until-success 'embark-target-finders)))
    (unless (string= target "") ; gets classified as a file otherwise
      (run-hook-with-args-until-success 'embark-target-classifiers target))))

(defun embark-active-region-type ()
  "Report type of active region target."
  (when-let ((target
              (when (use-region-p)
                (buffer-substring (region-beginning) (region-end)))))
    (or (run-hook-with-args-until-success 'embark-target-classifiers target)
        'general)))

(defun embark-file-target-type (cand)
  "Report file type if CAND is a file."
  (when (file-exists-p cand)
    'file))

(defun embark-url-target-type (cand)
  "Report url type if CAND is a URL."
  (when (eql (string-match-p ffap-url-regexp cand) 0)
    'url))

(defun embark-symbol-target-type (cand)
  "Report symbol type if CAND is a known symbol."
  (when-let ((sym (intern-soft cand)))
    (when (or (boundp sym)
              (fboundp sym)
              (facep sym))
    'symbol)))

(defun embark-buffer-target-type (cand)
  "Report buffer type if CAND is a buffer name."
  (when (get-buffer cand)
    'buffer))

(defun embark-classify ()
  "Classify current context."
  (or (if (use-region-p)
          (if embark--target-region
              (embark-active-region-type)
            'region))
      embark--type ; cached?
      (run-hook-with-args-until-success 'embark-classifiers)
      (embark-target-type)
      'general))

(defun embark-minibuffer-input ()
  "Return the current input string in the minibuffer.
This returns only the portion of the minibuffer contents within
the completion boundaries."
  (when (minibufferp)
    (pcase-let ((`(,beg . ,end) (embark--boundaries)))
      (substring (minibuffer-contents) beg
                 (+ end (- (point) (minibuffer-prompt-end)))))))

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

(defun embark-button-label ()
  "Return the label of the button at point."
  (when-let ((button (button-at (point)))
             (label (button-label button)))
    (if (eq embark--type 'file)
        (abbreviate-file-name (expand-file-name label))
      label)))

(defun embark-completion-at-point (&optional relative)
  "Return the completion candidate at point in a completions buffer.
If the completions are file names and RELATIVE is non-nil, return
relative path."
  (when (derived-mode-p 'completion-list-mode)
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
          (if (and (eq embark--type 'file) (not relative))
              (abbreviate-file-name (expand-file-name raw))
            raw))))))

(defun embark-symbol-at-point ()
  "Return name of symbol at point."
  (when-let ((symbol (symbol-at-point))
             (name (symbol-name symbol)))
    (if (and (derived-mode-p 'org-mode)
             (string-match-p "^\\([~=]\\).*\\1$" name))
        (substring name 1 -1)
      name)))

(defvar embark-general-map)             ; forward declaration

(defun embark--action-keymap ()
  "Return action keymap for current target."
  (make-composed-keymap
   (or embark-overriding-keymap
       (symbol-value (alist-get (embark-classify) embark-keymap-alist)))
   embark-general-map))

(defun embark--target ()
  "Return target for action."
  (if (use-region-p)
      (when embark--target-region
        (buffer-substring (region-beginning) (region-end)))
    (run-hook-with-args-until-success 'embark-target-finders)))

(defvar embark--keep-alive-list
  '(universal-argument
    universal-argument-more
    digit-argument
    negative-argument
    scroll-other-window
    scroll-other-window-down)
  "List of commands that can run without canceling the action keymap.")

(defmacro embark-after-exit (vars &rest body)
  "Run BODY after exiting all minibuffers.
Make sure the current values of VARS are still valid when running
BODY."
  (declare (indent defun))
  (let ((binds (cl-loop for var in vars collect
                        (list var (make-symbol (symbol-name var))))))
    `(progn
       (run-at-time 0 nil
                    (lambda ,(mapcar #'cadr binds)
                      (setq inhibit-message nil)
                      (let (,@binds)
                        ,@body))
                    ,@vars)
       (setq-local inhibit-message t)
       (top-level))))

(defun embark--show-indicator (indicator)
  "Show INDICATOR for a pending action or a instance of becoming.
If INDICATOR is a string, it is put in an overlay in the
minibuffer; the overlay is returned so it can be deleted when the
indicator is no longer needed.  If it is a function, this
function is called.  The function should return either nil, an
overlay to be deleted later, or a function to be called when the
indicator is no longer needed."
  (cond
   ((stringp indicator)
    (let ((mini (active-minibuffer-window)))
      (if (or (use-region-p) (not mini))
          (let (minibuffer-message-timeout)
            (minibuffer-message "%s on %s"
                                indicator
                                (if-let ((target (embark--target)))
                                    (format "'%s'" target)
                                  "region")))
        (let ((indicator-overlay
               (make-overlay (point-min) (point-min)
                             (window-buffer mini) t t)))
          (overlay-put indicator-overlay 'before-string
                       (concat indicator " "))
          indicator-overlay))))
   ((functionp indicator)
    (funcall indicator))))

(defun embark-keymap-prompter (keymap)
  "Let the user choose an action using the bindings in KEYMAP.
Besides the bindings in KEYMAP, the user is free to use all their
keybindings and even \\[execute-extended-command] to select a command."
  (let ((cmd (let* ((overriding-terminal-local-map keymap)
                    (key (read-key-sequence nil)))
               (key-binding key))))
    (when (eq cmd 'execute-extended-command)
      (setq cmd (intern-soft (read-extended-command))))
    (when (eq cmd 'embark-keymap-help)
      (setq cmd (embark-completing-read-prompter keymap)))
    cmd))

(defun embark-completing-read-prompter (keymap)
  "Prompt via completion for a command bound in KEYMAP."
  (let* ((commands
          (cl-loop
           for (key . cmd) in (cdr (keymap-canonicalize keymap))
           unless (embark--omit-binding-p cmd)
           collect (let ((desc (if (numberp key)
                                       (single-key-description key)
                                     (key-description key)))
                         (name (symbol-name cmd)))
                     (propertize name
                                 'display
                                 (concat (propertize desc 'face 'success)
                                         (propertize " → " 'face 'shadow)
                                         name))))))
    (intern-soft
     (completing-read
      "Command: "
      (lambda (s p a)
        (if (eq a 'metadata)
            `(metadata (metadata . command))
          (complete-with-action a commands s p)))
      nil t))))

(defun embark--with-indicator (indicator prompter &rest args)
  "Display INDICATOR while calling PROMPTER with ARGS."
  (let ((indicator (embark--show-indicator indicator))
        (cmd (condition-case nil
                 (minibuffer-with-setup-hook
                     ;; if the prompter opens its own minibuffer, show
                     ;; the indicator there too
                     (lambda () (embark--show-indicator indicator))
                   (apply prompter args))
               (quit nil))))
    (cond
     ((overlayp indicator) (delete-overlay indicator))
     ((functionp indicator) (funcall indicator)))
    cmd))

(defun embark--action ()
  "Prompt the user for an action and return a function that carries it out.

This uses `embark-prompter' to ask the user to specify an action
and returns a function that executes the chosen command, in the
correct target window, injecting the target at the first
minibuffer prompt.  If the user cancels the action selection,
return nil instead of a function."
  (let* ((keymap (make-composed-keymap (embark--action-keymap)
                                       embark-general-map))
         (action (embark--with-indicator embark-action-indicator
                                         embark-prompter
                                         keymap)))
    (if (null action)
        (progn (minibuffer-message "Canceled") nil)
      (let* ((target (embark--target))
             (command embark--command)
             (action-window (if (memq action '(embark-become
                                               embark-live-occur
                                               embark-occur
                                               embark-export))
                                (selected-window)
                              (embark--target-window)))
             (setup-hook (or (alist-get action embark-setup-overrides)
                             embark-setup-hook))
             (allow-edit (if embark-allow-edit-default
                             (not (memq action embark-skip-edit-commands))
                           (memq action embark-allow-edit-commands)))
             (inject (if (null target) ; for region actions target is nil
                         #'ignore
                       (lambda ()
                         (delete-minibuffer-contents)
                         (insert target)
                         (let ((embark-setup-hook setup-hook))
                           (run-hooks 'embark-setup-hook))
                         (unless allow-edit
                           (run-at-time 0 nil #'exit-minibuffer))))))
        (lambda ()
          (minibuffer-with-setup-hook inject
            (with-selected-window action-window
              (run-hooks 'embark-pre-action-hook)
              (let ((enable-recursive-minibuffers t)
                    (embark--command command))
                (command-execute action))
              (run-hooks 'embark-post-action-hook))))))))

(defun embark-act-noexit ()
  "Embark upon an action.
The target of the action is chosen by `embark-target-finders'.
By default, if called from a minibuffer the target is the top
completion candidate, if called from an Embark Occur or a
Completions buffer it is the candidate at point."
  (interactive)
  (when-let ((action (embark--action)))
    (funcall action)))

(defun embark-act ()
  "Embark upon an action and exit from all minibuffers (if any).
The target of the action is chosen by `embark-target-finders'.
By default, if called from a minibuffer the target is the top
completion candidate, if called from an Embark Occur or a
Completions buffer it ixs the candidate at point."
  (interactive)
  (when-let ((action (embark--action)))
    (if (minibufferp)
        (progn
          (run-at-time 0 nil action)
          (top-level))
      (funcall action))))

(defvar embark-meta-map) ; forward declaration

(defun embark-become ()
  "Make current command become a different command.
Take the current minibuffer input as initial input for new
command.  The new command can be run normally using keybindings or
\\[execute-extended-command], but if the current command is found in a keymap in
`embark-become-keymaps', that keymap is activated to provide
convenient access to the other commands in it."
  (interactive)
  (when (minibufferp)
    (let* ((keymap
            (cl-loop for keymap-name in embark-become-keymaps
                     for keymap = (symbol-value keymap-name)
                     when (where-is-internal embark--command (list keymap))
                     return keymap))
           (target (run-hook-with-args-until-success 'embark-input-getters))
           (become (embark--with-indicator embark-become-indicator
                                           embark-prompter
                                           (make-composed-keymap
                                            keymap
                                            embark-meta-map))))
      (if (null become)
          (minibuffer-message "Canceled")
        (run-at-time 0 nil (lambda ()
                               (minibuffer-with-setup-hook
                                   (lambda ()
                                     (delete-minibuffer-contents)
                                     (insert target))
                                 (command-execute become))))
        (top-level)))))

(defmacro embark-define-keymap (name doc &rest bindings)
  "Define keymap variable NAME.
DOC is the documentation string.
BINDINGS is the list of bindings."
  (declare (indent 1))
  (let* ((map (make-symbol "map"))
         (parent (if (eq :parent (car bindings)) (cadr bindings)))
         (bindings (if parent (cddr bindings) bindings)))
    `(defvar ,name
       (let ((,map (make-sparse-keymap)))
         ,@(mapcar (pcase-lambda (`(,key ,fn))
                     (when (stringp key) (setq key (kbd key)))
                     `(define-key ,map ,key ,(if (symbolp fn) `#',fn fn)))
                   bindings)
         ,(if parent `(make-composed-keymap ,map ,parent) map))
       ,doc)))

;;; embark occur

(defface embark-occur-candidate '((t :inherit default))
  "Face for candidates in Embark Occur."
  :group 'embark)

(defface embark-occur-zebra-highlight
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#efefef")
    (((class color) (min-colors 88) (background dark))
     :background "#242424"))
  "Face to highlight alternate rows in `embark-occur-zebra-minor-mode'"
  :group 'embark)

(defface embark-occur-annotation '((t :inherit completions-annotations))
  "Face for annotations in Embark Occur.
This is only used for annotation that are not already fontified."
  :group 'embark)

(defcustom embark-occur-post-revert-hook nil
  "Hook run after an Embark Occur buffer is updated."
  :type 'hook
  :group 'embark)

(defun embark-occur--post-revert (&rest _)
  "Run `embark-occur-post-revert-hook'.
This function is used as :after advice for `tabulated-list-revert'."
  (when (derived-mode-p 'embark-occur-mode)
    (run-hooks 'embark-occur-post-revert-hook)))

(advice-add 'tabulated-list-revert :after #'embark-occur--post-revert)

(autoload 'package-delete "package")
(autoload 'package--from-builtin "package")
(autoload 'package-desc-extras "package")
(defvar package--builtins)
(defvar package-alist)
(defvar package-archive-contents)

(defun embark--package-desc (pkg)
  "Return the description structure for package PKG."
  (or ; found this in `describe-package-1'
   (car (alist-get pkg package-alist))
   (if-let ((built-in (assq pkg package--builtins)))
           (package--from-builtin built-in)
           (car (alist-get pkg package-archive-contents)))))

(defun embark--annotation-function ()
  "Get current annotation-function."
  (cond
   ((minibufferp)
    (or (completion-metadata-get (embark--metadata) 'annotation-function)
        (plist-get completion-extra-properties :annotation-function)))
   ((boundp 'marginalia-annotators)
    (alist-get (embark-classify) (symbol-value (car marginalia-annotators))))))

(defun embark-minibuffer-candidates ()
  "Return all current completion candidates from the minibuffer."
  (when (minibufferp)
    (let* ((all (completion-all-completions
                 (minibuffer-contents)
                 minibuffer-completion-table
                 minibuffer-completion-predicate
                 (- (point) (minibuffer-prompt-end))))
           (last (last all)))
      (when last (setcdr last nil))
      all)))

(defun embark-sorted-minibuffer-candidates ()
  "Return a sorted list of current minibuffer completion candidates.
This using the same sort order that `icomplete' and
`minibuffer-force-complete' use.  The intended usage is that you
replace `embark-minibuffer-candidates' with this function in the
list `embark-candidate-collectors'."
  (when (minibufferp)
    (let* ((all (completion-all-sorted-completions))
           (last (last all)))
      (when last (setcdr last nil))
      all)))

(autoload 'dired-get-filename "dired")

(defun embark-dired-candidates ()
  "Return all files shown in dired buffer."
  (when (derived-mode-p 'dired-mode)
    (save-excursion
      (goto-char (point-min))
      (let (files)
        (while (not (eobp))
          (when-let ((file (dired-get-filename t 'no-error-if-not-filep)))
            (push file files))
          (forward-line))
        (nreverse files)))))

(autoload 'ibuffer-map-lines-nomodify "ibuffer")

(defun embark-ibuffer-candidates ()
  "Return names of buffers listed in ibuffer buffer."
  (when (derived-mode-p 'ibuffer-mode)
    (let (buffers)
      (ibuffer-map-lines-nomodify
       (lambda (buffer _mark)
         (push (buffer-name buffer) buffers)))
      (nreverse buffers))))

(defun embark-embark-occur-candidates ()
  "Return candidates in Embark Occur buffer.
This makes `embark-export' work in Embark Occur buffers."
  (when (derived-mode-p 'embark-occur-mode)
    embark-occur-candidates))

(defun embark-completions-buffer-candidates ()
  "Return all candidates in a completions buffer."
  (when (derived-mode-p 'completion-list-mode)
    (save-excursion
      (goto-char (point-min))
      (next-completion 1)
      (let (all)
        (while (not (eobp))
          (push (embark-completion-at-point 'relative-path) all)
          (next-completion 1))
        (nreverse all)))))

(defun embark--action-command (action)
  "Turn an ACTION into a command to perform the action.
Returns the name of the command."
  (let ((name (intern (format "embark-action--%s" action)))
        (fn (lambda ()
              (interactive)
              (setq this-command action)
              (embark--gather-target-info)
              (call-interactively action))))
    (fset name fn)
    (when (symbolp action)
      (put name 'function-documentation
           (documentation action)))
    name))

(defun embark--omit-binding-p (cmd)
  "Should CMD binding be hidden from the user?
Return non-nil if this is a key binding that should not be bound
in `embark-occur-direct-action-minor-mode' nor mentioned by
`embark-keymap-help'."
  (or (null cmd)
      (not (symbolp cmd))
      (eq cmd 'ignore)
      (memq cmd embark--keep-alive-list)))

(defvar embark-occur-direct-action-minor-mode-map (make-sparse-keymap)
  "Keymap for direct bindings to embark actions.")

(define-minor-mode embark-occur-direct-action-minor-mode
  "Bind type-specific actions directly (without need for `embark-act')."
  :init-value nil
  :lighter " Act"
  :keymap embark-occur-direct-action-minor-mode-map
  (when embark-occur-direct-action-minor-mode
    ;; must mutate keymap, not make new one
    (let ((map embark-occur-direct-action-minor-mode-map))
      (setcdr map nil)
      (map-keymap
       (lambda (key cmd)
         (unless (embark--omit-binding-p cmd)
           (define-key map (vector key) (embark--action-command cmd))))
       (make-composed-keymap
        (symbol-value (alist-get embark--type embark-keymap-alist))
        embark-general-map)))))

(define-button-type 'embark-occur-entry
  'face 'embark-occur-candidate
  'action 'embark-occur-choose)

(defun embark--boundaries ()
  "Get current minibuffer completion boundaries."
  (let ((contents (minibuffer-contents))
        (pt (- (point) (minibuffer-prompt-end))))
    (completion-boundaries
     (substring contents 0 pt)
     minibuffer-completion-table
     minibuffer-completion-predicate
     (substring contents pt))))

(defun embark-occur-choose (entry)
  "Run default action on Embark Occur ENTRY.

If the variable `embark-occur-minibuffer-completion' is non-nil,
this function does something special when the Embark Occur buffer
is associated to the active minibuffer and is live updating: it
completes the minibuffer input to ENTRY and, unless this leads to
new completion candidates (for example, when entering a directory
in `find-file') or the command was called with a prefix argument,
exit the minibuffer.

If you are using `embark-completing-read' as your
`completing-read-function' you might want to set
`embark-occur-minibuffer-completion' to t."
  (if (and embark-occur-minibuffer-completion
           (active-minibuffer-window)
           (eq embark-occur-from
               (window-buffer (active-minibuffer-window)))
           (memq 'embark-occur--update-linked ; live?
                 (buffer-local-value 'after-change-functions
                                     embark-occur-from)))
      (let ((text (button-label entry)))
        (select-window (active-minibuffer-window))
        (pcase-let ((origin (minibuffer-prompt-end))
                    (`(,beg . ,end) (embark--boundaries)))
          (delete-region (+ origin beg) (+ (point) end))
          (goto-char (+ origin beg))
          (insert text))
        ;; If the boundaries changed after insertion there are new
        ;; completion candidates (like when entering a directory in
        ;; find-file). If so, don't exit; otherwise revert.
        (unless (or current-prefix-arg
                    (= (car (embark--boundaries))
                       (- (point) (minibuffer-prompt-end))))
          (exit-minibuffer)))
    (goto-char entry)            ;; pretend RET was pressed even if
    (setq last-nonmenu-event 13) ;; mouse was clicked, to fool imenu
    (embark--gather-target-info)
    (let ((ecmd embark--command))
      (select-window (embark--target-window))
      (setq embark--command ecmd))
    (run-hooks 'embark-pre-action-hook)
    (embark-default-action)
    (run-hooks 'embark-post-action-hook)))

(embark-define-keymap embark-occur-mode-map
  "Keymap for Embark occur mode."
  ("a" embark-act)
  ("A" embark-occur-direct-action-minor-mode)
  ("z" embark-occur-zebra-minor-mode)
  ("M-q" embark-occur-toggle-view)
  ("v" embark-occur-toggle-view)
  ("e" embark-export)
  ("s" isearch-forward)
  ("f" forward-button)
  ("b" backward-button)
  ("<right>" forward-button)
  ("<left>" backward-button))

(define-derived-mode embark-occur-mode tabulated-list-mode "Embark Occur"
  "List of candidates to be acted on.
The command `embark-act' is bound `embark-occur-mode-map', but
you might prefer to change the keybinding to match your other
keybinding for it.  Or alternatively you might want to enable
`embark-occur-direct-action-minor-mode' in
`embark-occur-mode-hook'.")

(defun embark-occur--max-width ()
  "Maximum width of any Embark Occur candidate."
  (or (cl-loop for cand in embark-occur-candidates
               maximize (length cand))
      0))

(defun embark-occur--list-view ()
  "List view of candidates and annotations for Embark Occur buffer."
  (setq tabulated-list-format
        (if embark-occur-annotator
            (let ((width (embark-occur--max-width)))
              `[("Candidate" ,width t) ("Annotation" 0 nil)])
          [("Candidate" 0 t)]))
  (if tabulated-list-use-header-line
      (tabulated-list-init-header)
    (setq header-line-format nil))
  (setq tabulated-list-entries
        (mapcar (if embark-occur-annotator
                    (lambda (cand)
                      (let* ((annotation
                              (or (funcall embark-occur-annotator cand) ""))
                             (length (length annotation))
                             (facesp (text-property-not-all
                                      0 length 'face nil annotation)))
                        (when facesp (add-face-text-property
                                      0 length 'default t annotation))
                        `(,cand [(,cand type embark-occur-entry)
                                 (,annotation
                                  ,@(unless facesp
                                      '(face embark-occur-annotation)))])))
                  (lambda (cand)
                    `(,cand [(,cand type embark-occur-entry)])))
                embark-occur-candidates)))

(defun embark-occur--remove-zebra-stripes ()
  "Remove highlighting of alternate rows."
  (remove-overlays nil nil 'face 'embark-occur-zebra-highlight))

(defun embark-occur--add-zebra-stripes ()
  "Highlight alternate rows with the `embark-occur-highlight-row' face."
  (embark-occur--remove-zebra-stripes)
  (save-excursion
    (goto-char (point-min))
    (when (tabulated-list-header-overlay-p) (forward-line))
    (let ((columns (length tabulated-list-format)))
      (while (not (eobp))
        (condition-case nil
            (forward-button columns)
          (user-error (goto-char (point-max))))
        (unless (eobp)
          (let ((pt (point)))
            (condition-case nil
                (forward-button columns)
              (user-error (goto-char (point-max))))
            (overlay-put (make-overlay pt (point))
                         'face 'embark-occur-zebra-highlight)))))))

(define-minor-mode embark-occur-zebra-minor-mode
  "Minor mode to highlight alternate rows in an Embark Occur buffer.
This is specially useful to tell where multi-line entries begin and end."
  :init-value nil
  :lighter " Zebra"
  (if embark-occur-zebra-minor-mode
      (progn
        (add-hook 'embark-occur-post-revert-hook
                  #'embark-occur--add-zebra-stripes nil t)
        (embark-occur--add-zebra-stripes))
    (remove-hook 'embark-occur-post-revert-hook
                 #'embark-occur--add-zebra-stripes t)
    (embark-occur--remove-zebra-stripes)))

(defun embark-occur--grid-view ()
  "Grid view of candidates for Embark Occur buffer."
  (let* ((width (min (+ (embark-occur--max-width) 2) (floor (window-width) 2)))
         (columns (/ (window-width) width)))
    (setq tabulated-list-format
          (make-vector columns `("Candidate" ,width nil)))
    (if tabulated-list-use-header-line
        (tabulated-list-init-header)
      (setq header-line-format nil))
    (setq tabulated-list-entries
          (cl-loop with cands = (copy-tree embark-occur-candidates)
                   while cands
                   collect
                   (list nil
                         (apply #'vector
                                (cl-loop repeat columns
                                         collect
                                         `(,(or (pop cands) "")
                                           type embark-occur-entry))))))))

(defun embark-occur--revert (&rest _)
  "Recalculate Embark Occur candidates if possible."
  (when (buffer-live-p embark-occur-from)
    (when (minibufferp embark-occur-from)
      (setq embark-occur-annotator
            (with-current-buffer embark-occur-from
              (embark--annotation-function))))
    (setq embark-occur-candidates
          (with-current-buffer embark-occur-from
            (run-hook-with-args-until-success
             'embark-candidate-collectors))
          default-directory
          (with-current-buffer embark-occur-from
            (embark--default-directory))))
  (if (eq embark-occur-view 'list)
      (embark-occur--list-view)
    (embark-occur--grid-view)))

(defun embark-occur--update-linked (&rest _)
  "Update linked Embark Occur buffer."
  (when-let ((occur-buffer embark-occur-linked-buffer))
    (when embark--live-occur--timer
      (cancel-timer embark--live-occur--timer))
    (setq embark--live-occur--timer
          (run-with-idle-timer
           embark-live-occur-update-delay nil
           (lambda ()
             (let ((non-essential t))
               (while-no-input
                 (when (buffer-live-p occur-buffer) ; might be killed by now
                   (with-current-buffer occur-buffer
                     (revert-buffer))))))))))

(defun embark-occur--linked-buffer-is-live-p ()
  "Is this buffer linked to a live Embark Occur buffer?"
  (when-let ((linked embark-occur-linked-buffer)
             (name (buffer-name linked)))
    (string-match-p "Embark Live Occur" name)))

(defun embark-occur--kill-live-occur-buffer ()
  "Kill linked Embark Live Occur buffer."
  (when (embark-occur--linked-buffer-is-live-p)
    (kill-buffer embark-occur-linked-buffer)))

(defun embark-occur--toggle (variable this that)
  "Toggle Embark Occur buffer's local VARIABLE between THIS and THAT.
Refresh the buffer afterwards."
  (when-let ((buffer (if (derived-mode-p 'embark-occur-mode)
                         (current-buffer)
                       embark-occur-linked-buffer)))
    (with-current-buffer buffer
      (set variable
           (if (eq (buffer-local-value variable buffer) this) that this))
      (revert-buffer))))

(defun embark-occur-toggle-view ()
  "Toggle between list and grid views of Embark Occur buffer.
This command can be called either from the Embark Occur buffer
itself, or, from any buffer (particularly a minibuffer) that has
a linked Embark Live Occur buffer."
  (interactive)
  (embark-occur--toggle 'embark-occur-view 'list 'grid))

(defun embark-occur-toggle-header ()
  "Toggle the visibility of the header line of Embark Occur buffer.
This command can be called either from the Embark Occur buffer
itself, or, from any buffer (particularly a minibuffer) that has
a linked Embark Live Occur buffer."
  (interactive)
  (embark-occur--toggle 'tabulated-list-use-header-line t nil))

(defun embark-occur-noselect (buffer-name &optional initial-view)
  "Create and return a buffer of current candidates ready for action.
Optionally start in INITIAL-VIEW (either `list' or `grid')
instead of what `embark-occur-initial-view-alist' specifies.
Argument BUFFER-NAME specifies the name of the created buffer."
  (let ((from (current-buffer))
        (buffer (generate-new-buffer buffer-name))
        (type (embark-classify)))
    (embark-occur--kill-live-occur-buffer) ; live ones are ephemeral
    (setq embark-occur-linked-buffer buffer)
    (with-current-buffer buffer
      (delay-mode-hooks (embark-occur-mode)) ; we'll run them when the
                                             ; buffer is displayed, so
                                        ; they can use the window
      (setq tabulated-list-use-header-line nil) ; default to no header
      (setq embark-occur-from from)
      (add-hook 'tabulated-list-revert-hook #'embark-occur--revert nil t)
      (setq embark-occur-view
            (or initial-view
                (alist-get type embark-occur-initial-view-alist)
                (alist-get t embark-occur-initial-view-alist)
                'list))
      (when (eq embark-occur-view 'zebra)
        (setq embark-occur-view 'list)
        (add-hook 'embark-occur-mode-hook
                  #'embark-occur-zebra-minor-mode nil t)))
    (embark--cache-info buffer)
    buffer))

(defun embark-occur--display (occur-buffer &optional action)
  "Display the Embark OCCUR-BUFFER and run mode hooks.
This is also when we initially fill the buffer with candidates,
since the grid view needs to know the window width.  Return the
window where the buffer is displayed.

Optional argument ACTION is passed to `display-buffer' to control
window placement."
  (let ((occur-window (display-buffer occur-buffer action)))
    (with-selected-window occur-window
      (run-mode-hooks)
      (revert-buffer))
    occur-window))

(defun embark-occur--initial-view-arg ()
  "Translate current prefix arg to intial Embark Occur view.
\\[universal-argument] means grid view, a prefix argument of 1
means list view, anything else means proceed according to
`embark-occur-initial-view-alist'."
  (list (pcase current-prefix-arg
          ('(4) 'grid)
          (1 'list))))

(defun embark--reuse-live-occur-window (buffer alist)
  "Reuse an Embark Live Occur window in the current frame to display BUFFER.
ALIST comes from the action argument of `display-buffer'."
  (cl-loop for window in (window-list-1 nil 'nomini)
           for name = (buffer-name (window-buffer window))
           when (and (window-live-p window)
                     (string-match-p "Embark Live Occur" name))
           return (window--display-buffer buffer window 'reuse alist)))

(defun embark-live-occur (&optional initial-view)
  "Create a live-updating Embark Occur buffer.
Optionally start in INITIAL-VIEW (either `list' or `grid')
instead of what `embark-occur-initial-view-alist' specifies.
Interactively, \\[universal-argument] means grid view, a prefix
argument of 1 means list view.

To control the display, add an entry to `display-buffer-alist'
with key \"Embark Live Occur\"."
  (interactive (embark-occur--initial-view-arg))
  (let ((occur-buffer
         (embark-occur-noselect "*Embark Live Occur*" initial-view)))
    (add-hook 'after-change-functions   ; set up live updates!
              #'embark-occur--update-linked nil t)
    (let ((occur-window (embark-occur--display
                         occur-buffer
                         '((embark--reuse-live-occur-window
                            display-buffer-at-bottom)))))
      (set-window-dedicated-p occur-window t)
      (when (minibufferp)
        (add-hook 'minibuffer-exit-hook
                  #'embark-occur--kill-live-occur-buffer
                  nil t)
        (setq minibuffer-scroll-window occur-window)))))

(defun embark-occur (&optional initial-view)
  "Create an Embark Occur buffer and exit all minibuffers.
Optionally start in INITIAL-VIEW (either `list' or `grid')
instead of what `embark-occur-initial-view-alist' specifies.
Interactively, \\[universal-argument] means grid view, a prefix
argument of 1 means list view.

To control the display, add an entry to `display-buffer-alist'
with key \"Embark Occur\"."
  (interactive (embark-occur--initial-view-arg))
  (if-let ((candidates
            (run-hook-with-args-until-success 'embark-candidate-collectors))
           (occur-buffer
            (embark-occur-noselect "*Embark Occur*" initial-view)))
      (let ((annotator (embark--annotation-function)))
        (with-current-buffer occur-buffer
          (setq embark-occur-candidates candidates)
          (setq embark-occur-annotator annotator)
          (when (minibufferp embark-occur-from)
            (setq embark-occur-from nil)))
        (embark-after-exit ()
          (select-window
           (embark-occur--display occur-buffer))))
    (minibuffer-message "No candidates for occur")))

(defun embark-live-occur-after-delay ()
  "Start `embark-live-occur' after `embark-live-occur-initial-delay'.
Add this function to `minibuffer-setup-hook' to have an Embark
Live Occur buffer popup every time you use the minibuffer."
  (when minibuffer-completion-table
    (run-with-idle-timer
     embark-live-occur-initial-delay nil
     (lambda () (when (minibufferp) (embark-live-occur))))))

(defun embark--wait-for-input (_beg _end _len)
  "After input in the minibuffer, wait briefly and run `embark-live-occur'.
This is meant to be added to `after-change-functions' in the
minibuffer by the function `embark-live-occur-after-input', you
probably shouldn't use this function directly."
  (remove-hook 'after-change-functions 'embark--wait-for-input t)
  (embark-live-occur-after-delay))

(defun embark-live-occur-after-input ()
  "Start `embark-live-occur' shortly after the minibuffer receives some input.
Add this function to `minibuffer-setup-hook' to have an Embark
Live Occur buffer popup soon after you type something in the
minibuffer; the length of the delay after typing is given by
`embark-live-occur-initial-delay'."
  (when minibuffer-completion-table
   (add-hook 'after-change-functions #'embark--wait-for-input nil t)))

(defun embark-switch-to-live-occur ()
  "Switch to the Embark Live Occur buffer."
  (interactive)
  (if-let ((buffer (if (embark-occur--linked-buffer-is-live-p)
                       embark-occur-linked-buffer
                     (get-buffer "*Embark Live Occur*"))))
      (switch-to-buffer buffer)
    (user-error "No Embark Live Occur buffer")))

(defun embark-export ()
  "Create a type-specific buffer to manage current candidates.
The variable `embark-exporters-alist' controls how to make the
buffer for each type of completion."
  (interactive)
  (let* ((type (embark-classify))
         (exporter (or embark-overriding-export-function
                       (alist-get type embark-exporters-alist)
                       (alist-get t embark-exporters-alist))))
    (if (eq exporter 'embark-occur)
        ;; let embark-occur gather the candidates
        (embark-occur)
      (let ((candidates (run-hook-with-args-until-success
                         'embark-candidate-collectors))
            (dir (embark--default-directory)))
        (embark-after-exit ()
          (let ((default-directory dir)) ; dired needs this info
            (funcall exporter candidates)))))))

(defun embark-export-ibuffer (buffers)
  "Create an ibuffer buffer listing BUFFERS."
  (ibuffer t "*Embark Export Ibuffer*"
           `((predicate . (member (buffer-name) ',buffers)))))

(autoload 'dired-check-switches "dired")

(defun embark-export-dired (files)
  "Create a dired buffer listing FILES."
  (setq files (mapcar #'directory-file-name files))
  (when (dired-check-switches dired-listing-switches "A" "almost-all")
    (setq files (cl-remove-if-not
                 (lambda (path)
                   (let ((file (file-name-nondirectory path)))
                     (unless (or (string= file ".") (string= file ".."))
                       path)))
                 files)))
  (dired (cons default-directory files))
  (rename-buffer (format "*Embark Export Dired %s*" default-directory)))

(autoload 'package-menu-mode "package")
(autoload 'package-menu--generate "package")

(defun embark-export-list-packages (packages)
  "Create a package menu mode buffer listing PACKAGES."
  (interactive)
  (let ((buf (get-buffer-create "*Embark Export Packages*")))
    (with-current-buffer buf
      (package-menu-mode)
      (package-menu--generate nil (mapcar #'intern packages)))
    (switch-to-buffer buf)))

;;; custom actions

(defun embark-keymap-help ()
  "Prompt for an action to perform or command to become and run it."
  (interactive)
  (user-error "Not meant to be called directly."))

(defun embark-default-action ()
  "Default action.
This is whatever command opened the minibuffer in the first place."
  (interactive)
  (setq this-command embark--command)   ; so the proper hooks apply
  (call-interactively embark--command))

(defun embark-insert (string)
  "Insert STRING at point."
  (interactive "sInsert: ")
  (insert (substring-no-properties string)))

(defun embark-save (str)
  "Save STR in the kill ring."
  (interactive "sSave: ")
  (kill-new str))

(defun embark-eshell (file)
  "Run eshell in directory of FILE."
  (interactive "GDirectory: ")
  (let ((default-directory
          (file-name-directory
           (expand-file-name
            (substitute-in-file-name file)))))
    (eshell '(4))))

(defun embark-find-definition (symbol)
  "Find definition of SYMBOL."
  (interactive "SSymbol: ")
  (cond
   ((fboundp symbol) (find-function symbol))
   ((boundp symbol) (find-variable symbol))))

(defun embark-info-lookup-symbol (symbol)
  "Display the definition of SYMBOL, from the Elisp manual."
  (interactive "SSymbol: ")
  (info-lookup-symbol symbol 'emacs-lisp-mode))

(defun embark-rename-buffer (buf)
  "Rename buffer BUF."
  (interactive "bBuffer: ")
  (with-current-buffer buf
    (call-interactively #'rename-buffer)))

(defun embark-browse-package-url (pkg)
  "Open homepage for package PKG with `browse-url'."
  (interactive "SPackage: ")
  (if-let ((desc (embark--package-desc pkg))
           (url (alist-get :url (package-desc-extras desc))))
      (browse-url url)
    (message "No homepage found for `%s'" pkg)))

(defun embark-insert-relative-path (file)
  "Insert relative path to FILE.
The insert path is relative to `default-directory'."
  (interactive "FFile: ")
  (insert (file-relative-name (substitute-in-file-name file))))

(defun embark-save-relative-path (file)
  "Save the relative path to FILE in the kill ring.
The insert path is relative to `default-directory'."
  (interactive "FFile: ")
  (kill-new (file-relative-name (substitute-in-file-name file))))

(defun embark-shell-command-on-buffer (buffer command &optional replace)
  "Run shell COMMAND on contents of BUFFER.
Called with \\[universal-argument], replace contents of buffer
with command output.  For replacement behaviour see
`shell-command-dont-erase-buffer' setting."
  (interactive
   (list
    (read-buffer "Buffer: " nil t)
    (read-shell-command "Shell command: ")
    current-prefix-arg))
  (with-current-buffer buffer
    (shell-command-on-region (point-min) (point-max)
                             command
                             (and replace (current-buffer)))))

(defun embark-bury-buffer (buf)
  "Bury buffer BUF."
  (interactive "bBuffer: ")
  (if-let (win (get-buffer-window buf))
      (with-selected-window win
        (bury-buffer))
    (bury-buffer)))

(defun embark-kill-buffer-and-window (buf)
  "Kill buffer BUF and delete its window."
  (interactive "bBuffer: ")
  (when-let (buf (get-buffer buf))
    (if-let (win (get-buffer-window buf))
        (with-selected-window win
          (kill-buffer-and-window))
      (kill-buffer buf))))

(defun embark-save-unicode-character (char)
  "Save unicode character CHAR to kill ring."
  (interactive (list (read-char-by-name "Insert character  (Unicode name or hex): ")))
  (kill-new (format "%c" char)))

(defun embark-act-on-region-contents ()
  "Act on contents of active region."
  (interactive)
  (let ((embark--target-region t))
    (embark-act-noexit)))

;;; setup hooks for actions

(defun embark--shell-prep ()
  "Prepare target for use as argument for a shell command.
This quotes the spaces, inserts an extra space at the beginning
and leaves the point to the left of it."
  (let ((contents (minibuffer-contents)))
    (delete-minibuffer-contents)
    (insert " " (shell-quote-wildcard-pattern contents))
    (beginning-of-line)))

(defun embark--eval-prep ()
  "If target is: a variable, skip edit; a function, wrap in parens."
  (if (not (fboundp (intern (minibuffer-contents))))
      (run-at-time 0 nil #'exit-minibuffer)
    (beginning-of-line)
    (insert "(")
    (end-of-line)
    (insert ")")
    (backward-char)))

;;; keymaps

(embark-define-keymap embark-meta-map
  "Keymap for non-action Embark functions."
  :parent universal-argument-map
  ("C-h" embark-keymap-help)
  ("C-u" universal-argument))

(embark-define-keymap embark-general-map
  "Keymap for Embark general actions."
  :parent embark-meta-map
  ("i" embark-insert)
  ("w" embark-save)
  ("RET" embark-default-action)
  ("E" embark-export)
  ("O" embark-occur)
  ("L" embark-live-occur)
  ("B" embark-become))

(autoload 'org-table-convert-region "org-table")

(embark-define-keymap embark-region-map
  "Keymap for Embark actions on the active region."
  ("u" upcase-region)
  ("l" downcase-region)
  ("c" capitalize-region)
  ("|" shell-command-on-region)
  ("e" eval-region)
  ("i" indent-rigidly)
  ("TAB" indent-region)
  ("f" fill-region)
  ("p" fill-region-as-paragraph)
  ("r" rot13-region)
  ("=" count-words-region)
  ("s" whitespace-cleanup-region)
  ("t" transpose-regions)
  ("o" org-table-convert-region)
  (";" comment-or-uncomment-region)
  ("w" write-region)
  ("m" apply-macro-to-region-lines)
  ("n" narrow-to-region)
  ("RET" embark-act-on-region-contents))

(embark-define-keymap embark-file-map
  "Keymap for Embark file actions."
  ("f" find-file)
  ("o" find-file-other-window)
  ("d" delete-file)
  ("D" delete-directory)
  ("r" rename-file)
  ("c" copy-file)
  ("!" shell-command)
  ("&" async-shell-command)
  ("=" ediff-files)
  ("e" embark-eshell)
  ("+" make-directory)
  ("I" embark-insert-relative-path)
  ("W" embark-save-relative-path)
  ("l" load-file)
  ("b" byte-compile-file)
  ("R" byte-recompile-directory))

(embark-define-keymap embark-url-map
  "Keymap for Embark url actions."
  ("b" browse-url)
  ("e" eww))

(embark-define-keymap embark-buffer-map
  "Keymap for Embark buffer actions."
  ("k" kill-buffer)
  ("b" switch-to-buffer)
  ("o" switch-to-buffer-other-window)
  ("z" embark-bury-buffer)
  ("q" embark-kill-buffer-and-window)
  ("r" embark-rename-buffer)
  ("=" ediff-buffers)
  ("|" embark-shell-command-on-buffer))

(embark-define-keymap embark-symbol-map
  "Keymap for Embark symbol actions."
  ("h" describe-symbol)
  ("c" Info-goto-emacs-command-node)
  ("s" embark-info-lookup-symbol)
  ("d" embark-find-definition)
  ("b" where-is)
  ("e" eval-expression))

(embark-define-keymap embark-package-map
  "Keymap for Embark package actions."
  ("h" describe-package)
  ("i" package-install)
  ("I" embark-insert)
  ("d" package-delete)
  ("r" package-reinstall)
  ("u" embark-browse-package-url)
  ("a" package-autoremove)
  ("g" package-refresh-contents))

(embark-define-keymap embark-bookmark-map
  "Keymap for Embark bookmark actions."
  ("d" bookmark-delete)
  ("r" bookmark-rename)
  ("R" bookmark-relocate)
  ("l" bookmark-locate)
  ("i" bookmark-insert)
  ("I" embark-insert)
  ("j" bookmark-jump)
  ("o" bookmark-jump-other-window)
  ("f" bookmark-jump-other-frame))

(embark-define-keymap embark-unicode-name-map
  "Keymap for Embark unicode name actions."
  ("I" insert-char)
  ("W" embark-save-unicode-character))

(embark-define-keymap embark-become-help-map
  "Keymap for Embark help actions."
  ("V" apropos-variable)
  ("U" apropos-user-option)
  ("C" apropos-command)
  ("v" describe-variable)
  ("f" describe-function)
  ("s" describe-symbol)
  ("F" describe-face)
  ("p" describe-package)
  ("i" describe-input-method))

(autoload 'recentf-open-files "recentf")

(embark-define-keymap embark-become-file+buffer-map
  "Embark become keymap for files and buffers."
  ("f" find-file)
  ("p" project-find-file)
  ("r" recentf-open-files)
  ("b" switch-to-buffer)
  ("l" locate)
  ("L" find-library))

(embark-define-keymap embark-become-shell-command-map
  "Embark become keymap for shell commands."
  ("!" shell-command)
  ("&" async-shell-command)
  ("c" comint-run)
  ("t" term))

(embark-define-keymap embark-become-match-map
  "Embark become keymap for search."
  ("o" occur)
  ("k" keep-lines)
  ("f" flush-lines)
  ("c" count-matches))

(provide 'embark)
;;; embark.el ends here
