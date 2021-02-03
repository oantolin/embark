;;; embark.el --- Conveniently act on minibuffer completions   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Omar Antolín Camarena

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: convenience
;; Version: 0.10
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

;; This package provides a sort of right-click contextual menu for
;; Emacs, accessed through the `embark-act' command (which you should
;; bind to a convenient key), offering you relevant actions to use on
;; a target determined by the context:

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
;; in the classification.  The above introduction just mentions part of
;; the default configuration.

;; Configuring which actions are offered for a type is particularly
;; easy and requires no programming: the `embark-keymap-alist'
;; variable associates target types with variable containing keymaps,
;; and those keymaps containing binds for the actions.  For example,
;; in the default configuration the type `file' is associated with the
;; symbol `embark-file-keymap'.  That symbol names a keymap with
;; single-letter keybindings for common Emacs file commands, for
;; instance `c' is bound to `copy-file'.  This means that if while you
;; are in the minibuffer after running a command that prompts for a
;; file, such as `find-file' or `rename-file', you can copy a file by
;; running `embark-act' and then pressing `c'.

;; These action keymaps are very convenient but not strictly necessary
;; when using `embark-act': you can use any command that reads from the
;; minibuffer as an action and the target of the action will be inserted
;; at the first minibuffer prompt.  After running `embark-act' all of your
;; keybindings and even `execute-extended-command' can be used to run a
;; command.  The action keymaps are normal Emacs keymaps and you should
;; feel free to bind in them whatever commands you find useful as actions.

;; The actions in `embark-general-map' are available no matter what
;; type of completion you are in the middle of.  By default this
;; includes bindings to save the current candidate in the kill ring
;; and to insert the current candidate in the previously selected
;; buffer (the buffer that was current when you executed a command
;; that opened up the minibuffer).

;; You can read about the Embark GitHub project wiki:
;; https://github.com/oantolin/embark/wiki/Default-Actions

;; Besides acting individually on targets, Embark lets you work
;; collectively on a set of target candidates.  For example, while
;; you are in the minibuffer the candidates are simply the possible
;; completions of your input.  Embark provides three commands to work
;; on candidate sets:

;; - The `embark-collect-snapshot' command produces a buffer listing
;;   all candidates, for you to peruse and run actions on at your
;;   leisure.  The candidates can be viewed in a grid or as a list
;;   showing additional annotations.  The `embark-collect-live'
;;   variant produces "live" Embark Collect buffers, meaning they
;;   autoupdate as the set of candidates changes.

;; - The `embark-export' command tries to open a buffer in an
;;   appropriate major mode for the set of candidates.  If the
;;   candidates are files export produces a Dired buffer; if they are
;;   buffers, you get an Ibuffer buffer; and if they are packages you
;;   get a buffer in package menu mode.

;; These are always available as "actions" (although they do not act
;; on just the current target but on all candidates) for embark-act and
;; are bound to S, L and E, respectively, in embark-general-map.  This
;; means that you do not have to bind your own key bindings for these
;; (although you can, of course), just a key binding for `embark-act'.

;;; Code:

(eval-when-compile (require 'subr-x))

(require 'ffap) ; used it to recognize file and url targets

;;; user facing options

(defgroup embark nil
  "Emacs Mini-Buffer Actions Rooted in Keymaps"
  :group 'minibuffer)

(defcustom embark-keymap-alist
  '((file . embark-file-map)
    (environment-variables . embark-file-map) ; they come up in file completion
    (url . embark-url-map)
    (buffer . embark-buffer-map)
    (identifier . embark-identifier-map)
    (symbol . embark-symbol-map)
    (command . embark-command-map)
    (variable . embark-variable-map)
    (minor-mode . embark-command-map)
    (unicode-name . embark-unicode-name-map)
    (package . embark-package-map)
    (bookmark . embark-bookmark-map)
    (region . embark-region-map))
  "Alist of action types and corresponding keymaps.
For any type not listed here, `embark-act' will use
`embark-general-map'."
  :type '(alist :key-type symbol :value-type variable))

(defvar embark-overriding-keymap nil
  "Can be bound to short circuit `embark-keymap-alist'.
Embark will set the parent of this map to `embark-general-map'.")

(defcustom embark-target-finders
  '(embark-target-top-minibuffer-completion
    embark-target-active-region
    embark-target-collect-candidate
    embark-target-completion-at-point
    embark-target-url-at-point
    embark-target-file-at-point
    embark-target-identifier-at-point)
  "List of functions to determine the target in current context.
Each function should take no arguments and return either a cons
of the form (type . target) where type is a symbol and target is
a string, or nil to indicate it found no target."
  :type 'hook)

(defcustom embark-transformer-alist
  '((minor-mode . embark-lookup-lighter-minor-mode)
    (xref-location . embark-set-xref-location-default-action)
    (symbol . embark-refine-symbol-type))
  "Alist associating type to functions for transforming targets.
Each function should take a target string and return a pair of
the form a `cons' of the new type and the new target."
  :type 'hook)

(defcustom embark-become-keymaps
  '(embark-become-help-map
    embark-become-file+buffer-map
    embark-become-shell-command-map
    embark-become-match-map)
  "List of keymaps for `embark-become'.
Each keymap groups a set of related commands that can
conveniently become one another."
  :type '(repeat variable))

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
                 (function :tag "Other")))

(defface embark-keybinding '((t :inherit success))
  "Face used to display key bindings.
Used by `embark-completing-read-prompter' and `embark-keymap-help'.")

(defcustom embark-action-indicator
  (let ((act (propertize "Act" 'face 'highlight)))
    (cons act (concat act " on '%s'")))
  "Indicator to use when embarking upon an action.
If set to a string it is used as a format string where %s will be
replaced by the target of `embark-act', and the resulting string
is prepended to the minibuffer prompt or shown in the echo area
if `embark-act' was called outside of the minibuffer.

You can also set this variable to a cons of two format strings,
in which case the first is used if `embark-act' is called from
the minibuffer and the second is used otherwise.

Finally, you can set this variable to a function, which will be
called with the action keymap and the target.  The function
should return either nil or a function to be called when the
indicator is no longer needed.  Finally, if this variable is set
to nil no indication is shown."
  :type '(choice function string (cons string string) nil))

(defcustom embark-become-indicator (propertize "Become" 'face 'highlight)
  "Indicator to use when using `embark-become'.
If set to a string, it is prepended to the minibuffer prompt when
you call `embark-become'.  If set to a function it will be called
with one of the keymaps listed in `embark-become-keymaps'
containing the currently executing command (or nil, if no such
keymap exists).  The function should return either nil or a
function to be called when the indicator is no longer needed.
Finally, if this variable is set to nil no indication is shown."
  :type '(choice function string nil))

(defcustom embark-setup-hook nil
  "Hook to run after injecting target into minibuffer.
It can be overriden by the `embark-setup-overrides' alist."
  :type 'hook)

(defcustom embark-setup-overrides
  '((async-shell-command embark--shell-prep)
    (shell-command embark--shell-prep)
    (eval-expression embark--eval-prep)
    (package-delete minibuffer-force-complete))
  "Alist associating commands with post-injection setup hooks.
For commands appearing as keys in this alist, run the
corresponding value as a setup hook (instead of
`embark-setup-hook') after injecting the target into in the
minibuffer and before acting on it."
  :type '(alist :key-type function :value-type hook))

(defcustom embark-quit-after-action t
  "Should `embark-act' quit the minibuffer?
This controls whether calling `embark-act' without a prefix
argument quits the minibuffer or not.  You can always get the
opposite behavior to that indicated by this variable by calling
`embark-act' with \\[universal-argument].

Note that `embark-act' can also be called from outside the
minibuffer and this variable is irrelevant in that case."
  :type 'boolean)

(defcustom embark-allow-edit-default nil
  "Is the user allowed to edit the target before acting on it?
This variable sets the default policy, and can be overidden.
When this variable is nil, it is overridden by
`embark-allow-edit-commands'; when it is t, it is overidden by
`embark-skip-edit-commands'."
  :type 'boolean)

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
  :type 'hook)

(defcustom embark-skip-edit-commands nil
  "Skip editing of target prior to acting for these commands.
This list is used only when `embark-allow-edit-default' is t."
  :type 'hook)

(defcustom embark-pre-action-hook nil
  "Hook run right before an action is embarked upon."
  :type 'hook)

(defcustom embark-post-action-hook nil
  "Hook run after an embarked upon action concludes."
  :type 'hook)

;;; stashing information for actions in buffer local variables

(defvar-local embark--type nil
  "Cache for the completion type, meant to be set buffer-locally.")

(defvar-local embark--target-buffer nil
  "Cache for the previous buffer, meant to be set buffer-locally.")

(defvar-local embark--target-window nil
  "Cache for the previous window, meant to be set buffer-locally.
Since windows can be reused to display different buffers, this
window should only be used if it displays `embark--target-buffer'.")

(defvar-local embark--command nil
  "Command that started the completion session.")

(defvar-local embark--default-action nil
  "Command that should be used as the default action.
Defaults to `embark--command'.")

(defun embark--default-directory ()
  "Guess a reasonable default directory for the current candidates."
  (if (and (minibufferp) minibuffer-completing-file-name)
      (file-name-directory
       (expand-file-name
        (buffer-substring (minibuffer-prompt-end) (point))))
    default-directory))

(defun embark--target-buffer ()
  "Return buffer that should be targeted by Embark actions."
  (cond
   ((and (minibufferp) (minibuffer-selected-window))
    (window-buffer (minibuffer-selected-window)))
   ((and embark--target-buffer (buffer-live-p embark--target-buffer))
    embark--target-buffer)
   (t (current-buffer))))

(defun embark--target-window (&optional display)
  "Return window which should be selected when Embark actions run.
If DISPLAY is non-nil, call `display-buffer' to produce the
window if necessary."
  (cond
   ((and (minibufferp) (minibuffer-selected-window))
    (minibuffer-selected-window))
   ((and embark--target-window
         (window-live-p embark--target-window)
         (eq (window-buffer embark--target-window) embark--target-buffer))
    embark--target-window)
   (embark--target-buffer
    (or (get-buffer-window embark--target-buffer)
        (when display (display-buffer embark--target-buffer))))
   (display (selected-window))))

(defun embark--cache-info (buffer)
  "Cache information needed for actions in variables local to BUFFER.
BUFFER defaults to the current buffer."
  (let ((cmd (or embark--command this-command))
        (def embark--default-action)
        (dir (embark--default-directory))
        (target-buffer (embark--target-buffer))
        (target-window (embark--target-window)))
    (with-current-buffer buffer
      (setq-local embark--command cmd
                  embark--default-action def
                  default-directory dir
                  embark--target-buffer target-buffer
                  embark--target-window target-window))))

(defun embark--cache-info--completion-list ()
  "Cache information needed for actions in a *Completions* buffer.
Meant to be be added to `completion-setup-hook'."
  ;; when completion-setup-hook hook runs, the *Completions* buffer is
  ;; available in the variable standard-output
  (embark--cache-info standard-output)
  (when (minibufferp completion-reference-buffer)
    (setf (buffer-local-value 'embark--type standard-output)
          (completion-metadata-get (embark--metadata) 'category))))

;; We have to add this *after* completion-setup-function because that's
;; when the buffer is put in completion-list-mode and turning the mode
;; on kills all local variables! So we use a depth of 5.
(add-hook 'completion-setup-hook #'embark--cache-info--completion-list 5)

;;;###autoload
(progn
  (defun embark--record-this-command ()
    "Record command which opened the minibuffer.
We record this because it will be the default action.
This function is meant to be added to `minibuffer-setup-hook'."
    (setq-local embark--command this-command))
  (add-hook 'minibuffer-setup-hook #'embark--record-this-command))

;;; internal variables

(defvar-local embark-collect--kind nil
  "Kind of current collect buffer.

There are three kinds:
- :snapshot, which does not auto-update
- :live, which does
- :completions, which also auto-updates, but is ephemeral.")

(define-obsolete-variable-alias
  'embark-occur-candidates
  'embark-collect-candidates
  "0.10")

(defvar-local embark-collect-candidates nil
  "List of candidates in current collect buffer.")

(define-obsolete-variable-alias
  'embark-occur-view
  'embark-collect-view
  "0.10")

(defvar-local embark-collect-view 'list
  "Type of view in collect buffer: `list' or `grid'.")

(define-obsolete-variable-alias
  'embark-occur-from
  'embark-collect-from
  "0.10")

(defvar-local embark-collect-from nil
  "The buffer `embark-collect' was called from.")

(define-obsolete-variable-alias
  'embark-occur-linked-buffer
  'embark-collect-linked-buffer
  "0.10")

(defvar-local embark-collect-linked-buffer nil
  "Buffer local variable indicating which Embark Buffer to update.")

(define-obsolete-variable-alias
  'embark-occur-annotator
  'embark-collect-annotator
  "0.10")

(defvar-local embark-collect-annotator nil
  "Annotation function of minibuffer session for this collect.")

(defvar-local embark--collect-live--timer nil
  "Timer scheduled to update Embark Collect Live buffer.")

;;; core functionality

(defun embark--metadata ()
  "Return current minibuffer completion metadata."
  (completion-metadata
   (buffer-substring-no-properties (field-beginning) (point))
   minibuffer-completion-table
   minibuffer-completion-predicate))

(defun embark-target-active-region ()
  "Target the region if active."
  (when (use-region-p) '(region . <region>)))

(defun embark-target-file-at-point ()
  "Target file at point."
  (when-let ((file (ffap-file-at-point)))
    (cons 'file file)))

(defun embark-target-url-at-point ()
  "Target the URL at point."
  (when-let ((url (ffap-url-at-point)))
    (cons 'url url)))

(defun embark-target-identifier-at-point ()
  "Target identifier at point.

In Emacs Lisp buffers the identifier is promoted to a symbol, for
which more actions are available.  Identifiers are also promoted
to symbols if they are interned Emacs Lisp symbols and found in a
buffer whose major mode does not inherit from `prog-mode'.

As a convenience, in Org Mode surrounding == or ~~ are removed."
  (when-let ((name (thing-at-point 'symbol)))
    (when (and (derived-mode-p 'org-mode)
               (string-match-p "^\\([~=]\\).*\\1$" name))
      (setq name (substring name 1 -1)))
    (cons (if (or (derived-mode-p 'emacs-lisp-mode)
                  (and (intern-soft name)
                       (not (derived-mode-p 'prog-mode))))
              'symbol
            'identifier)
          name)))

(defun embark-target-top-minibuffer-completion ()
  "Target the top completion candidate in the minibuffer.
Return the category metadatum as the type of the target."
  (when (minibufferp)
    (let ((contents (minibuffer-contents)))
      (cons
       (completion-metadata-get (embark--metadata) 'category)
       (if (test-completion contents
                            minibuffer-completion-table
                            minibuffer-completion-predicate)
           contents
         (let ((completions (completion-all-sorted-completions)))
           (if (null completions)
               contents
             (concat
              (substring contents 0 (or (cdr (last completions)) 0))
              (car completions)))))))))

(define-obsolete-function-alias
  'embark-target-occur-candidate
  'embark-target-collect-candidate
  "0.10")

(defun embark-target-collect-candidate ()
  "Target the collect candidate at point."
  (when (derived-mode-p 'embark-collect-mode)
    (when-let ((button (button-at (point)))
               (label (button-label button)))
      (cons embark--type
            (if (eq embark--type 'file)
                (abbreviate-file-name (expand-file-name label))
              label)))))

(defun embark-target-completion-at-point (&optional relative)
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
          (cons embark--type
                (if (and (eq embark--type 'file) (not relative))
                    (abbreviate-file-name (expand-file-name raw))
                  raw)))))))

(defvar embark-general-map)             ; forward declarations
(defvar embark-meta-map)

(defun embark--action-keymap (type)
  "Return action keymap for targets of given TYPE."
  (make-composed-keymap
   (or embark-overriding-keymap
        (symbol-value (alist-get type embark-keymap-alist)))
   (if (eq type 'region)
       embark-meta-map
     (make-composed-keymap
      `(keymap (13 . ,(or embark--default-action embark--command)))
      embark-general-map))))

(defun embark--show-indicator (indicator keymap target)
  "Show INDICATOR for a pending action or a instance of becoming.
If INDICATOR is a string it is used as a format string and %s is
replaced by the target.  If the minibuffer is active, the
formatted string is put in an overlay in the minibuffer prompt
and the overlay is returned so it can be deleted when the
indicator is no longer needed.  If the minibuffer is inactive,
then the formatted string is shown in the echo area and returned.

If INDICATOR is a cons of two strings, they are used as format
strings as described above: if the minibuffer is active the first
string is used and if not, the second is used.

Finally, if INDICATOR is a function, this function is called with
the KEYMAP and TARGET.* The function should return either nil, or
a function to be called when the indicator is no longer needed.

*For a limited time only, if the function only accepts one
argument, it is called with the KEYMAP but a warning is displayed
saying you should update it to also accept the TARGET.
Eventually only the two argument case will be supported."
  (cond
   ((functionp indicator)
    (if target
        (condition-case nil
            (funcall indicator keymap target)
          (wrong-number-of-arguments
           (message "Update your `embark-action-indicator' to accept \
the keymap and target as arguments")
           (funcall indicator keymap)))
      (funcall indicator keymap)))
   ((or (stringp indicator) (consp indicator))
    (let* ((mini (active-minibuffer-window))
           (ind (format (if (consp indicator)
                            (if mini (car indicator) (cdr indicator))
                          indicator)
                        target)))
      (if mini
          (let ((indicator-overlay
                 (make-overlay (point-min) (point-min)
                               (window-buffer mini) t t)))
            (overlay-put indicator-overlay 'before-string (concat ind " "))
            indicator-overlay)
        (message ind))))))

(defun embark-keymap-prompter (keymap)
  "Let the user choose an action using the bindings in KEYMAP.
Besides the bindings in KEYMAP, the user is free to use all their
keybindings and even \\[execute-extended-command] to select a command."
  (let* ((key (let ((overriding-terminal-local-map keymap))
                (read-key-sequence nil)))
         (cmd (let ((overriding-terminal-local-map keymap))
                (key-binding key))))
    (setq cmd
          (pcase cmd
            ((or 'minibuffer-keyboard-quit 'abort-recursive-edit)
             nil)
            ('self-insert-command
             (minibuffer-message "Not an action")
             (embark-keymap-prompter keymap))
            ((or 'universal-argument 'negative-argument 'digit-argument)
             (let ((last-command-event (aref key 0)))
               (command-execute cmd))
             (embark-keymap-prompter keymap))
            ('execute-extended-command
             (intern-soft (read-extended-command)))
            ('embark-keymap-help
             (embark-completing-read-prompter keymap))
            (_ cmd)))
    cmd))

(defun embark-completing-read-prompter (keymap)
  "Prompt via completion for a command bound in KEYMAP."
  (let* ((commands
          (cl-loop for (key . cmd) in (embark--all-bindings keymap)
                   for name = (concat
                               (or
                                (when (symbolp cmd) (symbol-name cmd))
                                (when-let ((doc (documentation cmd)))
                                  (save-match-data
                                    (when (string-match "^\\(.*\\)$" doc)
                                      (match-string 1 doc))))
                                "<unnamed>"))
                   do (add-text-properties
                       0 1
                       `(display
                         ,(format "%-3s %s"
                                  (propertize key 'face 'embark-keybinding)
                                  (substring name 0 1)))
                       name)
                   unless (eq cmd 'embark-keymap-help)
                   collect (cons name cmd))))
    (cdr
     (assoc
      (minibuffer-with-setup-hook
          (lambda ()
            (use-local-map
             (make-composed-keymap
              (let ((map (make-sparse-keymap)))
                (define-key map "@"
                  (lambda ()
                    (interactive)
                    (message "Action key:")
                    (when-let ((cmd (embark-keymap-prompter keymap)))
                      (delete-minibuffer-contents)
                      (insert (symbol-name cmd))
                      (add-hook 'post-command-hook #'exit-minibuffer nil t))))
                map)
              (current-local-map))))
        (completing-read
         "Command: "
         (lambda (string predicate action)
           (if (eq action 'metadata)
               `(metadata (category . command))
             (complete-with-action action commands string predicate)))
         nil t))
      commands))))

(defun embark--with-indicator (indicator prompter keymap &optional target)
  "Display INDICATOR while calling PROMPTER with KEYMAP.
The optional argument TARGET is displayed for actions outside the
minibuffer."
  (let* ((remove-indicator (embark--show-indicator indicator keymap target))
         (cmd (condition-case nil
                  (minibuffer-with-setup-hook
                      ;; if the prompter opens its own minibuffer, show
                      ;; the indicator there too (don't bother with
                      ;; removing it since the whole recursive
                      ;; minibuffer disappears)
                      (lambda ()
                        ;; if the outer embark--show-indicator decided
                        ;; to display a message, remove-indicator is a
                        ;; string containing the message, which we use
                        (embark--show-indicator (if (stringp remove-indicator)
                                                    remove-indicator
                                                  indicator)
                                                keymap
                                                target))
                    (let ((enable-recursive-minibuffers t))
                      (funcall prompter keymap)))
                (quit nil))))
    (cond
     ((overlayp remove-indicator) (delete-overlay remove-indicator))
     ((functionp remove-indicator) (funcall remove-indicator)))
    cmd))

(defun embark--act (action target &optional quit)
  "Perform ACTION injecting the TARGET.
If called from a minibuffer with non-nil QUIT, quit the
minibuffer before executing the action."
  (if (memq action '(embark-become        ; these actions should not
                     embark-collect-live  ; run in the target window
                     embark-collect-snapshot
                     embark-export))
      (command-execute action)
    (let* ((command embark--command)
           (prefix prefix-arg)
           (action-window (embark--target-window t))
           (setup-hook (or (alist-get action embark-setup-overrides)
                           embark-setup-hook))
           (allow-edit (if embark-allow-edit-default
                           (not (memq action embark-skip-edit-commands))
                         (memq action embark-allow-edit-commands)))
           (inject (if (not (stringp target)) ; for region actions
                       #'ignore
                     (lambda ()
                       (delete-minibuffer-contents)
                       (insert target)
                       (let ((embark-setup-hook setup-hook))
                         (run-hooks 'embark-setup-hook))
                       (unless allow-edit
                         (add-hook 'post-command-hook #'exit-minibuffer nil t)))))
           (run-action (lambda ()
                         (minibuffer-with-setup-hook inject
                           (with-selected-window action-window
                             (run-hooks 'embark-pre-action-hook)
                             (let ((enable-recursive-minibuffers t)
                                   (embark--command command)
                                   (prefix-arg prefix)
                                   (use-dialog-box nil) ; avoid mouse dialogs
                                   (last-nonmenu-event 13)) ; avoid mouse dialogs
                               (command-execute action))
                             (run-hooks 'embark-post-action-hook))))))
      (if (not (and quit (minibufferp)))
          (funcall run-action)
        (run-at-time 0 nil run-action)
        (abort-recursive-edit)))))

(defun embark-refine-symbol-type (target)
  "Refine symbol TARGET to command or variable if possible."
  (cons (or (when-let ((symbol (intern-soft target)))
              (cond
               ((commandp symbol) 'command)
               ((boundp symbol) 'variable)))
            'symbol)
        target))

(defun embark-lookup-lighter-minor-mode (target)
  "If TARGET is a lighter, look up its minor mode.

The `describe-minor-mode' command has as completion candidates
both minor-modes and their lighters.  This function replaces the
lighters by their minor modes, so actions expecting a function
work on them."
  (cons 'minor-mode
        (let ((symbol (intern-soft target)))
          (if (and symbol (boundp symbol))
              target
            (symbol-name (lookup-minor-mode-from-indicator target))))))

(defun embark-set-xref-location-default-action (target)
  "Set `embark-goto-location' as the default action for TARGET."
  (setq embark--default-action 'embark-goto-location)
  (cons 'xref-location target))

(defun embark--target ()
  "Retrieve current target.

An initial guess at the current target and its type is determined
by running the functions in `emark-target-finders' until one
returns a non-nil result.  Each function should either a pair of
a type symbol and a target string, or nil.

The initial type is then looked up as a key in the variable
`embark-transformer-alist'.  If there is a transformer for
the type, it is called with the initial target, and must return a
`cons' of the transformed type and target."
  (pcase-let* ((`(,type . ,target)
                (run-hook-with-args-until-success 'embark-target-finders))
               (transformer (alist-get type embark-transformer-alist)))
    (if transformer
        (funcall transformer target)
      (cons type target))))

;;;###autoload
(defun embark-act (&optional arg)
  "Prompt the user for an action and perform it.
The target of the action is chosen by `embark-target-finders'.
By default, if called from a minibuffer the target is the top
completion candidate, if called from an Embark Collect or a
Completions buffer it is the candidate at point.

This command uses `embark-prompter' to ask the user to specify an
action, and calls it injecting the target at the first minibuffer
prompt.

If you call this from the minibuffer, it can optionally quit the
minibuffer.  The variable `embark-quit-after-action' controls
whether calling `embark-act' with nil ARG quits the minibuffer,
and if ARG is non-nil it will do the opposite.  Interactively,
ARG is the prefix argument."
  (interactive "P")
  (pcase-let* ((`(,type . ,target) (embark--target))
               (action (embark--with-indicator embark-action-indicator
                                               embark-prompter
                                               (embark--action-keymap type)
                                               target)))
    (if (null action)
        (minibuffer-message "Canceled")
      (embark--act action target (if embark-quit-after-action (not arg) arg)))))

(defun embark--become-keymap ()
  "Return keymap of commands to become for current command."
  (make-composed-keymap
   (cl-loop for keymap-name in embark-become-keymaps
            for keymap = (symbol-value keymap-name)
            when (where-is-internal embark--command (list keymap))
            collect keymap)
   embark-meta-map))

;;;###autoload
(defun embark-become (&optional full)
  "Make current command become a different command.
Take the current minibuffer input as initial input for new
command.  The new command can be run normally using keybindings or
\\[execute-extended-command], but if the current command is found in a keymap in
`embark-become-keymaps', that keymap is activated to provide
convenient access to the other commands in it.

If FULL is non-nil (interactively, if called with a prefix
argument), the entire minibuffer contents are used as the initial
input of the new command.  By default only the part of the
minibuffer contents between the current completion boundaries is
taken.  What this means is fairly technical, but (1) usually
there is no difference: the completion boundaries include the
entire minibuffer contents, and (2) the most common case where
these notions differ is file completion, in which case the
completion boundaries single out the path component containing
point."
  (interactive "P")
  (when (minibufferp)
    (let ((target (if full
                      (minibuffer-contents)
                    (pcase-let ((`(,beg . ,end) (embark--boundaries)))
                      (substring (minibuffer-contents) beg
                                 (+ end (- (point) (minibuffer-prompt-end)))))))
          (become (embark--with-indicator embark-become-indicator
                                          embark-prompter
                                          (embark--become-keymap))))
      (if (null become)
          (minibuffer-message "Canceled")
        (run-at-time 0 nil (lambda ()
                               (minibuffer-with-setup-hook
                                   (lambda ()
                                     (delete-minibuffer-contents)
                                     (insert target))
                                 (let ((use-dialog-box nil)
                                       (this-command become))
                                   (command-execute become)))))
        (abort-recursive-edit)))))

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

;;; embark collect

(defgroup embark-collect nil
  "Buffers for acting on collected Embark targets"
  :group 'embark)

(defcustom embark-candidate-collectors
  '(embark-minibuffer-candidates
    embark-completions-buffer-candidates
    embark-dired-candidates
    embark-ibuffer-candidates
    embark-embark-collect-candidates)
  "List of functions that collect all candidates in a given context.
These are used to fill an Embark Collect buffer.  Each function
should return either nil (to indicate it found no candidates) or
a list whose first element is a symbol indicating the type of
candidates and whose `cdr' is the list of candidates, each of
which should be a string."
  :type 'hook)

(define-obsolete-variable-alias
  'embark-occur-initial-view-alist
  'embark-collect-initial-view-alist
  "0.10")

(defcustom embark-collect-initial-view-alist
  '((file . grid)
    (buffer . grid)
    (symbol . list)
    (xref-location . list)
    (t . list))
  "Initial views for Embark Collect buffers by type.
This is an alist associating completion types to either `list',
`grid' or `zebra' (which means list view the Embark Collect Zebra
minor mode activated).  Additionally you can associate t to a
default initial view for types not mentioned separately."
  :type '(alist :key-type symbol
                :value-type (choice (const :tag "List view" list)
                                    (const :tag "Grid view" grid)
                                    (const :tag "List with Zebra stripes" zebra))))

(defcustom embark-exporters-alist
  '((buffer . embark-export-ibuffer)
    (file . embark-export-dired)
    (package . embark-export-list-packages)
    (xref-location . embark-export-grep)
    (bookmark . embark-export-bookmarks)
    (t . embark-collect-snapshot))
  "Alist associating completion types to export functions.
Each function should take a list of strings which are candidates
for actions and make a buffer appropriate to manage them.  For
example, the default is to make a dired buffer for files, and an
ibuffer for buffers.

The key t is also allowed in the alist, and the corresponding
value indicates the default function to use for other types.  The
default is `embark-collect-snapshot'."
  :type '(alist :key-type symbol :value-type function))

(defvar embark-overriding-export-function nil
  "Can be bound to short circuit `embark-exporters-alist'.
The expected format is the same as for functions in
`embark-exporters-alist'.")

(define-obsolete-variable-alias
  'embark-live-occur-update-delay
  'embark-collect-live-update-delay
  "0.10")

(defcustom embark-collect-live-update-delay 0.15
  "Wait this long for more input before updating Embark Collect Live buffer."
  :type 'number)

(define-obsolete-variable-alias
  'embark-live-occur-initial-delay
  'embark-collect-live-initial-delay
  "0.10")

(defcustom embark-collect-live-initial-delay 0.3
  "Wait this long for input before popping up Embark Collect Live buffer."
  :type 'number)

(define-obsolete-variable-alias
  'embark-occur-candidate
  'embark-collect-candidate
  "0.10")

(defface embark-collect-candidate '((t :inherit default))
  "Face for candidates in Embark Collect.")

(define-obsolete-variable-alias
  'embark-occur-zebra-highlight
  'embark-collect-zebra-highlight
  "0.10")

(defface embark-collect-zebra-highlight
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#efefef")
    (((class color) (min-colors 88) (background dark))
     :background "#242424"))
  "Face to highlight alternate rows in `embark-collect-zebra-minor-mode'")

(define-obsolete-variable-alias
  'embark-occur-annotation
  'embark-collect-annotation
  "0.10")

(defface embark-collect-annotation '((t :inherit completions-annotations))
  "Face for annotations in Embark Collect.
This is only used for annotation that are not already fontified.")

(define-obsolete-variable-alias
  'embark-occur-post-revert-hook
  'embark-collect-post-revert-hook
  "0.10")

(defcustom embark-collect-post-revert-hook nil
  "Hook run after an Embark Collect buffer is updated."
  :type 'hook)

(defun embark-collect--post-revert (&rest _)
  "Run `embark-collect-post-revert-hook'.
This function is used as :after advice for `tabulated-list-revert'."
  (when (derived-mode-p 'embark-collect-mode)
    (run-hooks 'embark-collect-post-revert-hook)))

(advice-add 'tabulated-list-revert :after #'embark-collect--post-revert)

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
      (cons
       (completion-metadata-get (embark--metadata) 'category)
       all))))

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
        (cons 'file (nreverse files))))))

(autoload 'ibuffer-map-lines-nomodify "ibuffer")

(defun embark-ibuffer-candidates ()
  "Return names of buffers listed in ibuffer buffer."
  (when (derived-mode-p 'ibuffer-mode)
    (let (buffers)
      (ibuffer-map-lines-nomodify
       (lambda (buffer _mark)
         (push (buffer-name buffer) buffers)))
      (cons 'buffer (nreverse buffers)))))

(define-obsolete-function-alias
  'embark-embark-occur-candidates
  'embark-embark-collect-candidates
  "0.10")

(defun embark-embark-collect-candidates ()
  "Return candidates in Embark Collect buffer.
This makes `embark-export' work in Embark Collect buffers."
  (when (derived-mode-p 'embark-collect-mode)
    (cons embark--type embark-collect-candidates)))

(defun embark-completions-buffer-candidates ()
  "Return all candidates in a completions buffer."
  (when (derived-mode-p 'completion-list-mode)
    (cons
     embark--type
     (save-excursion
       (goto-char (point-min))
       (next-completion 1)
       (let (all)
         (while (not (eobp))
           ;; TODO next line looks a little funny now
           (push (cdr (embark-target-completion-at-point 'relative-path)) all)
           (next-completion 1))
         (nreverse all))))))

(defun embark--action-command (action)
  "Turn an ACTION into a command to perform the action.
Returns the name of the command."
  (let ((name (intern (format "embark-action--%s" action)))
        (fn (lambda ()
              (interactive)
              (embark--act action (cdr (embark--target))))))
    (fset name fn)
    (when (symbolp action)
      (put name 'function-documentation
           (documentation action)))
    name))

(defun embark--all-bindings (keymap)
  "Return an alist of all bindings in KEYMAP."
  (let (bindings)
    (map-keymap
     (lambda (key def)
       (let ((desc (single-key-description key)))
         (cond
          ((null def))
          ((keymapp def)
           (dolist (bind (embark--all-bindings def))
             (push (cons (concat desc " " (car bind))
                         (cdr bind))
                   bindings)))
          (t (push (cons desc def) bindings)))))
     keymap)
    (nreverse bindings)))

(define-obsolete-variable-alias
  'embark-occur-direct-action-minor-mode-map
  'embark-collect-direct-action-minor-mode-map
  "0.10")

(defvar embark-collect-direct-action-minor-mode-map (make-sparse-keymap)
  "Keymap for direct bindings to embark actions.")

(define-obsolete-function-alias
  'embark-occur-direct-action-minor-mode
  'embark-collect-direct-action-minor-mode
  "0.10")

(define-minor-mode embark-collect-direct-action-minor-mode
  "Bind type-specific actions directly (without need for `embark-act')."
  :init-value nil
  :lighter " Act"
  :keymap embark-collect-direct-action-minor-mode-map
  (when embark-collect-direct-action-minor-mode
    ;; must mutate keymap, not make new one
    (let ((map embark-collect-direct-action-minor-mode-map))
      (setcdr map nil)
      (cl-loop for (key . cmd) in (embark--all-bindings
                                   (embark--action-keymap embark--type))
               unless (eq cmd 'embark-keymap-help)
               do (define-key map (kbd key) (embark--action-command cmd))))))

(define-button-type 'embark-collect-entry
  'face 'embark-collect-candidate
  'action 'embark-collect-choose)

(defun embark--boundaries ()
  "Get current minibuffer completion boundaries."
  (let ((contents (minibuffer-contents))
        (pt (- (point) (minibuffer-prompt-end))))
    (completion-boundaries
     (substring contents 0 pt)
     minibuffer-completion-table
     minibuffer-completion-predicate
     (substring contents pt))))

(define-obsolete-function-alias
  'embark-occur-choose
  'embark-collect-choose
  "0.10")

(defun embark-collect-choose (entry)
  "Select a completion or run default action on Embark Collect ENTRY.

If the current buffer is an Embark Collect Completions buffer,
complete the minibuffer input to ENTRY and, unless this leads to
new completion candidates (for example, when entering a directory
in `find-file') or the command was called with a prefix argument,
exit the minibuffer.

For other Embark Collect buffers, run the default action on ENTRY."
  (let ((text (button-label entry)))
    (if (and (eq embark-collect--kind :completions))
        (progn
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
      (embark--act (or embark--default-action embark--command) text))))

(make-obsolete 'embark-occur-mode-map 'embark-collect-mode-map "0.10")

(embark-define-keymap embark-collect-mode-map
  "Keymap for Embark collect mode."
  ("a" embark-act)
  ("A" embark-collect-direct-action-minor-mode)
  ("z" embark-collect-zebra-minor-mode)
  ("M-q" embark-collect-toggle-view)
  ("v" embark-collect-toggle-view)
  ("e" embark-export)
  ("s" isearch-forward)
  ("f" forward-button)
  ("b" backward-button)
  ("<right>" forward-button)
  ("<left>" backward-button))

(define-obsolete-function-alias
  'embark-occur-mode
  'embark-collect-mode
  "0.10")

(define-derived-mode embark-collect-mode tabulated-list-mode "Embark Collect"
  "List of candidates to be acted on.
The command `embark-act' is bound `embark-collect-mode-map', but
you might prefer to change the keybinding to match your other
keybinding for it.  Or alternatively you might want to enable
`embark-collect-direct-action-minor-mode' in
`embark-collect-mode-hook'.")

(defun embark-collect--max-width ()
  "Maximum width of any Embark Collect candidate."
  ;; TODO take into account display properties
  (or (cl-loop for cand in embark-collect-candidates
               maximize (length cand))
      0))

(defun embark-collect--list-view ()
  "List view of candidates and annotations for Embark Collect buffer."
  (setq tabulated-list-format
        (if embark-collect-annotator
            (let ((width (embark-collect--max-width)))
              `[("Candidate" ,width t) ("Annotation" 0 t)])
          [("Candidate" 0 t)]))
  (if tabulated-list-use-header-line
      (tabulated-list-init-header)
    (setq header-line-format nil tabulated-list--header-string nil))
  (setq tabulated-list-entries
        (if embark-collect-annotator
            (let ((dir default-directory) ; smuggle to the target window
                  (annotator embark-collect-annotator)
                  (candidates embark-collect-candidates))
              (with-current-buffer (embark--target-buffer)
                (let ((default-directory dir)) ; for marginalia's file annotator
                  (mapcar
                   (lambda (cand)
                     (let* ((annotation (or (funcall annotator cand) ""))
                            (length (length annotation))
                            (facesp (text-property-not-all
                                     0 length 'face nil annotation)))
                       (when facesp (add-face-text-property
                                     0 length 'default t annotation))
                       `(,cand [(,cand type embark-collect-entry)
                                (,annotation
                                 ,@(unless facesp
                                     '(face embark-collect-annotation)))])))
                   candidates))))
          (mapcar
           (lambda (cand)
             `(,cand [(,cand type embark-collect-entry)]))
           embark-collect-candidates))))

(defun embark-collect--remove-zebra-stripes ()
  "Remove highlighting of alternate rows."
  (remove-overlays nil nil 'face 'embark-collect-zebra-highlight))

(defun embark-collect--add-zebra-stripes ()
  "Highlight alternate rows with the `embark-collect-highlight-row' face."
  (embark-collect--remove-zebra-stripes)
  (save-excursion
    (goto-char (point-min))
    (when (overlays-at (point)) (forward-line))
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
                         'face 'embark-collect-zebra-highlight)))))))

(define-obsolete-function-alias
  'embark-occur-zebra-minor-mode
  'embark-collect-zebra-minor-mode
  "0.10")

(define-minor-mode embark-collect-zebra-minor-mode
  "Minor mode to highlight alternate rows in an Embark Collect buffer.
This is specially useful to tell where multi-line entries begin and end."
  :init-value nil
  :lighter " Zebra"
  (if embark-collect-zebra-minor-mode
      (progn
        (add-hook 'embark-collect-post-revert-hook
                  #'embark-collect--add-zebra-stripes nil t)
        (embark-collect--add-zebra-stripes))
    (remove-hook 'embark-collect-post-revert-hook
                 #'embark-collect--add-zebra-stripes t)
    (embark-collect--remove-zebra-stripes)))

(defun embark-collect--grid-view ()
  "Grid view of candidates for Embark Collect buffer."
  (let* ((width (min (+ (embark-collect--max-width) 2) (floor (window-width) 2)))
         (columns (/ (window-width) width)))
    (setq tabulated-list-format
          (make-vector columns `("Candidate" ,width nil)))
    (if tabulated-list-use-header-line
        (tabulated-list-init-header)
      (setq header-line-format nil tabulated-list--header-string nil))
    (setq tabulated-list-entries
          (cl-loop with cands = (copy-tree embark-collect-candidates)
                   while cands
                   collect
                   (list nil
                         (apply #'vector
                                (cl-loop repeat columns
                                         collect
                                         `(,(or (pop cands) "")
                                           type embark-collect-entry))))))))

(defun embark-collect--revert ()
  "Recalculate Embark Collect candidates if possible."
  (when (buffer-live-p embark-collect-from)
    (pcase-let ((`(,type . ,candidates)
                 (with-current-buffer embark-collect-from
                   (run-hook-with-args-until-success
                    'embark-candidate-collectors))))
      (setq embark--type type
            embark-collect-candidates candidates
            default-directory
            (with-current-buffer embark-collect-from
              (embark--default-directory)))))
  (setq embark-collect-annotator
        (or
         ;; for the active minibuffer, get annotation-function metadatum
         (when-let ((miniwin (active-minibuffer-window)))
           (when (eq (window-buffer miniwin) embark-collect-from)
             (or (completion-metadata-get (embark--metadata)
                                          'annotation-function)
                 (plist-get completion-extra-properties
                            :annotation-function))))
         ;; fallback on Marginalia if loaded
         (when (boundp 'marginalia-annotators)
           (alist-get embark--type (symbol-value
                                    (car marginalia-annotators))))))
  (if (eq embark-collect-view 'list)
      (embark-collect--list-view)
    (embark-collect--grid-view)))

(defun embark-collect--update-linked (&rest _)
  "Update linked Embark Collect buffer."
  (when-let ((collect-buffer embark-collect-linked-buffer))
    (when embark--collect-live--timer
      (cancel-timer embark--collect-live--timer))
    (setq embark--collect-live--timer
          (run-with-idle-timer
           embark-collect-live-update-delay nil
           (lambda ()
             (let ((non-essential t))
               (while-no-input
                 (when (buffer-live-p collect-buffer) ; might be killed by now
                   (with-current-buffer collect-buffer
                     (revert-buffer))))))))))

(defun embark-collect--toggle (variable this that)
  "Toggle Embark Collect buffer's local VARIABLE between THIS and THAT.
Refresh the buffer afterwards."
  (when-let ((buffer (if (derived-mode-p 'embark-collect-mode)
                         (current-buffer)
                       embark-collect-linked-buffer)))
    (with-current-buffer buffer
      (set variable
           (if (eq (buffer-local-value variable buffer) this) that this))
      (revert-buffer))))

(define-obsolete-function-alias
  'embark-occur-toggle-view
  'embark-collect-toggle-view
  "0.10")

(defun embark-collect-toggle-view ()
  "Toggle between list and grid views of Embark Collect buffer.
This command can be called either from the Embark Collect buffer
itself, or, from any buffer (particularly a minibuffer) that has
a linked Embark Collect Live buffer."
  (interactive)
  (embark-collect--toggle 'embark-collect-view 'list 'grid))

(define-obsolete-function-alias
  'embark-occur-toggle-header
  'embark-collect-toggle-header
  "0.10")

(defun embark-collect-toggle-header ()
  "Toggle the visibility of the header line of Embark Collect buffer.
This command can be called either from the Embark Collect buffer
itself, or, from any buffer (particularly a minibuffer) that has
a linked Embark Collect Live buffer."
  (interactive)
  (embark-collect--toggle 'tabulated-list-use-header-line t nil))

(defun embark-collect--initial-view-arg ()
  "Translate current prefix arg to intial Embark Collect view.
\\[universal-argument] means grid view, a prefix argument of 1
means list view, anything else means proceed according to
`embark-collect-initial-view-alist'."
  (list (pcase current-prefix-arg
          ('(4) 'grid)
          (1 'list))))

(defun embark--reuse-collect-completions-window (buffer alist)
  "Reuse an Embark Collect Completions window to display BUFFER.
ALIST comes from the action argument of `display-buffer'."
  (cl-loop for window in (window-list-1 nil 'no-minibuffer)
           when (and (window-live-p window)
                     (eq (buffer-local-value 'embark-collect--kind
                                             (window-buffer window))
                         :completions))
           return (window--display-buffer buffer window 'reuse alist)))

(defun embark--collect (name initial-view kind)
  "Create and display an Embark collect buffer of given KIND.
The buffer is put in INITIAL-VIEW and given the specified NAME.
The KIND can be :completions, :live or :snapshot.
Both :completions and :live buffer auto-update.  Additonally,
:completions buffers will be displayed in a dedicated window
at the bottom of the frame and are automatically killed when
the minibuffer is exited."
  (pcase-let ((from (current-buffer))
              (buffer (generate-new-buffer name))
              (`(,type . ,candidates)
               (run-hook-with-args-until-success 'embark-candidate-collectors)))
    (setq embark-collect-linked-buffer buffer)
    (with-current-buffer buffer
      ;; we'll run the mode hooks once the buffer is displayed, so
      ;; the hooks can make use of the window
      (delay-mode-hooks (embark-collect-mode))

      (setq embark-collect--kind kind)

      (setq tabulated-list-use-header-line nil) ; default to no header

      (unless (eq kind :snapshot)
        ;; setup live updating
        (with-current-buffer from
          (add-hook 'after-change-functions
                    #'embark-collect--update-linked nil t)))

      (unless (and (minibufferp from) (eq kind :snapshot))
        ;; for a snapshot of a minibuffer, don't link back to minibuffer:
        ;; they can get recycled and if so revert would do the wrong thing
        (setq embark-collect-from from))

      (setq embark--type type)
      (setq embark-collect-candidates candidates)
      (add-hook 'tabulated-list-revert-hook #'embark-collect--revert nil t)

      (setq embark-collect-view
            (or initial-view
                (alist-get type embark-collect-initial-view-alist)
                (alist-get t embark-collect-initial-view-alist)
                'list))
      (when (eq embark-collect-view 'zebra)
        (setq embark-collect-view 'list)
        (embark-collect-zebra-minor-mode))

      (with-current-buffer from (embark--cache-info buffer)))

    (let ((window (display-buffer
                   buffer
                   (when (eq kind :completions)
                     '((embark--reuse-collect-completions-window
                        display-buffer-at-bottom))))))

      (with-selected-window window
        (run-mode-hooks)
        (revert-buffer))

      (set-window-dedicated-p window t)

      (when (minibufferp from)
        ;; A function added to `minibuffer-exit-hook' locally isn't called if
        ;; we `abort-recursive-edit' from outside the minibuffer, that is why
        ;; we use `change-major-mode-hook', which is also run on minibuffer
        ;; exit.
        (add-hook
         'change-major-mode-hook
         (pcase kind
           (:completions
            (lambda ()
              ;; Killing a buffer shown in a selected dedicated window will
              ;; set-buffer to a random buffer for some reason, so preserve it.
              (save-current-buffer
                (kill-buffer buffer))))
           (:live
            (lambda ()
              (setf (buffer-local-value 'embark-collect-from buffer) nil)
              (with-current-buffer buffer
                (save-match-data
                  (rename-buffer
                   (replace-regexp-in-string " Live" "" (buffer-name))
                   t)))
              (run-at-time 0 nil #'pop-to-buffer buffer)))
           (:snapshot
            (lambda () (run-at-time 0 nil #'pop-to-buffer buffer))))
         nil t)
        (setq minibuffer-scroll-window window))

      window)))

(define-obsolete-function-alias
  'embark-live-occur
  'embark-collect-live
  "0.10")

;;;###autoload
(defun embark-collect-live (&optional initial-view)
  "Create a live-updating Embark Collect buffer.
Optionally start in INITIAL-VIEW (either `list' or `grid')
instead of what `embark-collect-initial-view-alist' specifies.
Interactively, \\[universal-argument] means grid view, a prefix
argument of 1 means list view.

To control the display, add an entry to `display-buffer-alist'
with key \"Embark Collect Live\"."
  (interactive (embark-collect--initial-view-arg))
  (embark--collect "*Embark Collect Live*" initial-view :live))

(define-obsolete-function-alias
  'embark-occur
  'embark-collect-snapshot
  "0.10")

;;;###autoload
(defun embark-collect-snapshot (&optional initial-view)
  "Create an Embark Collect buffer.
Optionally start in INITIAL-VIEW (either `list' or `grid')
instead of what `embark-collect-initial-view-alist' specifies.
Interactively, \\[universal-argument] means grid view, a prefix
argument of 1 means list view.

To control the display, add an entry to `display-buffer-alist'
with key \"Embark Collect\"."
  (interactive (embark-collect--initial-view-arg))
  (embark--collect "*Embark Collect*" initial-view :snapshot)
  (when (minibufferp) (abort-recursive-edit)))

;;;###autoload
(defun embark-collect-completions ()
  "Create an ephemeral live-updating Embark Collect buffer."
  (interactive)
  (embark--collect "*Embark Collect Completions*" nil :completions))

(define-obsolete-function-alias
  'embark-live-occur-after-delay
  'embark-collect-completions-after-delay
  "0.10")

;;;###autoload
(defun embark-collect-completions-after-delay ()
  "Start `embark-collect-live' after `embark-collect-live-initial-delay'.
Add this function to `minibuffer-setup-hook' to have an Embark
Live Collect buffer popup every time you use the minibuffer."
  (when minibuffer-completion-table
    (run-with-idle-timer
     embark-collect-live-initial-delay nil
     (lambda () (when (minibufferp) (embark-collect-completions))))))

(defun embark--wait-for-input (_beg _end _len)
  "After input in the minibuffer, wait briefly and run `embark-collect-live'.
This is meant to be added to `after-change-functions' in the
minibuffer by the function `embark-collect-live-after-input', you
probably shouldn't use this function directly."
  (remove-hook 'after-change-functions 'embark--wait-for-input t)
  (embark-collect-completions-after-delay))

(define-obsolete-function-alias
  'embark-live-occur-after-input
  'embark-collect-completions-after-input
  "0.10")

;;;###autoload
(defun embark-collect-completions-after-input ()
  "Start `embark-collect-completions' after some minibuffer input.
Add this function to `minibuffer-setup-hook' to have an Embark
Live Collect buffer popup soon after you type something in the
minibuffer; the length of the delay after typing is given by
`embark-collect-live-initial-delay'."
  (when minibuffer-completion-table
   (add-hook 'after-change-functions #'embark--wait-for-input nil t)))

(define-obsolete-function-alias
  'embark-switch-to-live-occur
  'embark-switch-to-collect-completions
  "0.10")

;;;###autoload
(defun embark-switch-to-collect-completions ()
  "Switch to the Embark Collect Completions buffer, creating it if necessary."
  (interactive)
  (switch-to-buffer
   (if (and embark-collect-linked-buffer
            (eq (buffer-local-value 'embark-collect--kind
                                    embark-collect-linked-buffer)
                :completions))
       embark-collect-linked-buffer
     (or (get-buffer "*Embark Collect Completions*")
         (progn (embark-collect-completions) embark-collect-linked-buffer)))))

;;;###autoload
(defun embark-export ()
  "Create a type-specific buffer to manage current candidates.
The variable `embark-exporters-alist' controls how to make the
buffer for each type of completion."
  (interactive)
  (pcase-let ((`(,type . ,candidates)
               (run-hook-with-args-until-success 'embark-candidate-collectors)))
    (if (null candidates)
        (message "No candidates for export")
      (let ((exporter (or embark-overriding-export-function
                          (alist-get type embark-exporters-alist)
                          (alist-get t embark-exporters-alist)))
            (transformer (alist-get type embark-transformer-alist)))

        ;; check to see if all candidates transform to same type
        (when (and (not embark-overriding-export-function) transformer)
          (pcase-let* ((`(,new-type . ,first-cand)
                        (funcall transformer (car candidates))))
            (unless (eq type new-type)
              (when-let ((new-exporter
                          (alist-get new-type embark-exporters-alist))
                         (new-candidates (list first-cand)))
                (when (cl-every
                       (lambda (cand)
                         (pcase-let ((`(,t-type . ,t-cand)
                                      (funcall transformer cand)))
                           (when (eq t-type new-type)
                             (push t-cand new-candidates)
                             t)))
                       (cdr candidates))
                  (setq type new-type
                        exporter new-exporter
                        candidates (nreverse new-candidates)))))))

        (if (eq exporter 'embark-collect-snapshot)
            (embark-collect-snapshot)
          (let ((dir (embark--default-directory)))
            (run-at-time 0 nil
                         (lambda ()
                           (message nil)
                           (let ((default-directory dir)) ; dired needs this info
                             (funcall exporter candidates))))
            (abort-recursive-edit)))))))

(defun embark-export-ibuffer (buffers)
  "Create an ibuffer buffer listing BUFFERS."
  (ibuffer t "*Embark Export Ibuffer*"
           `((predicate . (member (buffer-name) ',buffers)))))

(autoload 'dired-check-switches "dired")

(defun embark-export-dired (files)
  "Create a dired buffer listing FILES."
  (setq files (mapcar #'directory-file-name
                      (cl-remove-if-not #'file-exists-p files)))
  (when (dired-check-switches dired-listing-switches "A" "almost-all")
    (setq files (cl-remove-if
                 (lambda (path)
                   (let ((file (file-name-nondirectory path)))
                     (or (string= file ".") (string= file ".."))))
                 files)))
  (dired (cons
          ;; TODO: is it worth finding the deepest common containing directory?
          (if (cl-every #'file-name-absolute-p files) "/" default-directory)
          files))
  (rename-buffer (format "*Embark Export Dired %s*" default-directory)))

(autoload 'package-menu-mode "package")
(autoload 'package-menu--generate "package")

(defun embark-export-list-packages (packages)
  "Create a package menu mode buffer listing PACKAGES."
  (let ((buf (generate-new-buffer "*Embark Export Packages*")))
    (with-current-buffer buf
      (package-menu-mode)
      (package-menu--generate nil (mapcar #'intern packages)))
    (switch-to-buffer buf)))

(defvar wgrep-header/footer-parser)
(declare-function wgrep-setup "wgrep")

(defun embark-export-grep (lines)
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

(defvar bookmark-alist)

(defun embark-export-bookmarks (bookmarks)
  "Create a bookmark-bmenu-mode buffer listing BOOKMARKS."
  (let ((bookmark-alist
         (cl-remove-if-not
          (lambda (bmark)
            (member (car bmark) bookmarks))
          bookmark-alist))
        (saved-buffer
         (embark-rename-buffer "*Bookmark List*" "*Saved Bookmark List*" t)))
    (bookmark-bmenu-list)
    (pop-to-buffer
     (embark-rename-buffer "*Bookmark List*" "*Embark Export Bookmarks*" t))
    (when saved-buffer
      (embark-rename-buffer saved-buffer "*Bookmark List*"))))

;;; integration with external completion UIs

;; selectrum

(declare-function selectrum--get-meta "selectrum")
(declare-function selectrum-get-current-candidate "selectrum")
(declare-function selectrum-get-current-candidates "selectrum")

(defun embark-target-selectrum-selection ()
  "Target the currently selected item in Selectrum.
Return the category metadatum as the type of the target."
  (when (bound-and-true-p selectrum-active-p)
    (cons (selectrum--get-meta 'category)
	  (selectrum-get-current-candidate))))

(defun embark-selectrum-candidates ()
  "Collect the current Selectrum candidates.
Return the category metadatum as the type of the candidates."
  (when (bound-and-true-p selectrum-active-p)
    (cons (selectrum--get-meta 'category)
	  (selectrum-get-current-candidates
	   ;; Pass relative file names for dired.
	   minibuffer-completing-file-name))))

(with-eval-after-load 'selectrum
  (add-hook 'embark-target-finders #'embark-target-selectrum-selection)
  (add-hook 'embark-candidate-collectors #'embark-selectrum-candidates))

;; ivy

(declare-function ivy--expand-file-name "ivy")
(declare-function ivy-state-current "ivy")
(defvar ivy-text)
(defvar ivy-last)
(defvar ivy--old-cands) ; this stores the current candidates :)
(defvar ivy--length)

(defun embark-target-ivy-selection ()
  "Target the currently selected item in Ivy.
Return the category metadatum as the type of the target."
  ;; my favorite way of detecting Ivy
  (when (eq mwheel-scroll-up-function 'ivy-next-line)
    (cons
     (completion-metadata-get (embark--metadata) 'category)
     (ivy--expand-file-name
      (if (and (> ivy--length 0)
               (stringp (ivy-state-current ivy-last)))
          (ivy-state-current ivy-last)
        ivy-text)))))

(defun embark-ivy-candidates ()
  "Return all current Ivy candidates."
  ;; my favorite way of detecting Ivy
  (when (eq mwheel-scroll-up-function 'ivy-next-line)
    (cons
     ;; swiper-isearch uses swiper-isearch-function as a completion
     ;; table, but it doesn't understand metadata queries
     (ignore-errors
       (completion-metadata-get (embark--metadata) 'category))
     ivy--old-cands)))

(with-eval-after-load 'ivy
  (add-hook 'embark-target-finders #'embark-target-ivy-selection)
  (add-hook 'embark-candidate-collectors #'embark-ivy-candidates))

;;; custom actions

(defun embark-keymap-help ()
  "Prompt for an action to perform or command to become and run it."
  (interactive)
  (user-error "Not meant to be called directly"))

(declare-function compile-goto-error "compile")

(defun embark-goto-location (location)
  "Go to LOCATION, which should be a string with a grep match."
  (interactive "sLocation: ")
  (with-temp-buffer
    (insert location "\n")
    (grep-mode)
    (goto-char (point-min))
    (compile-goto-error)))

(defalias 'embark-execute-command
  ;; this one is kind of embarrassing: embark-keymap-prompter gives
  ;; execute-extended-command special treatment, so we need a command
  ;; just like it... but with a different name!
  #'execute-extended-command)

(defun embark-insert (string)
  "Insert STRING at point."
  (interactive "sInsert: ")
  (insert (substring-no-properties string)))

(defun embark-save (string)
  "Save STRING in the kill ring."
  (interactive "sSave: ")
  (kill-new string))

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

(defun embark-rename-buffer (buffer newname &optional unique)
  "Rename BUFFER to NEWNAME, optionally making it UNIQUE.
Interactively, you can set UNIQUE with a prefix argument.
Returns the new name actually used."
  (interactive "bBuffer: \nBRename %s to: \nP")
  (when-let ((buf (get-buffer buffer)))
    (with-current-buffer buf
      (rename-buffer newname unique))))

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

(defun embark-isearch ()
  "Prompt for string in the minibuffer and start isearch.
Unlike isearch, this command reads the string from the
minibuffer, which means it can be used as an Embark action."
  (interactive)
  (isearch-mode t)
  (isearch-edit-string))

(defun embark-act-on-region-contents ()
  "Act on the contents of the region."
  (interactive)
  (let* ((contents (buffer-substring (region-beginning) (region-end)))
         (embark-target-finders
          (lambda ()
            (cons
             (if (string-match-p "\\`\\_<.*?\\_>\\'" contents)
                 (if (or (derived-mode-p 'emacs-lisp-mode)
                         (and (not (derived-mode-p 'prog-mode))
                              (intern-soft contents)))
                     'symbol
                   'identifier)
               'general)
             contents))))
    (deactivate-mark)
    (embark-act)))

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
      (add-hook 'post-command-hook #'exit-minibuffer nil t)
    (beginning-of-line)
    (insert "(")
    (end-of-line)
    (insert ")")
    (backward-char)))

;;; keymaps

(embark-define-keymap embark-meta-map
  "Keymap for non-action Embark functions."
  ("C-h" embark-keymap-help))

(embark-define-keymap embark-general-map
  "Keymap for Embark general actions."
  :parent embark-meta-map
  ("i" embark-insert)
  ("w" embark-save)
  ("E" embark-export)
  ("S" embark-collect-snapshot)
  ("L" embark-collect-live)
  ("B" embark-become)
  ("C-s" embark-isearch))

(autoload 'org-table-convert-region "org-table")

(embark-define-keymap embark-region-map
  "Keymap for Embark actions on the active region."
  ("RET" embark-act-on-region-contents)
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
  ("n" narrow-to-region))

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

(embark-define-keymap embark-identifier-map
  "Keymap for Embark identifier actions."
  ("h" display-local-help)
  ("d" xref-find-definitions))

(embark-define-keymap embark-symbol-map
  "Keymap for Embark symbol actions."
  ("h" describe-symbol)
  ("s" embark-info-lookup-symbol)
  ("d" embark-find-definition)
  ("b" where-is)
  ("e" eval-expression))

(embark-define-keymap embark-command-map
  "Keymap for Embark command actions."
  :parent embark-symbol-map
  ("x" embark-execute-command)
  ("I" Info-goto-emacs-command-node)
  ("g" global-set-key)
  ("l" local-set-key))

(embark-define-keymap embark-variable-map
  "Keymap for Embark variable actions."
  :parent embark-symbol-map
  ("=" set-variable)
  ("c" customize-set-variable)
  ("u" customize-variable))

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
  ("." find-file-at-point)
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
