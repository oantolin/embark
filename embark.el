;;; embark.el --- Conveniently act on minibuffer completions   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Maintainer: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: convenience
;; Version: 0.13
;; Homepage: https://github.com/oantolin/embark
;; Package-Requires: ((emacs "26.1"))

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
;; single-letter key bindings for common Emacs file commands, for
;; instance `c' is bound to `copy-file'.  This means that if while you
;; are in the minibuffer after running a command that prompts for a
;; file, such as `find-file' or `rename-file', you can copy a file by
;; running `embark-act' and then pressing `c'.

;; These action keymaps are very convenient but not strictly necessary
;; when using `embark-act': you can use any command that reads from the
;; minibuffer as an action and the target of the action will be inserted
;; at the first minibuffer prompt.  After running `embark-act' all of your
;; key bindings and even `execute-extended-command' can be used to run a
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

;;; User facing options

(defgroup embark nil
  "Emacs Mini-Buffer Actions Rooted in Keymaps."
  :group 'minibuffer)

(defcustom embark-keymap-alist
  '((file . embark-file-map)
    (library . embark-library-map)
    (environment-variables . embark-file-map) ; they come up in file completion
    (url . embark-url-map)
    (email . embark-email-map)
    (buffer . embark-buffer-map)
    (expression . embark-expression-map)
    (identifier . embark-identifier-map)
    (defun . embark-defun-map)
    (symbol . embark-symbol-map)
    (face . embark-face-map)
    (command . embark-command-map)
    (variable . embark-variable-map)
    (function . embark-function-map)
    (minor-mode . embark-command-map)
    (unicode-name . embark-unicode-name-map)
    (package . embark-package-map)
    (bookmark . embark-bookmark-map)
    (region . embark-region-map)
    (sentence . embark-sentence-map)
    (paragraph . embark-paragraph-map)
    (kill-ring . embark-kill-ring-map)
    (heading . embark-heading-map)
    (t . embark-general-map))
  "Alist of action types and corresponding keymaps.
For any type not listed here, `embark-act' will use
`embark-general-map'."
  :type '(alist :key-type symbol :value-type variable))

(defcustom embark-target-finders
  '(embark-target-top-minibuffer-completion
    embark-target-active-region
    embark-target-collect-candidate
    embark-target-completion-at-point
    embark-target-bug-reference-at-point
    embark-target-package-at-point
    embark-target-email-at-point
    embark-target-url-at-point
    embark-target-file-at-point
    embark-target-custom-variable-at-point
    embark-target-identifier-at-point
    embark-target-library-file-at-point
    embark-target-expression-at-point
    embark-target-sentence-at-point
    embark-target-paragraph-at-point
    embark-target-defun-at-point
    embark-target-heading-at-point)
  "List of functions to determine the target in current context.
Each function should take no arguments and return either nil to
indicate that no target has been found, a cons (type . target)
where type is a symbol and target is a string, or a triple of the
form (type target . bounds), where bounds is the (beg . end)
bounds pair of the target at point for highlighting."
  :type 'hook)

(defcustom embark-transformer-alist
  '((minor-mode . embark--lookup-lighter-minor-mode)
    (symbol . embark--refine-symbol-type)
    (embark-keybinding . embark--keybinding-command)
    (project-file . embark--project-file-full-path)
    (package . embark--remove-package-version))
  "Alist associating type to functions for transforming targets.
Each function should take a type and a target string and return a
pair of the form a `cons' of the new type and the new target."
  :type '(alist :key-type symbol :value-type function))

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

(defcustom embark-keymap-prompter-key "@"
  "Key to switch to the keymap prompter from `embark-completing-read-prompter'.

The key must be either a string or a vector.
This is the key representation accepted by `define-key'."
  :type '(choice key-sequence (const :tag "None" nil)))

(defcustom embark-cycle-key nil
  "Key used for `embark-cycle'.

If the key is set to nil it defaults to the global binding of
`embark-act'.  The key must be either a string or a vector.  This
is the key representation accepted by `define-key'."
  :type '(choice key-sequence (const :tag "Use embark-act key" nil)))

(defcustom embark-help-key "\C-h"
  "Key used for help.

The key must be either nil, a string or a vector.  This
is the key representation accepted by `define-key'."
  :type '(choice (const :tag "Use 'C-h'" "\C-h")
                 (const :tag "Use '?'" "?")
                 (const :tag "None" nil)
                 key-sequence))

(defcustom embark-keybinding-repeat
  (propertize "*" 'face 'embark-keybinding-repeat)
  "Indicator string for repeatable keybindings.
Keybindings are formatted by the `completing-read' prompter and
the verbose indicator."
  :type 'string)

(defface embark-keybinding-repeat
  '((t :inherit font-lock-builtin-face))
  "Face used to indicate keybindings as repeatable.")

(defface embark-keybinding '((t :inherit success))
  "Face used to display key bindings.
Used by `embark-completing-read-prompter' and `embark-keymap-help'.")

(defface embark-keymap '((t :slant italic))
  "Face used to display keymaps.
Used by `embark-completing-read-prompter' and `embark-keymap-help'.")

(defface embark-target '((t :inherit highlight))
  "Face used to highlight the target at point during `embark-act'.")

(make-obsolete 'embark-indicator
               "see the new `embark-indicators' variable."
               "0.12")

(defcustom embark-indicators
  '(embark-mixed-indicator
    embark-highlight-indicator
    embark-isearch-highlight-indicator)
  "Indicator functions to use when acting or becoming.
The indicator functions are called from both `embark-act' and
from `embark-become' and should display information about this to
the user, such as: which of those two commands is running; a
description of the key bindings that are available for actions or
commands to become; and, in the case of `embark-act', the type
and value of the targets, and whether other targets are available
via `embark-cycle'.  The indicator function is free to display as
much or as little of this information as desired and can use any
Emacs interface elements to do so.

Embark comes with five such indicators:

- `embark-minimal-indicator', which does not display any
  information about keybindings, but does display types and
  values of acton targets in the echo area or minibuffer prompt,

- `embark-verbose-indicator', which pops up a buffer containing
  detailed information including key bindings and the first line
  of the docstring of the commands they run, and

- `embark-mixed-indicator', which combines the minimal and the
  verbose indicator: the minimal indicator is shown first and the
  verbose popup is shown after `embark-mixed-indicator-delay'
  seconds.

- `embark-highlight-indicator', which highlights the target
  at point.

- `embark-isearch-highlight-indicator', which when the target at
  point is an indentifier or symbol, lazily highlights all
  occurrences of it.

The protocol for indicator functions is as follows:

When called from `embark-act', an indicator function is called
without arguments.  The indicator function should then return a
closure, which captures the indicator state.  The returned
closure must accept up to three optional arguments, the action
keymap, the targets (plists as returned by `embark--targets') and
the prefix keys typed by the user so far.  The keymap, targets
and prefix keys may be updated when cycling targets at point
resulting in multiple calls to the closure.  When called from
`embark-become', the indicator closure will be called with the
keymap of commands to become, a fake target list containing a
single target of type `embark-become' and whose value is the
minibuffer input, and the prefix set to nil.  Note, in
particular, that if an indicator function wishes to distinguish
between `embark-act' and `embark-become' it should check whether
the `car' of the first target is `embark-become'.

After the action has been performed the indicator closure is
called without arguments, such that the indicator can perform the
necessary cleanup work.  For example, if the indicator adds
overlays, it should remove these overlays.  The indicator should
be written in a way that it is safe to call it for cleanup more
than once, in fact, it should be able to handle any sequence of
update and cleanup calls ending in a call for cleanup.

NOTE: Experience shows that the indicator calling convention may
change again in order to support more action features.  The
calling convention should currently be considered unstable.
Please keep this in mind when writing a custom indicator
function, or when using the `which-key' indicator function from
the wiki."
  :type '(repeat
          (choice
           (const :tag "Verbose indicator" embark-verbose-indicator)
           (const :tag "Minimal indicator" embark-minimal-indicator)
           (const :tag "Mixed indicator" embark-mixed-indicator)
           (const :tag "Highlight target" embark-highlight-indicator)
           (const :tag "Highlight all occurrences"
                  embark-isearch-highlight-indicator)
           (function :tag "Other"))))

(defcustom embark-quit-after-action t
  "Should `embark-act' quit the minibuffer?
This controls whether calling `embark-act' without a prefix
argument quits the minibuffer or not.  You can always get the
opposite behavior to that indicated by this variable by calling
`embark-act' with \\[universal-argument].

Note that `embark-act' can also be called from outside the
minibuffer and this variable is irrelevant in that case."
  :type 'boolean)

(defcustom embark-default-action-overrides nil
  "Alist associating target types with overriding default actions.
When the source of a target is minibuffer completion, the default
action for it is usually the command that opened the minibuffer
in the first place but this can be overridden for a given type by
an entry in this list.

For example, if you run `delete-file' the default action for its
completion candidates is `delete-file' itself.  You may prefer to
make `find-file' the default action for all files, even if they
wre obtained from a `delete-file' prompt.  In that case you can
configure that by adding an entry to this variable pairing `file'
with `find-file'."
  :type '(alist :key-type symbol :value-type command))

(define-obsolete-variable-alias
  'embark-allow-edit-commands
  'embark-allow-edit-actions
  "0.12")
(defcustom embark-allow-edit-actions
  '(delete-file
    delete-directory
    kill-buffer
    shell-command
    async-shell-command
    embark-kill-buffer-and-window
    pp-eval-expression)
  "Enable editing of target prior to acting for these commands.
Editing the target is useful as a confirmation feature for
destructive commands like `delete-file'."
  :type '(repeat symbol))

(defvar embark-skip-edit-commands nil)
(defvar embark-allow-edit-default t)
(dolist (var '(embark-skip-edit-commands embark-allow-edit-default))
  (make-obsolete-variable
   var
   "The action editing configuration has been simplified and
replaced by the single `embark-allow-edit-actions' variable."
   "0.12"))

(defcustom embark-setup-action-hooks
  '((async-shell-command embark--shell-prep)
    (shell-command embark--shell-prep)
    (pp-eval-expression embark--eval-prep)
    (package-delete embark--force-complete))
  "Alist associating commands with post-injection setup hooks.
For commands appearing as keys in this alist, run the
corresponding value as a setup hook after injecting the target
into in the minibuffer and before acting on it.  The hooks must
accept arbitrary keyword argument. The :action symbol, the
:target string and target :type are always present.  For actions
at point the target bounds are passed too.  The default pre-action
hook is specified by the entry with key t.  Furthermore hooks with
the key :always are executed always."
  :type '(alist :key-type
                (choice symbol
                        (const :tag "Default" t)
                        (const :tag "Always" :always))
                :value-type hook))

(dolist (obsolete
         '(embark-setup-hook embark-setup-hooks embark-setup-overrides))
  (make-obsolete obsolete
                 "see the new `embark-setup-action-hooks' variable."
                 "0.12"))

(defcustom embark-pre-action-hooks
  '(;; region commands which prompt for a filename
    (write-region embark--ignore-target embark--mark-target)
    (append-to-file embark--ignore-target embark--mark-target)
    ;; commands which evaluate code not given at a prompt
    (embark-pp-eval-defun embark--ignore-target)
    (eval-defun embark--ignore-target)
    (eval-last-sexp embark--end-of-target embark--ignore-target)
    (embark-eval-replace embark--ignore-target embark--mark-target)
    ;; motion commands that need to position point to skip current match
    (indent-pp-sexp embark--beginning-of-target)
    (backward-up-list embark--beginning-of-target)
    (backward-list embark--beginning-of-target)
    (forward-list embark--end-of-target)
    (forward-sexp embark--end-of-target)
    (backward-sexp embark--beginning-of-target)
    (raise-sexp embark--beginning-of-target)
    (kill-sexp embark--beginning-of-target)
    (mark-sexp embark--beginning-of-target)
    (transpose-sexps embark--end-of-target)
    (transpose-sentences embark--end-of-target)
    (transpose-paragraphs embark--end-of-target)
    (forward-sentence embark--end-of-target)
    (backward-sentence embark--beginning-of-target)
    (backward-paragraph embark--beginning-of-target)
    ;; region commands
    (mark embark--mark-target)
    (kill-region embark--mark-target)
    (kill-ring-save embark--mark-target)
    (indent-region embark--mark-target)
    (ispell-region embark--mark-target)
    (fill-region embark--mark-target)
    (upcase-region embark--mark-target)
    (downcase-region embark--mark-target)
    (capitalize-region embark--mark-target)
    (count-words-region embark--mark-target)
    (shell-command-on-region embark--mark-target)
    (delete-region embark--mark-target)
    (format-encode-region embark--mark-target embark--ignore-target)
    (format-decode-region embark--mark-target embark--ignore-target)
    ;; commands we want to be able to jump back from
    ;; (embark-find-definition achieves this by calling
    ;; xref-find-definitions which pushes the markers itself)
    (find-library embark--xref-push-markers))
  "Alist associating commands with pre-action hooks.
The hooks are run right before an action is embarked upon.  See
`embark-setup-action-hooks' for information about the hook
arguments and more details."
  :type '(alist :key-type
                (choice symbol
                        (const :tag "Default" t)
                        (const :tag "Always" :always))
                :value-type hook))

(make-obsolete 'embark-pre-action-hook
               "see the new `embark-pre-action-hooks' variable."
               "0.12")

(defcustom embark-post-action-hooks
  '((bookmark-delete embark--restart)
    (bookmark-rename embark--restart)
    (delete-file embark--restart)
    (embark-kill-ring-remove embark--restart)
    (embark-recentf-remove embark--restart)
    (embark-history-remove embark--restart)
    (rename-file embark--restart)
    (copy-file embark--restart)
    (delete-directory embark--restart)
    (make-directory embark--restart)
    (kill-buffer embark--restart)
    (embark-rename-buffer embark--restart)
    (package-delete embark--restart))
  "Alist associating commands with post-action hooks.
The hooks are run after an embarked upon action concludes.  See
`embark-setup-action-hooks' for information about the hook
arguments and more details."
  :type '(alist :key-type
                (choice symbol
                        (const :tag "Default" t)
                        (const :tag "Always" :always))
                :value-type hook))

(make-obsolete 'embark-post-action-hook
               "see the new `embark-post-action-hooks' variable."
               "0.12")

(defcustom embark-repeat-actions
  '(mark
    ;; outline commands
    outline-next-visible-heading outline-previous-visible-heading
    outline-forward-same-level outline-backward-same-level
    outline-demote outline-promote outline-mark-subtree
    outline-show-subtree outline-move-subtree-up outline-move-subtree-down
    outline-up-heading outline-hide-subtree outline-cycle
    ;; org commands (remapped outline commands)
    org-forward-heading-same-level org-backward-heading-same-level
    org-next-visible-heading org-previous-visible-heading
    org-demote-subtree org-promote-subtree org-mark-subtree
    org-show-subtree org-move-subtree-up org-move-subtree-down
    ;; transpose commands
    transpose-sexps transpose-sentences transpose-paragraphs
    ;; movement
    embark-next-symbol embark-previous-symbol
    backward-up-list backward-list forward-list forward-sexp
    backward-sexp forward-sentence backward-sentence
    forward-paragraph backward-paragraph)
  "List of repeatable actions."
  :type '(repeat function))

;;; Stashing information for actions in buffer local variables

(defvar-local embark--type nil
  "Cache for the completion type, meant to be set buffer-locally.")

(defvar-local embark--target-buffer nil
  "Cache for the previous buffer, meant to be set buffer-locally.")

(defvar-local embark--target-window nil
  "Cache for the previous window, meant to be set buffer-locally.
Since windows can be reused to display different buffers, this
window should only be used if it displays the buffer stored in
the variable `embark--target-buffer'.")

(defvar-local embark--command nil
  "Command that started the completion session.")

(defun embark--minibuffer-point ()
  "Return length of minibuffer contents."
  (max 0 (- (point) (minibuffer-prompt-end))))

(defun embark--default-directory ()
  "Guess a reasonable default directory for the current candidates."
  (if (and (minibufferp) minibuffer-completing-file-name)
      (let ((end (minibuffer-prompt-end))
            (contents (minibuffer-contents)))
        (expand-file-name
         (substitute-in-file-name
          (buffer-substring
           end
           (+ end
              (or (cdr
                   (last
                    (completion-all-completions
                     contents
                     minibuffer-completion-table
                     minibuffer-completion-predicate
                     (embark--minibuffer-point))))
                  (cl-position ?/ contents :from-end t)
                  0))))))
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
         (or (not (buffer-live-p embark--target-buffer))
             (eq (window-buffer embark--target-window) embark--target-buffer)))
    embark--target-window)
   ((and embark--target-buffer (buffer-live-p embark--target-buffer))
    (or (get-buffer-window embark--target-buffer)
        (when display (display-buffer embark--target-buffer))))
   (display (selected-window))))

(defun embark--cache-info (buffer)
  "Cache information needed for actions in variables local to BUFFER.
BUFFER defaults to the current buffer."
  (let ((cmd embark--command)
        (dir (embark--default-directory))
        (target-buffer (embark--target-buffer))
        (target-window (embark--target-window)))
    (with-current-buffer buffer
      (setq embark--command cmd
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

;;; Internal variables

(defvar embark--prompter-history nil
  "History used by the `embark-completing-read-prompter'.")

(defvar-local embark-collect--kind nil
  "Kind of current collect buffer.

There are three kinds:
- :snapshot, which does not auto-update
- :live, which does
- :completions, which also auto-updates, but is ephemeral.")

(defvar-local embark-collect-candidates nil
  "List of candidates in current collect buffer.")

(defvar-local embark-collect-view 'list
  "Type of view in collect buffer: `list' or `grid'.")

(defvar-local embark-collect-from nil
  "The buffer `embark-collect' was called from.")

(defvar-local embark-collect-linked-buffer nil
  "Buffer local variable indicating which Embark Buffer to update.")

(defvar-local embark-collect-affixator nil
  "Affixation function of minibuffer session for this collect.")

(defvar-local embark--collect-live--timer nil
  "Timer scheduled to update Embark Collect Live buffer.")

;;; Core functionality

(defconst embark--verbose-indicator-buffer " *Embark Actions*")

(defun embark--metadata ()
  "Return current minibuffer completion metadata."
  (completion-metadata
   (buffer-substring-no-properties
    (minibuffer-prompt-end)
    (max (minibuffer-prompt-end) (point)))
   minibuffer-completion-table
   minibuffer-completion-predicate))

(defun embark-target-active-region ()
  "Target the region if active."
  (when (use-region-p)
    (let ((start (region-beginning))
          (end (region-end)))
      `(region ,(buffer-substring start end) . (,start . ,end)))))

(autoload 'dired-get-filename "dired")

(defun embark-target-file-at-point ()
  "Target file at point.
This function mostly relies on `ffap-file-at-point', with one exception:
In `dired-mode', it uses `dired-get-filename' instead."
  (if-let (file (and (derived-mode-p 'dired-mode)
                     (dired-get-filename t 'no-error-if-not-filep)))
      (save-excursion
        (end-of-line)
        `(file ,(abbreviate-file-name (expand-file-name file))
               ,(save-excursion
                  (re-search-backward " " (line-beginning-position) 'noerror)
                  (1+ (point)))
               . ,(point)))
    (when-let* ((ffap-file (ffap-file-at-point))
                (tap-file (thing-at-point 'filename))
                ;; check the thingatpt candidate is a substring of the
                ;; ffap candidate, this avoids URLs and keyword
                ;; symbols when point is on the colon (see bug#52441)
                ((string-match-p (regexp-quote tap-file) ffap-file))
                ((not (ffap-el-mode tap-file))))
      `(file ,(abbreviate-file-name (expand-file-name file))
             ;; TODO the boundaries may be wrong, this should be generalized.
             ;; Unfortunately ffap does not make the bounds available.
             . ,(bounds-of-thing-at-point 'filename)))))

(defun embark-target-library-file-at-point ()
  "Target the file of the Emacs Lisp library at point.
The function `embark-target-file-at-point' could also easily
target Emacs Lisp library files, the only reason it doesn't is so
that library files and other types of file targets can be given
different priorities in `embark-target-finders'."
  (when-let* ((name (thing-at-point 'filename))
              (lib (ffap-el-mode name)))
    `(file ,lib . ,(bounds-of-thing-at-point 'filename))))

(defun embark-target-bug-reference-at-point ()
  "Target a bug reference at point."
  (when-let ((ov (seq-find (lambda (ov) (overlay-get ov 'bug-reference-url))
                           (overlays-at (point)))))
    `(url ,(overlay-get ov 'bug-reference-url)
          ,(overlay-start ov) . ,(overlay-end ov))))

(defun embark-target-package-at-point ()
  "Target the package on the current line in a `package-menu-mode-menu' buffer."
  (when (derived-mode-p 'package-menu-mode)
    (when-let ((pkg (get-text-property (point) 'tabulated-list-id)))
      `(package ,(symbol-name (package-desc-name pkg))
                ,(line-beginning-position) . ,(line-end-position)))))

(defun embark-target-email-at-point ()
  "Target the email address at point."
  (when-let ((email (thing-at-point 'email)))
    (when (string-prefix-p "mailto:" email)
      (setq email (string-remove-prefix "mailto:" email)))
    `(email ,email . ,(bounds-of-thing-at-point 'email))))

(defun embark-target-url-at-point ()
  "Target the URL at point."
  (if-let ((url (thing-at-point 'url)))
      `(url ,url . ,(thing-at-point-bounds-of-url-at-point t))
    (when-let ((url (or (get-text-property (point) 'shr-url)
                        (get-text-property (point) 'image-url))))
      `(url ,url
            ,(previous-single-property-change
              (min (1+ (point)) (point-max)) 'mouse-face nil (point-min))
            . ,(next-single-property-change
                (point) 'mouse-face nil (point-max))))))

(declare-function widget-at "wid-edit")

(defun embark-target-custom-variable-at-point ()
  "Target the variable corresponding to the customize widget at point."
  (when (derived-mode-p 'Custom-mode)
    (save-excursion
      (beginning-of-line)
      (when-let* ((widget (widget-at (point)))
                  (var (and (eq (car widget) 'custom-visibility)
                            (plist-get (cdr widget) :parent)))
                  (sym (and (eq (car var) 'custom-variable)
                            (plist-get (cdr var) :value))))
        `(variable
          ,(symbol-name sym)
          ,(point)
          . ,(progn
               (re-search-forward ":" (line-end-position) 'noerror)
               (point)))))))

;; NOTE: There is also (thing-at-point 'list), however it does
;; not work on strings and requires the point to be inside the
;; parentheses. This version here is slightly more general.
(defun embark-target-expression-at-point ()
  "Target expression at point."
  (cl-flet ((syntax-p (class &optional (delta 0))
              (and (<= (point-min) (+ (point) delta) (point-max))
                   (eq (pcase class
                         ('open 4) ('close 5) ('prefix 6) ('string 7))
                       (syntax-class (syntax-after (+ (point) delta)))))))
    (when-let
        ((start
          (pcase-let ((`(_ ,open _ ,string _ _ _ _ ,start _ _) (syntax-ppss)))
            (ignore-errors ; set start=nil if delimiters are unbalanced
              (cond
                (string start)
                ((or (syntax-p 'open) (syntax-p 'prefix))
                 (save-excursion (backward-prefix-chars) (point)))
                ((syntax-p 'close -1)
                 (save-excursion
                   (backward-sexp) (backward-prefix-chars) (point)))
                ((syntax-p 'string) (point))
                ((syntax-p 'string -1) (scan-sexps (point) -1))
                (t open)))))
         (end (ignore-errors (scan-sexps start 1))))
      (unless (eq start (car (bounds-of-thing-at-point 'defun)))
      `(expression ,(buffer-substring start end) ,start . ,end)))))

(defmacro embark-define-thingatpt-target (thing &rest modes)
  "Define a target finder for THING using the thingatpt library.
If any MODES are given, the target finder only applies to buffers
in one of those major modes."
  (declare (indent 1))
  `(defun ,(intern (format "embark-target-%s-at-point" thing)) ()
     ,(format "Target %s at point." thing)
     (when ,(if modes `(derived-mode-p ,@(mapcar (lambda (m) `',m) modes)) t)
       (when-let (bounds (bounds-of-thing-at-point ',thing))
         (cons ',thing (cons
                        (buffer-substring (car bounds) (cdr bounds))
                        bounds))))))

(embark-define-thingatpt-target defun)
(embark-define-thingatpt-target sentence
  text-mode help-mode Info-mode man-common)
(embark-define-thingatpt-target paragraph
  text-mode help-mode Info-mode man-common)

(defun embark-target-identifier-at-point ()
  "Target identifier at point.

In Emacs Lisp and IELM buffers the identifier is promoted to a
symbol, for which more actions are available.  Identifiers are
also promoted to symbols if they are interned Emacs Lisp symbols
and found in a buffer in a major mode derived from
`special-mode', `Info-mode' or `text-mode' (these are intended to
cover cases where you might be reading or writing about Emacs).

As a convenience, in Org Mode an initial ' or surrounding == or
~~ are removed."
  (when-let (bounds (bounds-of-thing-at-point 'symbol))
    (let ((name (buffer-substring (car bounds) (cdr bounds))))
      (when (derived-mode-p 'org-mode)
        (cond ((string-prefix-p "'" name)
               (setq name (substring name 1))
               (cl-incf (car bounds)))
              ((string-match-p "^\\([=~]\\).*\\1$" name)
               (setq name (substring name 1 -1))
               (cl-incf (car bounds))
               (cl-decf (cdr bounds)))))
      `(,(if (or (derived-mode-p 'emacs-lisp-mode)
                 (derived-mode-p 'inferior-emacs-lisp-mode)
                 (and (intern-soft name)
                      (derived-mode-p 'special-mode 'Info-mode 'text-mode)))
             'symbol
           'identifier)
        ,name
        . ,bounds))))

(defun embark-target-heading-at-point ()
  "Target the outline heading at point."
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (when (save-excursion
            (goto-char beg)
            (and (bolp)
                 (looking-at
                  ;; default definition from outline.el
                  (or (bound-and-true-p outline-regexp) "[*\^L]+"))))
      (require 'outline) ;; Ensure that outline commands are available
      `(heading ,(buffer-substring beg end) ,beg . ,end))))

(defun embark-target-top-minibuffer-completion ()
  "Target the top completion candidate in the minibuffer.
Return the category metadatum as the type of the target.

This target finder is meant for the default completion UI and
completion UI highly compatible with it, like Icomplete.
Many completion UIs can still work with Embark but will need
their own target finder.  See for example
`embark--vertico-selected' or `embark--selectrum-selected'."
  (when (minibufferp)
    (pcase-let* ((`(,category . ,candidates) (embark-minibuffer-candidates))
                 (contents (minibuffer-contents))
                 (top (if (test-completion contents
                                           minibuffer-completion-table
                                           minibuffer-completion-predicate)
                          contents
                        (let ((completions (completion-all-sorted-completions)))
                          (if (null completions)
                              contents
                            (concat
                             (substring contents
                                        0 (or (cdr (last completions)) 0))
                             (car completions)))))))
      (cons category (or (car (member top candidates)) top)))))

(defun embark-target-collect-candidate ()
  "Target the collect candidate at point."
  (when (derived-mode-p 'embark-collect-mode)
    (when-let (button
               (pcase (get-text-property (point) 'tabulated-list-column-name)
                 ("Candidate" (button-at (point)))
                 ("Annotation" (previous-button (point)))))
      ;; do not use button-label since it strips text properties
      (let* ((beg (button-start button))
             (end (button-end button))
             (label (buffer-substring beg end)))
        `(,embark--type
          ,(if (eq embark--type 'file)
               (abbreviate-file-name (expand-file-name label))
             label)
          ,beg . ,end)))))

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
        (let ((raw (buffer-substring beg end)))
          `(,embark--type
            ,(if (and (eq embark--type 'file) (not relative))
                 (abbreviate-file-name (expand-file-name raw))
               raw)
            ,beg . ,end))))))

(defun embark--cycle-key ()
  "Return the key to use for `embark-cycle'."
  (or embark-cycle-key (car (where-is-internal #'embark-act))))

(defun embark--action-keymap (type cycle)
  "Return action keymap for targets of given TYPE.
If CYCLE is non-nil bind `embark-cycle'."
  (make-composed-keymap
   (let ((map (make-sparse-keymap))
         (default-action (embark--default-action type)))
     (define-key map [13] default-action)
     (define-key map [return] default-action)
     (when-let ((cycle-key (and cycle (embark--cycle-key))))
       (define-key map cycle-key #'embark-cycle))
     (when embark-help-key
       (define-key map embark-help-key #'embark-keymap-help))
     map)
   (symbol-value (or (alist-get type embark-keymap-alist)
                     (alist-get t embark-keymap-alist)))))

(defun embark--truncate-target (target)
  "Truncate TARGET string."
  (unless (stringp target)
    (setq target (format "%s" target)))
  (if-let (pos (string-match-p "\n" target))
      (concat (car (split-string target "\n" 'omit-nulls "\\s-*")) "…")
    target))

(defun embark--act-label (rep multi)
  "Return highlighted Act/Rep indicator label given REP and MULTI."
  (propertize
   (cond
    (multi "∀ct")
    (rep "Rep")
    (t "Act"))
   'face 'highlight))

(defun embark-minimal-indicator ()
  "Minimal indicator, appearing in the minibuffer prompt or echo area.
This indicator displays a message showing the types of all
targets, starting with the current target, and the value of the
current target.  The message is displayed in the echo area, or if
the minibuffer is open, the message is added to the prompt."
  (let ((indicator-overlay))
    (lambda (&optional keymap targets _prefix)
      (if (null keymap)
          (when indicator-overlay
            (delete-overlay indicator-overlay))
        (let* ((target (car targets))
               (act (embark--act-label
                     (eq (lookup-key keymap [13]) #'embark-done)
                     (plist-get target :multi)))
               (shadowed-targets (cdr targets))
               (indicator
                (cond
                 ;; TODO code duplication with embark--verbose-indicator-section-target
                 ((eq (plist-get target :type) 'embark-become)
                  (propertize "Become" 'face 'highlight))
                 ((and (minibufferp)
                       (not (eq 'embark-keybinding
                                (completion-metadata-get
                                 (embark--metadata)
                                 'category))))
                  ;; we are in a minibuffer but not from the
                  ;; completing-read prompter, use just "Act"
                  act)
                 ((plist-get target :multi)
                  (format "%s on %s %ss"
                          act
                          (plist-get target :multi)
                          (plist-get target :type)))
                 (t (format
                     "%s on %s%s '%s'"
                     act
                     (plist-get target :type)
                     (if shadowed-targets
                         (format (propertize "(%s)" 'face 'shadow)
                                 (string-join
                                  (mapcar (lambda (x)
                                            (symbol-name (plist-get x :type)))
                                          shadowed-targets)
                                  ", "))
                       "")
                     (embark--truncate-target (plist-get target :target)))))))
          (if (not (minibufferp))
              (message "%s" indicator)
            (unless indicator-overlay
              (setq indicator-overlay (make-overlay (point-min) (point-min)
                                                    (current-buffer) t t)))
            (overlay-put indicator-overlay
                         'before-string (concat indicator
                                                (if (<= (length indicator)
                                                        (* 0.4 (frame-width)))
                                                    " "
                                                  "\n")))))))))

(defun embark--read-key-sequence (update)
  "Read key sequence, call UPDATE function with prefix keys."
  (let (timer prefix)
    (unwind-protect
        (progn
          (when (functionp update)
            (setq timer (run-at-time
                         0.05 0.05
                         (lambda ()
                           (let ((new-prefix (this-single-command-keys)))
                             (unless (equal prefix new-prefix)
                               (setq prefix new-prefix)
                               (when (/= (length prefix) 0)
                                 (funcall update prefix))))))))
          (read-key-sequence-vector nil nil nil t 'cmd-loop))
      (when timer
        (cancel-timer timer)))))

(defun embark-keymap-prompter (keymap update)
  "Let the user choose an action using the bindings in KEYMAP.
Besides the bindings in KEYMAP, the user is free to use all their
key bindings and even \\[execute-extended-command] to select a command.
UPDATE is the indicator update function."
  (let* ((keys (let ((overriding-terminal-local-map keymap))
                 (embark--read-key-sequence update)))
         (cmd (let ((overriding-terminal-local-map keymap))
                (key-binding keys 'accept-default))))
    ;; Set last-command-event as it would be from the command loop.
    ;; Previously we only set it locally for digit-argument and for
    ;; the mouse scroll commands handled in this function. But other
    ;; commands can need it too! For example, electric-pair-mode users
    ;; may wish to bind ( to self-insert-command in embark-region-map.
    ;; Also, as described in issue #402, there are circumstances where
    ;; you might run consult-narrow through the embark-keymap-prompter.
    (setq last-command-event (aref keys (1- (length keys))))
    (pcase cmd
      ((or 'embark-keymap-help
           (and 'nil            ; cmd is nil but last key is help-char
                (guard (eq help-char (aref keys (1- (length keys)))))))
       (let ((embark-indicators
              (cl-set-difference embark-indicators
                                 '(embark-verbose-indicator
                                   embark-mixed-indicator)))
             (prefix-map
              (if (eq cmd 'embark-keymap-help)
                  keymap
                (let ((overriding-terminal-local-map keymap))
                  (key-binding (seq-take keys (1- (length keys)))
                               'accept-default)))))
         (when-let ((win (get-buffer-window embark--verbose-indicator-buffer
                                            'visible)))
           (quit-window 'kill-buffer win))
         (embark-completing-read-prompter prefix-map update)))
      ((or 'universal-argument 'universal-argument-more
           'negative-argument 'digit-argument)
       ;; prevent `digit-argument' from modifying the overriding map
       (let ((overriding-terminal-local-map overriding-terminal-local-map))
         (command-execute cmd))
       (embark-keymap-prompter
        (make-composed-keymap universal-argument-map keymap)
        update))
      ((or 'minibuffer-keyboard-quit 'abort-recursive-edit 'abort-minibuffers)
       nil)
      ((guard (lookup-key keymap keys))  ; if directly bound, then obey
       cmd)
      ('self-insert-command
       (minibuffer-message "Not an action")
       (embark-keymap-prompter keymap update))
      ((or 'scroll-other-window 'scroll-other-window-down)
       (let ((minibuffer-scroll-window
              ;; NOTE: Here we special case the verbose indicator!
              (or (get-buffer-window embark--verbose-indicator-buffer 'visible)
                  minibuffer-scroll-window)))
         (ignore-errors (command-execute cmd)))
       (embark-keymap-prompter keymap update))
      ((or 'scroll-bar-toolkit-scroll 'mwheel-scroll 'mac-mwheel-scroll)
       (funcall cmd last-command-event)
       (embark-keymap-prompter keymap update))
      ('execute-extended-command
       (intern-soft (read-extended-command)))
      ((or 'keyboard-quit 'keyboard-escape-quit)
       nil)
      (_ cmd))))

(defun embark--command-name (cmd)
  "Return an appropriate name for CMD.
If CMD is a symbol, use its symbol name; for lambdas, use the
first line of the documentation string; otherwise use the word
'unnamed'."
  (concat ; fresh copy, so we can freely add text properties
   (cond
    ((stringp (car-safe cmd)) (car cmd))
    ((keymapp cmd)
     (propertize (if (symbolp cmd) (format "+%s" cmd) "<keymap>")
                 'face 'embark-keymap))
    ((symbolp cmd) (symbol-name cmd))
    ((when-let (doc (and (functionp cmd) (ignore-errors (documentation cmd))))
       (save-match-data
         (when (string-match "^\\(.*\\)$" doc)
           (match-string 1 doc)))))
    (t "<unnamed>"))))

;; Taken from Marginalia, needed by the verbose indicator.
;; We cannot use the completion annotators in this case.
(defconst embark--advice-regexp
  (rx bos
      (1+ (seq (? "This function has ")
               (or ":before" ":after" ":around" ":override"
                   ":before-while" ":before-until" ":after-while"
                   ":after-until" ":filter-args" ":filter-return")
               " advice: " (0+ nonl) "\n"))
      "\n")
  "Regexp to match lines about advice in function documentation strings.")

;; Taken from Marginalia, needed by the verbose indicator.
;; We cannot use the completion annotators in this case.
(defun embark--function-doc (sym)
  "Documentation string of function SYM."
  (let ((vstr (and (keymapp sym) (boundp sym)
                   (eq (symbol-function sym) (symbol-value sym))
                   (documentation-property sym 'variable-documentation))))
    (when-let (str (or (ignore-errors (documentation sym)) vstr))
      ;; Replace standard description with variable documentation
      (when (and vstr (string-match-p "\\`Prefix command" str))
        (setq str vstr))
      (save-match-data
        (if (string-match embark--advice-regexp str)
            (substring str (match-end 0))
          str)))))

(defun embark--formatted-bindings (keymap &optional nested)
  "Return the formatted keybinding of KEYMAP.
The keybindings are returned in their order of appearance.
If NESTED is non-nil subkeymaps are not flattened."
  (let* ((commands
          (cl-loop for (key . cmd) in (embark--all-bindings keymap nested)
                   for name = (embark--command-name cmd)
                   unless (or
                           ;; skip which-key pseudo keys and other invalid pairs
                           (and (not (keymapp cmd))
                                (consp cmd)
                                (not (stringp (car cmd))))
                           (memq cmd '(embark-keymap-help
                                       negative-argument digit-argument)))
                   collect (list name
                                 (cond
                                  ((and (not (symbolp cmd)) (keymapp cmd))
                                   'keymap)
                                  ((and (consp cmd) (stringp (car cmd)))
                                   (cdr cmd))
                                  (t cmd))
                                 key
                                 (concat (key-description key)))))
         (width (cl-loop for (_name _cmd _key desc) in commands
                         maximize (length desc)))
         (def)
         (candidates
          (cl-loop for item in commands
                   for (name cmd key desc) = item
                   for desc-rep =
                   (concat
                    (propertize desc 'face 'embark-keybinding)
                    (and (memq cmd embark-repeat-actions)
                         embark-keybinding-repeat))
                   for formatted =
                   (propertize
                    (concat desc-rep
                            (make-string (- width (length desc-rep) -1) ?\s)
                            name)
                    'embark-command cmd)
                   when (equal key [13])
                   do (setq def formatted)
                   collect (cons formatted item))))
    (cons candidates def)))

(defun embark-completing-read-prompter (keymap update &optional no-default)
  "Prompt via completion for a command bound in KEYMAP.
If NO-DEFAULT is t, no default value is passed to`completing-read'.

UPDATE is the indicator update function.  It is not used directly
here, but if the user switches to `embark-keymap-prompter', the
UPDATE function is passed to it."
  (let* ((candidates+def (embark--formatted-bindings keymap))
         (candidates (car candidates+def))
         (def (and (not no-default) (cdr candidates+def)))
         (choice
          (catch 'choice
            (minibuffer-with-setup-hook
                (lambda ()
                  (let ((map (make-sparse-keymap)))
                    (when-let (cycle (embark--cycle-key))
                      ;; Rebind `embark-cycle' in order allow cycling
                      ;; from the `completing-read' prompter. Additionally
                      ;; `embark-cycle' can be selected via
                      ;; `completing-read'. The downside is that this breaks
                      ;; recursively acting on the candidates of type
                      ;; embark-keybinding in the `completing-read' prompter.
                      (define-key map cycle
                        (cond
                         ((lookup-key keymap cycle)
                          (lambda ()
                            (interactive)
                            (throw 'choice 'embark-cycle)))
                         ((null embark-cycle-key)
                          (lambda ()
                            (interactive)
                            (minibuffer-message
                             (concat "Single target; can't cycle. "
                                     "Press `%s' again to act.")
                             (key-description cycle))
                            (define-key map cycle #'embark-act))))))
                    (when embark-keymap-prompter-key
                      (define-key map embark-keymap-prompter-key
                        (lambda ()
                          (interactive)
                          (message "Press key binding")
                          (let ((cmd (embark-keymap-prompter keymap update)))
                            (if (null cmd)
                                (user-error "Unknown key")
                              (throw 'choice cmd))))))
                    (use-local-map
                     (make-composed-keymap map (current-local-map)))))
              (completing-read
               "Command: "
               (lambda (string predicate action)
                 (if (eq action 'metadata)
                     `(metadata (category . embark-keybinding)
                                (display-sort-function . identity)
                                (cycle-sort-function . identity))
                   (complete-with-action action candidates string predicate)))
               nil nil nil 'embark--prompter-history def)))))
    (pcase (assoc choice candidates)
      (`(,_formatted ,_name ,cmd ,key ,_desc)
       ;; Set last-command-event as it would be from the command loop.
       (setq last-command-event (aref key (1- (length key))))
       cmd)
      ('nil (intern-soft choice)))))

;;; Verbose action indicator

(defface embark-verbose-indicator-documentation
  '((t :inherit completions-annotations))
  "Face used by the verbose action indicator to display binding descriptions.
Used by `embark-verbose-indicator'.")

(defface embark-verbose-indicator-title '((t :height 1.1 :weight bold))
  "Face used by the verbose action indicator for the title.
Used by `embark-verbose-indicator'.")

(defface embark-verbose-indicator-shadowed '((t :inherit shadow))
  "Face used by the verbose action indicator for the shadowed targets.
Used by `embark-verbose-indicator'.")

(defcustom embark-verbose-indicator-display-action
  '(display-buffer-reuse-window)
  "Parameters added to `display-buffer-alist' to show the actions buffer.
See the docstring of `display-buffer' for information on what
display actions and parameters are available."
  :type `(choice
          (const :tag "Reuse some window"
                 (display-buffer-reuse-window))
          (const :tag "Below target buffer"
                 (display-buffer-below-selected
                  (window-height . fit-window-to-buffer)))
          (const :tag "Bottom of frame (fixed-size)"
                 (display-buffer-at-bottom))
          (const :tag "Bottom of frame (resizes during cycling)"
                 (display-buffer-at-bottom
                  (window-height . fit-window-to-buffer)))
          (const :tag "Side window on the right"
                 (display-buffer-in-side-window (side . right)))
          (const :tag "Side window on the left"
                 (display-buffer-in-side-window (side . left)))
          (sexp :tag "Other")))

(define-obsolete-variable-alias
  'embark-verbose-indicator-excluded-commands
  'embark-verbose-indicator-excluded-actions
  "0.12")
(defcustom embark-verbose-indicator-excluded-actions nil
  "Commands not displayed by `embark-verbose-indicator'.
This variable should be set to a list of symbols and regexps.
The verbose indicator will exclude from its listing any commands
matching an element of this list."
  :type '(choice
          (const :tag "Exclude nothing" nil)
          (const :tag "Exclude Embark general actions"
                 ("\\`embark-collect-" embark-cycle embark-export
                  embark-keymap-help embark-become embark-isearch))
          (repeat :tag "Other" (choice regexp symbol))))

(defcustom embark-verbose-indicator-buffer-sections
  `(target "\n" shadowed-targets " " cycle "\n" bindings)
  "List of sections to display in the verbose indicator buffer, in order.
You can use either a symbol designating a concrete section (one
of the keywords below, but without the colon), a string literal
or a function returning a string or list of strings to insert and
that accepts the following keyword arguments:

- `:target', the target as a cons of type and value,
- `:shadowed-targets', a list of conses for the other targets,
- `:bindings' a list returned by `embark--formatted-bindings', and
- `:cycle', a string describing the key binding of `embark-cycle'."
  :type '(repeat
          (choice (const :tag "Current target name" target)
                  (const :tag "List of other shadowed targets" shadowed-targets)
                  (const :tag "Key bindings" bindings)
                  (const :tag "Cycle indicator" cycle)
                  (string :tag "Literal string")
                  (function :tag "Custom function"))))

(defcustom embark-verbose-indicator-nested t
  "Whether the verbose indicator should use nested keymap navigation.
When this variable is non-nil the actions buffer displayed by
`embark-verbose-indicator' will include any prefix keys found in
the keymap it is displaying, and will update to show what is
bound under the prefix if the prefix is pressed.  If this
variable is nil, then the actions buffer will contain a flat list
of all full key sequences bound in the keymap."
  :type 'boolean)

(defun embark--verbose-indicator-excluded-p (cmd)
  "Return non-nil if CMD should be excluded from the verbose indicator."
  (seq-find (lambda (x)
              (if (symbolp x)
                  (eq cmd x)
                (string-match-p x (symbol-name cmd))))
            embark-verbose-indicator-excluded-actions))

(cl-defun embark--verbose-indicator-section-target
    (&key targets bindings &allow-other-keys)
  "Format the TARGETS section for the indicator buffer.
BINDINGS is the formatted list of keybindings."
  (let* ((target (plist-get (car targets) :target))
         (kind (plist-get (car targets) :type))
         (result (cond
                  ;; TODO code duplication with embark-minimal-indicator
                  ((eq kind 'embark-become)
                   (concat (propertize "Become" 'face 'highlight)))
                  ((plist-get (car targets) :multi)
                   (format "%s on %s %ss"
                           (embark--act-label nil t)
                           (plist-get (car targets) :multi)
                           kind))
                  (t
                   (format "%s on %s '%s'"
                           (embark--act-label
                            (seq-find (lambda (b) (eq (caddr b) #'embark-done))
                                      bindings)
                            nil)
                           kind
                           (embark--truncate-target target))))))
    (add-face-text-property 0 (length result)
                            'embark-verbose-indicator-title
                            'append
                            result)
    result))

(cl-defun embark--verbose-indicator-section-cycle
    (&key cycle shadowed-targets &allow-other-keys)
  "Format the CYCLE key section for the indicator buffer.
SHADOWED-TARGETS is the list of other targets."
  (concat
   (and cycle(propertize (format "(%s to cycle)" cycle)
                         'face 'embark-verbose-indicator-shadowed))
   (and shadowed-targets "\n")))

(cl-defun embark--verbose-indicator-section-shadowed-targets
    (&key shadowed-targets &allow-other-keys)
  "Format the SHADOWED-TARGETS section for the indicator buffer."
  (when shadowed-targets
    (propertize (format "Shadowed targets at point: %s"
                        (string-join shadowed-targets ", "))
                'face 'embark-verbose-indicator-shadowed)))

(cl-defun embark--verbose-indicator-section-bindings
    (&key bindings &allow-other-keys)
  "Format the BINDINGS section for the indicator buffer."
  (let* ((max-width (apply #'max (cons 0 (mapcar (lambda (x)
                                                  (string-width (car x)))
                                                bindings))))
         (fmt (format "%%-%ds" (1+ max-width)))
         (result nil))
    (dolist (binding bindings (string-join (nreverse result)))
      (let ((cmd (caddr binding)))
        (unless (embark--verbose-indicator-excluded-p cmd)
          (let ((keys (format fmt (car binding)))
                (doc (embark--function-doc cmd)))
            (push (format "%s%s\n" keys
                          (propertize
                           (car (split-string (or doc "") "\n"))
                           'face 'embark-verbose-indicator-documentation))
                          result)))))))

(defun embark--verbose-indicator-update (keymap targets)
  "Update verbose indicator buffer.
The arguments are the new KEYMAP and TARGETS."
  (with-current-buffer (get-buffer-create embark--verbose-indicator-buffer)
    (let* ((inhibit-read-only t)
           (bindings
            (embark--formatted-bindings keymap embark-verbose-indicator-nested))
           (bindings (car bindings))
           (shadowed-targets (mapcar
                              (lambda (x) (symbol-name (plist-get x :type)))
                              (cdr targets)))
           (cycle (let ((ck (where-is-internal #'embark-cycle keymap)))
                    (and ck (key-description (car ck))))))
      (setq-local cursor-type nil)
      (setq-local truncate-lines t)
      (setq-local buffer-read-only t)
      (erase-buffer)
      (dolist (section embark-verbose-indicator-buffer-sections)
        (insert
         (if (stringp section)
             section
           (or (funcall
                (let ((prefixed (intern (format
                                         "embark--verbose-indicator-section-%s"
                                         section))))
                  (cond
                   ((fboundp prefixed) prefixed)
                   ((fboundp section) section)
                   (t (error "Undefined verbose indicator section `%s'"
                             section))))
                :targets targets :shadowed-targets shadowed-targets
                :bindings bindings :cycle cycle)
               ""))))
      (goto-char (point-min)))))

(defun embark-verbose-indicator ()
  "Indicator that displays a table of key bindings in a buffer.
The default display includes the type and vaue of the current
target, the list of other target types, and a table of key
bindings, actions and the first line of their docstrings.

The order and formatting of these items is completely
configurable through the variable
`embark-verbose-indicator-buffer-sections'.

If the keymap being shown contains prefix keys, the table of key
bindings can either show just the prefixes and update once the
prefix is pressed, or it can contain a flat list of all full key
sequences bound in the keymap.  This is controlled by the
variable `embark-verbose-indicator-nested'.

To reduce clutter in the key binding table, one can set the
variable `embark-verbose-indicator-excluded-actions' to a list
of symbols and regexps matching commands to exclude from the
table.

To configure how a window is chosen to display this buffer, see
the variable `embark-verbose-indicator-display-action'."
  (lambda (&optional keymap targets prefix)
    (if (not keymap)
        (when-let ((win (get-buffer-window embark--verbose-indicator-buffer
                                           'visible)))
          (quit-window 'kill-buffer win))
      (embark--verbose-indicator-update
       (if (and prefix embark-verbose-indicator-nested)
           ;; Lookup prefix keymap globally if not found in action keymap
           (let ((overriding-terminal-local-map keymap))
             (key-binding prefix 'accept-default))
         keymap)
       targets)
      (let ((display-buffer-alist
             `(,@display-buffer-alist
               (,(regexp-quote embark--verbose-indicator-buffer)
                ,@embark-verbose-indicator-display-action))))
        (display-buffer embark--verbose-indicator-buffer)))))

(defcustom embark-mixed-indicator-delay 0.5
  "Time in seconds after which the verbose indicator is shown.
The mixed indicator starts by showing the minimal indicator and
after this delay shows the verbose indicator."
  :type '(choice (const :tag "No delay" 0)
                 (number :tag "Delay in seconds")))

(defcustom embark-mixed-indicator-both nil
  "Show both indicators, even after the verbose indicator appeared."
  :type 'boolean)

(defun embark-mixed-indicator ()
  "Mixed indicator showing keymap and targets.
The indicator shows the `embark-minimal-indicator' by default.
After `embark-mixed-indicator-delay' seconds, the
`embark-verbose-indicator' is shown.  This which-key-like approach
ensures that Embark stays out of the way for quick actions.  The
helpful keybinding reminder still pops up automatically without
further user intervention."
  (let ((vindicator (embark-verbose-indicator))
        (mindicator (embark-minimal-indicator))
        vindicator-active
        vtimer)
    (lambda (&optional keymap targets prefix)
      ;; Always cancel the timer.
      ;; 1. When updating, cancel timer, since the user has pressed
      ;;    a key before the timer elapsed.
      ;; 2. For cleanup, the timer must also be cancelled.
      (when vtimer
        (cancel-timer vtimer)
        (setq vtimer nil))
      (if (not keymap)
          (progn
            (funcall vindicator)
            (when mindicator
              (funcall mindicator)))
        (when mindicator
          (funcall mindicator keymap targets prefix))
        (if vindicator-active
            (funcall vindicator keymap targets prefix)
          (setq vtimer
                (run-at-time
                 embark-mixed-indicator-delay nil
                 (lambda ()
                   (when (and (not embark-mixed-indicator-both) mindicator)
                     (funcall mindicator)
                     (setq mindicator nil))
                   (setq vindicator-active t)
                   (funcall vindicator keymap targets prefix)))))))))

;;;###autoload
(defun embark-prefix-help-command ()
  "Prompt for and run a command bound in the prefix used to reach this command.
The prefix described consists of all but the last event of the
key sequence that ran this command.  This function is intended to
be used as a value for `prefix-help-command'.

In addition to using completion to select a command, you can also
type @ and the key binding (without the prefix)."
  (interactive)
  (let ((keys (this-command-keys-vector)))
    (embark-bindings (seq-take keys (1- (length keys))))))

;;;###autoload
(defun embark-bindings (&optional prefix)
  "Explore all current command key bindings with `completing-read'.
The selected command will be executed.  The set of key bindings can
be restricted by passing a PREFIX key."
  (interactive)
  (let ((keymap (if prefix
                    (key-binding prefix 'accept-default)
                  (make-composed-keymap (current-active-maps t)))))
    (unless (keymapp keymap)
      (user-error "No key bindings found"))
    (when-let (command (embark-completing-read-prompter keymap nil 'no-default))
      (call-interactively command))))

(defun embark--prompt (indicators keymap targets)
  "Call the prompter with KEYMAP and INDICATORS.
The TARGETS are displayed for actions outside the minibuffer."
  (mapc (lambda (i) (funcall i keymap targets)) indicators)
  (condition-case nil
      (minibuffer-with-setup-hook
          (lambda ()
            ;; if the prompter opens its own minibuffer, show
            ;; the indicator there too
            (let ((inner-indicators (mapcar #'funcall embark-indicators)))
              (mapc (lambda (i) (funcall i keymap targets)) inner-indicators)
              (add-hook 'minibuffer-exit-hook
                        (lambda () (mapc #'funcall inner-indicators))
                        nil t)))
        (let ((enable-recursive-minibuffers t))
          (funcall embark-prompter keymap
                   (lambda (prefix)
                     (mapc (lambda (i) (funcall i keymap targets prefix))
                           indicators)))))
    (quit nil)))

(defvar embark--run-after-command-functions nil
  "Abnormal hook, used by `embark--run-after-command'.")

(defun embark--run-after-command (fn &rest args)
  "Call FN with ARGS after the current commands finishes.
If multiple functions are queued with this function during the
same command, they will be called in the order from the one
queued most recently to the one queued least recently."
  ;; We don't simply add FN to `post-command-hook' because FN may recursively
  ;; call this function.  In that case, FN would modify `post-command-hook'
  ;; from within post-command-hook, which doesn't behave properly in our case.
  ;; We use our own abnormal hook and run it from PCH in a way that it is OK to
  ;; modify it from within its own functions.
  (unless embark--run-after-command-functions
    (let (pch timer has-run)
      (setq pch
            (lambda ()
              (remove-hook 'post-command-hook pch)
              (cancel-timer timer)
              (unless has-run
                (setq has-run t)
                (while embark--run-after-command-functions
                  ;; The following funcall may recursively call
                  ;; `embark--run-after-command', modifying
                  ;; `embark--run-after-command-functions'.  This is why this
                  ;; loop has to be implemented carefully.  We have to pop the
                  ;; function off the hook before calling it.  Using `dolist'
                  ;; on the hook would also be incorrect, because it wouldn't
                  ;; take modifications of this hook into account.
                  (with-demoted-errors "embark PCH: %S"
                    (condition-case nil
                        (funcall (pop embark--run-after-command-functions))
                      (quit (message "Quit"))))))))
      (add-hook 'post-command-hook pch 'append)
      ;; Generally we prefer `post-command-hook' because it plays well with
      ;; keyboard macros.  In some cases, `post-command-hook' isn't run after
      ;; exiting a recursive edit, so set up the following timer as a backup.
      (setq timer (run-at-time 0 nil pch))))

  ;; Keep the default-directory alive, since this is often overwritten,
  ;; for example by Consult commands.
  ;; TODO it might be necessary to add more dynamically bound variables
  ;; here. What we actually want are functions `capture-dynamic-scope'
  ;; and `eval-in-dynamic-scope', but this does not exist?
  (let ((dir default-directory))
    (push (lambda ()
            (let ((default-directory dir))
              (apply fn args)))
          embark--run-after-command-functions)))

(defun embark--quit-and-run (fn &rest args)
  "Quit the minibuffer and then call FN with ARGS.
If called outside the minibuffer, simply apply FN to ARGS."
  (if (not (minibufferp))
      (apply fn args)
    (apply #'embark--run-after-command fn args)
    (embark--run-after-command #'set 'ring-bell-function ring-bell-function)
    (setq ring-bell-function #'ignore)
    (if (fboundp 'minibuffer-quit-recursive-edit)
        (minibuffer-quit-recursive-edit)
      (abort-recursive-edit))))

(defun embark--run-action-hooks (hooks action target quit)
  "Run HOOKS for ACTION.
The HOOKS argument must be alist.  The keys t and :always are
treated specially.  The :always hooks are executed always and the
t hooks are the default hooks, if there are no command-specific
hooks.  The QUIT, ACTION and TARGET arguments are passed to the
hooks."
  (mapc (lambda (h) (apply h :action action :quit quit target))
        (or (alist-get action hooks)
            (alist-get t hooks)))
  (mapc (lambda (h) (apply h :action action :quit quit target))
        (alist-get :always hooks)))

(defun embark--act (action target &optional quit)
  "Perform ACTION injecting the TARGET.
If called from a minibuffer with non-nil QUIT, quit the
minibuffer before executing the action."
  (if (memq action '(embark-become        ; these actions should not
                     embark-collect-live  ; run in the target window
                     embark-collect-snapshot
                     embark-export
                     embark-act-all))
      (command-execute action)
    (let* ((command embark--command)
           (prefix prefix-arg)
           (action-window (embark--target-window t))
           (inject
            (lambda ()
              (delete-minibuffer-contents)
              (insert (substring-no-properties (plist-get target :target)))
              (embark--run-action-hooks embark-setup-action-hooks
                                        action target quit)
              (unless (memq action embark-allow-edit-actions)
                (if (memq 'ivy--queue-exhibit post-command-hook)
                    ;; Ivy has special needs: (1) for file names
                    ;; ivy-immediate-done is not equivalent to
                    ;; exit-minibuffer, (2) it needs a chance to run
                    ;; its post command hook first, so use depth 10
                    (add-hook 'post-command-hook 'ivy-immediate-done 10 t)
                  (add-hook 'post-command-hook #'exit-minibuffer nil t)))))
           (dedicate (and (derived-mode-p 'embark-collect-mode)
                          (not (window-dedicated-p))
                          (selected-window)))
           (run-action
            (if (commandp action)
                (lambda ()
                  (minibuffer-with-setup-hook inject
                    (let (final-window)
                      (when dedicate (set-window-dedicated-p dedicate t))
                      (unwind-protect
                          (with-selected-window action-window
                            (let ((enable-recursive-minibuffers t)
                                  (embark--command command)
                                  (this-command action)
                                  ;; the next two avoid mouse dialogs
                                  (use-dialog-box nil)
                                  (last-nonmenu-event 13))
                              (setq prefix-arg prefix)
                              (embark--run-action-hooks embark-pre-action-hooks
                                                        action target quit)
                              (command-execute action))
                            (setq final-window (selected-window)))
                        (embark--run-action-hooks embark-post-action-hooks
                                                  action target quit)
                        (when dedicate (set-window-dedicated-p dedicate nil)))
                      (unless (eq final-window action-window)
                        (select-window final-window)))))
              (lambda ()
                (with-selected-window action-window
                  (embark--run-action-hooks embark-pre-action-hooks
                                            action target quit)
                  (unwind-protect (funcall action (plist-get target :target))
                    (embark--run-action-hooks embark-post-action-hooks
                                              action target quit)))))))
      (if quit (embark--quit-and-run run-action) (funcall run-action)))))

(defun embark--refine-symbol-type (_type target)
  "Refine symbol TARGET to command or variable if possible."
  (cons (let ((symbol (intern-soft target))
              (library (ffap-el-mode target)))
          (cond
           ((and library
                 (looking-back "\\(?:require\\|use-package\\).*"
                               (line-beginning-position)))
            'library)
           ((keywordp symbol) 'symbol) ; keywords are bound to themselves!
           ((commandp symbol) 'command)
           ((and symbol (boundp symbol)) 'variable)
           ;; Prefer variables over functions for backward compatibility.
           ;; Command > variable > function > symbol seems like a
           ;; reasonable order with decreasing usefulness of the actions.
           ((fboundp symbol) 'function)
           ((facep symbol) 'face)
           (library 'library)
           (t 'symbol)))
        target))

(defun embark--keybinding-command (_type target)
  "Treat an `embark-keybinding' TARGET as a command."
  (when-let ((cmd (get-text-property 0 'embark-command target)))
    (cons 'command (format "%s" cmd))))

(defun embark--lookup-lighter-minor-mode (_type target)
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

(declare-function project-current "project")
(declare-function project-roots "project")
(declare-function project-root "project")

(defun embark--project-file-full-path (_type target)
  "Get full path of project file TARGET."
  ;; TODO project-find-file can be called from outside all projects in
  ;; which case it prompts for a project first; we don't support that
  ;; case yet, since there is no current project.
  (cons 'file
        (if-let ((project (project-current))
                 (root (if (fboundp 'project-root)
                           (project-root project)
                         (with-no-warnings
                           (car (project-roots project))))))
            (expand-file-name target root)
          target)))

(defun embark--remove-package-version (_type target)
  "Remove version number from a versioned package TARGET."
  (cons 'package (replace-regexp-in-string "-[0-9.]+$" "" target)))

(defun embark--targets ()
  "Retrieve current targets.

An initial guess at the current targets and their types is
determined by running the functions in `embark-target-finders'.
Each function should either return nil, a pair of a type symbol
and target string or a triple of a type symbol, target string and
target bounds.

In the minibuffer only the first target finder returning non-nil
is taken into account.  When finding targets at point in other
buffers, all target finder function is executed.

For each target, the type is then looked up as a key in the
variable `embark-transformer-alist'.  If there is a transformer
for the type, it is called with the type and target, and must
return a `cons' of the transformed type and transformed target.

The return value of `embark--targets' is a list of plists.  Each
plist concerns one target, and has keys `:type', `:target',
`:orig-type', `:orig-target' and `:bounds'."
  (let ((targets))
    (run-hook-wrapped
     'embark-target-finders
     (lambda (fun)
       (when-let (found (funcall fun))
         (let* ((type (car found))
                (target+bounds (cdr found))
                (target (if (consp target+bounds)
                            (car target+bounds)
                          target+bounds))
                (bounds (and (consp target+bounds) (cdr target+bounds)))
                (full-target
                 (append
                  (list :orig-type type :orig-target target :bounds bounds)
                  (if-let (transform (alist-get type embark-transformer-alist))
                      (let ((trans (funcall transform type target)))
                        (list :type (car trans) :target (cdr trans)))
                    (list :type type :target target)))))
           (push full-target targets)
           (minibufferp)))))
    (cl-delete-duplicates
     (nreverse targets)
     :test (lambda (t1 t2)
             (and (equal (plist-get t1 :target) (plist-get t2 :target))
                  (eq (plist-get t1 :type) (plist-get t2 :type)))))))

(defun embark--default-action (type)
  "Return default action for the given TYPE of target.
The most common case is that the target comes from minibuffer
completion, in which case the default action is the command that
opened the minibuffer in the first place.  This can be overridden
by `embark-default-action-overrides'.

For targets that do not come from minibuffer completion
\(typically some thing at point in a regular buffer) and whose
type is not listed in `embark-default-action-overrides', the
default action is given by whatever binding RET has in the action
keymap for the given type."
  (or (alist-get type embark-default-action-overrides)
      (alist-get t embark-default-action-overrides)
      embark--command
      (lookup-key (symbol-value (or (alist-get type embark-keymap-alist)
                                    (alist-get t embark-keymap-alist)))
                  (kbd "RET"))))

(defun embark--rotate (list k)
  "Rotate LIST by K elements and return the rotated list."
  (setq k (mod k (length list)))
  (append (seq-drop list k) (seq-take list k)))

(defun embark--orig-target (target)
  "Convert TARGET to original target."
  (plist-put
   (plist-put
    (copy-sequence target)
    :target (plist-get target :orig-target))
   :type (plist-get target :orig-type)))

;;;###autoload
(defun embark-act (&optional arg)
  "Prompt the user for an action and perform it.
The targets of the action are chosen by `embark-target-finders'.
By default, if called from a minibuffer the target is the top
completion candidate.  When called from a non-minibuffer buffer
there can multiple targets and you can cycle among them by using
`embark-cycle' (which is bound by default to the same key
binding `embark-act' is, but see `embark-cycle-key').

This command uses `embark-prompter' to ask the user to specify an
action, and calls it injecting the target at the first minibuffer
prompt.

If you call this from the minibuffer, it can optionally quit the
minibuffer.  The variable `embark-quit-after-action' controls
whether calling `embark-act' with nil ARG quits the minibuffer,
and if ARG is non-nil it will do the opposite.  Interactively,
ARG is the prefix argument.

If instead you call this from outside the minibuffer, the first
ARG targets are skipped over (if ARG is negative the skipping is
done by cycling backwards) and cycling starts from the following
target."
  (interactive "P")
  (let* ((targets (or (embark--targets) (user-error "No target found")))
         (indicators (mapcar #'funcall embark-indicators))
         (default-done nil))
    (when (and arg (not (minibufferp)))
      (setq targets (embark--rotate targets (prefix-numeric-value arg))))
    (unwind-protect
        (while
            (let* ((target (car targets))
                   (action
                    (or (embark--prompt
                         indicators
                         (let ((embark-default-action-overrides
                                (if default-done
                                    `((t . ,default-done))
                                  embark-default-action-overrides)))
                           (embark--action-keymap (plist-get target :type)
                                                  (cdr targets)))
                         targets)
                        (user-error "Canceled")))
                   (default-action (or default-done
                                       (embark--default-action
                                        (plist-get target :type)))))
              (cond
               ;; When acting twice in the minibuffer, do not restart
               ;; `embark-act'.  Otherwise the next `embark-act' will
               ;; find a target in the original buffer.
               ((eq action #'embark-act)
                (message "Press an action key"))
               ((eq action #'embark-cycle)
                (setq targets (embark--rotate
                               targets (prefix-numeric-value prefix-arg))))
               (t
                ;; if the action is non-repeatable, cleanup indicator now
                (let ((repeat (memq action embark-repeat-actions)))
                  (unless repeat (mapc #'funcall indicators))
                  (condition-case err
                      (embark--act
                       action
                       (if (and (eq action default-action)
                                (eq action embark--command))
                           (embark--orig-target target)
                         target)
                       (if embark-quit-after-action (not arg) arg))
                    (user-error
                     (funcall (if repeat #'message #'user-error)
                              "%s" (cadr err))))
                  (when-let (new-targets (and repeat (embark--targets)))
                    ;; Terminate repeated prompter on default action,
                    ;; when repeating. Jump to the region type if the
                    ;; region is active after the action, or else to the
                    ;; current type again.
                    (setq default-done #'embark-done
                          targets
                          (embark--rotate
                           new-targets
                           (or (cl-position-if
                                (let ((desired-type
                                       ;; TODO Introduce customizable variable,
                                       ;; instead of hard-coding the mark commands.
                                       (if (memq action '(mark
                                                          outline-mark-subtree
                                                          org-mark-subtree))
                                           'region
                                         (plist-get (car targets) :type))))
                                  (lambda (x)
                                    (eq (plist-get x :type) desired-type)))
                                new-targets)
                               0)))))))))
      (mapc #'funcall indicators))))

(defun embark--maybe-transform-candidates ()
  "Collect candidates and see if they all transform to the same type.
Return a plist with keys `:type', `:orig-type', `:candidates', and
`:orig-candidates'."
 (pcase-let ((`(,type . ,candidates)
              (run-hook-with-args-until-success 'embark-candidate-collectors)))
   (append
    (list :orig-type type :orig-candidates candidates)
    (or (unless (null candidates)
          (when-let ((transformer (alist-get type embark-transformer-alist)))
            (pcase-let* ((`(,new-type . ,first-cand)
                          (funcall transformer type (car candidates))))
              (let ((new-candidates (list first-cand)))
                (when (cl-every
                       (lambda (cand)
                         (pcase-let ((`(,t-type . ,t-cand)
                                      (funcall transformer type cand)))
                           (when (eq t-type new-type)
                             (push t-cand new-candidates)
                             t)))
                       (cdr candidates))
                  (list :type new-type
                        :candidates (nreverse new-candidates)))))))
        (list :type type :candidates candidates)))))

;;;###autoload
(defun embark-act-all (&optional arg)
  "Prompt the user for an action and perform it on each candidate.
The candidates are chosen by `embark-candidate-collectors'.
By default, if called from a minibuffer the candidates are the
completion candidates.

This command uses `embark-prompter' to ask the user to specify an
action, and calls it injecting the target at the first minibuffer
prompt.

If you call this from the minibuffer, it can optionally quit the
minibuffer.  The variable `embark-quit-after-action' controls
whether calling `embark-act' with nil ARG quits the minibuffer,
and if ARG is non-nil it will do the opposite.  Interactively,
ARG is the prefix argument."
  (interactive "P")
  (let* ((transformed (embark--maybe-transform-candidates))
         (type (plist-get transformed :type))
         (orig-type (plist-get transformed :orig-type))
         (dir (embark--default-directory))
         (candidates
          (or (cl-mapcar
               (lambda (cand orig-cand)
                 (list :type type :orig-type orig-type
                       ;; TODO The file special casing here seems odd.
                       ;; Why do we need this?
                       :target (if (eq type 'file) (expand-file-name cand dir) cand)
                       :orig-target orig-cand))
               (plist-get transformed :candidates)
               (plist-get transformed :orig-candidates))
              (user-error "No candidates for export")))
         (indicators (mapcar #'funcall embark-indicators)))
    (unwind-protect
        (let* ((action
                (or (embark--prompt
                     indicators (embark--action-keymap type nil)
                     (list (list :type type :multi (length candidates))))
                    (user-error "Canceled")))
               (post-action-wo-restart
                (mapcar (lambda (x) (remq 'embark--restart x))
                        embark-post-action-hooks))
               (act (lambda (candidate)
                      (let ((embark-allow-edit-actions nil)
                            (embark-post-action-hooks post-action-wo-restart))
                        (embark--act action candidate)))))
          (when (and (eq action (embark--default-action type))
                     (eq action embark--command))
            (setq candidates (mapcar #'embark--orig-target candidates)))
          (when (y-or-n-p (format "Run %s on %d %ss? "
                                  action (length candidates) type))
            (if (if embark-quit-after-action (not arg) arg)
                (embark--quit-and-run #'mapc act candidates)
              (mapc act candidates)
              (when (memq 'embark--restart
                          (alist-get action embark-post-action-hooks))
                (embark--restart)))))
      (mapc #'funcall indicators))))

(defun embark-highlight-indicator ()
  "Action indicator highlighting the target at point."
  (let (overlay)
    (lambda (&optional keymap targets _prefix)
      (let ((bounds (plist-get (car targets) :bounds)))
        (when (and overlay (or (not keymap) (not bounds)))
          (delete-overlay overlay)
          (setq overlay nil))
        (when bounds
          (if overlay
              (move-overlay overlay (car bounds) (cdr bounds))
            (setq overlay (make-overlay (car bounds) (cdr bounds))))
          (overlay-put overlay 'face 'embark-target)
          (overlay-put overlay 'window (selected-window))
          ;; high priority to override both bug reference and the lazy
          ;; isearch highlights in embark-isearch-highlight-indicator
          (overlay-put overlay 'priority 1001))))))

(defun embark-isearch-highlight-indicator ()
  "Action indicator highlighting all occurrences of the identifier at point.
This indicator only does something for targets which are
identifiers or symbols.  For those it uses `isearch''s lazy
highlighting feature to highlight all occurrences of the target in
the buffer.  This indicator is best used in conjunction with
`embark-highlight-indicator': by using them both you get the
target and the other occurrences of it higlighted in different
colors."
  (lambda (&optional _keymap targets _prefix)
    (if (and (not (minibufferp))
             (memq (plist-get (car targets) :orig-type) '(symbol identifier)))
        (let ((isearch-string (plist-get (car targets) :target))
              (isearch-regexp-function #'isearch-symbol-regexp))
          (isearch-lazy-highlight-new-loop))
      (setq isearch-lazy-highlight-last-string nil)
      (lazy-highlight-cleanup t))))

(defun embark-cycle (_arg)
  "Cycle over the next ARG targets at point.
If ARG is negative, cycle backwards."
  (interactive "p")
  (user-error "Not meant to be called directly"))

(defun embark-done ()
  "Terminate sequence of repeated actions."
  (interactive))

;;;###autoload
(defun embark-dwim (&optional arg)
  "Run the default action on the current target.
The target of the action is chosen by `embark-target-finders'.

If the target comes from minibuffer completion, then the default
action is the command that opened the minibuffer in the first
place, unless overidden by `embark-default-action-overrides'.

For targets that do not come from minibuffer completion
\(typically some thing at point in a regular buffer) and whose
type is not listed in `embark-default-action-overrides', the
default action is given by whatever binding RET has in the action
keymap for the target's type.

See `embark-act' for the meaning of the prefix ARG."
  (interactive "P")
  (if-let ((targets (embark--targets)))
      (let* ((target
              (or (nth
                   (if (or (null arg) (minibufferp))
                       0
                     (mod (prefix-numeric-value arg) (length targets)))
                   targets)))
             (default-action (embark--default-action (plist-get target :type))))
        (embark--act (or (command-remapping default-action) default-action)
                     (if (eq default-action embark--command)
                         (embark--orig-target target)
                       target)
                     (if embark-quit-after-action (not arg) arg)))
    (user-error "No target found")))

(define-obsolete-function-alias
  'embark-default-action
  'embark-dwim
  "0.11")

(defun embark--become-keymap ()
  "Return keymap of commands to become for current command."
  (let ((map (make-composed-keymap
              (cl-loop for keymap-name in embark-become-keymaps
                       for keymap = (symbol-value keymap-name)
                       when (where-is-internal embark--command (list keymap))
                       collect keymap))))
    (when embark-help-key
      (define-key map embark-help-key #'embark-keymap-help))
    map))

;;;###autoload
(defun embark-become (&optional full)
  "Make current command become a different command.
Take the current minibuffer input as initial input for new
command.  The new command can be run normally using key bindings or
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
  (unless (minibufferp)
    (user-error "Not in a minibuffer"))
  (let* ((target (if full
                     (minibuffer-contents)
                   (pcase-let ((`(,beg . ,end) (embark--boundaries)))
                     (substring (minibuffer-contents) beg
                                (+ end (embark--minibuffer-point))))))
         (keymap (embark--become-keymap))
         (targets `((:type embark-become :target ,target)))
         (indicators (mapcar #'funcall embark-indicators))
         (become (unwind-protect
                     (embark--prompt indicators keymap targets)
                   (mapc #'funcall indicators))))
    (unless become
      (user-error "Canceled"))
    (embark--become-command become target)))

(defun embark--become-command (command input)
  "Quit current minibuffer and start COMMAND with INPUT."
  (embark--quit-and-run
   (lambda ()
     (minibuffer-with-setup-hook
         (lambda ()
           (delete-minibuffer-contents)
           (insert input))
       (let ((this-command command)
             ;; the next two avoid mouse dialogs
             (use-dialog-box nil)
             (last-nonmenu-event 13))
         (command-execute command))))))

(defmacro embark-define-keymap (name doc &rest bindings)
  "Define keymap variable NAME.
DOC is the documentation string.

BINDINGS specifies the key bindings as (string command) pairs;
the strings are passed to `kbd' to determine which key sequence
to bind.

Before the actual list of binding pairs you can include the
keyword `:parent' followed by a keymap, to specify a parent for
the defined keymap.  If the `:parent' keymap is absent,
`embark-general-map' is used by default."
  (declare (indent 1))
  (let* ((map (make-symbol "map"))
         (parent (if (eq :parent (car bindings))
                     (cadr bindings)
                   'embark-general-map))
         (bindings (if (eq :parent (car bindings)) (cddr bindings) bindings)))
    `(defvar ,name
       (let ((,map (make-sparse-keymap)))
         ,@(mapcar (pcase-lambda (`(,key ,fn))
                     (when (stringp key) (setq key (kbd key)))
                     `(define-key ,map ,key ,(if (symbolp fn) `#',fn fn)))
                   bindings)
         ,(if parent `(make-composed-keymap ,map ,parent) map))
       ,doc)))

;;; Embark collect

(defgroup embark-collect nil
  "Buffers for acting on collected Embark targets."
  :group 'embark)

(defcustom embark-candidate-collectors
  '(embark-minibuffer-candidates
    embark-completions-buffer-candidates
    embark-dired-candidates
    embark-ibuffer-candidates
    embark-embark-collect-candidates
    embark-custom-candidates)
  "List of functions that collect all candidates in a given context.
These are used to fill an Embark Collect buffer.  Each function
should return either nil (to indicate it found no candidates) or
a list whose first element is a symbol indicating the type of
candidates and whose `cdr' is the list of candidates, each of
which should be a string."
  :type 'hook)

(defcustom embark-collect-initial-view-alist
  '((file . grid)
    (buffer . grid)
    (symbol . list)
    (kill-ring . zebra)
    (t . list))
  "Initial views for Embark Collect buffers by type.
This is an alist associating completion types to either `list',
`grid' or `zebra' (which means list view the Embark Collect Zebra
minor mode activated).  Additionally you can associate t to a
default initial view for types not mentioned separately."
  :type '(alist
          :key-type symbol
          :value-type (choice (const :tag "List view" list)
                              (const :tag "Grid view" grid)
                              (const :tag "List with Zebra stripes" zebra))))

(defcustom embark-exporters-alist
  '((buffer . embark-export-ibuffer)
    (file . embark-export-dired)
    (package . embark-export-list-packages)
    (bookmark . embark-export-bookmarks)
    (variable . embark-export-customize-variable)
    (face . embark-export-customize-face)
    (symbol . embark-export-apropos)
    (minor-mode . embark-export-apropos)
    (function . embark-export-apropos)
    (command . embark-export-apropos)
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

(defcustom embark-after-export-hook nil
  "Hook run after `embark-export' in the newly created buffer."
  :type 'hook)

(defcustom embark-collect-live-update-delay 0.15
  "Wait this long for more input before updating Embark Collect Live buffer."
  :type 'number)

(defcustom embark-collect-live-initial-delay 0.3
  "Wait this long for input before popping up Embark Collect Live buffer."
  :type 'number)

(defface embark-collect-candidate '((t :inherit default))
  "Face for candidates in Embark Collect.")

(defface embark-collect-zebra-highlight
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#efefef")
    (((class color) (min-colors 88) (background dark))
     :background "#242424"))
  "Face to highlight alternate rows in `embark-collect-zebra-minor-mode'.")

(defface embark-collect-annotation '((t :inherit completions-annotations))
  "Face for annotations in Embark Collect.
This is only used for annotation that are not already fontified.")

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
(declare-function package--from-builtin "package")
(declare-function package-desc-extras "package")
(declare-function package-desc-name "package")
(defvar package--builtins)
(defvar package-alist)
(defvar package-archive-contents)
(defvar package--initialized)

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
                 (embark--minibuffer-point)))
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
    (cons
     (completion-metadata-get (embark--metadata) 'category)
     (nconc (cl-copy-list (completion-all-sorted-completions)) nil))))

(declare-function dired-get-marked-files "dired")

(defun embark-dired-candidates ()
  "Return marked or all files shown in dired buffer.
If any buffer is marked, return marked buffers; otherwise, return
all buffers."
  (when (derived-mode-p 'dired-mode)
    (cons 'file
          (or
           ;; dired-get-marked-files returns the file on the current
           ;; line if no marked files are found; and when the fourth
           ;; argument is non-nil, the "no marked files" case is
           ;; distinguished from the "single marked file" case by
           ;; returning (list t marked-file) in the latter
           (let ((marked (dired-get-marked-files t nil nil t)))
             (and (not (null (cdr marked)))
                  (if (eq (car marked) t) (cdr marked) marked)))
           (save-excursion
             (goto-char (point-min))
             (let (files)
               (while (not (eobp))
                 (when-let (file (dired-get-filename t t))
                   (push file files))
                 (forward-line))
               (nreverse files)))))))

(autoload 'ibuffer-marked-buffer-names "ibuffer")
(declare-function ibuffer-map-lines-nomodify "ibuffer")

(defun embark-ibuffer-candidates ()
  "Return marked or all buffers listed in ibuffer buffer.
If any buffer is marked, return marked buffers; otherwise, return
all buffers."
  (when (derived-mode-p 'ibuffer-mode)
    (cons 'buffer
          (or (ibuffer-marked-buffer-names)
              (let (buffers)
                (ibuffer-map-lines-nomodify
                 (lambda (buffer _mark)
                   (push (buffer-name buffer) buffers)))
                (nreverse buffers))))))

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

(defun embark-custom-candidates ()
  "Return all variables and faces listed in this `Custom-mode' buffer."
  (when (derived-mode-p 'Custom-mode)
    (cons 'symbol ; gets refined to variable or face when acted upon
          (save-excursion
            (goto-char (point-min))
            (let (symbols)
              (while (not (eobp))
                (when-let (widget (widget-at (point)))
                  (when (eq (car widget) 'custom-visibility)
                    (push
                     (symbol-name
                      (plist-get (cdr (plist-get (cdr widget) :parent)) :value))
                     symbols)))
                (forward-line))
              (nreverse symbols))))))

(defun embark--action-command (action)
  "Turn an ACTION into a command to perform the action.
Returns the name of the command."
  (let ((name (intern (format "embark-action--%s"
                              (embark--command-name action)))))
    (fset name (lambda ()
                 (interactive)
                 (when-let (target (car (embark--targets)))
                   (embark--act action target))))
    (put name 'function-documentation (documentation action))
    name))

(defun embark--all-bindings (keymap &optional nested)
  "Return an alist of all bindings in KEYMAP.
If NESTED is non-nil subkeymaps are not flattened."
  (let (bindings maps)
    (map-keymap
     (lambda (key def)
       (cond
        ((keymapp def)
         (if nested
             (push (cons (vector key) def) maps)
           (dolist (bind (embark--all-bindings def))
             (push (cons (vconcat (vector key) (car bind))
                         (cdr bind))
                   maps))))
        (def (push (cons (vector key) def) bindings))))
     (keymap-canonicalize keymap))
    (nconc (nreverse bindings) (nreverse maps))))

(defvar embark-collect-direct-action-minor-mode-map (make-sparse-keymap)
  "Keymap for direct bindings to embark actions.")

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
                                   (embark--action-keymap embark--type nil))
               unless (eq cmd 'embark-keymap-help)
               do (define-key map key (embark--action-command cmd))))))

(define-button-type 'embark-collect-entry
  'face 'embark-collect-candidate
  'action 'embark-collect-choose)

(defun embark--boundaries ()
  "Get current minibuffer completion boundaries."
  (let ((contents (minibuffer-contents))
        (pt (embark--minibuffer-point)))
    (completion-boundaries
     (substring contents 0 pt)
     minibuffer-completion-table
     minibuffer-completion-predicate
     (substring contents pt))))

(defun embark-collect-choose (entry)
  "Select a completion or run default action on Embark Collect ENTRY.

If the current buffer is an Embark Collect Completions buffer,
complete the minibuffer input to ENTRY and, unless this leads to
new completion candidates (for example, when entering a directory
in `find-file') or the command was called with a prefix argument,
exit the minibuffer.

For other Embark Collect buffers, run the default action on ENTRY."
  (let* ((start (button-start entry))
         (end (button-end entry))
         (text (buffer-substring start end))) ; keep properties
    (when (eq embark--type 'file)
      (setq text (abbreviate-file-name (expand-file-name text))))
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
          ;; find-file). If so, don't exit.
          (unless (or current-prefix-arg
                      (= (car (embark--boundaries))
                         (embark--minibuffer-point)))
            (exit-minibuffer)))
      (embark--act (embark--default-action embark--type)
                   (list :target text
                         :type embark--type
                         :bounds (cons start end))))))

(embark-define-keymap embark-collect-mode-map
  "Keymap for Embark collect mode."
  :parent tabulated-list-mode-map
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

(define-derived-mode embark-collect-mode tabulated-list-mode "Embark Collect"
  "List of candidates to be acted on.
The command `embark-act' is bound `embark-collect-mode-map', but
you might prefer to change the key binding to match your other
key binding for it.  Or alternatively you might want to enable
`embark-collect-direct-action-minor-mode' in
`embark-collect-mode-hook'.")

(defmacro embark--static-if (cond then &rest else)
  "If COND yields non-nil at compile time, do THEN, else do ELSE."
  (declare (indent 2))
  (if (eval cond) then (macroexp-progn else)))

(defun embark--display-width (string)
  "Return width of STRING taking display and invisible properties into account."
  (let ((len (length string)) (pos 0) (width 0))
    (while (/= pos len)
      (let ((dis (next-single-property-change pos 'display string len))
            (display (get-text-property pos 'display string)))
        (if (stringp display)
            (setq width (+ width (string-width display)) pos dis)
          (while (/= pos dis)
            (let ((inv (next-single-property-change pos 'invisible string dis)))
              (unless (get-text-property pos 'invisible string)
                (setq width
                      (+ width
                         ;; bug#47712: Emacs 28 can compute `string-width'
                         ;; of substrings
                         (embark--static-if (= (cdr (func-arity #'string-width))
                                               3)
                             (string-width string pos inv)
                           (string-width
                            ;; Avoid allocation for the full string.
                            (if (and (= pos 0) (= inv len))
                                string
                              (substring-no-properties string pos inv)))))))
              (setq pos inv))))))
    width))

(defun embark-collect--max-width (items)
  "Maximum width of any of the ITEMS.
Each item can be a string or a list of three strings.  In the
latter case, the lengths of the first two elements are added to
determine the width."
  (or (if (stringp (car items))
          (cl-loop for str in items maximize (embark--display-width str))
        (cl-loop for (pre str _) in items
                 maximize (+ (embark--display-width pre)
                             (embark--display-width str))))
      0))

(defun embark-collect--list-view ()
  "List view of candidates and annotations for Embark Collect buffer."
  (let ((candidates embark-collect-candidates))
    (when-let ((affixator embark-collect-affixator)
               (dir default-directory)) ; smuggle to the target window
      (with-selected-window (or (embark--target-window) (selected-window))
          (let ((default-directory dir)) ; for file annotator
            (setq candidates (funcall affixator candidates)))))
    (setq tabulated-list-format
          (if embark-collect-affixator
              `[("Candidate" ,(embark-collect--max-width candidates) t)
                ("Annotation" 0 t)]
            [("Candidate" 0 t)]))
    (if tabulated-list-use-header-line
        (tabulated-list-init-header)
      (setq header-line-format nil tabulated-list--header-string nil))
    (setq tabulated-list-entries
          (mapcar
           (if embark-collect-affixator
               (pcase-lambda (`(,cand ,prefix ,annotation))
                 (let* ((length (length annotation))
                        (faces (text-property-not-all
                                0 length 'face nil annotation)))
                   (when faces (add-face-text-property
                                0 length 'default t annotation))
                   `(,cand
                     [(,(propertize cand 'line-prefix prefix)
                       type embark-collect-entry)
                      (,annotation
                       ,@(unless faces
                           '(face embark-collect-annotation)))])))
             (lambda (cand)
               `(,cand [(,cand type embark-collect-entry)])))
           candidates))))

(defun embark-collect--remove-zebra-stripes ()
  "Remove highlighting of alternate rows."
  (remove-overlays nil nil 'face 'embark-collect-zebra-highlight))

(defun embark-collect--add-zebra-stripes ()
  "Highlight alternate rows with the `embark-collect-zebra-highlight' face."
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
            (let ((stripe (make-overlay pt (point))))
              (overlay-put stripe 'priority -100) ; below hl-line-mode's -50
              (overlay-put stripe 'face 'embark-collect-zebra-highlight))))))))

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
  (let* ((width (min (1+ (embark-collect--max-width embark-collect-candidates))
                     (1- (floor (window-width) 2))))
         (columns (/ (window-width) (1+ width))))
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

(defun embark-collect--metadatum (type metadatum)
  "Get METADATUM for current buffer's candidates.
For non-minibuffers, assume candidates are of given TYPE."
  (if (minibufferp)
      (or (completion-metadata-get (embark--metadata) metadatum)
          (plist-get completion-extra-properties
                     (intern (format ":%s" metadatum))))
    ;; otherwise fake some metadata for Marginalia users's benefit
    (completion-metadata-get `((category . ,type)) metadatum)))

(defun embark-collect--affixator (type)
  "Get affixation function for current buffer's candidates.
For non-minibuffers, assume candidates are of given TYPE."
  (or (embark-collect--metadatum type 'affixation-function)
      (when-let ((annotator
                  (embark-collect--metadatum type 'annotation-function)))
        (lambda (candidates)
          (mapcar (lambda (c)
                    (if-let (a (funcall annotator c)) (list c "" a) c))
                  candidates)))))

(defun embark-collect--revert ()
  "Recalculate Embark Collect candidates if possible."
  (when (buffer-live-p embark-collect-from)
    (pcase-let ((`(,type . ,candidates)
                 (with-current-buffer embark-collect-from
                   (run-hook-with-args-until-success
                    'embark-candidate-collectors))))
      (setq embark--type type
            embark-collect-candidates candidates
            default-directory (with-current-buffer embark-collect-from
                                (embark--default-directory))
            embark-collect-affixator (or ; new annotator? (marginalia-cycle)
                                      (with-current-buffer embark-collect-from
                                        (embark-collect--affixator type))
                                      embark-collect-affixator))))
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

(defun embark-collect-toggle-view ()
  "Toggle between list and grid views of Embark Collect buffer.
This command can be called either from the Embark Collect buffer
itself, or, from any buffer (particularly a minibuffer) that has
a linked Embark Collect Live buffer."
  (interactive)
  (embark-collect--toggle 'embark-collect-view 'list 'grid))

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
  (pcase-let*
      ((from (current-buffer))
       (buffer (generate-new-buffer name))
       (`(,type . ,candidates)
        (run-hook-with-args-until-success 'embark-candidate-collectors))
       (affixator (embark-collect--affixator type)))
    (when (and (null candidates) (eq kind :snapshot))
      (user-error "No candidates to collect"))
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

      (setq embark--type type
            embark-collect-candidates candidates
            embark-collect-affixator affixator)

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
              ;; set-buffer to a random buffer for some reason, so preserve it
              (save-current-buffer
                (kill-buffer buffer))))
           (:live
            (lambda ()
              (when (buffer-live-p buffer)
                (setf (buffer-local-value 'embark-collect-from buffer) nil)
                (with-current-buffer buffer
                  (save-match-data
                    (rename-buffer
                     (replace-regexp-in-string " Live" "" (buffer-name))
                     t)))
                (embark--run-after-command #'pop-to-buffer buffer))))
           (:snapshot
            (lambda ()
              (when (buffer-live-p buffer)
                (embark--run-after-command #'pop-to-buffer buffer)))))
         nil t)
        (setq minibuffer-scroll-window window))

      window)))

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
  (embark--quit-and-run #'message nil))

;;;###autoload
(defun embark-collect-completions ()
  "Create an ephemeral live-updating Embark Collect buffer."
  (interactive)
  (embark--collect "*Embark Collect Completions*" nil :completions))

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

;;;###autoload
(defun embark-collect-completions-after-input ()
  "Start `embark-collect-completions' after some minibuffer input.
Add this function to `minibuffer-setup-hook' to have an Embark
Live Collect buffer popup soon after you type something in the
minibuffer; the length of the delay after typing is given by
`embark-collect-live-initial-delay'."
  (when minibuffer-completion-table
   (add-hook 'after-change-functions #'embark--wait-for-input nil t)))

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
  (let* ((transformed (embark--maybe-transform-candidates))
         (candidates (or (plist-get transformed :candidates)
                         (user-error "No candidates for export")))
         (type (plist-get transformed :type)))
    (let ((exporter (or (alist-get type embark-exporters-alist)
                        (alist-get t embark-exporters-alist))))
      (if (eq exporter 'embark-collect-snapshot)
          (embark-collect-snapshot)
        (let ((dir (embark--default-directory))
              (after embark-after-export-hook))
          (embark--quit-and-run
           (lambda ()
             ;; TODO see embark--quit-and-run and embark--run-after-command,
             ;; there the default-directory is also smuggled to the lambda.
             ;; This should be fixed properly.
             (let ((default-directory dir) ;; dired needs this info
                   (embark-after-export-hook after))
               (funcall exporter candidates)
               (run-hooks 'embark-after-export-hook)))))))))

(defmacro embark--export-rename (buffer title &rest body)
  "Run BODY and rename BUFFER to Embark export buffer with TITLE."
  (declare (indent 2))
  (let ((saved (make-symbol "saved")))
    `(let ((,saved (embark-rename-buffer
                    ,buffer " *Embark Saved*" t)))
       ,@body
       (pop-to-buffer (embark-rename-buffer
                       ,buffer ,(format "*Embark Export %s*" title) t))
       (when ,saved (embark-rename-buffer ,saved ,buffer)))))

(defun embark--export-customize (items title type pred)
  "Create a customization buffer listing ITEMS.
TYPE is the items type.
TITLE is the buffer title.
PRED is a predicate function used to filter the items."
  (custom-buffer-create
   (cl-loop for item in items
            for sym = (intern-soft item)
            when (and sym (funcall pred sym)) collect `(,sym ,type))
   (format "*Embark Export %s*" title)))

(autoload 'apropos-parse-pattern "apropos")
(autoload 'apropos-symbols-internal "apropos")
(defun embark-export-apropos (symbols)
  "Create apropos buffer listing SYMBOLS."
  (embark--export-rename "*Apropos*" "Apropos"
    (apropos-parse-pattern "") ;; Initialize apropos pattern
    (apropos-symbols-internal
     (delq nil (mapcar #'intern-soft symbols))
     (bound-and-true-p apropos-do-all))
    (with-current-buffer "*Apropos*"
      ;; Reverting the apropos buffer is not possible
      (setq-local revert-buffer-function #'revert-buffer--default))))

(defun embark-export-customize-face (faces)
  "Create a customization buffer listing FACES."
  (embark--export-customize faces "Faces" 'custom-face #'facep))

(defun embark-export-customize-variable (variables)
  "Create a customization buffer listing VARIABLES."
  ;; The widget library serializes/deserializes the values.
  ;; We advise the serialization in order to avoid errors for nonserializable variables.
  (cl-letf* ((ht (make-hash-table :test #'equal))
             (orig-read (symbol-function #'read))
             (orig-write (symbol-function 'widget-sexp-value-to-internal))
             ((symbol-function #'read)
              (lambda (&optional str)
                (condition-case nil
                    (funcall orig-read str)
                  (error (gethash str ht)))))
             ((symbol-function 'widget-sexp-value-to-internal)
              (lambda (widget val)
                (let ((str (funcall orig-write widget val)))
                  (puthash str val ht)
                  str))))
    (embark--export-customize variables "Variables" 'custom-variable #'boundp)))

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
  (let ((buf
         (dired-noselect
          (cons
           ;; TODO: is it worth finding the deepest common containing directory?
           (if (cl-every #'file-name-absolute-p files) "/" default-directory)
           files))))
    (with-current-buffer buf
      (rename-buffer (format "*Embark Export Dired %s*" default-directory)))
    (pop-to-buffer buf)))

(autoload 'package-menu-mode "package")
(autoload 'package-menu--generate "package")

(defun embark-export-list-packages (packages)
  "Create a package menu mode buffer listing PACKAGES."
  (let ((buf (generate-new-buffer "*Embark Export Packages*")))
    (with-current-buffer buf
      (package-menu-mode)
      (package-menu--generate nil (mapcar #'intern packages)))
    (pop-to-buffer buf)))

(defvar bookmark-alist)

(defun embark-export-bookmarks (bookmarks)
  "Create a `bookmark-bmenu-mode' buffer listing BOOKMARKS."
  (embark--export-rename "*Bookmark List*" "Bookmarks"
    (let ((bookmark-alist
           (cl-remove-if-not
            (lambda (bmark)
              (member (car bmark) bookmarks))
            bookmark-alist)))
      (bookmark-bmenu-list))))

;;; Integration with external completion UIs

;; vertico

(declare-function vertico--candidate "ext:vertico")
(declare-function vertico--exhibit "ext:vertico")
(defvar vertico--input)
(defvar vertico--candidates)

(defun embark--vertico-selected ()
  "Target the currently selected item in Vertico.
Return the category metadatum as the type of the target."
  (when vertico--input
    ;; Force candidate computation, if candidates are not yet available.
    (when (eq vertico--input t)
      (vertico--exhibit))
    (cons (completion-metadata-get (embark--metadata) 'category)
          (vertico--candidate))))

(defun embark--vertico-candidates ()
  "Collect the current Vertico candidates.
Return the category metadatum as the type of the candidates."
  (when vertico--input
    ;; Force candidate computation, if candidates are not yet available.
    (when (eq vertico--input t)
      (vertico--exhibit))
    (cons (completion-metadata-get (embark--metadata) 'category)
          vertico--candidates)))

(with-eval-after-load 'vertico
  (add-hook 'embark-target-finders #'embark--vertico-selected)
  (add-hook 'embark-candidate-collectors #'embark--vertico-candidates))

;; selectrum

(declare-function selectrum--get-meta "ext:selectrum")
(declare-function selectrum-get-current-candidate "ext:selectrum")
(declare-function selectrum-get-current-candidates "ext:selectrum")
(declare-function selectrum-exhibit "ext:selectrum")
(defvar selectrum-is-active)
(defvar selectrum--previous-input-string)

(defun embark--selectrum-selected ()
  "Target the currently selected item in Selectrum.
Return the category metadatum as the type of the target."
  (when selectrum-is-active
    ;; Force candidate computation, if candidates are not yet available.
    (unless selectrum--previous-input-string
      (selectrum-exhibit))
    (cons (selectrum--get-meta 'category)
	  (selectrum-get-current-candidate))))

(defun embark--selectrum-candidates ()
  "Collect the current Selectrum candidates.
Return the category metadatum as the type of the candidates."
  (when selectrum-is-active
    ;; Force candidate computation, if candidates are not yet available.
    (unless selectrum--previous-input-string
      (selectrum-exhibit))
    (cons (selectrum--get-meta 'category)
	  (selectrum-get-current-candidates
	   ;; Pass relative file names for dired.
	   minibuffer-completing-file-name))))

(with-eval-after-load 'selectrum
  (add-hook 'embark-target-finders #'embark--selectrum-selected)
  (add-hook 'embark-candidate-collectors #'embark--selectrum-candidates))

;; ivy

(declare-function ivy--expand-file-name "ext:ivy")
(declare-function ivy-state-current "ext:ivy")
(defvar ivy-text)
(defvar ivy-last)
(defvar ivy--old-cands) ; this stores the current candidates :)
(defvar ivy--length)

(defun embark--ivy-selected ()
  "Target the currently selected item in Ivy.
Return the category metadatum as the type of the target."
  ;; my favorite way of detecting Ivy
  (when (memq 'ivy--queue-exhibit post-command-hook)
    (cons
     (completion-metadata-get (embark--metadata) 'category)
     (ivy--expand-file-name
      (if (and (> ivy--length 0)
               (stringp (ivy-state-current ivy-last)))
          (ivy-state-current ivy-last)
        ivy-text)))))

(defun embark--ivy-candidates ()
  "Return all current Ivy candidates."
  ;; my favorite way of detecting Ivy
  (when (memq 'ivy--queue-exhibit post-command-hook)
    (cons
     ;; swiper-isearch uses swiper-isearch-function as a completion
     ;; table, but it doesn't understand metadata queries
     (ignore-errors
       (completion-metadata-get (embark--metadata) 'category))
     ivy--old-cands)))

(with-eval-after-load 'ivy
  (add-hook 'embark-target-finders #'embark--ivy-selected)
  (add-hook 'embark-candidate-collectors #'embark--ivy-candidates))

;;; Custom actions

(defun embark-keymap-help ()
  "Prompt for an action to perform or command to become and run it."
  (interactive)
  (user-error "Not meant to be called directly"))

(defun embark-insert (string)
  "Insert STRING at point."
  (interactive "sInsert: ")
  (if buffer-read-only
      (with-selected-window (other-window-for-scrolling)
        (insert string))
    (insert string)))

(define-obsolete-function-alias
  'embark-save
  'kill-new
  "0.11")

(defun embark-eshell (file)
  "Run eshell in directory of FILE."
  (interactive "GDirectory: ")
  (let ((default-directory
          (file-name-directory
           (expand-file-name
            (substitute-in-file-name file)))))
    (eshell '(4))))

;; For Emacs 28 dired-jump will be moved to dired.el, but it seems
;; that since it already has an autoload in Emacs 28, this next
;; autoload is ignored.
(autoload 'dired-jump "dired-x" nil t)

(defun embark-dired-jump (file &optional other-window)
  "Open dired buffer in directory containg FILE and move to its line.
When called with a prefix argument OTHER-WINDOW, open dired in other window."
  (interactive "fJump to Dired file: \nP")
  (dired-jump other-window file))

(defun embark--read-from-history (prompt candidates &optional category)
  "Read with completion from list of history CANDIDATES of CATEGORY.
Sorting and history are disabled. PROMPT is the prompt message."
  (completing-read prompt
                   (lambda (string predicate action)
                     (if (eq action 'metadata)
                         `(metadata (display-sort-function . identity)
                                    (cycle-sort-function . identity)
                                    (category . ,category))
                       (complete-with-action action candidates string predicate)))
                   nil t nil t))

(defun embark-kill-ring-remove (text)
  "Remove TEXT from `kill-ring'."
  (interactive (list (embark--read-from-history
                      "Remove from kill-ring: " kill-ring 'kill-ring)))
  (embark-history-remove text)
  (setq kill-ring (delete text kill-ring)))

(defvar recentf-list)
(defun embark-recentf-remove (file)
  "Remove FILE from the list of recent files."
  (interactive (list (embark--read-from-history
                      "Remove recent file: " recentf-list 'file)))
  (embark-history-remove file)
  (setq recentf-list (delete (expand-file-name file) recentf-list)))

(defun embark-history-remove (str)
  "Remove STR from `minibuffer-history-variable'.
Many completion UIs sort by history position.  This command can be used
to remove entries from the history, such that they are not sorted closer
to the top."
  (interactive (list (embark--read-from-history
                      "Remove history item: "
                      (if (eq minibuffer-history-variable t)
                          (user-error "No minibuffer history")
                        (symbol-value minibuffer-history-variable)))))
  (unless (eq minibuffer-history-variable t)
    (set minibuffer-history-variable
         (delete str (symbol-value minibuffer-history-variable)))))

(defvar xref-backend-functions)

(defun embark-find-definition (symbol)
  "Find definition of Emacs Lisp SYMBOL."
  (interactive "sSymbol: ")
  (let ((xref-backend-functions (lambda () 'elisp)))
    (xref-find-definitions symbol)))

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

(defun embark--package-url (pkg)
  "Return homepage for package PKG."
  (when-let (desc (embark--package-desc pkg))
    (alist-get :url (package-desc-extras desc))))

(defun embark--prompt-for-package ()
  "Prompt user for a package name."
  ;; this code is taken from the interactive spec of describe-package
  (unless package--initialized
    (package-initialize t))
  (intern
   (completing-read "Package: "
                    (append (mapcar #'car package-alist)
                            (mapcar #'car package-archive-contents)
                            (mapcar #'car package--builtins)))))

(defun embark-browse-package-url (pkg)
  "Open homepage for package PKG with `browse-url'."
  (interactive (list (embark--prompt-for-package)))
  (if-let ((url (embark--package-url pkg)))
      (browse-url url)
    (user-error "No homepage found for `%s'" pkg)))

(defun embark-save-package-url (pkg)
  "Save URL of homepage for package PKG on the `kill-ring'."
  (interactive (list (embark--prompt-for-package)))
  (if-let ((url (embark--package-url pkg)))
      (kill-new url)
    (user-error "No homepage found for `%s'" pkg)))

(defun embark-save-variable-value (var)
  "Save value of VAR in the `kill-ring'."
  (interactive "SVariable: ")
  (kill-new (string-trim (pp-to-string (symbol-value var)))))

(defun embark-insert-variable-value (var)
  "Insert value of VAR."
  (interactive "SVariable: ")
  (insert (string-trim (pp-to-string (symbol-value var)))))

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
  (interactive
   (list (read-char-by-name "Insert character  (Unicode name or hex): ")))
  (kill-new (format "%c" char)))

(defun embark-isearch ()
  "Prompt for string in the minibuffer and start isearch.
Unlike isearch, this command reads the string from the
minibuffer, which means it can be used as an Embark action."
  (interactive)
  (isearch-mode t)
  (isearch-edit-string))

(defun embark-toggle-highlight ()
  "Toggle symbol highlighting using `highlight-symbol-at-point'."
  (interactive)
  (let ((regexp (find-tag-default-as-symbol-regexp))
        (highlighted (cl-find-if #'boundp
                                 '(hi-lock-interactive-lighters
                                   hi-lock-interactive-patterns))))
    (if (and highlighted (assoc regexp (symbol-value highlighted)))
        (unhighlight-regexp regexp)
      (highlight-symbol-at-point))))

(defun embark-next-symbol (symbol)
  "Jump to next occurrence of SYMBOL.
The search respects symbol boundaries."
  (interactive "sSymbol: ")
  (let ((regexp (format "\\_<%s\\_>" (regexp-quote symbol))))
    (when (looking-at regexp)
      (forward-symbol 1))
    (unless (re-search-forward regexp nil t)
      (user-error "Symbol `%s' not found" symbol))))

(defun embark-previous-symbol (symbol)
  "Jump to previous occurrence SYMBOL.
The search respects symbol boundaries."
  (interactive "sSymbol: ")
  (let ((regexp (format "\\_<%s\\_>" (regexp-quote symbol))))
    (when (looking-back regexp (- (point) (length symbol)))
      (forward-symbol -1))
    (unless (re-search-backward regexp nil t)
      (user-error "Symbol `%s' not found" symbol))))

(defun embark-compose-mail (address)
  "Compose email to ADDRESS."
  ;; The only reason we cannot use compose-mail directly is its
  ;; interactive specification, which just supllies nil for the
  ;; address (and several other arguments).
  (interactive "sTo: ")
  (compose-mail address))

(autoload 'pp-display-expression "pp")
(defun embark-pp-eval-defun (edebug)
  "Run `eval-defun' and pretty print the result.
With a prefix argument EDEBUG, instrument the code for debugging."
  (interactive "P")
  (cl-letf (((symbol-function #'eval-expression-print-format)
             (lambda (result)
               (pp-display-expression result "*Pp Eval Output*"))))
    (eval-defun edebug)))

(defun embark-eval-replace ()
  "Evaluate region and replace with evaluated result."
  (interactive)
  (let ((beg (region-beginning))
        (end (region-end)))
    (save-excursion
      (goto-char end)
      (insert (prin1-to-string
               (eval (read (buffer-substring beg end)) lexical-binding)))
      (delete-region beg end))))

;; TODO Report Emacs bug, this function should be provided by Emacs itself.
(defun embark-elp-restore-package (prefix)
  "Remove instrumentation from functions with names starting with PREFIX."
  (interactive "SPrefix: ")
  (when (fboundp 'elp-restore-list)
    (elp-restore-list
     (mapcar #'intern
             (all-completions (symbol-name prefix)
                              obarray 'elp-profilable-p)))))

(defmacro embark--define-hash (algorithm)
  "Define command which computes hash from a string.
ALGORITHM is the hash algorithm symbol understood by `secure-hash'."
  `(defun ,(intern (format "embark-hash-%s" algorithm)) (str)
     ,(format "Compute %s hash of STR and store it in the kill ring." algorithm)
     (interactive "sString: ")
     (let ((hash (secure-hash ',algorithm str)))
       (kill-new hash)
       (message "%s: %s" ',algorithm hash))))

(embark--define-hash md5)
(embark--define-hash sha1)
(embark--define-hash sha224)
(embark--define-hash sha256)
(embark--define-hash sha384)
(embark--define-hash sha512)

(defun embark-encode-url (start end)
  "Properly URI-encode the region between START and END in current buffer."
  (interactive "r")
  (let ((encoded (url-encode-url (buffer-substring-no-properties start end))))
    (delete-region start end)
    (insert encoded)))

(defun embark-decode-url (start end)
  "Decode the URI-encoded region between START and END in current buffer."
  (interactive "r")
  (let ((decoded (url-unhex-string (buffer-substring-no-properties start end))))
    (delete-region start end)
    (insert decoded)))

;;; Setup and pre-action hooks

(defun embark--restart (&rest _)
  "Restart current command with current input.
Use this to refresh the list of candidates for commands that do
not handle that themselves."
  (when (minibufferp)
    (embark--become-command embark--command (minibuffer-contents))))

(defun embark--shell-prep (&rest _)
  "Prepare target for use as argument for a shell command.
This quotes the spaces, inserts an extra space at the beginning
and leaves the point to the left of it."
  (let ((contents (minibuffer-contents)))
    (delete-minibuffer-contents)
    (insert " " (shell-quote-wildcard-pattern contents))
    (goto-char (minibuffer-prompt-end))))

(defun embark--force-complete (&rest _)
  "Select first minibuffer completion candidate matching target."
  (minibuffer-force-complete))

(defun embark--eval-prep (&rest _)
  "If target is: a variable, skip edit; a function, wrap in parens."
  (if (not (fboundp (intern-soft (minibuffer-contents))))
      (add-hook 'post-command-hook #'exit-minibuffer nil t)
    (goto-char (minibuffer-prompt-end))
    (insert "(")
    (goto-char (point-max))
    (insert ")")
    (backward-char)))

(cl-defun embark--beginning-of-target (&key bounds &allow-other-keys)
  "Go to beginning of the target BOUNDS."
  (when bounds
    (goto-char (car bounds))))

(cl-defun embark--end-of-target (&key bounds &allow-other-keys)
  "Go to end of the target BOUNDS."
  (when bounds
    (goto-char (cdr bounds))))

(cl-defun embark--mark-target (&key bounds &allow-other-keys)
  "Mark the target if its BOUNDS are known."
  (when bounds
    (set-mark (cdr bounds))
    (goto-char (car bounds))))

(defun embark--ignore-target (&rest _)
  "Ignore the target."
  (ignore (read-from-minibuffer "")))

(autoload 'xref--push-markers "xref")
(defun embark--xref-push-markers (&rest _)
  "Push the xref markers to leave a location trail."
  (xref--push-markers))

;;; keymaps

(embark-define-keymap embark-meta-map
  "Keymap for non-action Embark functions."
  :parent nil
  ("-" negative-argument)
  ("0" digit-argument)
  ("1" digit-argument)
  ("2" digit-argument)
  ("3" digit-argument)
  ("4" digit-argument)
  ("5" digit-argument)
  ("6" digit-argument)
  ("7" digit-argument)
  ("8" digit-argument)
  ("9" digit-argument))

(embark-define-keymap embark-general-map
  "Keymap for Embark general actions."
  :parent embark-meta-map
  ("i" embark-insert)
  ("w" kill-new)
  ("E" embark-export)
  ("S" embark-collect-snapshot)
  ("L" embark-collect-live)
  ("B" embark-become)
  ("A" embark-act-all)
  ("C-s" embark-isearch)
  ("SPC" mark)
  ("DEL" delete-region))

;; TODO add more encode actions, see M-x or C-h f encode region
(embark-define-keymap embark-encode-map
  "Keymap for Embark region encoding actions."
  :parent nil
  ("r" rot13-region)
  ("." morse-region)
  ("-" unmorse-region)
  ("m" embark-hash-md5)
  ("1" embark-hash-sha1)
  ("2" embark-hash-sha256)
  ("3" embark-hash-sha384)
  ("4" embark-hash-sha224)
  ("5" embark-hash-sha512)
  ("f" format-encode-region)
  ("F" format-decode-region)
  ("u" embark-encode-url)
  ("U" embark-decode-url))

(fset 'embark-encode-map embark-encode-map)

(embark-define-keymap embark-sort-map
  "Keymap for Embark actions that sort the region"
  :parent nil
  ("l" sort-lines)
  ("P" sort-pages)
  ("f" sort-fields)
  ("c" sort-columns)
  ("p" sort-paragraphs)
  ("r" sort-regexp-fields)
  ("n" sort-numeric-fields))

(fset 'embark-sort-map embark-sort-map)

;; these will have autoloads in Emacs 28
(autoload 'calc-grab-sum-down "calc" nil t)
(autoload 'calc-grab-sum-across "calc" nil t)

;; this has had an autoload cookie since at least Emacs 26
;; but that autoload doesn't seem to work for me
(autoload 'org-table-convert-region "org-table" nil t)

(embark-define-keymap embark-region-map
  "Keymap for Embark actions on the active region."
  ("u" upcase-region)
  ("l" downcase-region)
  ("c" capitalize-region)
  ("|" shell-command-on-region)
  ("e" eval-region)
  ("<" embark-eval-replace)
  ("a" align)
  ("A" align-regexp)
  ("i" indent-rigidly)
  ("TAB" indent-region)
  ("f" fill-region)
  ("p" fill-region-as-paragraph)
  ("$" ispell-region)
  ("=" count-words-region)
  ("SPC" whitespace-cleanup-region)
  ("t" transpose-regions)
  ("o" org-table-convert-region)
  (";" comment-or-uncomment-region)
  ("W" write-region)
  ("+" append-to-file)
  ("m" apply-macro-to-region-lines)
  ("n" narrow-to-region)
  ("*" calc-grab-region)
  (":" calc-grab-sum-down)
  ("_" calc-grab-sum-across)
  ("r" reverse-region)
  ("D" delete-duplicate-lines)
  ("s" 'embark-sort-map)
  (">" 'embark-encode-map))

(embark-define-keymap embark-file-map
  "Keymap for Embark file actions."
  ("RET" find-file)
  ("f" find-file)
  ("o" find-file-other-window)
  ("d" delete-file)
  ("D" delete-directory)
  ("r" rename-file)
  ("c" copy-file)
  ("j" embark-dired-jump)
  ("!" shell-command)
  ("&" async-shell-command)
  ("<" insert-file)
  ("m" chmod)
  ("=" ediff-files)
  ("e" embark-eshell)
  ("+" make-directory)
  ("\\" embark-recentf-remove)
  ("I" embark-insert-relative-path)
  ("W" embark-save-relative-path)
  ("l" load-file)
  ("b" byte-compile-file)
  ("R" byte-recompile-directory))

(embark-define-keymap embark-kill-ring-map
  "Keymap for `kill-ring' commands."
  ("\\" embark-kill-ring-remove))

(embark-define-keymap embark-url-map
  "Keymap for Embark url actions."
  ("RET" browse-url)
  ("b" browse-url)
  ("e" eww))

(embark-define-keymap embark-email-map
  "Keymap for Embark email actions."
  ("RET" embark-compose-mail)
  ("c" embark-compose-mail))

(embark-define-keymap embark-library-map
  "Keymap for operations on Emacs Lisp libraries."
  ("RET" find-library)
  ("l" load-library)
  ("f" find-library)
  ("h" finder-commentary)
  ("a" apropos-library)
  ("L" locate-library))

(embark-define-keymap embark-buffer-map
  "Keymap for Embark buffer actions."
  ("RET" switch-to-buffer)
  ("k" kill-buffer)
  ("b" switch-to-buffer)
  ("o" switch-to-buffer-other-window)
  ("z" embark-bury-buffer)
  ("q" embark-kill-buffer-and-window)
  ("r" embark-rename-buffer)
  ("=" ediff-buffers)
  ("|" embark-shell-command-on-buffer)
  ("<" insert-buffer))

(embark-define-keymap embark-identifier-map
  "Keymap for Embark identifier actions."
  ("RET" xref-find-definitions)
  ("h" display-local-help)
  ("H" embark-toggle-highlight)
  ("d" xref-find-definitions)
  ("r" xref-find-references)
  ("a" xref-find-apropos)
  ("s" info-lookup-symbol)
  ("n" embark-next-symbol)
  ("p" embark-previous-symbol)
  ("$" ispell-word))

(embark-define-keymap embark-expression-map
  "Keymap for Embark expression actions."
  ("RET" pp-eval-expression)
  ("e" pp-eval-expression)
  ("<" embark-eval-replace)
  ("m" pp-macroexpand-expression)
  ("TAB" indent-region)
  ("r" raise-sexp)
  ("t" transpose-sexps)
  ("k" kill-region)
  ("u" backward-up-list)
  ("n" forward-list)
  ("p" backward-list))

(embark-define-keymap embark-defun-map
  "Keymap for Embark defun actions."
  :parent embark-expression-map
  ("RET" embark-pp-eval-defun)
  ("e" embark-pp-eval-defun)
  ("c" compile-defun)
  ("l" elint-defun)
  ("D" edebug-defun)
  ("o" checkdoc-defun)
  ("N" narrow-to-defun))

;; Use quoted symbols to avoid bytecompiler warnings.
(embark-define-keymap embark-heading-map
  "Keymap for Embark heading actions."
  ("RET" 'outline-show-subtree)
  ("TAB" 'outline-cycle) ;; New in Emacs 28!
  ("SPC" 'outline-mark-subtree)
  ("n" 'outline-next-visible-heading)
  ("p" 'outline-previous-visible-heading)
  ("f" 'outline-forward-same-level)
  ("b" 'outline-backward-same-level)
  ("^" 'outline-move-subtree-up)
  ("v" 'outline-move-subtree-down)
  ("u" 'outline-up-heading)
  ("s" 'outline-show-subtree)
  ("d" 'outline-hide-subtree)
  (">" 'outline-demote)
  ("<" 'outline-promote))

(embark-define-keymap embark-symbol-map
  "Keymap for Embark symbol actions."
  :parent embark-identifier-map
  ("RET" embark-find-definition)
  ("h" describe-symbol)
  ("s" embark-info-lookup-symbol)
  ("d" embark-find-definition)
  ("b" where-is)
  ("e" pp-eval-expression)
  ("a" apropos)
  ("\\" embark-history-remove))

(embark-define-keymap embark-face-map
  "Keymap for Embark face actions."
  :parent embark-symbol-map
  ("c" customize-face)
  ("+" make-face-bold)
  ("-" make-face-unbold)
  ("/" make-face-italic)
  ("|" make-face-unitalic)
  ("!" invert-face)
  ("f" set-face-foreground)
  ("b" set-face-background))

(embark-define-keymap embark-variable-map
  "Keymap for Embark variable actions."
  :parent embark-symbol-map
  ("=" set-variable)
  ("c" customize-set-variable)
  ("u" customize-variable)
  ("v" embark-save-variable-value)
  ("<" embark-insert-variable-value))

(embark-define-keymap embark-function-map
  "Keymap for Embark function actions."
  :parent embark-symbol-map
  ("m" elp-instrument-function) ;; m=measure
  ("M" 'elp-restore-function) ;; quoted, not autoloaded
  ("t" trace-function)
  ("T" 'untrace-function)) ;; quoted, not autoloaded

(embark-define-keymap embark-command-map
  "Keymap for Embark command actions."
  :parent embark-function-map
  ("x" execute-extended-command)
  ("I" Info-goto-emacs-command-node)
  ("g" global-set-key)
  ("l" local-set-key))

(embark-define-keymap embark-package-map
  "Keymap for Embark package actions."
  ("h" describe-package)
  ("i" package-install)
  ("I" embark-insert)
  ("d" package-delete)
  ("r" package-reinstall)
  ("u" embark-browse-package-url)
  ("W" embark-save-package-url)
  ("a" package-autoremove)
  ("g" package-refresh-contents)
  ("m" elp-instrument-package) ;; m=measure
  ("M" embark-elp-restore-package))

(embark-define-keymap embark-bookmark-map
  "Keymap for Embark bookmark actions."
  ("RET" bookmark-jump)
  ("s" bookmark-set)
  ("d" bookmark-delete)
  ("r" bookmark-rename)
  ("R" bookmark-relocate)
  ("l" bookmark-locate)
  ("<" bookmark-insert)
  ("j" bookmark-jump)
  ("o" bookmark-jump-other-window)
  ("f" bookmark-jump-other-frame))

(embark-define-keymap embark-unicode-name-map
  "Keymap for Embark unicode name actions."
  ("RET" insert-char)
  ("I" insert-char)
  ("W" embark-save-unicode-character))

(embark-define-keymap embark-prose-map
  "Keymap for Embark actions for dealing with prose."
  ("$" ispell-region)
  ("f" fill-region)
  ("u" upcase-region)
  ("d" downcase-region)
  ("c" capitalize-region)
  ("s" whitespace-cleanup-region)
  ("=" count-words-region))

(embark-define-keymap embark-sentence-map
  "Keymap for Embark actions for dealing with sentences."
  :parent embark-prose-map
  ("t" transpose-sentences)
  ("n" forward-sentence)
  ("p" backward-sentence))

(embark-define-keymap embark-paragraph-map
  "Keymap for Embark actions for dealing with paragraphs."
  :parent embark-prose-map
  ("t" transpose-paragraphs)
  ("n" forward-paragraph)
  ("p" backward-paragraph))

(embark-define-keymap embark-become-help-map
  "Keymap for Embark help actions."
  :parent embark-meta-map
  ("V" apropos-variable)
  ("U" apropos-user-option)
  ("C" apropos-command)
  ("v" describe-variable)
  ("f" describe-function)
  ("s" describe-symbol)
  ("F" describe-face)
  ("p" describe-package)
  ("i" describe-input-method))

(autoload 'recentf-open-files "recentf" nil t)

(embark-define-keymap embark-become-file+buffer-map
  "Embark become keymap for files and buffers."
  :parent embark-meta-map
  ("f" find-file)
  ("4f" find-file-other-window)
  ("." find-file-at-point)
  ("p" project-find-file)
  ("r" recentf-open-files)
  ("b" switch-to-buffer)
  ("4b" switch-to-buffer-other-window)
  ("l" locate)
  ("L" find-library))

(embark-define-keymap embark-become-shell-command-map
  "Embark become keymap for shell commands."
  :parent embark-meta-map
  ("!" shell-command)
  ("&" async-shell-command)
  ("c" comint-run)
  ("t" term))

(embark-define-keymap embark-become-match-map
  "Embark become keymap for search."
  :parent embark-meta-map
  ("o" occur)
  ("k" keep-lines)
  ("f" flush-lines)
  ("c" count-matches))

(provide 'embark)
;;; embark.el ends here
