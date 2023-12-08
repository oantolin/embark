;;; embark.el --- Conveniently act on minibuffer completions   -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023  Free Software Foundation, Inc.

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Maintainer: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: convenience
;; Version: 1.0
;; Homepage: https://github.com/oantolin/embark
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.0"))

;; This file is part of GNU Emacs.

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
;; symbol `embark-file-map'.  That symbol names a keymap with
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

;; - The `embark-act-all' command runs the same action on each of the
;;   current candidates.  It is just like using `embark-act' on each
;;   candidate in turn.

;; - The `embark-collect' command produces a buffer listing all
;;   candidates, for you to peruse and run actions on at your leisure.
;;   The candidates are displayed as a list showing additional
;;   annotations.

;; - The `embark-export' command tries to open a buffer in an
;;   appropriate major mode for the set of candidates.  If the
;;   candidates are files export produces a Dired buffer; if they are
;;   buffers, you get an Ibuffer buffer; and if they are packages you
;;   get a buffer in package menu mode.

;; These are always available as "actions" (although they do not act
;; on just the current target but on all candidates) for embark-act
;; and are bound to A, S (for "snapshot") and E, respectively, in
;; embark-general-map.  This means that you do not have to bind your
;; own key bindings for these (although you can, of course), just a
;; key binding for `embark-act'.

;;; Code:

(require 'compat)
(eval-when-compile (require 'subr-x))

(require 'ffap) ; used to recognize file and url targets

;;; User facing options

(defgroup embark nil
  "Emacs Mini-Buffer Actions Rooted in Keymaps."
  :link '(info-link :tag "Info Manual" "(embark)")
  :link '(url-link :tag "Homepage" "https://github.com/oantolin/embark")
  :link '(emacs-library-link :tag "Library Source" "embark.el")
  :group 'minibuffer
  :prefix "embark-")

(defcustom embark-keymap-alist
  '((file embark-file-map)
    (library embark-library-map)
    (environment-variables embark-file-map) ; they come up in file completion
    (url embark-url-map)
    (email embark-email-map)
    (buffer embark-buffer-map)
    (tab embark-tab-map)
    (expression embark-expression-map)
    (identifier embark-identifier-map)
    (defun embark-defun-map)
    (symbol embark-symbol-map)
    (face embark-face-map)
    (command embark-command-map)
    (variable embark-variable-map)
    (function embark-function-map)
    (minor-mode embark-command-map)
    (unicode-name embark-unicode-name-map)
    (package embark-package-map)
    (bookmark embark-bookmark-map)
    (region embark-region-map)
    (sentence embark-sentence-map)
    (paragraph embark-paragraph-map)
    (kill-ring embark-kill-ring-map)
    (heading embark-heading-map)
    (flymake embark-flymake-map)
    (smerge smerge-basic-map embark-general-map)
    (t embark-general-map))
  "Alist of action types and corresponding keymaps.
The special key t is associated with the default keymap to use.
Each value can be either a single symbol whose value is a keymap,
or a list of such symbols."
  :type '(alist :key-type (symbol :tag "Target type")
                :value-type (choice (variable :tag "Keymap")
                             (repeat :tag "Keymaps" variable))))

(defcustom embark-target-finders
  '(embark-target-top-minibuffer-candidate
    embark-target-active-region
    embark-target-collect-candidate
    embark-target-completion-list-candidate
    embark-target-text-heading-at-point
    embark-target-bug-reference-at-point
    embark-target-flymake-at-point
    embark-target-smerge-at-point
    embark-target-package-at-point
    embark-target-email-at-point
    embark-target-url-at-point
    embark-target-file-at-point
    embark-target-custom-variable-at-point
    embark-target-identifier-at-point
    embark-target-guess-file-at-point
    embark-target-expression-at-point
    embark-target-sentence-at-point
    embark-target-paragraph-at-point
    embark-target-defun-at-point
    embark-target-prog-heading-at-point)
  "List of functions to determine the target in current context.
Each function should take no arguments and return one of:

1. a cons (TYPE . TARGET) where TARGET is a string and TYPE is a
   symbol (which is looked up in `embark-keymap-alist' to
   determine which additional keybindings for actions to setup);

2. a dotted list of the form (TYPE TARGET START . END), where
   START and END are the buffer positions bounding TARGET, used
   for highlighting; or

3. a possibly empty list of targets, each of type 1 or 2 (in
   particular if a target finder does not find any targets, it
   should return nil)."
  :type 'hook)

(defcustom embark-transformer-alist
  '((minor-mode . embark--lookup-lighter-minor-mode)
    (embark-keybinding . embark--keybinding-command)
    (project-file . embark--project-file-full-path)
    (package . embark--remove-package-version)
    (multi-category . embark--refine-multi-category)
    (file . embark--simplify-path))
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

The key must be either nil or a string.  The
string must be accepted by `key-valid-p'."
  :type '(choice key (const :tag "None" nil)))

(defcustom embark-cycle-key nil
  "Key used for `embark-cycle'.

If the key is set to nil it defaults to the global binding of
`embark-act'.  The key must be a string which is accepted by
`key-valid-p'."
  :type '(choice key (const :tag "Use embark-act key" nil)))

(defcustom embark-help-key "C-h"
  "Key used for help.

The key must be either nil or a string.  The
string must be accepted by `key-valid-p'."
  :type '(choice (const "C-h")
                 (const "?")
                 (const :tag "None" nil)
                 key))

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

(defcustom embark-quit-after-action t
  "Should `embark-act' quit the minibuffer?
This controls whether calling `embark-act' without a prefix
argument quits the minibuffer or not.  You can always get the
opposite behavior to that indicated by this variable by calling
`embark-act' with \\[universal-argument].

Note that `embark-act' can also be called from outside the
minibuffer and this variable is irrelevant in that case.

In addition to t or nil this variable can also be set to an
alist to specify the minibuffer quitting behavior per command.
In the alist case one can additionally use the key t to
prescribe a default for commands not used as alist keys."
  :type '(choice (const :tag "Always quit" t)
                 (const :tag "Never quit" nil)
                 (alist :tag "Configure per action"
                        :key-type (choice (function :tag "Action")
                                          (const :tag "All other actions" t))
                        :value-type (choice (const :tag "Quit" t)
                                            (const :tag "Do not quit" nil)))))

(defcustom embark-confirm-act-all t
  "Should `embark-act-all' prompt the user for confirmation?
Even if this variable is nil you may still be prompted to confirm
some uses of `embark-act-all', namely, for those actions whose
entry in `embark-pre-action-hooks' includes `embark--confirm'."
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
were obtained from a `delete-file' prompt.  In that case you can
configure that by adding an entry to this variable pairing `file'
with `find-file'.

In addition to target types, you can also use as keys in this alist,
pairs of a target type and a command name.  Such a pair indicates that
the override only applies if the target was obtained from minibuffer
completion from that command.  For example adding an
entry (cons (cons \\='file \\='delete-file) \\='find-file) to this alist would
indicate that for files at the prompt of the `delete-file' command,
`find-file' should be used as the default action."
  :type '(alist :key-type (choice (symbol :tag "Type")
                                  (cons (symbol :tag "Type")
                                        (symbol :tag "Command")))
                :value-type (function :tag "Default action")))

(defcustom embark-target-injection-hooks
  '((async-shell-command embark--allow-edit embark--shell-prep)
    (shell-command embark--allow-edit embark--shell-prep)
    (pp-eval-expression embark--eval-prep)
    (eval-expression embark--eval-prep)
    (package-delete embark--force-complete)
    ;; commands evaluating code found in the buffer, which may in turn prompt
    (embark-pp-eval-defun embark--ignore-target)
    (eval-defun embark--ignore-target)
    (eval-last-sexp embark--ignore-target)
    (embark-eval-replace embark--ignore-target)
    ;; commands which prompt for something that is *not* the target
    (write-region embark--ignore-target)
    (append-to-file embark--ignore-target)
    (append-to-buffer embark--ignore-target)
    (shell-command-on-region embark--ignore-target)
    (format-encode-region embark--ignore-target)
    (format-decode-region embark--ignore-target)
    (xref-find-definitions embark--ignore-target)
    (xref-find-references embark--ignore-target)
    (sort-regexp-fields embark--ignore-target)
    (align-regexp embark--ignore-target))
  "Alist associating commands with post-injection setup hooks.
For commands appearing as keys in this alist, run the
corresponding value as a setup hook after injecting the target
into in the minibuffer and before acting on it.  The hooks must
accept arbitrary keyword arguments.  The :action command, the
:target string and target :type are always present.  For actions
at point the target :bounds are passed too.  The default pre-action
hook is specified by the entry with key t.  Furthermore, hooks with
the key :always are executed always."
  :type '(alist :key-type
                (choice symbol
                        (const :tag "Default" t)
                        (const :tag "Always" :always))
                :value-type hook))

(defcustom embark-pre-action-hooks
  `(;; commands that need to position point at the beginning or end
    (eval-last-sexp embark--end-of-target)
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
    (embark-insert embark--end-of-target)
    ;; commands we want to be able to jump back from
    ;; (embark-find-definition achieves this by calling
    ;; xref-find-definitions which pushes the markers itself)
    (find-library embark--xref-push-marker)
    ;; commands which prompt the user for confirmation before running
    (delete-file embark--confirm)
    (delete-directory embark--confirm)
    (kill-buffer embark--confirm)
    (embark-kill-buffer-and-window embark--confirm)
    (bookmark-delete embark--confirm)
    (package-delete embark--confirm)
    (,'tab-bar-close-tab-by-name embark--confirm) ;; Avoid package-lint warning
    ;; search for region contents outside said region
    (embark-isearch-forward embark--unmark-target)
    (embark-isearch-backward embark--unmark-target)
    (occur embark--unmark-target)
    (query-replace embark--beginning-of-target embark--unmark-target)
    (query-replace-regexp embark--beginning-of-target embark--unmark-target)
    (replace-string embark--beginning-of-target embark--unmark-target)
    (replace-regexp embark--beginning-of-target embark--unmark-target)
    ;; mark pseudo-action
    (mark embark--mark-target)
    ;; shells in new buffers
    (shell embark--universal-argument)
    (eshell embark--universal-argument))
  "Alist associating commands with pre-action hooks.
The hooks are run right before an action is embarked upon.  See
`embark-target-injection-hooks' for information about the hook
arguments and more details."
  :type '(alist :key-type
                (choice symbol
                        (const :tag "Default" t)
                        (const :tag "Always" :always))
                :value-type hook))

(defcustom embark-post-action-hooks
  `((bookmark-delete embark--restart)
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
    (,'tab-bar-rename-tab-by-name embark--restart) ;; Avoid package-lint warning
    (,'tab-bar-close-tab-by-name embark--restart)
    (package-delete embark--restart))
  "Alist associating commands with post-action hooks.
The hooks are run after an embarked upon action concludes.  See
`embark-target-injection-hooks' for information about the hook
arguments and more details."
  :type '(alist :key-type
                (choice symbol
                        (const :tag "Default" t)
                        (const :tag "Always" :always))
                :value-type hook))

(defcustom embark-around-action-hooks
  '(;; use directory of target as default-directory
    (shell embark--cd)
    (eshell embark--cd)
    ;; mark the target preserving point and previous mark
    (kill-region embark--mark-target)
    (kill-ring-save embark--mark-target)
    (indent-region embark--mark-target)
    (ispell-region embark--mark-target)
    (fill-region embark--mark-target)
    (upcase-region embark--mark-target)
    (downcase-region embark--mark-target)
    (capitalize-region embark--mark-target)
    (count-words-region embark--mark-target)
    (count-words embark--mark-target)
    (delete-duplicate-lines embark--mark-target)
    (shell-command-on-region embark--mark-target)
    (delete-region embark--mark-target)
    (format-encode-region embark--mark-target)
    (format-decode-region embark--mark-target)
    (write-region embark--mark-target)
    (append-to-file embark--mark-target)
    (append-to-buffer embark--mark-target)
    (shell-command-on-region embark--mark-target)
    (embark-eval-replace embark--mark-target)
    (delete-indentation embark--mark-target)
    (comment-dwim embark--mark-target)
    (insert-parentheses embark--mark-target)
    (insert-pair embark--mark-target)
    (org-emphasize embark--mark-target)
    ;; do the actual work of selecting & deselecting targets
    (embark-select embark--select))
  "Alist associating commands with post-action hooks.
The hooks are run instead of the embarked upon action.  The hook
can decide whether or not to run the action or it can run it
in some special environment, like inside a let-binding or inside
`save-excursion'.  Each hook is called with keyword argument :run
providing a function encapsulating the following around hooks and
the action; the hook additionally receives the keyword arguments
used for other types of action hooks, for more details see
`embark-target-injection-hooks'."
  :type '(alist :key-type
                (choice symbol
                        (const :tag "Default" t)
                        (const :tag "Always" :always))
                :value-type hook))

(when (version-list-< (version-to-list emacs-version) '(29 1))
  ;; narrow to target for duration of action
  (setf (alist-get 'repunctuate-sentences embark-around-action-hooks)
        '(embark--narrow-to-target)))

(defcustom embark-multitarget-actions '(embark-insert embark-copy-as-kill)
  "Commands for which `embark-act-all' should pass a list of targets.
Normally `embark-act-all' runs the same action on each candidate
separately, but when a command included in this variable's value
is used as an action, `embark-act-all' will instead call it
non-interactively with a single argument: the list of all
candidates.  For commands on this list `embark-act' behaves
similarly: it calls them non-interactively with a single
argument: a one element list containing the target."
  :type '(repeat function))

(defcustom embark-repeat-actions
  '((mark . region)
    ;; outline commands
    outline-next-visible-heading outline-previous-visible-heading
    outline-forward-same-level outline-backward-same-level
    outline-demote outline-promote
    outline-show-subtree (outline-mark-subtree . region)
    outline-move-subtree-up outline-move-subtree-down
    outline-up-heading outline-hide-subtree outline-cycle
    ;; org commands (remapped outline commands)
    org-forward-heading-same-level org-backward-heading-same-level
    org-next-visible-heading org-previous-visible-heading
    org-demote-subtree org-promote-subtree
    org-show-subtree (org-mark-subtree . region)
    org-move-subtree-up org-move-subtree-down
    ;; transpose commands
    transpose-sexps transpose-sentences transpose-paragraphs
    ;; navigation commands
    flymake-goto-next-error flymake-goto-prev-error
    embark-next-symbol embark-previous-symbol
    backward-up-list backward-list forward-list forward-sexp
    backward-sexp forward-sentence backward-sentence
    forward-paragraph backward-paragraph
    ;; smerge commands
    smerge-refine smerge-combine-with-next smerge-prev smerge-next)
  "List of repeatable actions.
When you use a command on this list as an Embark action from
outside the minibuffer, `embark-act' does not exit but instead
lets you act again on the possibly new target you reach.

By default, after using one of these actions, when `embark-act'
looks for targets again, it will start the target cycle at the
same type as the previously acted upon target; that is, you
\"don't loose your place in the target cycle\".

Sometimes, however, you'll want to prioritize a different type of
target to continue acting on.  The main example of this that if
you use a marking command as an action, you almost always want to
act on the region next.  For those cases, in addition to
commands, you can also place on this list a pair of a command and
the desired starting type for the target cycle for the next
action."
  :type '(repeat (choice function
                         (cons function
                               (symbol :tag "Next target type")))))

;;; Overlay properties

;; high priority to override both bug reference and the lazy
;; isearch highlights in embark-isearch-highlight-indicator
(put 'embark-target-overlay 'face 'embark-target)
(put 'embark-target-overlay 'priority 1001)
(put 'embark-selected-overlay 'face 'embark-selected)
(put 'embark-selected-overlay 'priority 1001)

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

(defvar-local embark--toggle-quit nil
  "Should we toggle the default quitting behavior for the next action?")

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
   ((and (minibufferp) minibuffer-completion-table (minibuffer-selected-window))
    (window-buffer (minibuffer-selected-window)))
   ((and embark--target-buffer (buffer-live-p embark--target-buffer))
    embark--target-buffer)
   (t (current-buffer))))

(defun embark--target-window (&optional display)
  "Return window which should be selected when Embark actions run.
If DISPLAY is non-nil, call `display-buffer' to produce the
window if necessary."
  (cond
   ((and (minibufferp) minibuffer-completion-table (minibuffer-selected-window))
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
  (with-current-buffer standard-output
    (when (minibufferp completion-reference-buffer)
      (setq embark--type
            (completion-metadata-get
             (with-current-buffer completion-reference-buffer
               (embark--metadata))
             'category)))))

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

;;; Core functionality

(defconst embark--verbose-indicator-buffer " *Embark Actions*")

(defvar embark--minimal-indicator-overlay nil)

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
(declare-function image-dired-original-file-name "image-dired")

(defun embark-target-guess-file-at-point ()
  "Target the file guessed by `ffap' at point."
  (when-let ((tap-file (thing-at-point 'filename))
             ((not (ffap-url-p tap-file))) ; no URLs, those have a target finder
             (bounds (bounds-of-thing-at-point 'filename))
             (file (ffap-file-at-point)))
    ;; ffap doesn't make bounds available, so we use
    ;; thingatpt bounds, which might be a little off
    ;; adjust bounds if thingatpt gobbled punctuation around file
    (when (or (string-match (regexp-quote file) tap-file)
              (string-match (regexp-quote (file-name-base file)) tap-file))
      (setq bounds (cons (+ (car bounds) (match-beginning 0))
                         (- (cdr bounds) (- (length tap-file)
                                            (match-end 0))))))
    `(file ,(abbreviate-file-name (expand-file-name file)) ,@bounds)))

(defun embark-target-file-at-point ()
  "Target file at point.
This function mostly relies on `ffap-file-at-point', with the
following exceptions:

- In `dired-mode', it uses `dired-get-filename' instead.

- In `imaged-dired-thumbnail-mode', it uses
  `image-dired-original-file-name' instead."
  (let (file bounds)
    (or (and (derived-mode-p 'dired-mode)
             (setq file (dired-get-filename t 'no-error-if-not-filep))
             (setq bounds
                   (cons
                    (save-excursion (dired-move-to-filename) (point))
                    (save-excursion (dired-move-to-end-of-filename) (point)))))
        (and (derived-mode-p 'image-dired-thumbnail-mode)
             (setq file (image-dired-original-file-name))
             (setq bounds (cons (point) (1+ (point)))))
        (when-let ((tap-file (thing-at-point 'filename))
                   ((not (equal (file-name-base tap-file) tap-file)))
                   (guess (embark-target-guess-file-at-point)))
          (setq file (cadr guess) bounds (cddr guess))))
    (when file
      `(file ,(abbreviate-file-name (expand-file-name file)) ,@bounds))))

(defun embark-target-package-at-point ()
  "Target the package on the current line in a packages buffer."
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
  (if-let ((url (or (get-text-property (point) 'shr-url)
                    (get-text-property (point) 'image-url))))
      `(url ,url
            ,(previous-single-property-change
              (min (1+ (point)) (point-max)) 'mouse-face nil (point-min))
            . ,(next-single-property-change
                (point) 'mouse-face nil (point-max)))
    (when-let ((url (thing-at-point 'url)))
      `(url ,url . ,(thing-at-point-bounds-of-url-at-point t)))))

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

(defmacro embark-define-overlay-target (name prop &optional pred type target)
  "Define a target finder for NAME that targets overlays with property PROP.
The function defined is named embark-target-NAME-at-point and it
returns Embark targets based on the overlays around point.  An
overlay provides a target if its property named PROP is non-nil.

If the optional PRED argument is given, it should be an
expression and it further restricts the targets to only those
overlays for which PRED evaluates to non-nil.

The target finder returns target type NAME or optional symbol
TYPE if given.

The target finder returns the substring of the buffer covered by
the overlay as the target string or the result of evaluating the
optional TARGET expression if given.

PRED and TARGET are expressions (not functions) and when evaluated the
symbols `%o' and `%p' are bound to the overlay and the overlay's
property respectively."
  `(defun ,(intern (format "embark-target-%s-at-point" name)) ()
     ,(format "Target %s at point." name)
     (when-let ((%o (seq-find
                           (lambda (%o)
                             (when-let ((%p (overlay-get %o ',prop)))
                               (ignore %p)
                               ,(or pred t)))
                           (overlays-in (max (point-min) (1- (point)))
                                        (min (point-max) (1+ (point))))))
                (%p (overlay-get %o ',prop)))
       (ignore %p)
       (cons ',(or type name)
             (cons ,(or target `(buffer-substring-no-properties
                                 (overlay-start %o) (overlay-end %o)))
                   (cons (overlay-start %o) (overlay-end %o)))))))

(embark-define-overlay-target flymake flymake-diagnostic)
(embark-define-overlay-target bug-reference bug-reference-url nil url %p)
(embark-define-overlay-target smerge smerge (eq %p 'conflict))

(defmacro embark-define-thingatpt-target (thing &rest modes)
  "Define a target finder for THING using the thingatpt library.
The function defined is named embark-target-NAME-at-point and it
uses (thing-at-point 'THING) to find its targets.

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

(defmacro embark-define-regexp-target
    (name regexp &optional type target bounds limit)
  "Define a target finder for matches of REGEXP around point.
The function defined is named embark-target-NAME-at-point and it
uses (thing-at-point-looking-at REGEXP) to find its targets.

The target finder returns target type NAME or optional symbol
TYPE if given.

The target finder returns the substring of the buffer matched by
REGEXP as the target string or the result of evaluating the
optional TARGET expression if given.  In the expression TARGET
you can use `match-string' to recover the match of the REGEXP or
of any sub-expressions it has.

BOUNDS is an optional expression to compute the bounds of the
target and defaults to (cons (match-beginning 0) (match-end 0)).

The optional LIMIT is the number of characters before and after
point to limit the search to.  If LIMIT is nil, search a little
more than the current line (more precisely, the smallest interval
centered at point that includes the current line)."
  `(defun ,(intern (format "embark-target-%s-at-point" name)) ()
     ,(format "Target %s at point." name)
     (save-match-data
       (when (thing-at-point-looking-at
              ,regexp
              ,(or limit '(max (- (pos-eol) (point)) (- (point) (pos-bol)))))
         (cons ',(or type name)
               (cons ,(or target '(match-string 0))
                     ,(or bounds
                          '(cons (match-beginning 0) (match-end 0)))))))))

(defun embark--identifier-types (identifier)
  "Return list of target types appropriate for IDENTIFIER."
  (let ((symbol (intern-soft identifier)))
    (if (not
         (or (derived-mode-p 'emacs-lisp-mode 'inferior-emacs-lisp-mode)
             (and (not (derived-mode-p 'prog-mode))
                  symbol
                  (or (boundp symbol) (fboundp symbol) (symbol-plist symbol)))))
        '(identifier)
      (let* ((library (ffap-el-mode identifier))
             (types
              (append
               (and (commandp symbol) '(command))
               (and symbol (boundp symbol) (not (keywordp symbol)) '(variable))
               (and (fboundp symbol) (not (commandp symbol)) '(function))
               (and (facep symbol) '(face))
               (and library '(library))
               (and (featurep 'package) (embark--package-desc symbol)
                    '(package)))))
        (when (and library
                   (looking-back "\\(?:require\\|use-package\\).*"
                                 (line-beginning-position)))
          (setq types (embark--rotate types (cl-position 'library types))))
        (or types '(symbol))))))

(defun embark-target-identifier-at-point ()
  "Target identifier at point.

In Emacs Lisp and IELM buffers the identifier is promoted to a
symbol, for which more actions are available.  Identifiers are
also promoted to symbols if they are interned Emacs Lisp symbols
and found in a buffer in a major mode that is not derived from
`prog-mode' (this is intended for when you might be reading or
writing about Emacs).

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
      (mapcar (lambda (type) `(,type ,name . ,bounds))
              (embark--identifier-types name)))))

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

(defun embark-target-text-heading-at-point ()
  "Target the outline heading at point in text modes."
  (when (derived-mode-p 'text-mode)
    (embark-target-heading-at-point)))

(defun embark-target-prog-heading-at-point ()
  "Target the outline heading at point in programming modes."
  (when (derived-mode-p 'prog-mode)
    (embark-target-heading-at-point)))

(defun embark-target-top-minibuffer-candidate ()
  "Target the top completion candidate in the minibuffer.
Return the category metadatum as the type of the target.

This target finder is meant for the default completion UI and
completion UI highly compatible with it, like Icomplete.
Many completion UIs can still work with Embark but will need
their own target finder.  See for example
`embark--vertico-selected'."
  (when (and (minibufferp) minibuffer-completion-table)
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
    (when-let ((button
                (pcase (get-text-property (point) 'tabulated-list-column-name)
                  ("Candidate" (button-at (point)))
                  ("Annotation" (previous-button (point)))))
               (start (button-start button))
               (end (button-end button))
               (candidate (tabulated-list-get-id)))
      `(,embark--type
        ,(if (eq embark--type 'file)
             (abbreviate-file-name (expand-file-name candidate))
           candidate)
        ,start . ,end))))

(defun embark-target-completion-list-candidate ()
  "Return the completion candidate at point in a completions buffer."
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
        (let ((raw (or (get-text-property beg 'completion--string)
                       (buffer-substring beg end))))
          `(,embark--type
            ,(if (eq embark--type 'file)
                 (abbreviate-file-name (expand-file-name raw))
               raw)
            ,beg . ,end))))))

(defun embark--cycle-key ()
  "Return the key to use for `embark-cycle'."
  (if embark-cycle-key
      (if (key-valid-p embark-cycle-key)
          (key-parse embark-cycle-key)
        (error "`embark-cycle-key' is invalid"))
    (car (where-is-internal #'embark-act))))

(defun embark--raw-action-keymap (type)
  "Return raw action map for targets of given TYPE.
This does not take into account the default action, help key or
cycling bindings, just what's registered in
`embark-keymap-alist'."
  (make-composed-keymap
   (mapcar #'symbol-value
           (let ((actions (or (alist-get type embark-keymap-alist)
                              (alist-get t embark-keymap-alist))))
             (ensure-list actions)))))

(defun embark--action-keymap (type cycle)
  "Return action keymap for targets of given TYPE.
If CYCLE is non-nil bind `embark-cycle'."
  (make-composed-keymap
   (let ((map (make-sparse-keymap))
         (default-action (embark--default-action type)))
     (define-key map [13] default-action)
     (when-let ((cycle-key (and cycle (embark--cycle-key))))
       (define-key map cycle-key #'embark-cycle))
     (when embark-help-key
       (keymap-set map embark-help-key #'embark-keymap-help))
     map)
   (embark--raw-action-keymap type)))

(defun embark--truncate-target (target)
  "Truncate TARGET string."
  (unless (stringp target)
    (setq target (format "%s" target)))
  (if-let (pos (string-match-p "\n" target))
      (concat (car (split-string target "\n" 'omit-nulls "\\s-*")) "…")
    target))

;;;###autoload
(defun embark-eldoc-first-target (report &rest _)
  "Eldoc function reporting the first Embark target at point.
This function uses the eldoc REPORT callback and is meant to be
added to `eldoc-documentation-functions'."
  (when-let (((not (minibufferp)))
             (target (car (embark--targets))))
    (funcall report
             (format "Embark on %s ‘%s’"
                     (plist-get target :type)
                     (embark--truncate-target (plist-get target :target))))))

;;;###autoload
(defun embark-eldoc-target-types (report &rest _)
  "Eldoc function reporting the types of all Embark targets at point.
This function uses the eldoc REPORT callback and is meant to be
added to `eldoc-documentation-functions'."
  (when-let (((not (minibufferp)))
             (targets (embark--targets)))
    (funcall report
             (format "Embark target types: %s"
                     (mapconcat
                      (lambda (target) (symbol-name (plist-get target :type)))
                      targets
                      ", ")))))

(defun embark--format-targets (target shadowed-targets rep)
  "Return a formatted string indicating the TARGET of an action.

This is used internally by the minimal indicator and for the
targets section of the verbose indicator.  The string will also
mention any SHADOWED-TARGETS.  A non-nil REP indicates we are in
a repeating sequence of actions."
  (let ((act (propertize
              (cond
               ((plist-get target :multi) "∀ct")
               (rep "Rep")
               (t "Act"))
              'face 'highlight)))
    (cond
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
         "%s on %s%s ‘%s’"
         act
         (plist-get target :type)
         (if shadowed-targets
             (format (propertize "(%s)" 'face 'shadow)
                     (mapconcat
                      (lambda (target) (symbol-name (plist-get target :type)))
                      shadowed-targets
                      ", "))
           "")
         (embark--truncate-target (plist-get target :target)))))))

(defun embark-minimal-indicator ()
  "Minimal indicator, appearing in the minibuffer prompt or echo area.
This indicator displays a message showing the types of all
targets, starting with the current target, and the value of the
current target.  The message is displayed in the echo area, or if
the minibuffer is open, the message is added to the prompt."
  (lambda (&optional keymap targets _prefix)
    (if (null keymap)
        (when embark--minimal-indicator-overlay
          (delete-overlay embark--minimal-indicator-overlay)
          (setq-local embark--minimal-indicator-overlay nil))
      (let ((indicator (embark--format-targets
                        (car targets) (cdr targets)
                        (eq (lookup-key keymap [13]) #'embark-done))))
        (if (not (minibufferp))
            (message "%s" indicator)
          (unless embark--minimal-indicator-overlay
            (setq-local embark--minimal-indicator-overlay
                        (make-overlay (point-min) (point-min)
                                      (current-buffer) t t)))
          (overlay-put embark--minimal-indicator-overlay
                       'before-string (concat indicator
                                              (if (<= (length indicator)
                                                      (* 0.4 (frame-width)))
                                                  " "
                                                "\n"))))))))

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

(defvar embark-indicators) ; forward declaration

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
                               'accept-default))))
             (prefix-arg prefix-arg)) ; preserve prefix arg
         (when-let ((win (get-buffer-window embark--verbose-indicator-buffer
                                            'visible)))
           (quit-window 'kill-buffer win))
         (embark-completing-read-prompter prefix-map update)))
      ((or 'universal-argument 'universal-argument-more
           'negative-argument 'digit-argument 'embark-toggle-quit)
       ;; prevent `digit-argument' from modifying the overriding map
       (let ((overriding-terminal-local-map overriding-terminal-local-map))
         (command-execute cmd))
       (embark-keymap-prompter
        (make-composed-keymap universal-argument-map keymap)
        update))
      ((or 'minibuffer-keyboard-quit 'abort-recursive-edit 'abort-minibuffers)
       nil)
      ((guard (let ((def (lookup-key keymap keys))) ; if directly
                                                    ; bound, then obey
                (and def (not (numberp def))))) ; number means "invalid prefix"
       cmd)
      ((and (pred symbolp)
            (guard (string-suffix-p "self-insert-command" (symbol-name cmd))))
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
       (let ((prefix-arg prefix-arg)) ; preserve prefix arg
         (intern-soft (read-extended-command))))
      ((or 'keyboard-quit 'keyboard-escape-quit)
       nil)
      (_ cmd))))

(defun embark--command-name (cmd)
  "Return an appropriate name for CMD.
If CMD is a symbol, use its symbol name; for lambdas, use the
first line of the documentation string; for keyboard macros use
`key-description'; otherwise use the word \"unnamed\"."
  (concat ; fresh copy, so we can freely add text properties
   (cond
    ((or (stringp cmd) (vectorp cmd)) (key-description cmd))
    ((stringp (car-safe cmd)) (car cmd))
    ((eq (car-safe cmd) 'menu-item) (eval (cadr cmd)))
    ((keymapp cmd)
     (propertize (if (symbolp cmd) (format "+%s" cmd) "<keymap>")
                 'face 'embark-keymap))
    ((symbolp cmd)
     (let ((name (symbol-name cmd)))
       (if (string-prefix-p "embark-action--" name) ; direct action mode
           (format "(%s)" (string-remove-prefix "embark-action--" name))
         name)))
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
  (let ((vstr (and (symbolp sym) (keymapp sym) (boundp sym)
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

(defun embark--action-repeatable-p (action)
  "Is ACTION repeatable?
When the return value is non-nil it will be the desired starting
point of the next target cycle or t to indicate the default,
namely that the target cycle for the next action should begin at
the type of the current target."
  (or (cdr (assq action embark-repeat-actions))
      (and (memq action embark-repeat-actions) t)))

(defun embark--formatted-bindings (keymap &optional nested)
  "Return the formatted keybinding of KEYMAP.
The keybindings are returned in their order of appearance.
If NESTED is non-nil subkeymaps are not flattened."
  (let* ((commands
          (cl-loop for (key . def) in (embark--all-bindings keymap nested)
                   for name = (embark--command-name def)
                   for cmd = (keymap--menu-item-binding def)
                   unless (memq cmd '(nil embark-keymap-help
                                      negative-argument digit-argument))
                   collect (list name cmd key
                                 (concat
                                  (if (eq (car-safe def) 'menu-item)
                                      "menu-item"
                                    (key-description key))))))
         (width (cl-loop for (_name _cmd _key desc) in commands
                         maximize (length desc)))
         (default)
         (candidates
          (cl-loop for item in commands
                   for (name cmd key desc) = item
                   for desc-rep =
                   (concat
                    (propertize desc 'face 'embark-keybinding)
                    (and (embark--action-repeatable-p cmd)
                         embark-keybinding-repeat))
                   for formatted =
                   (propertize
                    (concat desc-rep
                            (make-string (- width (length desc-rep) -1) ?\s)
                            name)
                    'embark-command cmd)
                   when (equal key [13])
                   do (setq default formatted)
                   collect (cons formatted item))))
    (cons candidates default)))

(defun embark--with-category (category candidates)
  "Return completion table for CANDIDATES of CATEGORY with sorting disabled."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        `(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity)
                   (category . ,category))
      (complete-with-action
       action candidates string predicate))))

(defun embark-completing-read-prompter (keymap update &optional no-default)
  "Prompt via completion for a command bound in KEYMAP.
If NO-DEFAULT is t, no default value is passed to`completing-read'.

UPDATE is the indicator update function.  It is not used directly
here, but if the user switches to `embark-keymap-prompter', the
UPDATE function is passed to it."
  (let* ((candidates+def (embark--formatted-bindings keymap))
         (candidates (car candidates+def))
         (def (and (not no-default) (cdr candidates+def)))
         (buf (current-buffer))
         (choice
          (catch 'choice
            (minibuffer-with-setup-hook
                (lambda ()
                  (let ((map (make-sparse-keymap)))
                    (define-key map "\M-q"
                                (lambda ()
                                  (interactive)
                                  (with-current-buffer buf
                                    (embark-toggle-quit))))
                    (when-let (cycle (embark--cycle-key))
                      ;; Rebind `embark-cycle' in order allow cycling
                      ;; from the `completing-read' prompter. Additionally
                      ;; `embark-cycle' can be selected via
                      ;; `completing-read'. The downside is that this breaks
                      ;; recursively acting on the candidates of type
                      ;; embark-keybinding in the `completing-read' prompter.
                      (define-key map cycle
                        (cond
                         ((eq (lookup-key keymap cycle) 'embark-cycle)
                          (lambda ()
                            (interactive)
                            (throw 'choice 'embark-cycle)))
                         ((null embark-cycle-key)
                          (lambda ()
                            (interactive)
                            (minibuffer-message
                             "No cycling possible; press `%s' again to act."
                             (key-description cycle))
                            (define-key map cycle #'embark-act))))))
                    (when embark-keymap-prompter-key
                      (keymap-set map embark-keymap-prompter-key
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
               (embark--with-category 'embark-keybinding candidates)
               nil nil nil 'embark--prompter-history def)))))
    (pcase (assoc choice candidates)
      (`(,_formatted ,_name ,cmd ,key ,_desc)
       ;; Set last-command-event as it would be from the command loop.
       (setq last-command-event (aref key (1- (length key))))
       cmd)
      ('nil (intern-soft choice)))))

;;; Verbose action indicator

(defgroup embark-indicators nil
  "Indicators display information about actions and targets."
  :group 'embark)

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
  values of action targets in the echo area or minibuffer prompt,

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
  point is an identifier or symbol, lazily highlights all
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

(defcustom embark-verbose-indicator-excluded-actions nil
  "Commands not displayed by `embark-verbose-indicator'.
This variable should be set to a list of symbols and regexps.
The verbose indicator will exclude from its listing any commands
matching an element of this list."
  :type '(choice
          (const :tag "Exclude nothing" nil)
          (const :tag "Exclude Embark general actions"
                 (embark-collect embark-live embark-export
                  embark-cycle embark-act-all embark-keymap-help
                  embark-become embark-isearch-forward
                  embark-isearch-backward))
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
  (let ((result (embark--format-targets
                 (car targets)
                 nil   ; the shadowed targets section deals with these
                 (cl-find 'embark-done bindings :key #'caddr :test #'eq))))
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
   (and cycle (propertize (format "(%s to cycle)" cycle)
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
The default display includes the type and value of the current
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
      ;; 2. For cleanup, the timer must also be canceled.
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
(defun embark-bindings-in-keymap (keymap)
  "Explore command key bindings in KEYMAP with `completing-read'.
The selected command will be executed.  Interactively, prompt the
user for a KEYMAP variable."
  (interactive
   (list
    (symbol-value
     (intern-soft
      (completing-read
       "Keymap: "
       (embark--with-category
        'variable
        (cl-loop for x being the symbols
                 if (and (boundp x) (keymapp (symbol-value x)))
                 collect (symbol-name x)))
       nil t nil 'variable-name-history
       (let ((major-mode-map
              (concat (symbol-name major-mode) "-map")))
         (when (intern-soft major-mode-map) major-mode-map)))))))
  (when-let (command (embark-completing-read-prompter keymap nil 'no-default))
    (call-interactively command)))

;;;###autoload
(defun embark-bindings (global)
  "Explore current command key bindings with `completing-read'.
The selected command will be executed.

This shows key bindings from minor mode maps and the local
map (usually set by the major mode), but also less common keymaps
such as those from a text property or overlay, or the overriding
maps: `overriding-terminal-local-map' and `overriding-local-map'.

Additionally, if GLOBAL is non-nil (interactively, if called with
a prefix argument), this command includes global key bindings."
  (interactive "P")
  (embark-bindings-in-keymap
   (make-composed-keymap
    (let ((all-maps (current-active-maps t)))
      (if global all-maps (remq global-map all-maps))))))

;;;###autoload
(defun embark-bindings-at-point ()
  "Explore all key bindings at point with `completing-read'.
The selected command will be executed.

This command lists key bindings found in keymaps specified by the
text properties `keymap' or `local-map', from either buffer text
or an overlay.  These are not widely used in Emacs, and when they
are used can be somewhat hard to discover.  Examples of locations
that have such a keymap are links and images in `eww' buffers,
attachment links in `gnus' article buffers, and the stash line
in a `vc-dir' buffer."
  (interactive)
  (if-let ((keymaps (delq nil (list (get-char-property (point) 'keymap)
                                    (get-char-property (point) 'local-map)))))
      (embark-bindings-in-keymap (make-composed-keymap keymaps))
    (user-error "No key bindings found at point")))

;;;###autoload
(defun embark-prefix-help-command ()
  "Prompt for and run a command bound in the prefix used for this command.
The prefix described consists of all but the last event of the
key sequence that ran this command.  This function is intended to
be used as a value for `prefix-help-command'.

In addition to using completion to select a command, you can also
type @ and the key binding (without the prefix)."
  (interactive)
  (when-let ((keys (this-command-keys-vector))
             (prefix (seq-take keys (1- (length keys))))
             (keymap (key-binding prefix 'accept-default)))
    (minibuffer-with-setup-hook
        (lambda ()
          (let ((pt (- (minibuffer-prompt-end) 2)))
            (overlay-put (make-overlay pt pt) 'before-string
                         (format " under %s" (key-description prefix)))))
      (embark-bindings-in-keymap keymap))))

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
t hooks are the default hooks, for when there are no
command-specific hooks for ACTION.  The QUIT, ACTION and TARGET
arguments are passed to the hooks as keyword arguments."
  (mapc (lambda (h) (apply h :action action :quit quit target))
        (or (alist-get action hooks)
            (alist-get t hooks)))
  (mapc (lambda (h) (apply h :action action :quit quit target))
        (alist-get :always hooks)))

(defun embark--run-around-action-hooks
    (action target quit &optional non-interactive)
  "Run the `embark-around-action-hooks' for ACTION.
All the applicable around hooks are composed in the order they
are present in `embark-around-action-hooks'.  The keys t and
:always in `embark-around-action-hooks' are treated specially.
The :always hooks are executed always (outermost) and the t hooks
are the default hooks, for when there are no command-specific
hooks for ACTION.  The QUIT, ACTION and TARGET arguments are
passed to the hooks as keyword arguments.

The optional argument NON-INTERACTIVE controls whether the action
is run with `command-execute' or with `funcall' passing the
target as argument."
  (apply
   (seq-reduce
    (lambda (fn hook)
      (lambda (&rest args) (apply hook (plist-put args :run fn))))
    (let ((hooks embark-around-action-hooks))
      (reverse
       (append (or (alist-get action hooks) (alist-get t hooks))
               (alist-get :always hooks))))
    (if non-interactive
        (lambda (&rest args)
          (funcall (plist-get args :action)
                   (or (plist-get args :candidates) (plist-get args :target))))
      (lambda (&rest args)
        (command-execute (plist-get args :action)))))
   :action action :quit quit target))

(defun embark--act (action target &optional quit)
  "Perform ACTION injecting the TARGET.
If called from a minibuffer with non-nil QUIT, quit the
minibuffer before executing the action."
  (if (memq action '(embark-become       ; these actions should run in
                     embark-collect      ; the current buffer, not the
                     embark-live         ; target buffer
                     embark-export
                     embark-select
                     embark-act-all))
      (progn
        (embark--run-action-hooks embark-pre-action-hooks action target quit)
        (unwind-protect (embark--run-around-action-hooks action target quit)
          (embark--run-action-hooks embark-post-action-hooks
                                    action target quit)))
    (let* ((command embark--command)
           (prefix prefix-arg)
           (action-window (embark--target-window t))
           (directory default-directory)
           (inject
            (lambda ()
              (let ((contents (minibuffer-contents)))
                (delete-minibuffer-contents)
                (insert
                 (propertize
                  (substring-no-properties (plist-get target :target))
                  'embark--initial-input contents)))
              (if (memq 'ivy--queue-exhibit post-command-hook)
                  ;; Ivy has special needs: (1) for file names
                  ;; ivy-immediate-done is not equivalent to
                  ;; exit-minibuffer, (2) it needs a chance to run
                  ;; its post command hook first, so use depth 10
                  (add-hook 'post-command-hook 'ivy-immediate-done 10 t)
                (add-hook 'post-command-hook #'exit-minibuffer nil t))
              (embark--run-action-hooks embark-target-injection-hooks
                                        action target quit)))
           (dedicate (and (derived-mode-p 'embark-collect-mode)
                          (not (window-dedicated-p))
                          (selected-window)))
           (multi (memq action embark-multitarget-actions))
           (run-action
            (if (and (commandp action) (not multi))
                (lambda ()
                  (let (final-window)
                    (when dedicate (set-window-dedicated-p dedicate t))
                    (unwind-protect
                        (with-selected-window action-window
                          (let ((enable-recursive-minibuffers t)
                                (embark--command command)
                                (prefix-arg prefix)
                                ;; the next two avoid mouse dialogs
                                (use-dialog-box nil)
                                (last-nonmenu-event 13)
                                (default-directory directory))
                            (embark--run-action-hooks embark-pre-action-hooks
                                                      action target quit)
                            (minibuffer-with-setup-hook inject
                              ;; pacify commands that use (this-command-keys)
                              (when (= (length (this-command-keys)) 0)
                                (set--this-command-keys
                                 (if (characterp last-command-event)
                                     (string last-command-event)
                                  "\r")))
                              (setq this-command action)
                              (embark--run-around-action-hooks
                               action target quit)))
                          (setq final-window (selected-window)))
                      (embark--run-action-hooks embark-post-action-hooks
                                                action target quit)
                      (when dedicate (set-window-dedicated-p dedicate nil)))
                    (unless (eq final-window action-window)
                      (select-window final-window))))
              (let ((target
                     (if (and multi (null (plist-get target :candidates)))
                         (plist-put
                          target :candidates (list (plist-get target :target)))
                       target)))
                (lambda ()
                  (with-selected-window action-window
                    (embark--run-action-hooks embark-pre-action-hooks
                                              action target quit)
                    (unwind-protect
                        (let ((current-prefix-arg prefix)
                              (default-directory directory))
                          (embark--run-around-action-hooks
                           action target quit :non-interactive))
                      (embark--run-action-hooks embark-post-action-hooks
                                                action target quit))))))))
      (setq prefix-arg nil)
      (if quit (embark--quit-and-run run-action) (funcall run-action)))))

(defun embark--refine-multi-category (_type target)
  "Refine `multi-category' TARGET to its actual type."
  (or (get-text-property 0 'multi-category target)
      (cons 'general target)))

(defun embark--simplify-path (_type target)
  "Simplify and '//' or '~/' in the TARGET file path."
  (cons 'file (substitute-in-file-name target)))

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
buffers, all target finder functions are executed.

For each target, the type is then looked up as a key in the
variable `embark-transformer-alist'.  If there is a transformer
for the type, it is called with the type and target, and must
return a `cons' of the transformed type and transformed target.

The return value of `embark--targets' is a list of plists.  Each
plist concerns one target, and has keys `:type', `:target',
`:orig-type', `:orig-target' and `:bounds'."
  (let (targets)
    (run-hook-wrapped
     'embark-target-finders
     (lambda (fun)
       (dolist (found (when-let (result (funcall fun))
                        (if (consp (car result)) result (list result))))
         (let* ((type (or (car found) 'general))
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
           (push full-target targets)))
       (and targets (minibufferp))))
    (nreverse
     (cl-delete-duplicates ; keeps last duplicate, but we reverse
      targets
      :test (lambda (t1 t2)
              (and (equal (plist-get t1 :target) (plist-get t2 :target))
                   (eq (plist-get t1 :type) (plist-get t2 :type))))))))

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
  (or (alist-get (cons type embark--command) embark-default-action-overrides
                 nil nil #'equal)
      (alist-get type embark-default-action-overrides)
      (alist-get t embark-default-action-overrides)
      embark--command
      (lookup-key (embark--raw-action-keymap type) "\r")))

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

(defun embark--quit-p (action)
  "Determine whether to quit the minibuffer after ACTION.
This function consults `embark-quit-after-action' to decide
whether or not the user wishes to quit the minibuffer after
performing the ACTION, assuming this is done from a minibuffer."
  (let* ((cfg embark-quit-after-action)
         (quit (if (consp cfg) (alist-get action cfg (alist-get t cfg)) cfg)))
    (when embark--toggle-quit (setq quit (not quit)))
    (setq embark--toggle-quit nil)
    quit))

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
    (when arg
      (if (minibufferp)
          (embark-toggle-quit)
        (setq targets (embark--rotate targets (prefix-numeric-value arg)))))
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
                (let ((repeat (embark--action-repeatable-p action)))
                  (unless repeat (mapc #'funcall indicators))
                  (condition-case err
                      (embark--act
                       action
                       (if (and (eq action default-action)
                                (eq action embark--command)
                                (not (memq action embark-multitarget-actions)))
                           (embark--orig-target target)
                         target)
                       (embark--quit-p action))
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
                                       (if (eq repeat t)
                                           (plist-get (car targets) :type)
                                         repeat)))
                                  (lambda (x)
                                    (eq (plist-get x :type) desired-type)))
                                new-targets)
                               0)))))))))
      (mapc #'funcall indicators))))

(defun embark--maybe-transform-candidates ()
  "Collect candidates and see if they all transform to the same type.
Return a plist with keys `:type', `:orig-type', `:candidates', and
`:orig-candidates'."
  (pcase-let* ((`(,type . ,candidates)
                (run-hook-with-args-until-success 'embark-candidate-collectors))
               (bounds (mapcar #'cdr-safe candidates)))
    (setq candidates
          (mapcar (lambda (x) (if (consp x) (car x) x)) candidates))
    (when (eq type 'file)
      (let ((dir (embark--default-directory)))
        (setq candidates
              (mapcar (lambda (cand)
                        (abbreviate-file-name
                         (expand-file-name (substitute-in-file-name cand) dir)))
                      candidates))))
    ;; TODO more systematic approach to applying substitute-in-file-name
    (append
     (list :orig-type type :orig-candidates candidates :bounds bounds)
     (or (when candidates
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
The candidates are chosen by `embark-candidate-collectors'.  By
default, if `embark-select' has been used to select some
candidates, then `embark-act-all' will act on those candidates;
otherwise, if the selection is empty and `embark-act-all' is
called from a minibuffer, then the candidates are the completion
candidates.

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
         (candidates
          (or (cl-mapcar
               (lambda (cand orig-cand bounds)
                 (list :type type :target cand
                       :bounds (when bounds
                                 (cons (copy-marker (car bounds))
                                       (copy-marker (cdr bounds))))
                       :orig-type orig-type :orig-target orig-cand))
               (plist-get transformed :candidates)
               (plist-get transformed :orig-candidates)
               (plist-get transformed :bounds))
              (user-error "No candidates to act on")))
         (indicators (mapcar #'funcall embark-indicators)))
    (when arg (embark-toggle-quit))
    (unwind-protect
        (let* ((action
                (or (embark--prompt
                     indicators (embark--action-keymap type nil)
                     (list (list :type type :multi (length candidates))))
                    (user-error "Canceled")))
               (prefix prefix-arg)
               (act (lambda (candidate)
                      (cl-letf (((symbol-function 'embark--restart) #'ignore)
                                ((symbol-function 'embark--confirm) #'ignore))
                        (let ((prefix-arg prefix))
                          (when-let ((bounds (plist-get candidate :bounds)))
                            (goto-char (car bounds)))
                          (embark--act action candidate)))))
               (quit (embark--quit-p action)))
          (when (and (eq action (embark--default-action type))
                     (eq action embark--command))
            (setq candidates (mapcar #'embark--orig-target candidates)))
          (when (or (not (or embark-confirm-act-all
                             (memq 'embark--confirm
                                   (alist-get action embark-pre-action-hooks))))
                    (y-or-n-p (format "Run %s on %d %ss? "
                                      action (length candidates) type)))
            (if (memq action embark-multitarget-actions)
                (let ((prefix-arg prefix))
                  (embark--act action transformed quit))
              (save-excursion
                (if quit
                    (embark--quit-and-run #'mapc act candidates)
                  (mapc act candidates))))
            (when (and (not quit)
                       (memq 'embark--restart
                             (alist-get action embark-post-action-hooks)))
              (embark--restart))))
      (dolist (cand candidates)
        (when-let ((bounds (plist-get cand :bounds)))
          (set-marker (car bounds) nil) ; yay, manual memory management!
          (set-marker (cdr bounds) nil)))
      (setq prefix-arg nil)
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
            (setq overlay (make-overlay (car bounds) (cdr bounds)))
            (overlay-put overlay 'category 'embark-target-overlay))
          (overlay-put overlay 'window (selected-window)))))))

(defun embark-isearch-highlight-indicator ()
  "Action indicator highlighting all occurrences of the identifier at point.
This indicator only does something for targets which are
identifiers or symbols.  For those it uses `isearch''s lazy
highlighting feature to highlight all occurrences of the target in
the buffer.  This indicator is best used in conjunction with
`embark-highlight-indicator': by using them both you get the
target and the other occurrences of it highlighted in different
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
place, unless overridden by `embark-default-action-overrides'.

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
             (type (plist-get target :type))
             (default-action (embark--default-action type))
             (action (or (command-remapping default-action) default-action)))
        (unless action
          (user-error "No default action for %s targets" type))
        (when (and arg (minibufferp)) (setq embark--toggle-quit t))
        (embark--act action
                     (if (and (eq default-action embark--command)
                              (not (memq default-action
                                         embark-multitarget-actions)))
                         (embark--orig-target target)
                       target)
                     (embark--quit-p action)))
    (user-error "No target found")))

(defun embark--become-keymap ()
  "Return keymap of commands to become for current command."
  (let ((map (make-composed-keymap
              (cl-loop for keymap-name in embark-become-keymaps
                       for keymap = (symbol-value keymap-name)
                       when (where-is-internal embark--command (list keymap))
                       collect keymap))))
    (when embark-help-key
      (keymap-set map embark-help-key #'embark-keymap-help))
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
  (let* ((target (embark--display-string ; remove invisible portions
                  (if full
                      (minibuffer-contents)
                    (pcase-let ((`(,beg . ,end) (embark--boundaries)))
                      (substring (minibuffer-contents) beg
                                 (+ end (embark--minibuffer-point)))))))
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
       (let ((use-dialog-box nil) ;; avoid mouse dialogs
             (last-nonmenu-event 13))
         (setq this-command command)
         (command-execute command))))))

;;; Embark collect

(defgroup embark-collect nil
  "Buffers for acting on collected Embark targets."
  :group 'embark)

(defcustom embark-candidate-collectors
  '(embark-selected-candidates
    embark-minibuffer-candidates
    embark-completion-list-candidates
    embark-dired-candidates
    embark-ibuffer-candidates
    embark-embark-collect-candidates
    embark-custom-candidates)
  "List of functions that collect all candidates in a given context.
These are used to fill an Embark Collect buffer.  Each function
should return either nil (to indicate it found no candidates) or
a list whose first element is a symbol indicating the type of
candidates and whose `cdr' is the list of candidates, each of
which should be either a string or a dotted list of the
form (TARGET START . END), where START and END are the buffer
positions bounding the TARGET string."
  :type 'hook)

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
    (t . embark-collect))
  "Alist associating completion types to export functions.
Each function should take a list of strings which are candidates
for actions and make a buffer appropriate to manage them.  For
example, the default is to make a Dired buffer for files, and an
ibuffer for buffers.

The key t is also allowed in the alist, and the corresponding
value indicates the default function to use for other types.  The
default is `embark-collect'"
  :type '(alist :key-type symbol :value-type function))

(defcustom embark-after-export-hook nil
  "Hook run after `embark-export' in the newly created buffer."
  :type 'hook)

(defface embark-collect-candidate '((t :inherit default))
  "Face for candidates in Embark Collect buffers.")

(defface embark-collect-group-title
  '((t :inherit shadow :slant italic))
  "Face for group titles in Embark Collect buffers.")

(defface embark-collect-group-separator
  '((t :inherit shadow :strike-through t italic))
  "Face for group titles in Embark Collect buffers.")

(defcustom embark-collect-group-format
  (concat
   (propertize "    " 'face 'embark-collect-group-separator)
   (propertize " %s " 'face 'embark-collect-group-title)
   (propertize " " 'face 'completions-group-separator
               'display '(space :align-to right)))
  "Format string used for the group title in Embark Collect buffers."
  :type 'string)

(defface embark-collect-annotation '((t :inherit completions-annotations))
  "Face for annotations in Embark Collect.
This is only used for annotation that are not already fontified.")

(defvar-local embark--rerun-function nil
  "Function to rerun the collect or export that made the current buffer.")

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
(declare-function dired-move-to-filename "dired")
(declare-function dired-move-to-end-of-filename "dired")

(defun embark-dired-candidates ()
  "Return marked or all files shown in Dired buffer.
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
             (and (cdr marked)
                  (if (eq (car marked) t) (cdr marked) marked)))
           (save-excursion
             (goto-char (point-min))
             (let (files)
               (while (not (eobp))
                 (when-let (file (dired-get-filename t t))
                   (push `(,file
                           ,(progn (dired-move-to-filename) (point))
                           . ,(progn (dired-move-to-end-of-filename t) (point)))
                         files))
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
    (cons embark--type
          (save-excursion
            (goto-char (point-min))
            (let (all)
              (when-let ((cand (embark-target-collect-candidate)))
                (push (cdr cand) all))
              (while (forward-button 1 nil nil t)
                (when-let ((cand (embark-target-collect-candidate)))
                  (push (cdr cand) all)))
              (nreverse all))))))

(defun embark-completion-list-candidates ()
  "Return all candidates in a completions buffer."
  (when (derived-mode-p 'completion-list-mode)
    (cons
     embark--type
     (save-excursion
       (goto-char (point-min))
       (next-completion 1)
       (let (all)
         (while (not (eobp))
           (push (cdr (embark-target-completion-list-candidate)) all)
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
                     `(,(symbol-name
                         (plist-get (cdr (plist-get (cdr widget) :parent))
                                    :value))
                       ,(point)
                       . ,(progn
                            (re-search-forward ":" (line-end-position) 'noerror)
                            (point)))
                     symbols)))
                (forward-line))
              (nreverse symbols))))))


(defun embark-collect--target ()
  "Return the Embark Collect candidate at point.
This takes into account `embark-transformer-alist'."
  (let ((embark-target-finders '(embark-target-collect-candidate)))
    (car (embark--targets))))

(defun embark--action-command (action)
  "Turn an ACTION into a command to perform the action.
Returns the name of the command."
  (let ((name (intern (format "embark-action--%s"
                              (embark--command-name action)))))
    (fset name (lambda (arg)
                 (interactive "P")
                 (when-let (target (embark-collect--target))
                   (let ((prefix-arg arg))
                     (embark--act action target)))))
    (when (fboundp action)
      (put name 'function-documentation (documentation action)))
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
             (push (cons (vconcat (vector key) (car bind)) (cdr bind))
                   maps))))
        (def (push (cons (vector key) def) bindings))))
     (keymap-canonicalize keymap))
    (nconc (nreverse bindings) (nreverse maps))))

(defun embark-collect--direct-action-map (type)
  "Return a direct action keymap for targets of given TYPE."
  (let* ((actions (embark--action-keymap type nil))
         (map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (pcase-dolist (`(,key . ,cmd) (embark--all-bindings actions))
      (unless (or (equal key [13])
                  (memq cmd '(digit-argument negative-argument)))
        (define-key map key (if (eq cmd 'embark-keymap-help)
                                #'embark-bindings-at-point
                              (embark--action-command cmd)))))
    map))

(define-minor-mode embark-collect-direct-action-minor-mode
  "Bind type-specific actions directly (without need for `embark-act')."
  :init-value nil
  :lighter " Act"
  (unless (derived-mode-p 'embark-collect-mode)
    (user-error "Not in an Embark Collect buffer"))
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t) maps)
      (while (progn
               (when (tabulated-list-get-id)
                 (put-text-property
                  (point) (button-end (point)) 'keymap
                  (if embark-collect-direct-action-minor-mode
                      (when-let ((target (embark-collect--target))
                                 (type (plist-get target :type)))
                        (or (alist-get type maps)
                            (setf (alist-get type maps)
                                  (embark-collect--direct-action-map type)))))))
               (forward-button 1 nil nil t))))))

(define-button-type 'embark-collect-entry
  'face 'embark-collect-candidate
  'action 'embark-collect-choose)

(declare-function outline-toggle-children "outline")
(define-button-type 'embark-collect-group
  'face 'embark-collect-group-title
  'action (lambda (_) (outline-toggle-children)))

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
  "Run default action on Embark Collect ENTRY."
  (pcase-let ((`(,type ,text ,start . ,end)
               (save-excursion
                 (goto-char entry)
                 (embark-target-collect-candidate))))
    (embark--act (embark--default-action type)
                 (list :target text
                       :type type
                       :bounds (cons start end)))))

(defvar-keymap embark-collect-mode-map
  :doc "Keymap for Embark collect mode."
  :parent tabulated-list-mode-map
  "a" #'embark-act
  "A" #'embark-act-all
  "M-a" #'embark-collect-direct-action-minor-mode
  "E" #'embark-export
  "s" #'isearch-forward
  "n" #'forward-button
  "p" #'backward-button
  "}" 'outline-next-heading
  "{" 'outline-previous-heading
  "<remap> <forward-paragraph>" 'outline-next-heading
  "<remap> <backward-paragraph>" 'outline-previous-heading
  "<remap> <revert-buffer>" #'embark-rerun-collect-or-export)

(defconst embark-collect--outline-string (string #x210000)
  "Special string used for outline headings in Embark Collect buffers.
Chosen to be extremely unlikely to appear in a candidate.")

(define-derived-mode embark-collect-mode tabulated-list-mode "Embark Collect"
  "List of candidates to be acted on.
The command `embark-act' is bound `embark-collect-mode-map', but
you might prefer to change the key binding to match your other
key binding for it.  Or alternatively you might want to enable the
embark collect direct action minor mode by adding the function
`embark-collect-direct-action-minor-mode' to
`embark-collect-mode-hook'.

Reverting an Embark Collect buffer has slightly unusual behavior
if the buffer was obtained by running `embark-collect' from
within a minibuffer completion session.  In that case reverting
just restarts the completion session, that is, the command that
opened the minibuffer is run again and the minibuffer contents
restored.  You can then interact normally with the command,
perhaps editing the minibuffer contents, and, if you wish, you
can rerun `embark-collect' to get an updated buffer."
    :interactive nil :abbrev-table nil :syntax-table nil)

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
      (let ((annotator
             (or (embark-collect--metadatum type 'annotation-function)
                 (lambda (_) ""))))
        (lambda (candidates)
          (mapcar (lambda (c)
                    (if-let (a (funcall annotator c)) (list c "" a) c))
                  candidates)))))

(defun embark--display-string (str)
  ;; Note: Keep in sync with vertico--display-string
  "Return display STR without display and invisible properties."
  (let ((end (length str)) (pos 0) chunks)
    (while (< pos end)
      (let ((nextd (next-single-property-change pos 'display str end))
            (disp (get-text-property pos 'display str)))
        (if (stringp disp)
            (let ((face (get-text-property pos 'face str)))
              (when face
                (add-face-text-property
                 0 (length disp) face t (setq disp (concat disp))))
              (setq pos nextd chunks (cons disp chunks)))
          (while (< pos nextd)
            (let ((nexti
                   (next-single-property-change pos 'invisible str nextd)))
              (unless (or (get-text-property pos 'invisible str)
                          (and (= pos 0) (= nexti end))) ;; full=>no allocation
                (push (substring str pos nexti) chunks))
              (setq pos nexti))))))
    (if chunks (apply #'concat (nreverse chunks)) str)))

(defconst embark--hline
  (propertize
   (concat "\n" (propertize
                 " " 'display '(space :align-to right)
                 'face '(:inherit completions-group-separator :height 0.01)
                 'cursor-intangible t 'intangible t)))
  "Horizontal line used to separate multiline collect entries.")

(defun embark-collect--format-entries (candidates grouper)
  "Format CANDIDATES for `tabulated-list-mode' grouped by GROUPER.
The GROUPER is either nil or a function like the `group-function'
completion metadatum, that is, a function of two arguments, the
first of which is a candidate and the second controls what is
computed: if nil, the title of the group the candidate belongs
to, and if non-nil, a rewriting of the candidate (useful to
simplify the candidate so it doesn't repeat the group title, for
example)."
  (let ((max-width 0)
        (transform
         (if grouper (lambda (cand) (funcall grouper cand t)) #'identity)))
    (setq
     tabulated-list-entries
     (mapcan
      (lambda (group)
        (let ((multiline (seq-some (lambda (x) (string-match-p "\n" (car x)))
                                   candidates)))
          (cons
           `(nil [(,(concat (propertize embark-collect--outline-string
                                        'invisible t)
                            (format embark-collect-group-format (car group)))
                   type embark-collect-group)
                  ("" skip t)])
           (mapcar
            (pcase-lambda (`(,cand ,prefix ,annotation))
              (let* ((display (embark--display-string (funcall transform cand)))
                     (length (length annotation))
                     (faces (text-property-not-all
                             0 length 'face nil annotation)))
                (setq max-width (max max-width (+ (string-width prefix)
                                                  (string-width display))))
                (when faces
                  (add-face-text-property 0 length 'default t annotation))
                `(,cand
                  [(,(propertize
                      (if multiline (concat display embark--hline) display)
                      'line-prefix prefix)
                    type embark-collect-entry)
                   (,annotation
                    skip t
                    ,@(unless faces
                        '(face embark-collect-annotation)))])))
            (cdr group)))))
     (if grouper
         (seq-group-by (lambda (item) (funcall grouper (car item) nil))
                       candidates)
       (list (cons "" candidates)))))
  (if (null grouper)
      (pop tabulated-list-entries)
    (setq-local outline-regexp embark-collect--outline-string)
    (outline-minor-mode))
  (setq tabulated-list-format
        `[("Candidate" ,max-width t) ("Annotation" 0 t)])))

(defun embark-collect--update-candidates (buffer)
  "Update candidates for Embark Collect BUFFER."
  (let* ((transformed (embark--maybe-transform-candidates))
         (type (plist-get transformed :orig-type)) ; we need the originals for
         (candidates (plist-get transformed :orig-candidates)) ; default action
         (bounds (plist-get transformed :bounds))
         (affixator (embark-collect--affixator type))
         (grouper (embark-collect--metadatum type 'group-function)))
    (when (eq type 'file)
      (let ((dir (buffer-local-value 'default-directory buffer)))
        (setq candidates
              (mapcar (lambda (cand)
                        (let ((rel (file-relative-name cand dir)))
                          (if (string-prefix-p "../" rel) cand rel)))
                      candidates))))
    (if (seq-some #'identity bounds)
      (cl-loop for cand in candidates and (start . _end) in bounds
               when start
               do (add-text-properties
                   0 1 `(embark--location ,(copy-marker start)) cand)))
    (setq candidates (funcall affixator candidates))
    (with-current-buffer buffer
      (setq embark--type type)
      (unless embark--command
        (setq embark--command #'embark--goto))
      (embark-collect--format-entries candidates grouper))
    candidates))

(defun embark--goto (target)
  "Jump to the original location of TARGET.
This function is used as a default action in Embark Collect
buffers when the candidates were a selection from a regular
buffer."
  ;; TODO: ensure the location jumped to is visible
  ;; TODO: remove duplication with embark-org-goto-heading
  (when-let ((marker (get-text-property 0 'embark--location target)))
    (pop-to-buffer (marker-buffer marker))
    (widen)
    (goto-char marker)
    (pulse-momentary-highlight-one-line)))

(defun embark--collect (buffer-name)
  "Create an Embark Collect buffer named BUFFER-NAME.

The function `generate-new-buffer-name' is used to ensure the
buffer has a unique name."
  (let ((buffer (generate-new-buffer buffer-name))
        (rerun (embark--rerun-function #'embark-collect)))
    (with-current-buffer buffer
      ;; we'll run the mode hooks once the buffer is displayed, so
      ;; the hooks can make use of the window
      (delay-mode-hooks (embark-collect-mode)))

    (embark--cache-info buffer)
    (unless (embark-collect--update-candidates buffer)
      (user-error "No candidates to collect"))

    (with-current-buffer buffer
      (setq tabulated-list-use-header-line nil ; default to no header
            header-line-format nil
            tabulated-list--header-string nil)
      (setq embark--rerun-function rerun))

    (let ((window (display-buffer buffer)))
      (with-selected-window window
        (run-mode-hooks)
        (tabulated-list-revert))
      (set-window-dedicated-p window t)
      buffer)))

(defun embark--descriptive-buffer-name (type)
  "Return a descriptive name for an Embark collect or export buffer.
TYPE should be either `collect' or `export'."
  (format "*Embark %s: %s*"
          (capitalize (symbol-name type))
          (if (minibufferp)
              (format "%s - %s" embark--command
                      (minibuffer-contents-no-properties))
            (buffer-name))))

;;;###autoload
(defun embark-collect ()
  "Create an Embark Collect buffer.

To control the display, add an entry to `display-buffer-alist'
with key \"Embark Collect\".

In Embark Collect buffers `revert-buffer' is remapped to
`embark-rerun-collect-or-export', which has slightly unusual
behavior if the buffer was obtained by running `embark-collect'
from within a minibuffer completion session.  In that case
rerunning just restarts the completion session, that is, the
command that opened the minibuffer is run again and the
minibuffer contents restored.  You can then interact normally with
the command, perhaps editing the minibuffer contents, and, if you
wish, you can rerun `embark-collect' to get an updated buffer."
  (interactive)
  (let ((buffer (embark--collect (embark--descriptive-buffer-name 'collect))))
    (when (minibufferp)
      (embark--run-after-command #'pop-to-buffer buffer)
      (embark--quit-and-run #'message nil))))

;;;###autoload
(defun embark-live ()
  "Create a live-updating Embark Collect buffer.

To control the display, add an entry to `display-buffer-alist'
with key \"Embark Live\"."
  (interactive)
  (let ((live-buffer (embark--collect
                      (format "*Embark Live: %s*"
                              (if (minibufferp)
                                  (format "M-x %s" embark--command)
                                (buffer-name)))))
        (run-collect (make-symbol "run-collect"))
        (stop-collect (make-symbol "stop-collect"))
        timer)
    (setf (symbol-function stop-collect)
          (lambda ()
            (remove-hook 'change-major-mode-hook stop-collect t)
            (remove-hook 'after-change-functions run-collect t)))
    (setf (symbol-function run-collect)
          (lambda (_1 _2 _3)
            (unless timer
              (setq timer
                    (run-with-idle-timer
                     0.05 nil
                     (lambda ()
                       (if (not (buffer-live-p live-buffer))
                           (funcall stop-collect)
                         (embark-collect--update-candidates live-buffer)
                         (with-current-buffer live-buffer
                           ;; TODO figure out why I can't restore point
                           (tabulated-list-print t t))
                         (setq timer nil))))))))
    (add-hook 'after-change-functions run-collect nil t)
    (when (minibufferp)
      (add-hook 'change-major-mode-hook stop-collect nil t))))

(defun embark--rerun-function (kind)
  "Return a rerun function for an export or collect buffer in this context.
The parameter KIND should be either `embark-export' or `embark-collect'."
  (let ((buffer (or embark--target-buffer (embark--target-buffer)))
        (command embark--command))
    (cl-flet ((rerunner (action)
                (lambda (&rest _)
                  (quit-window 'kill-buffer)
                  (with-current-buffer
                      (if (buffer-live-p buffer) buffer (current-buffer))
                    (let ((embark--command command))
                      (funcall action))))))
        (if (minibufferp)
          (rerunner
           (let ((input (minibuffer-contents-no-properties)))
             (lambda ()
               (minibuffer-with-setup-hook
                   (lambda ()
                     (delete-minibuffer-contents)
                     (insert input))
                 (setq this-command embark--command)
                 (command-execute embark--command)))))
          (rerunner kind)))))

(defun embark-rerun-collect-or-export ()
  "Rerun the `embark-collect' or `embark-export' that created this buffer."
  (interactive)
  (if embark--rerun-function
      (funcall embark--rerun-function)
    (user-error "No function to rerun collect or export found")))

;;;###autoload
(defun embark-export ()
  "Create a type-specific buffer to manage current candidates.
The variable `embark-exporters-alist' controls how to make the
buffer for each type of completion.

In Embark Export buffers `revert-buffer' is remapped to
`embark-rerun-collect-or-export', which has slightly unusual
behavior if the buffer was obtained by running `embark-export'
from within a minibuffer completion session.  In that case
reverting just restarts the completion session, that is, the
command that opened the minibuffer is run again and the
minibuffer contents restored.  You can then interact normally
with the command, perhaps editing the minibuffer contents, and,
if you wish, you can rerun `embark-export' to get an updated
buffer."
  (interactive)
  (let* ((transformed (embark--maybe-transform-candidates))
         (candidates (or (plist-get transformed :candidates)
                         (user-error "No candidates for export")))
         (type (plist-get transformed :type)))
    (let ((exporter (or (alist-get type embark-exporters-alist)
                        (alist-get t embark-exporters-alist))))
      (if (eq exporter 'embark-collect)
          (embark-collect)
        (let* ((after embark-after-export-hook)
               (cmd embark--command)
               (name (embark--descriptive-buffer-name 'export))
               (rerun (embark--rerun-function #'embark-export))
               (buffer (save-excursion
                         (funcall exporter candidates)
                         (rename-buffer name t)
                         (current-buffer))))
          (embark--quit-and-run
           (lambda ()
             (pop-to-buffer buffer)
             (setq embark--rerun-function rerun)
             (use-local-map
              (make-composed-keymap
               '(keymap
                 (remap keymap
                        (revert-buffer . embark-rerun-collect-or-export)))
               (current-local-map)))
             (let ((embark-after-export-hook after)
                   (embark--command cmd))
               (run-hooks 'embark-after-export-hook)))))))))

(defmacro embark--export-rename (buffer title &rest body)
  "Run BODY and rename BUFFER to Embark export buffer with TITLE."
  (declare (indent 2))
  (let ((saved (make-symbol "saved")))
    `(let ((,saved (embark-rename-buffer
                    ,buffer " *Embark Saved*" t)))
       ,@body
       (set-buffer (embark-rename-buffer
                    ,buffer ,(format "*Embark Export %s*" title) t))
       (when ,saved (embark-rename-buffer ,saved ,buffer)))))

(defun embark--export-customize (items type pred)
  "Create a customization buffer listing ITEMS.
TYPE is the items type.
PRED is a predicate function used to filter the items."
  (custom-buffer-create
   (cl-loop for item in items
            for sym = (intern-soft item)
            when (and sym (funcall pred sym)) collect `(,sym ,type))))

(autoload 'apropos-parse-pattern "apropos")
(autoload 'apropos-symbols-internal "apropos")
(defun embark-export-apropos (symbols)
  "Create apropos buffer listing SYMBOLS."
  (embark--export-rename "*Apropos*" "Apropos"
    (apropos-parse-pattern "") ;; Initialize apropos pattern
    ;; HACK: Ensure that order of exported symbols is kept.
    (cl-letf (((symbol-function #'sort) (lambda (list _pred) (nreverse list))))
      (apropos-symbols-internal
       (delq nil (mapcar #'intern-soft symbols))
       (bound-and-true-p apropos-do-all)))))

(defun embark-export-customize-face (faces)
  "Create a customization buffer listing FACES."
  (embark--export-customize faces 'custom-face #'facep))

(defun embark-export-customize-variable (variables)
  "Create a customization buffer listing VARIABLES."
  ;; The widget library serializes/deserializes the values. We advise
  ;; the serialization in order to avoid errors for nonserializable
  ;; variables.
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
    (embark--export-customize variables 'custom-variable #'boundp)))

(defun embark-export-ibuffer (buffers)
  "Create an ibuffer buffer listing BUFFERS."
  (ibuffer t "*Embark Export Ibuffer*"
           `((predicate . (member (buffer-name) ',buffers)))))

(autoload 'dired-check-switches "dired")
(declare-function dired-unadvertise "dired")
(defvar dired-directory)

(defun embark-export-dired (files)
  "Create a Dired buffer listing FILES."
  (setq files (mapcar #'directory-file-name
                      (cl-remove-if-not #'file-exists-p files)))
  (when (dired-check-switches dired-listing-switches "A" "almost-all")
    (setq files (cl-remove-if
                 (lambda (path)
                   (let ((file (file-name-nondirectory path)))
                     (or (string= file ".") (string= file ".."))))
                 files)))
  (cl-letf* ((dir (or (file-name-directory (try-completion "" files)) ""))
             ;; Prevent reusing existing Dired buffer.
             ((symbol-function 'dired-find-buffer-nocreate) #'ignore)
             (buf (dired-noselect
                   (cons (expand-file-name dir)
                         (mapcar (lambda (file) (string-remove-prefix dir file))
                                 files)))))
    (with-current-buffer buf
      ;; Unadvertise to prevent the new buffer from being reused.
      (dired-unadvertise (car dired-directory))
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

;;; Multiple target selection

(defface embark-selected '((t (:inherit match)))
  "Face for selected candidates.")

(defcustom embark-selection-indicator
  #("  Embark:%s " 1 12 (face (embark-selected bold)))
  "Mode line indicator used for selected candidates."
  :type '(choice string (const nil)))

(defvar-local embark--selection nil
  "Buffer local list of selected targets.
Add or remove elements to this list using the `embark-select'
action.")

(defun embark--selection-indicator ()
  "Mode line indicator showing number of selected items."
  (when-let ((sel
              (buffer-local-value
               'embark--selection
               (or (when-let ((win (active-minibuffer-window)))
                     (window-buffer win))
                   (current-buffer)))))
    (format embark-selection-indicator (length sel))))

(cl-defun embark--select
    (&key orig-target orig-type bounds &allow-other-keys)
  "Add or remove ORIG-TARGET of given ORIG-TYPE to the selection.
If BOUNDS are given, also highlight the target when selecting it."
  (cl-flet ((multi-type (x) (car (get-text-property 0 'multi-category x))))
    (if-let* ((existing (seq-find
                         (pcase-lambda (`(,cand . ,ov))
                           (and
                            (equal cand orig-target)
                            (if (and bounds ov)
                                (and (= (car bounds) (overlay-start ov))
                                     (= (cdr bounds) (overlay-end ov)))
                              (let ((cand-type (multi-type cand)))
                                (or (eq cand-type orig-type)
                                    (eq cand-type (multi-type orig-target)))))))
                         embark--selection)))
        (progn
          (when (cdr existing) (delete-overlay (cdr existing)))
          (setq embark--selection (delq existing embark--selection)))
      (let ((target (copy-sequence orig-target)) overlay)
        (when bounds
          (setq overlay (make-overlay (car bounds) (cdr bounds)))
          (overlay-put overlay 'category 'embark-selected-overlay))
        (add-text-properties 0 (length orig-target)
                             `(multi-category ,(cons orig-type orig-target))
                             target)
        (push (cons target overlay) embark--selection))))
  (when embark-selection-indicator
    (add-to-list 'mode-line-misc-info '(:eval (embark--selection-indicator)))
    (force-mode-line-update t)))

;;;###autoload
(defun embark-select ()
  "Add or remove the target from the current buffer's selection.
You can act on all selected targets at once with `embark-act-all'.
When called from outside `embark-act' this command will select
the first target at point."
  (interactive)
  (if-let ((target (car (embark--targets))))
      (apply #'embark--select target)
    (user-error "No target to select")))

(defun embark-selected-candidates ()
  "Return currently selected candidates in the buffer."
  (when embark--selection
    (cl-flet ((unwrap (x) (get-text-property 0 'multi-category x)))
      (let* ((first-type (car (unwrap (caar embark--selection))))
             (same (cl-every (lambda (item)
                               (eq (car (unwrap (car item))) first-type))
                             embark--selection))
             (extract (if same
                          (pcase-lambda (`(,cand . ,overlay))
                            (cons (cdr (unwrap cand)) overlay))
                        #'identity)))
        (cons
         (if same first-type 'multi-category)
         (nreverse
          (mapcar
           (lambda (item)
             (pcase-let ((`(,cand . ,ov) (funcall extract item)))
               (if ov `(,cand ,(overlay-start ov) . ,(overlay-end ov)) cand)))
           embark--selection)))))))

;;; Integration with external packages, mostly completion UIs

;; marginalia

;; Ensure that the Marginalia cache is reset, such that
;; `embark-toggle-variable-value' updates the display (See #540).
(with-eval-after-load 'marginalia
  (push 'marginalia--cache-reset (alist-get :always embark-post-action-hooks)))

;; vertico

(declare-function vertico--candidate "ext:vertico")
(declare-function vertico--update "ext:vertico")
(defvar vertico--input)
(defvar vertico--candidates)
(defvar vertico--base)

(defun embark--vertico-selected ()
  "Target the currently selected item in Vertico.
Return the category metadatum as the type of the target."
  (when vertico--input
    ;; Force candidate computation, if candidates are not yet available.
    (vertico--update)
    (cons (completion-metadata-get (embark--metadata) 'category)
          (vertico--candidate))))

(defun embark--vertico-candidates ()
  "Collect the current Vertico candidates.
Return the category metadatum as the type of the candidates."
  (when vertico--input
    ;; Force candidate computation, if candidates are not yet available.
    (vertico--update)
    (cons (completion-metadata-get (embark--metadata) 'category)
          vertico--candidates)))

(defun embark--vertico-indicator ()
  "Embark indicator highlighting the current Vertico candidate."
  (let ((fr face-remapping-alist))
    (lambda (&optional keymap _targets _prefix)
      (when vertico--input
        (setq-local face-remapping-alist
                    (if keymap
                        (cons '(vertico-current . embark-target) fr)
                      fr))))))

(with-eval-after-load 'vertico
  (cl-defmethod vertico--format-candidate
    :around (cand prefix suffix index start &context (embark--selection cons))
    (when (cl-find (concat vertico--base (nth index vertico--candidates))
                   embark--selection
                   :test #'equal :key #'car)
      (setq cand (copy-sequence cand))
      (add-face-text-property 0 (length cand) 'embark-selected t cand))
    (cl-call-next-method cand prefix suffix index start))
  (add-hook 'embark-indicators #'embark--vertico-indicator)
  (add-hook 'embark-target-finders #'embark--vertico-selected)
  (add-hook 'embark-candidate-collectors #'embark--vertico-candidates)
  (remove-hook 'embark-candidate-collectors #'embark-selected-candidates)
  (add-hook 'embark-candidate-collectors #'embark-selected-candidates))

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
  (add-hook 'embark-candidate-collectors #'embark--ivy-candidates)
  (remove-hook 'embark-candidate-collectors #'embark-selected-candidates)
  (add-hook 'embark-candidate-collectors #'embark-selected-candidates))

;;; Custom actions

(defvar embark-separator-history nil
  "Input history for the separators used by some embark commands.
The commands that prompt for a string separator are
`embark-insert' and `embark-copy-as-kill'.")

(defun embark-keymap-help ()
  "Prompt for an action to perform or command to become and run it."
  (interactive)
  (user-error "Not meant to be called directly"))

(defun embark-toggle-quit ()
  "Toggle whether the following action quits the minibuffer."
  (interactive)
  (when (minibufferp)
    (setq embark--toggle-quit (not embark--toggle-quit))
    (if (consp embark-quit-after-action)
        (message "Will %sobey embark-quit-after-action."
                 (if embark--toggle-quit "dis" ""))
      (message
       "Will %squit minibuffer after action"
       (if (eq embark--toggle-quit embark-quit-after-action) "not " "")))))

(defun embark--separator (strings)
  "Return a separator to join the STRINGS together.
With a prefix argument, prompt the user (unless STRINGS has 0 or
1 elements, in which case a separator is not needed)."
  (if (and current-prefix-arg (cdr strings))
      (read-string "Separator: " nil 'embark-separator-history)
    "\n"))

(defun embark-copy-as-kill (strings)
  "Join STRINGS and save on the `kill-ring'.
With a prefix argument, prompt for the separator to join the
STRINGS, which defaults to a newline."
  (kill-new (string-join strings (embark--separator strings))))

(defun embark-insert (strings)
  "Join STRINGS and insert the result at point.
With a prefix argument, prompt for the separator to join the
STRINGS, which defaults to a newline.

Some whitespace is also inserted if necessary to avoid having the
inserted string blend into the existing buffer text.  More
precisely:

1. If the inserted string does not contain newlines, a space may
be added before or after it as needed to avoid inserting a word
constituent character next to an existing word constituent.

2. For a multiline inserted string, newlines may be added before
or after as needed to ensure the inserted string is on lines of
its own."
  (let* ((separator (embark--separator strings))
         (multiline
          (or (and (cdr strings) (string-match-p "\n" separator))
              (and (null (cdr strings))
                   (equal (buffer-substring (line-beginning-position)
                                            (line-end-position))
                          (car strings)))
              (seq-some (lambda (s) (string-match-p "\n" s)) strings))))
    (cl-labels ((maybe-space ()
                  (and (looking-at "\\w") (looking-back "\\w" 1)
                       (insert " ")))
                (maybe-newline ()
                  (or (looking-back "^[ \t]*" 40) (looking-at "\n")
                      (newline-and-indent)))
                (maybe-whitespace ()
                  (if multiline (maybe-newline) (maybe-space)))
                (ins-string ()
                  (let ((start (point)))
                    (insert (string-join strings separator))
                    (save-excursion (goto-char start) (maybe-whitespace))
                    (when (looking-back "\n" 1) (delete-char -1))
                    (save-excursion (maybe-whitespace)))))
      (if buffer-read-only
          (with-selected-window (other-window-for-scrolling)
            (ins-string))
        (ins-string)))))

;; For Emacs 28 dired-jump will be moved to dired.el, but it seems
;; that since it already has an autoload in Emacs 28, this next
;; autoload is ignored.
(autoload 'dired-jump "dired-x" nil t)

(defun embark-dired-jump (file &optional other-window)
  "Open Dired buffer in directory containing FILE and move to its line.
When called with a prefix argument OTHER-WINDOW, open Dired in other window."
  (interactive "fJump to Dired file: \nP")
  (dired-jump other-window file))

(defun embark--read-from-history (prompt candidates &optional category)
  "Read with completion from list of history CANDIDATES of CATEGORY.
Sorting and history are disabled.  PROMPT is the prompt message."
  (completing-read prompt
                   (embark--with-category category candidates)
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
  (embark-history-remove (expand-file-name file))
  (embark-history-remove (abbreviate-file-name file))
  (when (and (boundp 'recentf-list) (fboundp 'recentf-expand-file-name))
    (setq recentf-list (delete (recentf-expand-file-name file) recentf-list))))

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
  (embark-insert (list (string-trim (pp-to-string (symbol-value var))))))

(defun embark-toggle-variable (var &optional local)
  "Toggle value of boolean variable VAR.
If prefix LOCAL is non-nil make variable local."
  (interactive "SVariable: \nP")
  (let ((val (symbol-value var)))
    (unless (memq val '(nil t))
      (user-error "Not a boolean variable"))
    (when local
      (make-local-variable var))
    (funcall (or (get var 'custom-set) 'set) var (not val))))

(defun embark-insert-relative-path (file)
  "Insert relative path to FILE.
The insert path is relative to `default-directory'."
  (interactive "FFile: ")
  (embark-insert (list (file-relative-name (substitute-in-file-name file)))))

(defun embark-save-relative-path (file)
  "Save the relative path to FILE in the kill ring.
The insert path is relative to `default-directory'."
  (interactive "FFile: ")
  (kill-new (file-relative-name (substitute-in-file-name file))))

(defun embark-shell-command-on-buffer (buffer command &optional replace)
  "Run shell COMMAND on contents of BUFFER.
Called with \\[universal-argument], replace contents of buffer
with command output.  For replacement behavior see
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

(defun embark-open-externally (file)
  "Open FILE or url using system's default application."
  (interactive "sOpen externally: ")
  (unless (string-match-p "\\`[a-z]+://" file)
    (setq file (expand-file-name file)))
  (message "Opening `%s' externally..." file)
  (if (and (eq system-type 'windows-nt)
           (fboundp 'w32-shell-execute))
      (w32-shell-execute "open" file)
    (call-process (pcase system-type
                    ('darwin "open")
                    ('cygwin "cygstart")
                    (_ "xdg-open"))
                  nil 0 nil file)))

(declare-function bookmark-prop-get "bookmark")
(declare-function bookmark-completing-read "bookmark")

(defun embark-bookmark-open-externally (bookmark)
  "Open BOOKMARK in external application."
  (interactive (list (bookmark-completing-read "Open externally: ")))
  (embark-open-externally
   (or (bookmark-prop-get bookmark 'location)
       (bookmark-prop-get bookmark 'filename)
       (user-error "Bookmark `%s' does not have a location" bookmark))))

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
  "Save Unicode character CHAR to kill ring."
  (interactive
   (list (read-char-by-name "Insert character  (Unicode name or hex): ")))
  (kill-new (format "%c" char)))

(defun embark-isearch-forward ()
  "Prompt for string in the minibuffer and start isearch forwards.
Unlike isearch, this command reads the string from the
minibuffer, which means it can be used as an Embark action."
  (interactive)
  (isearch-mode t)
  (isearch-edit-string))

(defun embark-isearch-backward ()
  "Prompt for string in the minibuffer and start isearch backwards.
Unlike isearch, this command reads the string from the
minibuffer, which means it can be used as an Embark action."
  (interactive)
  (isearch-mode nil)
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

(defun embark-next-symbol ()
  "Jump to next occurrence of symbol at point.
The search respects symbol boundaries."
  (interactive)
  (if-let ((symbol (thing-at-point 'symbol)))
      (let ((regexp (format "\\_<%s\\_>" (regexp-quote symbol))))
        (when (looking-at regexp)
          (forward-symbol 1))
        (unless (re-search-forward regexp nil t)
          (user-error "Symbol `%s' not found" symbol)))
    (user-error "No symbol at point")))

(defun embark-previous-symbol ()
  "Jump to previous occurrence of symbol at point.
The search respects symbol boundaries."
  (interactive)
  (if-let ((symbol (thing-at-point 'symbol)))
      (let ((regexp (format "\\_<%s\\_>" (regexp-quote symbol))))
        (when (looking-back regexp (- (point) (length symbol)))
          (forward-symbol -1))
        (unless (re-search-backward regexp nil t)
          (user-error "Symbol `%s' not found" symbol)))
    (user-error "No symbol at point")))

(defun embark-compose-mail (address)
  "Compose email to ADDRESS."
  ;; The only reason we cannot use compose-mail directly is its
  ;; interactive specification, which just supplies nil for the
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

(defun embark-eval-replace (noquote)
  "Evaluate region and replace with evaluated result.
If NOQUOTE is non-nil (interactively, if called with a prefix
argument), no quoting is used for strings."
  (interactive "P")
  (let ((beg (region-beginning))
        (end (region-end)))
    (save-excursion
      (goto-char end)
      (insert (format (if noquote "%s" "%S")
               (eval (read (buffer-substring beg end)) lexical-binding)))
      (delete-region beg end))))

(when (< emacs-major-version 29)
  (defun embark-elp-restore-package (prefix)
    "Remove instrumentation from functions with names starting with PREFIX."
    (interactive "SPrefix: ")
    (when (fboundp 'elp-restore-list)
      (elp-restore-list
       (mapcar #'intern
               (all-completions (symbol-name prefix)
                                obarray 'elp-profilable-p))))))

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

(defvar epa-replace-original-text)
(defun embark-epa-decrypt-region (start end)
  "Decrypt region between START and END."
  (interactive "r")
  (let ((epa-replace-original-text t))
    (epa-decrypt-region start end)))

(defvar eww-download-directory)
(autoload 'eww-download-callback "eww")

(defun embark-download-url (url)
  "Download URL to `eww-download-directory'."
  (interactive "sDownload URL: ")
  (let ((dir eww-download-directory))
    (when (functionp dir) (setq dir (funcall dir)))
    (access-file dir "Download failed")
    (url-retrieve
     url #'eww-download-callback
     (if (>= emacs-major-version 28) (list url dir) (list url)))))

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

(cl-defun embark--eval-prep (&key type &allow-other-keys)
  "If target's TYPE is variable, skip edit; if function, wrap in ()."
  (when (memq type '(command function))
    (embark--allow-edit)
    (goto-char (minibuffer-prompt-end))
    (insert "(")
    (goto-char (point-max))
    (insert ")")
    (backward-char)))

(cl-defun embark--beginning-of-target (&key bounds &allow-other-keys)
  "Go to beginning of the target BOUNDS."
  (when (number-or-marker-p (car bounds))
    (goto-char (car bounds))))

(cl-defun embark--end-of-target (&key bounds &allow-other-keys)
  "Go to end of the target BOUNDS."
  (when (number-or-marker-p (cdr bounds))
    (goto-char (cdr bounds))))

(cl-defun embark--mark-target (&rest rest &key run bounds &allow-other-keys)
  "Mark the target if its BOUNDS are known.
After marking the target, call RUN with the REST of its arguments."
  (cond
   ((and bounds run)
    (save-mark-and-excursion
      (set-mark (cdr bounds))
      (goto-char (car bounds))
      (apply run :bounds bounds rest)))
   (bounds ;; used as pre- or post-action hook
    (set-mark (cdr bounds))
    (goto-char (car bounds)))
   (run (apply run rest))))

(cl-defun embark--unmark-target (&rest _)
  "Deactivate the region target."
  (deactivate-mark t))

(cl-defun embark--narrow-to-target
    (&rest rest &key run bounds &allow-other-keys)
  "Narrow buffer to target if its BOUNDS are known.
Intended for use as an Embark around-action hook.  This function
runs RUN with the buffer narrowed to given BOUNDS passing along
the REST of the arguments."
  (if bounds
    (save-excursion
      (save-restriction
        (narrow-to-region (car bounds) (cdr bounds))
        (goto-char (car bounds))
        (apply run :bounds bounds rest)))
    (apply run rest)))

(defun embark--allow-edit (&rest _)
  "Allow editing the target."
  (remove-hook 'post-command-hook #'exit-minibuffer t)
  (remove-hook 'post-command-hook 'ivy-immediate-done t))

(defun embark--ignore-target (&rest _)
  "Ignore the target."
  (let ((contents
         (get-text-property (minibuffer-prompt-end) 'embark--initial-input)))
    (delete-minibuffer-contents)
    (when contents (insert contents)))
  (embark--allow-edit))

(autoload 'xref-push-marker-stack "xref")
(defun embark--xref-push-marker (&rest _)
  "Push a marker onto the xref marker stack."
  (xref-push-marker-stack))

(cl-defun embark--confirm (&key action target &allow-other-keys)
  "Ask for confirmation before running the ACTION on the TARGET."
  (unless (y-or-n-p (format "Run %s on %s? " action target))
    (user-error "Canceled")))

(defconst embark--associated-file-fn-alist
  `((file . identity)
    (buffer . ,(lambda (target)
                 (let ((buffer (get-buffer target)))
                   (or (buffer-file-name buffer)
                       (buffer-local-value 'default-directory buffer)))))
    (bookmark . bookmark-location)
    (library . locate-library))
  "Alist of functions that extract a file path from targets of a given type.")

(defun embark--associated-directory (target type)
  "Return directory associated to TARGET of given TYPE.
The supported values of TYPE are file, buffer, bookmark and
library, which have an obvious notion of associated directory."
  (when-let ((file-fn (alist-get type embark--associated-file-fn-alist))
             (file (funcall file-fn target)))
    (if (file-directory-p file)
        (file-name-as-directory file)
      (file-name-directory file))))

(cl-defun embark--cd (&rest rest &key run target type &allow-other-keys)
  "Run action with `default-directory' set to the directory of TARGET.
The supported values of TYPE are file, buffer, bookmark and
library, which have an obvious notion of associated directory.
The REST of the arguments are also passed to RUN."
  (let ((default-directory
          (or (embark--associated-directory target type) default-directory)))
    (apply run :target target :type type rest)))

(cl-defun embark--save-excursion (&rest rest &key run &allow-other-keys)
  "Run action without moving point.
This simply calls RUN with the REST of its arguments inside
`save-excursion'."
  (save-excursion (apply run rest)))

(defun embark--universal-argument (&rest _)
  "Run action with a universal prefix argument."
  (setq prefix-arg '(4)))

;;; keymaps

(defvar-keymap embark-meta-map
  :doc "Keymap for non-action Embark functions."
  "-" #'negative-argument
  "0" #'digit-argument
  "1" #'digit-argument
  "2" #'digit-argument
  "3" #'digit-argument
  "4" #'digit-argument
  "5" #'digit-argument
  "6" #'digit-argument
  "7" #'digit-argument
  "8" #'digit-argument
  "9" #'digit-argument)

(defvar-keymap embark-general-map
  :doc "Keymap for Embark general actions."
  :parent embark-meta-map
  "i" #'embark-insert
  "w" #'embark-copy-as-kill
  "q" #'embark-toggle-quit
  "E" #'embark-export
  "S" #'embark-collect
  "L" #'embark-live
  "B" #'embark-become
  "A" #'embark-act-all
  "C-s" #'embark-isearch-forward
  "C-r" #'embark-isearch-backward
  "C-SPC" #'mark
  "DEL" #'delete-region
  "SPC" #'embark-select)

(defvar-keymap embark-encode-map
  :doc "Keymap for Embark region encoding actions."
  "r" #'rot13-region
  "." #'morse-region
  "-" #'unmorse-region
  "s" #'studlify-region
  "m" #'embark-hash-md5
  "1" #'embark-hash-sha1
  "2" #'embark-hash-sha256
  "3" #'embark-hash-sha384
  "4" #'embark-hash-sha224
  "5" #'embark-hash-sha512
  "f" #'format-encode-region
  "F" #'format-decode-region
  "b" #'base64-encode-region
  "B" #'base64-decode-region
  "u" #'embark-encode-url
  "U" #'embark-decode-url
  "c" #'epa-encrypt-region
  "C" #'embark-epa-decrypt-region)

(fset 'embark-encode-map embark-encode-map)

(defvar-keymap embark-sort-map
  :doc "Keymap for Embark actions that sort the region"
  "l" #'sort-lines
  "P" #'sort-pages
  "f" #'sort-fields
  "c" #'sort-columns
  "p" #'sort-paragraphs
  "r" #'sort-regexp-fields
  "n" #'sort-numeric-fields)

(fset 'embark-sort-map embark-sort-map)

;; these will have autoloads in Emacs 28
(autoload 'calc-grab-sum-down "calc" nil t)
(autoload 'calc-grab-sum-across "calc" nil t)

;; this has had an autoload cookie since at least Emacs 26
;; but that autoload doesn't seem to work for me
(autoload 'org-table-convert-region "org-table" nil t)

(defvar-keymap embark-region-map
  :doc "Keymap for Embark actions on the active region."
  :parent embark-general-map
  "u" #'upcase-region
  "l" #'downcase-region
  "c" #'capitalize-region
  "|" #'shell-command-on-region
  "e" #'eval-region
  "<" #'embark-eval-replace
  "a" #'align
  "A" #'align-regexp
  "<left>" #'indent-rigidly
  "<right>" #'indent-rigidly
  "TAB" #'indent-region
  "f" #'fill-region
  "p" #'fill-region-as-paragraph
  "$" #'ispell-region
  "=" #'count-words-region
  "F" #'whitespace-cleanup-region
  "t" #'transpose-regions
  "o" #'org-table-convert-region
  ";" #'comment-or-uncomment-region
  "W" #'write-region
  "+" #'append-to-file
  "m" #'apply-macro-to-region-lines
  "n" #'narrow-to-region
  "*" #'calc-grab-region
  ":" #'calc-grab-sum-down
  "_" #'calc-grab-sum-across
  "r" #'reverse-region
  "d" #'delete-duplicate-lines
  "b" #'browse-url-of-region
  "h" #'shr-render-region
  "'" #'expand-region-abbrevs
  "v" #'vc-region-history
  "R" #'repunctuate-sentences
  "s" 'embark-sort-map
  ">" 'embark-encode-map)

(defvar-keymap embark-vc-file-map
  :doc "Keymap for Embark VC file actions."
  "d" #'vc-delete-file
  "r" #'vc-rename-file
  "i" #'vc-ignore)

(fset 'embark-vc-file-map embark-vc-file-map)

(defvar-keymap embark-file-map
  :doc "Keymap for Embark file actions."
  :parent embark-general-map
  "RET" #'find-file
  "f" #'find-file
  "F" #'find-file-literally
  "o" #'find-file-other-window
  "d" #'delete-file
  "D" #'delete-directory
  "r" #'rename-file
  "c" #'copy-file
  "s" #'make-symbolic-link
  "j" #'embark-dired-jump
  "!" #'shell-command
  "&" #'async-shell-command
  "$" #'eshell
  "<" #'insert-file
  "m" #'chmod
  "=" #'ediff-files
  "+" #'make-directory
  "\\" #'embark-recentf-remove
  "I" #'embark-insert-relative-path
  "W" #'embark-save-relative-path
  "x" #'embark-open-externally
  "e" #'eww-open-file
  "l" #'load-file
  "b" #'byte-compile-file
  "R" #'byte-recompile-directory
  "v" 'embark-vc-file-map)

(defvar-keymap embark-kill-ring-map
  :doc "Keymap for `kill-ring' commands."
  :parent embark-general-map
  "\\" #'embark-kill-ring-remove)

(defvar-keymap embark-url-map
  :doc "Keymap for Embark url actions."
  :parent embark-general-map
  "RET" #'browse-url
  "b" #'browse-url
  "d" #'embark-download-url
  "x" #'embark-open-externally
  "e" #'eww)

(defvar-keymap embark-email-map
  :doc "Keymap for Embark email actions."
  :parent embark-general-map
  "RET" #'embark-compose-mail
  "c" #'embark-compose-mail)

(defvar-keymap embark-library-map
  :doc "Keymap for operations on Emacs Lisp libraries."
  :parent embark-general-map
  "RET" #'find-library
  "l" #'load-library
  "f" #'find-library
  "h" #'finder-commentary
  "a" #'apropos-library
  "L" #'locate-library
  "m" #'info-display-manual
  "$" #'eshell)

(defvar-keymap embark-buffer-map
  :doc "Keymap for Embark buffer actions."
  :parent embark-general-map
  "RET" #'switch-to-buffer
  "k" #'kill-buffer
  "b" #'switch-to-buffer
  "o" #'switch-to-buffer-other-window
  "z" #'embark-bury-buffer
  "K" #'embark-kill-buffer-and-window
  "r" #'embark-rename-buffer
  "=" #'ediff-buffers
  "|" #'embark-shell-command-on-buffer
  "<" #'insert-buffer
  "$" #'eshell)

(defvar-keymap embark-tab-map
  :doc "Keymap for actions for tab-bar tabs."
  :parent embark-general-map
  "RET" #'tab-bar-select-tab-by-name
  "s" #'tab-bar-select-tab-by-name
  "r" #'tab-bar-rename-tab-by-name
  "k" #'tab-bar-close-tab-by-name)

(defvar-keymap embark-identifier-map
  :doc "Keymap for Embark identifier actions."
  :parent embark-general-map
  "RET" #'xref-find-definitions
  "h" #'display-local-help
  "H" #'embark-toggle-highlight
  "d" #'xref-find-definitions
  "r" #'xref-find-references
  "a" #'xref-find-apropos
  "s" #'info-lookup-symbol
  "n" #'embark-next-symbol
  "p" #'embark-previous-symbol
  "'" #'expand-abbrev
  "$" #'ispell-word
  "o" #'occur)

(defvar-keymap embark-expression-map
  :doc "Keymap for Embark expression actions."
  :parent embark-general-map
  "RET" #'pp-eval-expression
  "e" #'pp-eval-expression
  "<" #'embark-eval-replace
  "m" #'pp-macroexpand-expression
  "TAB" #'indent-region
  "r" #'raise-sexp
  "t" #'transpose-sexps
  "k" #'kill-region
  "u" #'backward-up-list
  "n" #'forward-list
  "p" #'backward-list)

(defvar-keymap embark-defun-map
  :doc "Keymap for Embark defun actions."
  :parent embark-expression-map
  "RET" #'embark-pp-eval-defun
  "e" #'embark-pp-eval-defun
  "c" #'compile-defun
  "D" #'edebug-defun
  "o" #'checkdoc-defun
  "N" #'narrow-to-defun)

;; Use quoted symbols to avoid byte-compiler warnings.
(defvar-keymap embark-heading-map
  :doc "Keymap for Embark heading actions."
  :parent embark-general-map
  "RET" 'outline-show-subtree
  "TAB" 'outline-cycle ;; New in Emacs 28!
  "C-SPC" 'outline-mark-subtree
  "n" 'outline-next-visible-heading
  "p" 'outline-previous-visible-heading
  "f" 'outline-forward-same-level
  "b" 'outline-backward-same-level
  "^" 'outline-move-subtree-up
  "v" 'outline-move-subtree-down
  "u" 'outline-up-heading
  "+" 'outline-show-subtree
  "-" 'outline-hide-subtree
  ">" 'outline-demote
  "<" 'outline-promote)

(defvar-keymap embark-symbol-map
  :doc "Keymap for Embark symbol actions."
  :parent embark-identifier-map
  "RET" #'embark-find-definition
  "h" #'describe-symbol
  "s" #'embark-info-lookup-symbol
  "d" #'embark-find-definition
  "e" #'pp-eval-expression
  "a" #'apropos
  "\\" #'embark-history-remove)

(defvar-keymap embark-face-map
  :doc "Keymap for Embark face actions."
  :parent embark-symbol-map
  "h" #'describe-face
  "c" #'customize-face
  "+" #'make-face-bold
  "-" #'make-face-unbold
  "/" #'make-face-italic
  "|" #'make-face-unitalic
  "!" #'invert-face
  "f" #'set-face-foreground
  "b" #'set-face-background)

(defvar-keymap embark-variable-map
  :doc "Keymap for Embark variable actions."
  :parent embark-symbol-map
  "=" #'set-variable
  "c" #'customize-set-variable
  "u" #'customize-variable
  "v" #'embark-save-variable-value
  "<" #'embark-insert-variable-value
  "t" #'embark-toggle-variable)

(defvar-keymap embark-function-map
  :doc "Keymap for Embark function actions."
  :parent embark-symbol-map
  "m" #'elp-instrument-function ;; m=measure
  "M" 'elp-restore-function ;; quoted, not autoloaded
  "k" #'debug-on-entry ;; breaKpoint (running out of letters, really)
  "K" #'cancel-debug-on-entry
  "t" #'trace-function
  "T" 'untrace-function) ;; quoted, not autoloaded

(defvar-keymap embark-command-map
  :doc "Keymap for Embark command actions."
  :parent embark-function-map
  "x" #'execute-extended-command
  "I" #'Info-goto-emacs-command-node
  "b" #'where-is
  "g" #'global-set-key
  "l" #'local-set-key)

(defvar-keymap embark-package-map
  :doc "Keymap for Embark package actions."
  :parent embark-general-map
  "RET" #'describe-package
  "h" #'describe-package
  "i" #'package-install
  "I" #'embark-insert
  "d" #'package-delete
  "r" #'package-reinstall
  "u" #'embark-browse-package-url
  "W" #'embark-save-package-url
  "a" #'package-autoremove
  "g" #'package-refresh-contents
  "m" #'elp-instrument-package ;; m=measure
  "M" (if (fboundp 'embark-elp-restore-package)
        'embark-elp-restore-package
        'elp-restore-package))

(defvar-keymap embark-bookmark-map
  :doc "Keymap for Embark bookmark actions."
  :parent embark-general-map
  "RET" #'bookmark-jump
  "s" #'bookmark-set
  "d" #'bookmark-delete
  "r" #'bookmark-rename
  "R" #'bookmark-relocate
  "l" #'bookmark-locate
  "<" #'bookmark-insert
  "j" #'bookmark-jump
  "o" #'bookmark-jump-other-window
  "f" #'bookmark-jump-other-frame
  "a" 'bookmark-show-annotation
  "e" 'bookmark-edit-annotation
  "x" #'embark-bookmark-open-externally
  "$" #'eshell)

(defvar-keymap embark-unicode-name-map
  :doc "Keymap for Embark Unicode name actions."
  :parent embark-general-map
  "RET" #'insert-char
  "I" #'insert-char
  "W" #'embark-save-unicode-character)

(defvar-keymap embark-prose-map
  :doc "Keymap for Embark actions for dealing with prose."
  :parent embark-general-map
  "$" #'ispell-region
  "f" #'fill-region
  "u" #'upcase-region
  "l" #'downcase-region
  "c" #'capitalize-region
  "F" #'whitespace-cleanup-region
  "=" #'count-words-region)

(defvar-keymap embark-sentence-map
  :doc "Keymap for Embark actions for dealing with sentences."
  :parent embark-prose-map
  "t" #'transpose-sentences
  "n" #'forward-sentence
  "p" #'backward-sentence)

(defvar-keymap embark-paragraph-map
  :doc "Keymap for Embark actions for dealing with paragraphs."
  :parent embark-prose-map
  "t" #'transpose-paragraphs
  "n" #'forward-paragraph
  "p" #'backward-paragraph
  "R" #'repunctuate-sentences)

(defvar-keymap embark-flymake-map
  :doc "Keymap for Embark actions on Flymake diagnostics."
  :parent embark-general-map
  "RET" 'flymake-show-buffer-diagnostics
  "n" 'flymake-goto-next-error
  "p" 'flymake-goto-prev-error)

(defvar-keymap embark-become-help-map
  :doc "Keymap for Embark help actions."
  :parent embark-meta-map
  "V" #'apropos-variable
  "U" #'apropos-user-option
  "C" #'apropos-command
  "v" #'describe-variable
  "f" #'describe-function
  "s" #'describe-symbol
  "F" #'describe-face
  "p" #'describe-package
  "i" #'describe-input-method)

(autoload 'recentf-open-files "recentf" nil t)

(defvar-keymap embark-become-file+buffer-map
  :doc "Embark become keymap for files and buffers."
  :parent embark-meta-map
  "f" #'find-file
  "4 f" #'find-file-other-window
  "." #'find-file-at-point
  "p" #'project-find-file
  "r" #'recentf-open-files
  "b" #'switch-to-buffer
  "4 b" #'switch-to-buffer-other-window
  "l" #'locate
  "L" #'find-library
  "v" #'vc-dir)

(defvar-keymap embark-become-shell-command-map
  :doc "Embark become keymap for shell commands."
  :parent embark-meta-map
  "!" #'shell-command
  "&" #'async-shell-command
  "c" #'comint-run
  "t" #'term)

(defvar-keymap embark-become-match-map
  :doc "Embark become keymap for search."
  :parent embark-meta-map
  "o" #'occur
  "k" #'keep-lines
  "f" #'flush-lines
  "c" #'count-matches)

(provide 'embark)

;; Check that embark-consult is installed. If Embark is used in
;; combination with Consult, you should install the integration package,
;; such that features like embark-export from consult-grep work as
;; expected.

(with-eval-after-load 'consult
  (unless (require 'embark-consult nil 'noerror)
    (warn "The package embark-consult should be installed if you use both Embark and Consult")))

(with-eval-after-load 'org
  (require 'embark-org))

;;; embark.el ends here
