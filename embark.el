;;; embark.el --- Conveniently act on minibuffer completions   -*- lexical-binding: t; -*-

;; Copyright (C) 2020, 2021  Omar Antolín Camarena

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: convenience
;; Version: 0.12
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
    (environment-variables . embark-file-map) ; they come up in file completion
    (url . embark-url-map)
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
    embark-target-url-at-point
    embark-target-file-at-point
    embark-target-custom-variable-at-point
    embark-target-identifier-at-point
    embark-target-library-at-point
    embark-target-expression-at-point
    embark-target-defun-at-point)
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
    (project-file . embark--project-file-full-path))
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
  :type '(choice key-sequence (const :tag "None" nil)))

(defface embark-keybinding '((t :inherit success))
  "Face used to display key bindings.
Used by `embark-completing-read-prompter' and `embark-keymap-help'.")

(defface embark-target '((t :inherit highlight))
  "Face used to highlight the target at point during `embark-act'.")

(defcustom embark-indicator #'embark-mixed-indicator
  "Indicator function to use when acting or becoming.
The indicator function is called from both `embark-act' and from
`embark-become' and should display information about this to the
user, such as: which of those two commands is running; a
description of the key bindings that are available for actions or
commands to become; and, in the case of `embark-act', the type
and value of the targets, and whether other targets are available
via `embark-cycle'.  The indicator function is free to display as
much or as little of this information as desired and can use any
Emacs interface elements to do so.

Embark comes with three such indicators:

- `embark-minimal-indicator', which does not display any
  information about keybindings, but does display types and
  values of acton targets,

- `embark-verbose-indicator', which pops up a buffer containing
  detailed information including key bindings and the first line
  of the docstring of the commands they run, and

- `embark-mixed-indicator', which combines the minimal and the
  verbose indicator: the minimal indicator is shown first and the
  verbose popup is shown after `embark-mixed-indicator-delay'
  seconds.

The calling convention for indicator functions is as follows:

When called from `embark-act', the indicator function will be
called with the action keymap, the target (in the form of a cons
of the type and value) and a list of other shadowed targets (each
of which is also a cons).  When called from `embark-become', the
indicator function will be called the keymap of commands to
become, with a fake target of type `embark-become' and whose
value is the minibuffer input, and with nil.  Note, in
particular, that if an indicator function wishes to distinguish
between `embark-act' and `embark-become' it should check whether
the `car' of its target argument is `embark-become'.

The function should return either nil or a function to be called
when the indicator is no longer needed to clean up the display.
For example, if the indicator works by adding overlays, it should
return a function that removes those overlays."
  :type '(choice
          (const :tag "Verbose indicator" embark-verbose-indicator)
          (const :tag "Minimal indicator" embark-minimal-indicator)
          (const :tag "Mixed indicator" embark-mixed-indicator)
          (function :tag "Other")))

(defcustom embark-setup-hooks
  '((async-shell-command embark--shell-prep)
    (shell-command embark--shell-prep)
    (pp-eval-expression embark--eval-prep)
    (package-delete minibuffer-force-complete))
  "Alist associating commands with post-injection setup hooks.
For commands appearing as keys in this alist, run the
corresponding value as a setup hook after injecting the target
into in the minibuffer and before acting on it.  The default setup
hook is specified by the entry with the key t."
  :type '(alist :key-type command :value-type hook))

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
    pp-eval-expression)
  "Allowing editing of target prior to acting for these commands.
This list is used only when `embark-allow-edit-default' is nil."
  :type '(repeat symbol))

(defcustom embark-skip-edit-commands nil
  "Skip editing of target prior to acting for these commands.
This list is used only when `embark-allow-edit-default' is t."
  :type '(repeat symbol))

(defcustom embark-pre-action-hook nil
  "Hook run right before an action is embarked upon."
  :type 'hook)

(defcustom embark-post-action-hook nil
  "Hook run after an embarked upon action concludes."
  :type 'hook)

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

(defvar-local embark--target-bounds nil
  "Bounds of the current target.")

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

(defvar-local embark-collect-annotator nil
  "Annotation function of minibuffer session for this collect.")

(defvar-local embark--collect-live--timer nil
  "Timer scheduled to update Embark Collect Live buffer.")

;;; Core functionality

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
  ;; TODO consider returning a string
  (when (use-region-p) '(region . <region>)))

(autoload 'dired-get-filename "dired")

(defun embark-target-file-at-point ()
  "Target file at point.
This function mostly relies on `ffap-file-at-point', with one exception:
In `dired-mode', it uses `dired-get-filename' instead."
  (if-let (file (and (derived-mode-p 'dired-mode)
                     (dired-get-filename t 'no-error-if-not-filep)))
      (save-excursion
        (end-of-line)
        `(file ,(abbreviate-file-name file)
               ,(save-excursion
                  (re-search-backward " " (line-beginning-position) 'noerror)
                  (1+ (point)))
               . ,(point)))
    (when-let (file (ffap-file-at-point))
      (unless (or (string-match-p "^/https?:/" file)
                  (ffap-el-mode (thing-at-point 'filename)))
        `(file ,(abbreviate-file-name file)
               ;; TODO the boundaries may be wrong, this should be generalized.
               ;; Unfortunately ffap does not make the bounds available.
               . ,(bounds-of-thing-at-point 'filename))))))

(defun embark-target-library-at-point ()
  "Target the Emacs Lisp library name at point."
  (when-let ((filename (thing-at-point 'filename))
             (library (ffap-el-mode filename)))
    `(file ,library . ,(bounds-of-thing-at-point 'filename))))

(defun embark-target-bug-reference-at-point ()
  "Target a bug reference at point."
  (when-let ((ov (seq-find (lambda (ov) (overlay-get ov 'bug-reference-url))
                           (overlays-at (point)))))
    `(url ,(overlay-get ov 'bug-reference-url)
          ,(overlay-start ov) . ,(overlay-end ov))))

(defun embark-target-url-at-point ()
  "Target the URL at point."
  (when-let ((url (ffap-url-at-point)))
    `(url ,url
          ;; TODO the boundaries may be wrong, this should be generalized.
          ;; Unfortunately ffap does not make the bounds available.
          . ,(bounds-of-thing-at-point 'url))))

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
  (when-let*
      ((pt (point))
       (bounds
        (save-excursion
          (catch 'found
            (while
                ;; Looking at opening parenthesis or find last one
                (or (memq (syntax-class (syntax-after pt)) '(4 6 7))
                    (re-search-backward "\\(\\s(\\|\\s/\\|\\s\"\\)"
                                        nil 'noerror))
              (when-let (bounds (bounds-of-thing-at-point 'sexp))
                ;; Point must be located within the sexp at point.
                ;; Otherwise continue the search for the next larger
                ;; outer sexp.
                (when (<= (car bounds) pt (cdr bounds))
                  (throw 'found bounds))))))))
    (unless (eq (car bounds) (car (bounds-of-thing-at-point 'defun)))
      `(expression
        ,(buffer-substring (car bounds) (cdr bounds))
        . ,bounds))))

(defun embark-target-defun-at-point ()
  "Target defun at point."
  (when-let (bounds (bounds-of-thing-at-point 'defun))
    `(defun ,(buffer-substring (car bounds) (cdr bounds)) . ,bounds)))

(defun embark-target-identifier-at-point ()
  "Target identifier at point.

In Emacs Lisp buffers the identifier is promoted to a symbol, for
which more actions are available.  Identifiers are also promoted
to symbols if they are interned Emacs Lisp symbols and found in a
buffer whose major mode does not inherit from `prog-mode'.

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
                 (and (intern-soft name)
                      (not (derived-mode-p 'prog-mode))))
             'symbol
           'identifier)
        ,name
        . ,bounds))))

(defun embark-target-top-minibuffer-completion ()
  "Target the top completion candidate in the minibuffer.
Return the category metadatum as the type of the target."
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
    ;; do not use button-label since it strips text properties
    (when-let (button (button-at (point)))
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
   (let ((map (make-sparse-keymap)))
     (define-key map [13] (embark--default-action type))
     (when cycle
       (define-key map (embark--cycle-key) #'embark-cycle))
     map)
   (symbol-value (or (alist-get type embark-keymap-alist)
                     (alist-get t embark-keymap-alist)))))

(defun embark--truncate-target (target)
  "Truncate TARGET string."
  (unless (stringp target)
    (setq target (format "%s" target)))
  (if-let (pos (string-match-p "\n" target))
      (concat (substring target 0 pos) "…")
    target))

(defun embark-minimal-indicator (_keymap targets)
  "Minimal action indicator.
Display a message in the minibuffer prompt or echo area showing the TARGETS."
  (let* ((act (propertize "Act" 'face 'highlight))
         (target (car targets))
         (shadowed-targets (cdr targets))
         (indicator (cond
                     ((eq (car target) 'embark-become)
                      (propertize "Become" 'face 'highlight))
                     ((and (minibufferp)
                           (not (eq 'embark-keybinding
                                    (completion-metadata-get
                                     (embark--metadata)
                                     'category))))
                      ;; we are in a minibuffer but not from the
                      ;; completing-read prompter, use just "Act"
                      act)
                     (t (format
                         "%s on%s%s '%s'"
                         act
                         (if (car target) (format " %s" (car target)) "")
                         (if shadowed-targets
                             (format (propertize "(%s)" 'face 'shadow)
                                     (string-join
                                      (mapcar (lambda (x)
                                                (symbol-name (car x)))
                                              shadowed-targets)
                                      ", "))
                           "")
                         (embark--truncate-target (cdr target)))))))
    (if (minibufferp)
        (let ((indicator-overlay
               (make-overlay (point-min) (point-min) (current-buffer) t t)))
          (overlay-put indicator-overlay
                       'before-string (concat indicator
                                              (if (<= (length indicator)
                                                      (* 0.4 (frame-width)))
                                                  " "
                                                "\n")))
          (lambda (_) (delete-overlay indicator-overlay)))
      (message "%s" indicator)
      nil)))

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
          (read-key-sequence nil nil nil t 'cmd-loop))
      (when timer
        (cancel-timer timer)))))

(defun embark-keymap-prompter (keymap update)
  "Let the user choose an action using the bindings in KEYMAP.
Besides the bindings in KEYMAP, the user is free to use all their
key bindings and even \\[execute-extended-command] to select a command.
UPDATE is the indicator update function."
  (let* ((key (let ((overriding-terminal-local-map keymap))
                (embark--read-key-sequence update)))
         (cmd (let ((overriding-terminal-local-map keymap))
                (key-binding key))))
    (pcase cmd
      ((or 'minibuffer-keyboard-quit 'abort-recursive-edit)
       nil)
      ('self-insert-command
       (minibuffer-message "Not an action")
       (embark-keymap-prompter keymap update))
      ((or 'universal-argument 'negative-argument 'digit-argument
           'scroll-other-window 'scroll-other-window-down)
       (let ((last-command-event (aref key 0))
             (minibuffer-scroll-window
              (or (get-buffer-window " *Embark Actions*" 'visible)
                  minibuffer-scroll-window)))
         (ignore-errors (command-execute cmd)))
       (embark-keymap-prompter keymap update))
      ((or 'scroll-bar-toolkit-scroll 'mwheel-scroll)
       (funcall cmd (aref key (1- (length key))))
       (embark-keymap-prompter keymap update))
      ('execute-extended-command
       (intern-soft (read-extended-command)))
      ('embark-keymap-help
       (embark-completing-read-prompter keymap nil))
      (_ cmd))))

(defun embark--command-name (cmd)
  "Return an appropriate name for CMD.
If CMD is a symbol, use its symbol name; for lambdas, use the
first line of the documentation string; otherwise use the word
'unnamed'."
  (concat ; fresh copy, so we can freely add text properties
   (cond
    ((stringp (car-safe cmd)) (car cmd))
    ((symbolp cmd) (symbol-name cmd))
    ((keymapp cmd) "<keymap>")
    ((when-let (doc (and (functionp cmd) (documentation cmd)))
       (save-match-data
         (when (string-match "^\\(.*\\)$" doc)
           (match-string 1 doc)))))
    (t "<unnamed>"))))

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
                           (eq cmd #'embark-keymap-help))
                   collect (list name
                                 (cond
                                  ((keymapp cmd) 'keymap)
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
                   for formatted =
                   (propertize
                    (concat (propertize desc 'face 'embark-keybinding)
                            (make-string (- width (length desc) -1) ? )
                            name)
                    'embark-command cmd)
                   when (equal key [13])
                   do (setq def formatted)
                   collect (cons formatted item))))
    (cons candidates def)))

(defun embark-completing-read-prompter (keymap _update &optional no-default)
  "Prompt via completion for a command bound in KEYMAP.
If NO-DEFAULT is t, no default value is passed to `completing-read'."
  (let* ((candidates+def (embark--formatted-bindings keymap))
         (candidates (car candidates+def))
         (def (and (not no-default) (cdr candidates+def)))
         (choice
          (catch 'choice
            (minibuffer-with-setup-hook
                (lambda ()
                  (when embark-keymap-prompter-key
                    (use-local-map
                     (make-composed-keymap
                      (let ((map (make-sparse-keymap))
                            (cycle (embark--cycle-key)))
                        ;; Rebind `embark-cycle' in order allow cycling
                        ;; from the `completing-read' prompter. Additionally
                        ;; `embark-cycle' can be selected via
                        ;; `completing-read'. The downside is that this breaks
                        ;; recursively acting on the candidates of type
                        ;; embark-keybinding in the `completing-read' prompter.
                        (define-key map cycle
                          (if (lookup-key keymap cycle)
                              (lambda ()
                                (interactive)
                                (throw 'choice 'embark-cycle))
                            (lambda ()
                              (interactive)
                              (minibuffer-message "Only a single target"))))
                        (define-key map embark-keymap-prompter-key
                          (lambda ()
                            (interactive)
                            (let*
                                ((desc
                                  (let ((overriding-terminal-local-map keymap))
                                    (key-description
                                     (read-key-sequence "Key:"))))
                                 (cmd
                                  (cl-loop
                                   for (_s _n cmd _k desc1) in candidates
                                   when (equal desc desc1) return cmd)))
                              (if (null cmd)
                                  (user-error "Unknown key")
                                (throw 'choice cmd)))))
                        map)
                      (current-local-map)))))
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
       (setq last-command-event (seq-elt key (1- (length key))))
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

(defcustom embark-verbose-indicator-excluded-commands nil
  "Commands not displayed by `embark-verbose-indicator'."
  :type '(choice
          (const :tag "Exclude nothing" nil)
          (const :tag "Exclude Embark general actions"
                 ("\\`embark-collect-" embark-cycle embark-export
                  embark-keymap-help embark-become embark-isearch))
          (repeat :tag "Other" (choice regexp symbol))))

(defcustom embark-verbose-indicator-buffer-sections
  `(target "\n" shadowed-targets " " cycle "\n" bindings)
  "List of sections to display in the verbose indicator buffer, in order.
You can use either a symbol designating a concrete section, a string literal
or a function that will take the list of targets, bindings and the cycle key
and should return a string or list of strings to insert."
  :type '(repeat
          (choice (const :tag "Current target name" target)
                  (const :tag "List of other shadowed targets" shadowed-targets)
                  (const :tag "Key bindings" bindings)
                  (const :tag "Cycle indicator" cycle)
                  (string :tag "Literal string")
                  (function :tag "Custom function"))))

(defcustom embark-verbose-indicator-nested t
  "Whether the verbose indicator should use nested keymap navigation."
  :type 'boolean)

(defun embark--verbose-indicator-excluded-p (cmd)
  "Return non-nil if CMD is excluded from the verbose indicator."
  (seq-find (lambda (x)
              (if (symbolp x)
                  (eq cmd x)
                (string-match-p x (symbol-name cmd))))
            embark-verbose-indicator-excluded-commands))

(cl-defun embark--verbose-indicator-section-target
    (&key target &allow-other-keys)
  "Format the TARGET section for the indicator buffer."
  (let* ((kind (car target))
         (result (if (eq kind 'embark-become)
                     (concat (propertize "Become" 'face 'highlight))
                   (format "%s on%s '%s'"
                           (propertize "Act" 'face 'highlight)
                           (if kind (format " %s" kind) "")
                           (embark--truncate-target (cdr target))))))
    (add-face-text-property 0 (length result)
                            'embark-verbose-indicator-title
                            'append
                            result)
    result))

(cl-defun embark--verbose-indicator-section-cycle (&key cycle &allow-other-keys)
  "Format the CYCLE key section for the indicator buffer."
  (when cycle
    (propertize (format "(%s to cycle)\n" cycle)
                'face 'embark-verbose-indicator-shadowed)))

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
                (doc (ignore-errors
                       (propertize
                        (car (split-string (documentation cmd) "\n"))
                        'face 'embark-verbose-indicator-documentation))))
            (push (format "%s%s\n" keys (or doc "")) result)))))))

(defun embark--verbose-indicator-update (keymap target shadowed-targets)
  "Update verbose indicator buffer.
The arguments are the new KEYMAP, TARGET and SHADOWED-TARGETS."
  (with-current-buffer (get-buffer-create " *Embark Actions*")
    (let* ((inhibit-read-only t)
           (bindings
            (embark--formatted-bindings keymap embark-verbose-indicator-nested))
           (bindings (car bindings))
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
                (let ((prefixed
                       (intern
                        (format "embark--verbose-indicator-section-%s" section))))
                  (cond
                   ((fboundp prefixed) prefixed)
                   ((fboundp section) section)
                   (t (error "Undefined verbose indicator section `%s'" section))))
                :target target :shadowed-targets shadowed-targets
                :bindings bindings :cycle cycle)
               ""))))
      (goto-char (point-min)))))

(defun embark-verbose-indicator (keymap targets)
  "Indicator that displays a list of available key bindings.
KEYMAP is the action (or become) keymap.
TARGETS is the list of targets."
  (let ((target (car targets))
        (shadowed-targets
         (and (cdr targets)
              (mapcar (lambda (x) (symbol-name (car x))) (cdr targets)))))
    (embark--verbose-indicator-update keymap target shadowed-targets)
    (let ((display-buffer-alist
           `(,@display-buffer-alist
             (,(regexp-quote " *Embark Actions*")
              ,@embark-verbose-indicator-display-action))))
      (display-buffer " *Embark Actions*"))
    (lambda (prefix)
      (if prefix
          (when embark-verbose-indicator-nested
            (embark--verbose-indicator-update (lookup-key keymap prefix)
                                              target shadowed-targets))
        (when-let ((indicator-window
                    (get-buffer-window " *Embark Actions*" 'visible)))
          (quit-window 'kill-buffer indicator-window))))))

(defcustom embark-mixed-indicator-delay 0.5
  "Time in seconds after which the verbose indicator is shown.
The mixed indicator starts by showing the minimal indicator and
after this delay shows the verbose indicator."
  :type '(choice (const :tag "No delay" 0)
                 (number :tag "Delay in seconds")))

(defun embark-mixed-indicator (keymap targets)
  "Mixed indicator showing KEYMAP and TARGETS.
The indicator shows the `embark-minimal-indicator' by default.
After `embark-mixed-indicator-delay' seconds, the
`embark-verbose-indicator' is shown.  This which-key-like approach
ensures that Embark stays out of the way for quick actions.  The
helpful keybinding reminder still pops up automatically without
further user intervention."
  (let ((vtimer) (vindicator) (mindicator))
    (if (> embark-mixed-indicator-delay 0)
        (setq vtimer
              (run-at-time
               embark-mixed-indicator-delay nil
               (lambda ()
                 (setq vindicator (embark-verbose-indicator keymap targets)))))
      (setq vindicator (embark-verbose-indicator keymap targets)))
    (setq mindicator (embark-minimal-indicator keymap targets))
    (lambda (prefix)
      (when (and (not prefix) vtimer)
        (cancel-timer vtimer))
      (when (functionp vindicator)
        (funcall vindicator prefix))
      (when (functionp mindicator)
        (funcall mindicator prefix)))))

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
                    (key-binding prefix)
                  (make-composed-keymap (current-active-maps t)))))
    (unless (keymapp keymap)
      (user-error "No key bindings found"))
    (when-let (command (embark-completing-read-prompter keymap nil 'no-default))
      (call-interactively command))))

(defun embark--prompt (keymap targets)
  "Call the prompter with KEYMAP.
The TARGETS are displayed for actions outside the minibuffer."
  (let ((indicator (funcall embark-indicator keymap targets)))
    (unwind-protect
        (condition-case nil
            (minibuffer-with-setup-hook
                (lambda ()
                  ;; if the prompter opens its own minibuffer, show
                  ;; the indicator there too
                  (let ((inner-indicator
                         (funcall embark-indicator keymap targets)))
                    (when (functionp inner-indicator)
                      (add-hook 'minibuffer-exit-hook
                                (lambda () (funcall inner-indicator nil))
                                nil 'local))))
              (let ((enable-recursive-minibuffers t))
                (funcall embark-prompter keymap indicator)))
          (quit nil))
      (when (functionp indicator)
        (funcall indicator nil)))))

(defun embark--quit-and-run (fn &rest args)
  "Quit the minibuffer and then call FN with ARGS."
  (run-at-time 0 nil #'set 'ring-bell-function ring-bell-function)
  (apply #'run-at-time 0 nil fn args)
  (setq ring-bell-function #'ignore)
  (abort-recursive-edit))

(defvar embark--setup-hook nil
  "Temporary variable used as setup hook.")

(defun embark--act (action target bounds &optional quit)
  "Perform ACTION injecting the TARGET.
If called from a minibuffer with non-nil QUIT, quit the
minibuffer before executing the action.  BOUNDS are the bounds of
the target at point."
  (if (memq action '(embark-become        ; these actions should not
                     embark-collect-live  ; run in the target window
                     embark-collect-snapshot
                     embark-export))
      (command-execute action)
    (let* ((command embark--command)
           (prefix prefix-arg)
           (action-window (embark--target-window t))
           (setup-hook (or (alist-get action embark-setup-hooks)
                           (alist-get t embark-setup-hooks)))
           (allow-edit (if embark-allow-edit-default
                           (not (memq action embark-skip-edit-commands))
                         (memq action embark-allow-edit-commands)))
           (inject
            ;; TODO consider using strings for regions,
            ;; remove special casing?
            (if (not (stringp target))  ; for region actions
                #'ignore
              (lambda ()
                (delete-minibuffer-contents)
                (insert (substring-no-properties target))
                (let ((embark--setup-hook setup-hook))
                  (run-hooks 'embark--setup-hook))
                (unless allow-edit
                  (if (memq 'ivy--queue-exhibit post-command-hook)
                      ;; Ivy has special needs: (1) for file names
                      ;; ivy-immediate-done is not equivalent to
                      ;; exit-minibuffer, (2) it needs a chance to run
                      ;; its post command hook first, so use depth 10
                      (add-hook 'post-command-hook 'ivy-immediate-done 10 t)
                    (add-hook 'post-command-hook #'exit-minibuffer nil t))))))
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
                            (run-hooks 'embark-pre-action-hook)
                            (let ((enable-recursive-minibuffers t)
                                  (embark--command command)
                                  (embark--target-bounds bounds)
                                  (this-command action)
                                  ;; the next two avoid mouse dialogs
                                  (use-dialog-box nil)
                                  (last-nonmenu-event 13))
                              (setq prefix-arg prefix)
                              (command-execute action))
                            (setq final-window (selected-window)))
                        (run-hooks 'embark-post-action-hook)
                        (when dedicate (set-window-dedicated-p dedicate nil)))
                      (unless (eq final-window action-window)
                        (select-window final-window)))))
              (lambda ()
                (with-selected-window action-window
                  (run-hooks 'embark-pre-action-hook)
                  (unwind-protect (funcall action target)
                    (run-hooks 'embark-post-action-hook)))))))
      (if (not (and quit (minibufferp)))
          (funcall run-action)
        (embark--quit-and-run run-action)))))

(defun embark--refine-symbol-type (_type target)
  "Refine symbol TARGET to command or variable if possible."
  (cons (or (when-let ((symbol (intern-soft target)))
              (cond
               ((keywordp symbol) 'symbol) ; keywords are bound to themselves!
               ((commandp symbol) 'command)
               ((boundp symbol) 'variable)
               ;; Prefer variables over functions for backward compatibility.
               ;; Command > variable > function > symbol seems like a
               ;; reasonable order with decreasing usefulness of the actions.
               ((fboundp symbol) 'function)
               ((facep symbol) 'face)))
            'symbol)
        target))

(defun embark--keybinding-command (_type target)
  "Treat an `embark-keybinding' TARGET as a command."
  (when-let ((cmd (get-text-property 0 'embark-command target)))
    (cons 'command cmd)))

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

(defun embark--targets ()
  "Retrieve current target.

An initial guess at the current target and its type is determined
by running the functions in `embark-target-finders'.  Each
function should either return nil, a pair of a type symbol and
target string or a triple of a type symbol, target string and
target bounds.

In the minibuffer only the first target finder returning non-nil
is taken into account.  When finding targets at point in other
buffers, each target finder function is executed.

For each target, the type is then looked up as a key in the
variable `embark-transformer-alist'.  If there is a transformer
for the type, it is called with the type and target, and must
return a `cons' of the transformed type and transformed target.

The return value of `embark--targets' is a list.  Each list
element has the form (target original-target . bounds), where
target and original-target are (type . string) pairs and bounds
is the optional bounds of the target at point for highlighting."
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
                (orig (cons type target)))
           (push (if-let (transformer (alist-get type embark-transformer-alist))
                     `(,(funcall transformer type target) ,orig . ,bounds)
                   `(,orig ,orig . ,bounds))
                 targets)
           (minibufferp)))))
    (nreverse targets)))

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

;;;###autoload
(defun embark-act (&optional arg)
  "Prompt the user for an action and perform it.
The targets of the action are chosen by `embark-target-finders'.
By default, if called from a minibuffer the target is the top
completion candidate. When called from a non-minibuffer buffer
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
         (n (length targets))
         (skip 1))
    (cl-flet ((rotate (k)
                (setq k (mod k n)
                      targets (append (seq-drop targets k)
                                      (seq-take targets k)))))
      (when (and arg (not (minibufferp)))
        (rotate (prefix-numeric-value arg)))
      (while
          (pcase-let* ((`((,type . ,target)
                          (,_otype . ,otarget)
                          . ,bounds)
                         (car targets))
                       (action (or (embark--highlight-target
                                    bounds
                                    #'embark--prompt
                                    (embark--action-keymap
                                     type (cdr targets))
                                    (mapcar #'car targets))
                                   (user-error "Canceled")))
                       (default-action (embark--default-action type)))
            (setq skip
                  (catch 'embark--cycle
                    (embark--act action
                                 (if (and (eq action default-action)
                                          (eq action embark--command))
                                     otarget
                                   target)
                                 bounds
                                 (if embark-quit-after-action (not arg) arg))
                    nil)))
        (rotate skip)))))

(defun embark--highlight-target (bounds &rest fun)
  "Highlight target at BOUNDS and call FUN."
  (if bounds
      (let ((ov (make-overlay (car bounds) (cdr bounds) nil)))
        (overlay-put ov 'face 'embark-target)
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'priority 100) ;; override bug reference
        (unwind-protect
            (apply fun)
          (delete-overlay ov)))
    (apply fun)))

(defun embark-cycle (arg)
  "Cycle over the next ARG targets at point.
If ARG is negative, cycle backwards."
  (interactive "p")
  (throw 'embark--cycle arg))

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
      (pcase-let* ((`((,type . ,target)
                      (,_otype . ,otarget)
                      . ,bounds)
                    (or (nth
                         (if (or (null arg) (minibufferp))
                             0
                           (mod (prefix-numeric-value arg) (length targets)))
                         targets)))
                   (default-action (embark--default-action type)))
        (embark--act default-action
                     (if (eq default-action embark--command)
                         otarget
                       target)
                     bounds
                     (if embark-quit-after-action (not arg) arg)))
    (user-error "No target found")))

(define-obsolete-function-alias
  'embark-default-action
  'embark-dwim
  "0.11")

(defun embark--become-keymap ()
  "Return keymap of commands to become for current command."
  (make-composed-keymap
   (cl-loop for keymap-name in embark-become-keymaps
            for keymap = (symbol-value keymap-name)
            when (where-is-internal embark--command (list keymap))
            collect keymap)))

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
  (when (minibufferp)
    (let* ((target (if full
                       (minibuffer-contents)
                     (pcase-let ((`(,beg . ,end) (embark--boundaries)))
                       (substring (minibuffer-contents) beg
                                  (+ end (embark--minibuffer-point))))))
           (become (embark--prompt (embark--become-keymap)
                                   ;; Pass a fake target list here
                                   `((embark-become . ,target)))))
      (if (null become)
          (user-error "Canceled")
        (embark--quit-and-run
         (lambda ()
           (minibuffer-with-setup-hook
               (lambda ()
                 (delete-minibuffer-contents)
                 (insert target))
             (let ((use-dialog-box nil)
                   (this-command become))
               (command-execute become)))))))))

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
    embark-embark-collect-candidates)
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
  (let ((name (intern (format "embark-action--%s"
                              (embark--command-name action)))))
    (fset name (lambda ()
                 (interactive)
                 (pcase (car (embark--targets))
                   (`((,_type . ,target) (,_otype . ,_otarget) . ,bounds)
                    (embark--act action target bounds)))))
    (put name 'function-documentation (documentation action))
    name))

(defun embark--all-bindings (keymap &optional nested)
  "Return an alist of all bindings in KEYMAP.
If NESTED is non-nil subkeymaps are not flattened."
  (let (bindings)
    (map-keymap
     (lambda (key def)
       (cond
        ((and (not nested) (keymapp def))
         (dolist (bind (embark--all-bindings def))
           (push (cons (vconcat (vector key) (car bind))
                       (cdr bind))
                 bindings)))
        (def (push (cons (vector key) def) bindings))))
     (keymap-canonicalize keymap))
    (nreverse bindings)))

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
         (text (buffer-substring start end)) ; keep properties
         (bounds (cons start end)))
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
      (embark--act (embark--default-action embark--type) text bounds))))

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

(defun embark-collect--max-width ()
  "Maximum width of any Embark Collect candidate."
  (or (cl-loop for cand in embark-collect-candidates
               maximize (embark--display-width cand))
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
            (overlay-put (make-overlay pt (point))
                         'face 'embark-collect-zebra-highlight)))))))

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
  (let* ((width (min (1+ (embark-collect--max-width))
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

(defun embark-collect--annotator (type)
  "Get annotator for current buffer's candidates.
For non-minibuffers, assume candidates are of given TYPE."
  (if (minibufferp)
      (or
       (completion-metadata-get (embark--metadata) 'annotation-function)
       (plist-get completion-extra-properties :annotation-function))
    ;; otherwise fake some metadata for Marginalia users's benefit
    (completion-metadata-get `((category . ,type)) 'annotation-function)))

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
            embark-collect-annotator (or
                                      ;; new annotator? (marginalia-cycle)
                                      (with-current-buffer embark-collect-from
                                        (embark-collect--annotator type))
                                      embark-collect-annotator))))
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
       (annotator (embark-collect--annotator type)))
    (if (and (null candidates) (eq kind :snapshot))
        (user-error "No candidates to collect")
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
              embark-collect-annotator annotator)

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
                  (run-at-time 0 nil #'pop-to-buffer buffer))))
             (:snapshot
              (lambda ()
                (when (buffer-live-p buffer)
                  (run-at-time 0 nil #'pop-to-buffer buffer)))))
           nil t)
          (setq minibuffer-scroll-window window))

        window))))

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
  (when (minibufferp) (embark--quit-and-run #'message nil)))

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
  (pcase-let ((`(,type . ,candidates)
               (run-hook-with-args-until-success 'embark-candidate-collectors)))
    (if (null candidates)
        (user-error "No candidates for export")
      (let ((exporter (or (alist-get type embark-exporters-alist)
                          (alist-get t embark-exporters-alist)))
            (transformer (alist-get type embark-transformer-alist)))

        ;; check to see if all candidates transform to same type
        (when transformer
          (pcase-let* ((`(,new-type . ,first-cand)
                        (funcall transformer type (car candidates))))
            (unless (eq type new-type)
              (when-let ((new-exporter
                          (alist-get new-type embark-exporters-alist))
                         (new-candidates (list first-cand)))
                (when (cl-every
                       (lambda (cand)
                         (pcase-let ((`(,t-type . ,t-cand)
                                      (funcall transformer type cand)))
                           (when (eq t-type new-type)
                             (push t-cand new-candidates)
                             t)))
                       (cdr candidates))
                  (setq type new-type
                        exporter new-exporter
                        candidates (nreverse new-candidates)))))))

        (if (eq exporter 'embark-collect-snapshot)
            (embark-collect-snapshot)
          (let ((dir (embark--default-directory))
                (after embark-after-export-hook))
            (embark--quit-and-run
             (lambda ()
               (let ((default-directory dir) ; dired needs this info
                     (embark-after-export-hook after))
                 (funcall exporter candidates)
                 (run-hooks 'embark-after-export-hook))))))))))

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

(defun embark-export-customize-face (faces)
  "Create a customization buffer listing FACES."
  (embark--export-customize faces "Faces" 'custom-face #'facep))

(defun embark-export-customize-variable (variables)
  "Create a customization buffer listing VARIABLES."
  (embark--export-customize variables "Variables" 'custom-variable #'boundp))

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

;;; Integration with external completion UIs

;; vertico

(declare-function vertico--candidate "ext:vertico")
(defvar vertico--input)
(defvar vertico--candidates)

(defun embark--vertico-selected ()
  "Target the currently selected item in Vertico.
Return the category metadatum as the type of the target."
  (when vertico--input
    (cons (completion-metadata-get (embark--metadata) 'category)
          (vertico--candidate))))

(defun embark--vertico-candidates ()
  "Collect the current Vertico candidates.
Return the category metadatum as the type of the candidates."
  (when vertico--input
    (cons (completion-metadata-get (embark--metadata) 'category)
          vertico--candidates)))

(with-eval-after-load 'vertico
  (add-hook 'embark-target-finders #'embark--vertico-selected)
  (add-hook 'embark-candidate-collectors #'embark--vertico-candidates))

;; selectrum

(declare-function selectrum--get-meta "ext:selectrum")
(declare-function selectrum-get-current-candidate "ext:selectrum")
(declare-function selectrum-get-current-candidates "ext:selectrum")
(defvar selectrum-is-active)

(defun embark--selectrum-selected ()
  "Target the currently selected item in Selectrum.
Return the category metadatum as the type of the target."
  (when selectrum-is-active
    (cons (selectrum--get-meta 'category)
	  (selectrum-get-current-candidate))))

(defun embark--selectrum-candidates ()
  "Collect the current Selectrum candidates.
Return the category metadatum as the type of the candidates."
  (when selectrum-is-active
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

(defalias 'embark-execute-command
  ;; this one is kind of embarrassing: embark-keymap-prompter gives
  ;; execute-extended-command special treatment, so we need a command
  ;; just like it... but with a different name!
  #'execute-extended-command)

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
(autoload 'dired-jump "dired-x")

(defun embark-dired-jump (file &optional other-window)
  "Open dired buffer in directory containg FILE and move to its line.
When called with a prefix argument OTHER-WINDOW, open dired in other window."
  (interactive "fJump to Dired file: \nP")
  (dired-jump other-window file))

(autoload 'xref-push-marker-stack "xref")

(defun embark-find-definition (symbol)
  "Find definition of SYMBOL."
  (interactive "SSymbol: ")
  (xref-push-marker-stack)
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
    (user-error "No homepage found for `%s'" pkg)))

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

(defmacro embark--sexp-command (cmd)
  "Derive from CMD a command acting on the sexp before or after point.
Given a CMD that acts on the sexp starting at point, this macro
defines a command called embark-CMD which works with point either
before or after the sexp (those are the two locations at which
`embark-target-expression-at-point' detects a sexp)."
  `(defun ,(intern (format "embark-%s" cmd)) ()
     ,(format "Run `%s' on the sexp at or before point." cmd)
     (interactive)
     (goto-char (car embark--target-bounds))
     (,cmd)))

(embark--sexp-command indent-pp-sexp)
(embark--sexp-command kill-sexp)
(embark--sexp-command raise-sexp)
(embark--sexp-command mark-sexp)

;;; Setup hooks for actions

(defun embark--shell-prep ()
  "Prepare target for use as argument for a shell command.
This quotes the spaces, inserts an extra space at the beginning
and leaves the point to the left of it."
  (let ((contents (minibuffer-contents)))
    (delete-minibuffer-contents)
    (insert " " (shell-quote-wildcard-pattern contents))
    (goto-char (minibuffer-prompt-end))))

(defun embark--eval-prep ()
  "If target is: a variable, skip edit; a function, wrap in parens."
  (if (not (fboundp (intern (minibuffer-contents))))
      (add-hook 'post-command-hook #'exit-minibuffer nil t)
    (goto-char (minibuffer-prompt-end))
    (insert "(")
    (goto-char (point-max))
    (insert ")")
    (backward-char)))

;;; keymaps

(embark-define-keymap embark-meta-map
  "Keymap for non-action Embark functions."
  :parent nil
  ("C-h" embark-keymap-help))

(embark-define-keymap embark-general-map
  "Keymap for Embark general actions."
  :parent embark-meta-map
  ("i" embark-insert)
  ("w" kill-new)
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
  ("a" align)
  ("A" align-regexp)
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
  ("W" append-to-file)
  ("m" apply-macro-to-region-lines)
  ("n" narrow-to-region))

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
  ("m" chmod)
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
  ("RET" browse-url)
  ("b" browse-url)
  ("e" eww))

(embark-define-keymap embark-buffer-map
  "Keymap for Embark buffer actions."
  ("RET" switch-to-buffer)
  ("k" kill-buffer)
  ("I" insert-buffer)
  ("b" switch-to-buffer)
  ("o" switch-to-buffer-other-window)
  ("z" embark-bury-buffer)
  ("q" embark-kill-buffer-and-window)
  ("r" embark-rename-buffer)
  ("=" ediff-buffers)
  ("|" embark-shell-command-on-buffer))

(embark-define-keymap embark-identifier-map
  "Keymap for Embark identifier actions."
  ("RET" xref-find-definitions)
  ("h" display-local-help)
  ("H" embark-toggle-highlight)
  ("d" xref-find-definitions)
  ("r" xref-find-references)
  ("a" xref-find-apropos))

(embark-define-keymap embark-expression-map
  "Keymap for Embark expression actions."
  ("RET" pp-eval-expression)
  ("e" pp-eval-expression)
  ("m" pp-macroexpand-expression)
  ("TAB" embark-indent-pp-sexp)
  ("r" embark-raise-sexp)
  ("k" embark-kill-sexp)
  ("SPC" embark-mark-sexp))

(embark-define-keymap embark-defun-map
  "Keymap for Embark defun actions."
  :parent embark-expression-map
  ("RET" eval-defun)
  ("e" eval-defun)
  ("c" compile-defun)
  ("l" elint-defun)
  ("d" edebug-defun)
  ("o" checkdoc-defun)
  ("n" narrow-to-defun)
  ("SPC" mark-defun))

(embark-define-keymap embark-symbol-map
  "Keymap for Embark symbol actions."
  ("RET" embark-find-definition)
  ("h" describe-symbol)
  ("H" embark-toggle-highlight)
  ("s" embark-info-lookup-symbol)
  ("d" embark-find-definition)
  ("r" xref-find-references)
  ("b" where-is)
  ("e" pp-eval-expression)
  ("a" apropos))

(embark-define-keymap embark-command-map
  "Keymap for Embark command actions."
  :parent embark-symbol-map
  ("x" embark-execute-command)
  ("I" Info-goto-emacs-command-node)
  ("g" global-set-key)
  ("l" local-set-key))

(embark-define-keymap embark-face-map
  "Keymap for Embark face actions."
  :parent embark-symbol-map
  ("c" customize-face)
  ("b" make-face-bold)
  ("B" make-face-unbold)
  ("i" make-face-italic)
  ("I" make-face-unitalic)
  ("!" invert-face)
  ("gf" set-face-foreground)
  ("gb" set-face-background))

(embark-define-keymap embark-variable-map
  "Keymap for Embark variable actions."
  :parent embark-symbol-map
  ("=" set-variable)
  ("c" customize-set-variable)
  ("u" customize-variable))

(declare-function untrace-function "trace")

(embark-define-keymap embark-function-map
  "Keymap for Embark function actions."
  :parent embark-symbol-map
  ("t" trace-function)
  ("T" untrace-function))

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
  ("RET" bookmark-jump)
  ("s" bookmark-set)
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
  ("RET" insert-char)
  ("I" insert-char)
  ("W" embark-save-unicode-character))

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

(autoload 'recentf-open-files "recentf")

(embark-define-keymap embark-become-file+buffer-map
  "Embark become keymap for files and buffers."
  :parent embark-meta-map
  ("f" find-file)
  ("." find-file-at-point)
  ("p" project-find-file)
  ("r" recentf-open-files)
  ("b" switch-to-buffer)
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
