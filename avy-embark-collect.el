;;; avy-embark-collect.el --- Use avy to jump to Embark Collect entries  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Omar Antolín Camarena

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: convenience
;; Version: 0.3
;; Homepage: https://github.com/oantolin/embark
;; Package-Requires: ((emacs "25.1") (embark "0.9") (avy "0.5"))

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

;; This packages provides two commands, `avy-embark-collect-choose' and
;; `avy-embark-collect-act', that use avy to jump to an Embark Collect
;; entry and choose it or act on it, respectively.

;;; Code:

(require 'avy)
(require 'embark)
(eval-when-compile (require 'subr-x))

(defvar avy-embark-collect--initial-window nil
  "Window that was selected before jumping.")

(defun avy-embark-collect--choose (pt)
  "Choose completion at PT."
  (goto-char pt)
  (embark-collect-choose (button-at pt)))

(defun avy-embark-collect--act (pt)
  "Act on the completion at PT."
  (goto-char pt)
  (add-hook 'embark-post-action-hook
            (lambda () (select-window avy-embark-collect--initial-window))
            nil t)
  (embark-act))

(defun avy-embark-collect--choose-window ()
  "Choose a window displaying an Embark Collect buffer to jump to.
The Embark Collect buffer to use is chosen in order of priority as:
- the current buffer,
- a linked Embark Collect buffer,
- some visible Embark Collect buffer."
  (cond
   ((derived-mode-p 'embark-collect-mode) (selected-window))
   (embark-collect-linked-buffer
    (get-buffer-window embark-collect-linked-buffer))
   (t (catch 'return
        (dolist (window (window-list-1))
          (with-selected-window window
            (when (derived-mode-p 'embark-collect-mode)
              (throw 'return window))))))))

(defun avy-embark-collect--jump (action dispatch-alist)
  "Jump to an Embark Collect candidate and perform ACTION.
Other actions are listed in the DISPATCH-ALIST.
The Embark Collect buffer to use is chosen in order of priority as:
- the current buffer,
- a linked Embark Collect buffer,
- some visible Embark Collect buffer."
  (setq avy-embark-collect--initial-window (selected-window))
  (if-let ((wnd (avy-embark-collect--choose-window)))
      (with-current-buffer (window-buffer wnd)
        (avy-with avy-completion
          (let ((avy-action action)
                (avy-dispatch-alist dispatch-alist))
            (avy-process
             (save-excursion
               (goto-char (point-min))
               (let ((btns `((,(point) . ,wnd))))
                 (forward-button 1 t)
                 (while (not (bobp))
                   (when (eq (button-type (button-at (point)))
                             'embark-collect-entry) ; skip annotations
                     (push (cons (point) wnd) btns))
                   (forward-button 1 t))
                 (nreverse btns)))))))
    (user-error "No *Embark Collect* found")))

(defun avy-embark-collect-choose ()
  "Choose an Embark Collect candidate."
  (interactive)
  (avy-embark-collect--jump #'avy-embark-collect-choose
                            '((?x . avy-embark-collect--act)
                              (?m . avy-action-goto))))

(defun avy-embark-collect-act ()
  "Act on an Embark Collect candidate."
  (interactive)
  (avy-embark-collect--jump #'avy-embark-collect-act
                            '((?x . avy-embark-collect--choose)
                              (?m . avy-action-goto))))

(provide 'avy-embark-collect)
;;; avy-embark-collect.el ends here
