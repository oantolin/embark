;;; avy-embark-occur.el --- Use avy to jump to Embark Occur entries  -*- lexical-binding: t; -*-

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

;; This packages provides two commands, `avy-embark-occur-choose' and
;; `avy-embark-occur-act', that use avy to jump to an Embark Occur
;; entry and choose it or act on it, respectively.

;;; Code:

(require 'avy)
(require 'embark)

(defun avy-action-embark-choose (pt)
  "Choose completion at PT."
  (goto-char pt)
  (embark-occur-choose (button-at pt)))

(declare-function embark-act "embark")

(defun avy-action-embark-act (pt)
  "Act on the completion at PT."
  (goto-char pt)
  (embark-act))

(defun avy-embark-occur--choose-window ()
  "Choose a window displaying an Embark Occur buffer to jump to.
The Embark Occur buffer to use is chosen in order of priority as:
- the current buffer,
- a linked Embark Occur buffer,
- some visible Embark Occur buffer."
  (cond
   ((derived-mode-p 'embark-occur-mode) (get-buffer-window))
   (embark-occur-linked-buffer
    (get-buffer-window embark-occur-linked-buffer))
   (t (catch 'return
        (dolist (window (window-list-1))
          (with-selected-window window
            (when (derived-mode-p 'embark-occur-mode)
              (throw 'return window))))))))

(defun avy-embark-occur--jump (action dispatch-alist)
  "Jump to an Embark Occur candidate and perform ACTION.
Other actions are listed in the DISPATCH-LIST.
The Embark Occur buffer to use is chosen in order of priority as:
- the current buffer,
- a linked Embark Occur buffer,
- some visible Embark Occur buffer."
  (let ((wnd (avy-embark-occur--choose-window)))
    (if wnd
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
                               'embark-occur-entry) ; skip annotations
                       (push (cons (point) wnd) btns))
                     (forward-button 1 t))
                   (nreverse btns)))))))
      (user-error "No *Embark Occur* found"))))

(defun avy-embark-occur-choose ()
  "Choose an Embark Occur candidate."
  (interactive)
  (avy-embark-occur--jump #'avy-action-embark-choose
                          '((?x . avy-action-embark-act)
                            (?m . avy-action-goto))))

(defun avy-embark-occur-act ()
  "Act on an Embark Occur candidate."
  (interactive)
  (avy-embark-occur--jump #'avy-action-embark-act
                          '((?x . avy-action-embark-choose)
                            (?m . avy-action-goto))))

(provide 'avy-embark-occur)
;;; avy-embark-occur.el ends here
