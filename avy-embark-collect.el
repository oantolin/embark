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

(defun avy-embark-collect--candidates ()
  "Collect all visible Embark collect candidates."
  (let (candidates)
    (avy-dowindows current-prefix-arg   ; avy-dowindows binds wnd! 🤯
      (when (derived-mode-p 'embark-collect-mode)
        (dolist (pair (avy--find-visible-regions
                       (window-start) (window-end wnd t)))
          (save-excursion
            (goto-char (car pair))
            (when (button-at (point))
              (push (cons (point) wnd) candidates))
            (while (and (condition-case nil (forward-button 1)
                          (error nil))
                        (< (point) (cdr pair)))
              (push (cons (point) wnd) candidates))))))
    (nreverse candidates)))

(defun avy-embark-collect--act (pt)
  "Act on the completion at PT."
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window (cdr (ring-ref avy-ring 0)))
    t))

(defun avy-embark-collect--choose (pt)
  "Choose on the completion at PT."
  (unwind-protect (push-button pt)
    (select-window (cdr (ring-ref avy-ring 0)))
    t))

(defun avy-embark-collect--jump (action dispatch-alist)
  "Jump to a visible Embark Collect candidate and perform ACTION.
Other actions are listed in the DISPATCH-ALIST."
  (interactive)
  (avy-with avy-embark-collect-choose
    (let ((avy-action action)
          (avy-dispatch-alist dispatch-alist))
      (avy-process (avy-embark-collect--candidates)))))

;;;###autoload
(defun avy-embark-collect-choose ()
  "Choose an Embark Collect candidate."
  (interactive)
  (avy-embark-collect--jump #'avy-embark-collect--choose
                            '((?e . avy-embark-collect--act)
                              (?p . avy-action-goto))))

;;;###autoload
(defun avy-embark-collect-act ()
  "Act on an Embark Collect candidate."
  (interactive)
  (avy-embark-collect--jump #'avy-embark-collect--act
                            '((?e . avy-embark-collect--choose)
                              (?p . avy-action-goto))))

(provide 'avy-embark-collect)
;;; avy-embark-collect.el ends here
