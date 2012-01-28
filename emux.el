;;; emux.el --- Emacs Terminal Multiplexer

;; Copyright (C) 2012  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <tomo@cx4a.org>
;; Keywords: terminals

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'cl)
(require 'term)

(defgroup emux nil
  "Emacs Terminal Multiplexer."
  :group 'terminals
  :prefix "emux:")

(defcustom emux:shell-program nil
  "A file name of the default shell program. If not specified,
SHELL envrionment variable or ESHELL environment variable will be
used."
  :type 'string
  :group 'emux)

(defcustom emux:terminal-name "emux"
  "A prefix string of terminal buffer names."
  :type 'string
  :group 'emux)

(defface emux:tab-bar-face
  '((t (:background "gray60" :foreground "white")))
  "Tab bar face."
  :group 'emux)

(defface emux:tab-face
  '((t (:background "gray60" :foreground "white")))
  "Tab face."
  :group 'emux)

(defface emux:current-tab-face
  '((t (:background "#999" :foreground "white" :bold t :box (:style released-button))))
  "Current tab face."
  :group 'emux)

(defun emux:shell-program ()
  "Return a shell program name to be executed, If no shell
programs is found in the environment, this function asks a shell
program name and return it."
  (or emux:shell-program
      (getenv "SHELL")
      (getenv "ESHELL")
      (read-from-minibuffer "Shell program: " "/bin/sh")))



(defvar emux:current-terminal nil
  "A current terminal. Note that this may point to null or a dead
terminal.")

(defstruct (emux:terminal (:constructor emux:make-terminal))
  name next previous buffer)

(defun emux:terminal-live-p (term)
  (and (emux:terminal-p term)
       (buffer-live-p (emux:terminal-buffer term))
       (term-check-proc (emux:terminal-buffer term))
       t))

(defun emux:current-live-terminal ()
  (let ((term emux:current-terminal))
    (if (emux:terminal-live-p term)
        term
      (or (emux:previous-live-terminal term)
          (emux:next-live-terminal term)))))

(defun* emux:next-live-terminal (term &key cycle)
  (while (and term
              (setq term (emux:terminal-next term))
              (not (emux:terminal-live-p term))))
  (or term (and cycle (emux:first-terminal))))

(defun* emux:previous-live-terminal (term &key cycle)
  (while (and term
              (setq term (emux:terminal-previous term))
              (not (emux:terminal-live-p term))))
  (or term (and cycle (emux:last-terminal))))

(defun emux:first-terminal ()
  (do ((term emux:current-terminal (emux:terminal-previous term)))
      ((or (null term)
           (null (emux:terminal-previous term)))
       term)))

(defun emux:last-terminal ()
  (do ((term emux:current-terminal (emux:terminal-next term)))
      ((or (null term)
           (null (emux:terminal-next term)))
       term)))

(defun emux:list-terminals ()
  (loop for term = (emux:first-terminal) then (emux:terminal-next term)
        while term collect term))

(defun emux:live-terminals ()
  (loop for term in (emux:list-terminals)
        if (emux:terminal-live-p term)
        collect term))

(defun emux:dead-terminals ()
  (loop for term in (emux:list-terminals)
        unless (emux:terminal-live-p term)
        collect term))

(defun emux:add-terminal (term)
  (let ((last (emux:last-terminal)))
    (if last
        (setf (emux:terminal-next last) term
              (emux:terminal-previous term) last)
      (setq emux:current-terminal term)))
  term)

(defun emux:clean-dead-terminals ()
  (setq emux:current-terminal (emux:current-live-terminal))
  (loop for prev = nil then term
        for terms on (emux:live-terminals)
        for term = (first terms)
        for next = (second terms)
        do (setf (emux:terminal-previous term) prev
                 (emux:terminal-next term) next))
  (loop for term in (emux:dead-terminals)
        for buffer = (emux:terminal-buffer term)
        if (buffer-live-p)
        do (kill-buffer buffer)))



(defun emux:make-terminal-buffer (name program)
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer (emux:term-mode))
    (term-exec buffer name program nil nil)
    (with-current-buffer buffer (term-char-mode))
    buffer))

(defun emux:new-terminal-1 (name program)
  (let* ((buffer (emux:make-terminal-buffer name program))
         (term (emux:make-terminal :name name :buffer buffer)))
    (emux:add-terminal term)))

(defun emux:new-terminal ()
  (emux:clean-dead-terminals)
  (emux:new-terminal-1 emux:terminal-name (emux:shell-program)))

(defun emux:make-terminal-header (term &optional window)
  (loop with win = (or window (selected-window))
        with win-width = (window-total-width win)
        with win-left-fringe = (nth 0 (window-inside-edges win))
        for live-term in (emux:live-terminals)
        for tab = (propertize
                   (format " %s " (emux:terminal-name live-term))
                   'face (if (eq term live-term)
                             'emux:current-tab-face
                           'emux:tab-face))
        collect tab into tabs
        finally return
        (let* ((left-fringe (propertize (make-string win-left-fringe ? )
                                        'face 'emux:tab-bar-face))
               (tabs-string (mapconcat 'identity tabs ""))
               (right-padding (propertize
                               (make-string
                                (max 0
                                     (- win-width
                                        (length tabs-string)
                                        win-left-fringe))
                                ? )
                               'face 'emux:tab-bar-face)))
          (concat left-fringe tabs-string right-padding))))

(defun emux:update-terminal-header (term &optional window)
  (with-current-buffer (emux:terminal-buffer term)
    (setq header-line-format (emux:make-terminal-header term window))))

(defun emux:update-terminal (term &optional window)
  (emux:update-terminal-header term window))

(defun emux:switch-to-terminal (term)
  (setq emux:current-terminal term)
  (prog1 term
    (switch-to-buffer (emux:terminal-buffer term))
    (emux:update-terminal term)))

(defun emux:switch-to-terminal-other-window (term)
  (setq emux:current-terminal term)
  (prog1 term
    (switch-to-buffer-other-window (emux:terminal-buffer term))
    (emux:update-terminal term)))



(defun emux:term-mode ()
  "Emux Terminal Mode."
  (interactive)
  (term-mode))

(defun emux:term-noselect ()
  "Open the current terminal or a new terminal without selecting."
  (interactive)
  (or (emux:current-live-terminal) (emux:new-terminal)))

;;;###autoload
(defun emux:term ()
  "Open the current terminal or a new terminal in this window."
  (interactive)
  (emux:switch-to-terminal (emux:term-noselect)))

;;;###autoload
(defun emux:term-other-window ()
  "Open the current terminal or a new terminal in other window."
  (interactive)
  (emux:switch-to-terminal-other-window (emux:term-noselect)))

(defun emux:term-new ()
  "Create and open a new terminal."
  (interactive)
  (emux:switch-to-terminal (emux:new-terminal)))

(defun emux:term-previous ()
  "Switch to the previous terminal."
  (interactive)
  (emux:switch-to-terminal (emux:previous-live-terminal emux:current-terminal :cycle t)))

(defun emux:term-next ()
  "Switch to the next terminal."
  (interactive)
  (emux:switch-to-terminal (emux:next-live-terminal emux:current-terminal :cycle t)))

(defun emux:term-rename ()
  "Rename the current terminal."
  (interactive)
  (let* ((term (or (emux:current-live-terminal) (error "No current terminal")))
         (new-name (read-from-minibuffer "New name: ")))
    (setf (emux:terminal-name term) new-name)
    (emux:update-terminal term)))

(provide 'emux)
;;; emux.el ends here
