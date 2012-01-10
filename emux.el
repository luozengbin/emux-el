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

(require 'term)

(defgroup emux nil
  "Emacs Terminal Multiplexer."
  :group 'terminals
  :prefix "emux:")

(defcustom emux:shell-program nil
  "Default shell program. If not specified, SHELL envrionment
variable or ESHELL environment variable will be used."
  :type 'string
  :group 'emux)

(defcustom emux:terminal-name "emux"
  "Default terminal name prefix."
  :type 'string
  :group 'emux)

(defface emux:tab-face
  '()
  "Tab face."
  :group 'emux)

(defface emux:current-tab-face
  '((t (:background "steelblue" :foreground "white")))
  "Current tab face."
  :group 'emux)

(defun emux:shell-program ()
  (or emux:shell-program
      (getenv "SHELL")
      (getenv "ESHELL")
      (read-from-minibuffer "Shell program: " "/bin/sh")))



(defvar emux:terminals nil
  "A list of terminal buffers.")

(defvar emux:current-terminal nil
  "A current terminal buffer.")

(defun emux:terminal-live-p (term)
  (and (buffer-live-p term)
       (term-check-proc term)
       t))

(defun emux:live-terminals ()
  (loop for term in emux:terminals
        if (emux:terminal-live-p term)
        collect term))

(defun emux:cleanup-dead-terminals ()
  (setq emux:terminals
        (loop for term in emux:terminals
              if (emux:terminal-live-p term)
              collect term
              else if (buffer-live-p term)
              do (kill-buffer term))))

(defun emux:current-terminal ()
  (if (emux:terminal-live-p emux:current-terminal)
      emux:current-terminal
    (setq emux:current-terminal
          (first (emux:live-terminals)))))

(defun emux:next-terminal ()
  (let* ((terminals (emux:live-terminals))
         (current-term (emux:current-terminal))
         (next-term (second (memq current-term terminals))))
    (or next-term (first terminals))))

(defun emux:previous-terminal ()
  (let* ((terminals (reverse (emux:live-terminals)))
         (current-term (emux:current-terminal))
         (previous-term (second (memq current-term terminals))))
    (or previous-term (first terminals))))



(defun emux:make-terminal (name program)
  (emux:cleanup-dead-terminals)
  (let ((term (generate-new-buffer name)))
    (with-current-buffer term
      (emux:term-mode))
    (term-exec term name program nil nil)
    (with-current-buffer term
      (term-char-mode))
    (add-to-list 'emux:terminals term t)
    term))

(defun emux:make-terminal-header (term)
  (loop for other-term in (emux:live-terminals)
        for tab = (format " %s " (buffer-name other-term))
        collect (propertize tab
                            'face (if (eq term other-term)
                                      'emux:current-tab-face
                                    'emux:tab-face))
        into tabs
        finally return (mapconcat 'identity tabs "")))

(defun emux:update-terminal-header (term)
  (with-current-buffer term
    (setq header-line-format (emux:make-terminal-header term))))

(defun emux:update-terminal (term)
  (emux:update-terminal-header term))

(defun emux:switch-to-terminal (term)
  (setq emux:current-terminal term)
  (emux:update-terminal term)
  (switch-to-buffer term))

(defun emux:switch-to-terminal-other-window (term)
  (setq emux:current-terminal term)
  (emux:update-terminal term)
  (switch-to-buffer-other-window term))

(defun emux:new-terminal ()
  (emux:make-terminal emux:terminal-name
                      (emux:shell-program)))



(defun emux:term-mode ()
  (interactive)
  (term-mode))

(defun emux:term-noselect ()
  (interactive)
  (or (emux:current-terminal) (emux:new-terminal)))

;;;###autoload
(defun emux:term ()
  (interactive)
  (emux:switch-to-terminal
   (emux:term-noselect)))

;;;###autoload
(defun emux:term-other-window ()
  (interactive)
  (emux:switch-to-terminal-other-window
   (emux:term-noselect)))

(defun emux:term-new ()
  (interactive)
  (emux:switch-to-terminal (emux:new-terminal)))

(defun emux:term-next ()
  (interactive)
  (emux:switch-to-terminal (emux:next-terminal)))

(defun emux:term-previous ()
  (interactive)
  (emux:switch-to-terminal (emux:previous-terminal)))

(provide 'emux)
;;; emux.el ends here
