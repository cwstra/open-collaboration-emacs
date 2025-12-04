;;; open-collaboration.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Corin Stratton
;;
;; Author: Corin Stratton <cwstra@gmail.com>
;; Maintainer: Corin Stratton <cwstra@gmail.com>
;; Created: December 04, 2025
;; Modified: December 04, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/cwstra/open-collaboration
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


;;(defun open-collaboration-)

;; Logging
(setq open-collaboration-log-level 'info)
(defun open-collaboration-log-level-rank (level)
  (cond ((eq level 'error) 3
         (eq level 'warning) 2
         (eq level 'info) 1
         0)))
(defun open-collaboration-log (level &rest args)
  (let ((msg-level (open-collaboration-log-level-rank level))
        (config-level (open-collaboration-log-level-rank open-collaboration-log-level)))
    (when (>= msg-level config-level)
      (apply message args))))


;; Requests
(setq open-collaboration-server-url "https://api.open-collab.tools/")
(defun open-collaboration-get-url (path)
  (let ((url (string-remove-suffix "/" open-collaboration-server-url))
        (path (string-remove-prefix "/" path)))
    (concat url "/" path)))

(defun open-collaboration-login (callback)
  (open-collaboration-log 'info "Performing login")
  (let ((url-request-method "POST"))
    (url-retrieve
     (open-collaboration-get-url "/api/login/initial")
     (lambda (status cbargs)
       (open-collaboration-log 'info "Login request finished" status cbargs)
       (funcall callback status cbargs)))))



;; Initializers
(defun open-collaboration-create-room ()
  "Create a new open collaboration room"
  (open-collaboration-log 'info "Sending create room request")
  (url-retrieve
   open-collaboration-server-url
   (lambda (status cbargs)
     (open-collaboration-log 'info "request finished" status cbargs))))

(defun open-collaboration-join-room ()
  "Join an existing open collaboration room"
  (message "open-collaboration initialized"))

(defvar open-collaboration-create-room 'open-collaboration-create-room)
(defvar open-collaboration-join-room 'open-collaboration-join-room)

(provide 'open-collaboration)
;;; open-collaboration.el ends here
