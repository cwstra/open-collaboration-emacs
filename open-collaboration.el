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
;; Package-Requires: ((emacs "24.4") (s "1.10.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 's)

;;(defun open-collaboration-)

;; Logging
(defvar open-collaboration-log-level 'info)
(defun open-collaboration-log-level-rank (level)
  (cond ((eq level 'error) 4)
        ((eq level 'warning) 3)
        ((eq level 'info) 2)
        ((eq level 'verbose) 1)
        (0)))
(defun open-collaboration-log (level format-str &rest args)
  (let ((msg-level (open-collaboration-log-level-rank level))
        (config-level (open-collaboration-log-level-rank open-collaboration-log-level)))
    (when (>= msg-level config-level)
      (apply 'message format-str args))))

;; Requests
(defvar open-collaboration-server-url "https://api.open-collab.tools/")
(defun open-collaboration-get-url (path)
  (let ((url (string-remove-suffix "/" open-collaboration-server-url))
        (path (string-remove-prefix "/" path)))
    (concat url "/" path)))
(defun open-collaboration-parse-buffer ()
  (search-forward "\n\n")
  (json-parse-string (buffer-substring (point) (point-max))))

(defun open-collaboration-login (callback)
  (open-collaboration-log 'info "Performing login to %s" (open-collaboration-get-url "/api/login/initial"))
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Length" . "0"))))
    (url-retrieve
     (open-collaboration-get-url "/api/login/initial")
     (lambda (status &optional cbargs)
       (open-collaboration-log 'info "Login request finished")
       (open-collaboration-log 'verbose "Login request params; %s %s" status cbargs)
       (funcall callback status cbargs)))))

(defun open-collaboration-parse-auth-provider (provider)
  (if (eq (gethash "type" provider) "form")
      ()))

(defun open-collaboration-handle-auth-form-fields (fields acc)
  (if (seq-empty-p fields) acc
    (let* ((field (car fields))
           (required (if (eq t (gethash "required" field)) t nil))
           (label (gethash "message" (gethash "label" field)))
           (placeholder (gethash "message" (gethash "placeholder" field)))
           (result (read-string (concat label (if required " (Required)" "") ": ") nil nil placeholder)))
      (if (and required (seq-empty-p result))
          (message (concat label "is required."))
        (open-collaboration-handle-auth-form-fields
         (cdr fields)
         (cons (cons (intern (gethash "name" field)) result) acc))))))

(defun open-collaboration-handle-auth (token auth)
  (let ((login-url
         (let ((raw (gethash "loginPageUrl" auth)))
           (if (and raw (s-starts-with? "/" raw)) (open-collaboration-get-url raw) raw)))
        (provider-lookup
         (seq-map-indexed
          (lambda (p i) (cons (gethash "message" (gethash "label" p)) i))
          (gethash "providers" auth))))
    (if (and login-url provider-lookup)
        (browse-url login-url)
      (let* ((selected
              (let* ((label (completing-read "Select an authentication provider:" provider-lookup nil t))
                     (index (alist-get label provider-lookup nil nil 'string-equal)))
                (cdr (nth index provider-lookup))))
             (endpoint (open-collaboration-get-url (gethash "endpoint" selected))))
        (if (string-equal "web" (gethash "type" selected))
            (browse-url (concat endpoint "?token=" token))
          (let* ((fields (open-collaboration-handle-auth-form-fields (gethash "fields" selected) (list (cons 'token token))))
                 (body (json-serialize fields))
                 (url-request-method "POST")
                 (url-request-extra-headers '(("Content-Type" . "application/json"))))
            (url-retrieve
             endpoint
             (lambda ()
               (open-collaboration-log 'info "Form post finished")))))))))

;; Initializers
(defun open-collaboration-create-room ()
  "Create a new open collaboration room"
  (open-collaboration-login
   (lambda (status &optional cbargs)
     (open-collaboration-log 'info "Create room request finished" status cbargs)
     (let* ((body (open-collaboration-parse-buffer)))
       (open-collaboration-handle-auth
        (gethash "pollToken" body)
        (gethash "auth" body))))))


(defun open-collaboration-join-room ()
  "Join an existing open collaboration room"
  (message "open-collaboration initialized"))

(defvar open-collaboration-create-room 'open-collaboration-create-room)
(defvar open-collaboration-join-room 'open-collaboration-join-room)

(provide 'open-collaboration)
;;; open-collaboration.el ends here
