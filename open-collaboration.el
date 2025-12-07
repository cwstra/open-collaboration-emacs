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

(require 'cl-lib)
(require 'subr-x)
(require 's)

(declare-function projectile-project-root "ext:projectile")

;;(defun open-collaboration-)

(defgroup open-collaboration nil
  "Open Collaboration Protocol client."
  :group 'tools)

;; Helpers

(defmacro let-do (varlist &rest body)
  (declare (debug ((&rest [&or (sexp form) ("useh" sexp form) ("uset" sexp form) ("usefn" sexp form)]) body))
           (indent 1))
  (named-let go ((varlist varlist)
                 (body body)
                 (acc nil))
    (let ((first (car varlist)))
      (cond
       ((and (not first)
             (not acc))
        (cons 'progn body))
       ((not first)
        (progn
          (message "returning: %s" acc)
          (let ((result (car (seq-reduce
                              (lambda (acc next)
                                (list
                                 (cond
                                  ((eq (car next) 'let*)
                                   (cons 'let* (cons (cadr next) acc)))
                                  ((or (eq (car next) 'usefn) (not (consp (caddr next))))
                                   (message "Inside: %s" (list (caddr next) (cons 'lambda (cons (list (cadr next)) acc))))
                                   (list (caddr next) (cons 'lambda (cons (list (cadr next)) acc))))
                                  ((eq (car next) 'uset)
                                   (seq-concatenate 'list (caddr next) (list (cons 'lambda (cons (list (cadr next)) acc)))))
                                  (t
                                   (seq-concatenate 'list (list (cons 'lambda (cons (list (cadr next)) acc))) (caddr next))))))
                              acc
                              body))))
            (message "result: %s" result)
            result)))
       ((eq 3 (seq-length first))
        (go
         (cdr varlist) body
         (cons first acc)))
       (t
        (let* ((other-vars (cdr varlist))
               (other-bindings
                (seq-take-while
                 (lambda (b) (eq 2 (seq-length b)))
                 other-vars))
               (next-varlist
                (seq-drop other-vars (length other-bindings))))
          (go
           next-varlist body
           (cons (list 'let* (cons first other-bindings)) acc))))))))

(macroexpand
 (let-do ((uset duckduckgo-length (test-fn "https://duckduckgo.com"))
          (uset google-length (test-fn "https://google.com"))
          (total-length (+ duckduckgo-length google-length)))
   (message "Total length: %s" total-length)))

;; Logging
(defcustom open-collaboration-log-level 'info
  "Minimum logging level for outputing open collaboration error messages"
  :group 'open-collaboration
  :type '(choice
          (const :tag "None" none)
          (const :tag "Error" error)
          (const :tag "Warning" warning)
          (const :tag "Info" info)
          (const :tag "Verbose" verbose)))
(defun open-collaboration-log-level-rank (level)
  (cond ((eq level 'none) 5)
        ((eq level 'error) 4)
        ((eq level 'warning) 3)
        ((eq level 'info) 2)
        ((eq level 'verbose) 1)
        (0)))
(defmacro open-collaboration--log (level format-str &rest args)
  `(let ((msg-level (open-collaboration-log-level-rank ,level))
         (config-level (open-collaboration-log-level-rank open-collaboration-log-level)))
     (when (>= msg-level config-level)
       (message ,format-str ,@args))))
(defun open-collaboration--trace (level fmt value)
  (open-collaboration--log level fmt value)
  value)

;; Requests
(defcustom open-collaboration-server-url "https://api.open-collab.tools/"
  "Server to use for open collaboration protocol"
  :group 'open-collaboration
  :type 'string)
(defun open-collaboration--get-url (path)
  (let ((url (string-remove-suffix "/" open-collaboration-server-url))
        (path (string-remove-prefix "/" path)))
    (concat url "/" path)))
(defun open-collaboration--parse-json-buffer ()
  (search-forward "\n\n")
  (json-parse-string (buffer-substring (point) (point-max))))
(defmacro open-collaboration--url-callback (err-fmt &rest body)
  `(lambda (status)
     (let ((err (plist-get status :error)))
       (if err
           (open-collaboration--log 'error ,err-fmt err)
         ,@body))))

(defun open-collaboration--fetch-json (path err-fmt return)
  (let* ((url (string-remove-suffix "/" open-collaboration-server-url))
         (path (string-remove-prefix "/" path))
         (full-path (concat url "/" path)))
    (url-retrieve
     full-path
     (lambda (status)
       (let ((err (plist-get status :error)))
         (if err
             (open-collaboration--log 'error err-fmt err)
           (search-forward "\n\n")
           (funcall return (json-parse-string (buffer-substring (point) (point-max))))))))))

;; Login

(defun open-collaboration--get-metadata (callback)
  (url-retrieve
   (open-collaboration--get-url "/api/meta")
   (open-collaboration--url-callback
    "Metadata request failed with %s"
    (funcall callback (open-collaboration--parse-json-buffer)))))

(defun open-collaboration--ensure-compatibility (callback)
  (open-collaboration--get-metadata
   (lambda (metadata)
     (let ((server-version (gethash "version" metadata)))
       ()))))


(defvar open-collaboration-user-auth-token)
(defun open-collaboration--login (callback)
  (open-collaboration--log 'info "Performing login to %s" (open-collaboration--get-url "/api/login/initial"))
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Length" . "0"))))
    (url-retrieve
     (open-collaboration--get-url "/api/login/initial")
     (open-collaboration--url-callback
      "Login request failed with error %s"
      (open-collaboration--log 'info "Login request finished")
      (funcall callback)))))

(defun open-collaboration--handle-auth-form-fields (fields acc)
  (open-collaboration--log 'info "open-collaboration--handle-auth-form-fields: %s;;%s" fields acc)
  (if (seq-empty-p fields) acc
    (let* ((field (car fields))
           (required (if (eq t (gethash "required" field)) t nil))
           (label (gethash "message" (gethash "label" field)))
           (placeholder (gethash "message" (gethash "placeHolder" field)))
           (result (read-string (concat label (if required " (Required)" "") ": ") nil nil placeholder)))
      (if (and required (seq-empty-p result))
          (message (concat label "is required."))
        (open-collaboration--handle-auth-form-fields
         (cdr fields)
         (cons (cons (intern (gethash "name" field)) result) acc))))))

(defun open-collaboration--handle-auth (token auth on-success)
  (let ((login-url
         (let ((raw (gethash "loginPageUrl" auth)))
           (if (and raw (s-starts-with? "/" raw)) (open-collaboration--get-url raw) raw)))
        (provider-lookup
         (seq-map-indexed
          (lambda (p i) (cons (gethash "message" (gethash "label" p)) i))
          (gethash "providers" auth))))
    (if (and login-url (seq-empty-p provider-lookup))
        (progn
          (browse-url login-url)
          (funcall on-success))
      (open-collaboration--log 'info "Starting selection with these options: %s" provider-lookup)
      (let* ((selected
              (let* ((label (completing-read "Select an authentication provider:" provider-lookup nil t))
                     (index (alist-get label provider-lookup nil nil 'string-equal)))
                (seq-elt (gethash "providers" auth) index)))
             (endpoint (open-collaboration--get-url (gethash "endpoint" selected))))
        (open-collaboration--log 'verbose "Selected %s, starting login" selected)
        (if (string-equal "web" (gethash "type" selected))
            (progn
              (browse-url (concat endpoint "?token=" token))
              (funcall on-success))
          (open-collaboration--log 'verbose "form fields: %s" (gethash "fields" selected))
          (let* ((fields (open-collaboration--handle-auth-form-fields (append (gethash "fields" selected) nil) (list (cons 'token token))))
                 (body (json-serialize fields))
                 (url-request-method "POST")
                 (url-request-extra-headers '(("Content-Type" . "application/json")))
                 (url-request-data body))
            (url-retrieve
             endpoint
             (open-collaboration--url-callback
              "Form position failed with error %s"
              (open-collaboration--log 'info "Form post finished")
              (funcall on-success)))))))))

;; Session
(defvar open-collaboration-session)
(defun open-collaboration--poll-for-login (token &optional room-folder)
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Length" . "0"))))
    (url-retrieve
     (open-collaboration--get-url (concat "/api/login/poll/" token))
     (open-collaboration--url-callback
      "Login poll failed with error %s"
      (open-collaboration--log 'info "Login poll came back with body: %s" (buffer-string))
      (let* ((body (open-collaboration--parse-json-buffer))
             (login-token (gethash "loginToken" body)))
        (if (not (seq-empty-p login-token))
            (setq open-collaboration-session (if room-folder (cons login-token room-folder) login-token))
          (open-collaboration--poll-for-login token room-folder)))))))

;; TODO (cite-sources): https://github.com/emacs-lsp/lsp-mode/blob/4cedcbf453e7f199cdcf457657fd50657db81c42/lsp-mode.el#L9249
(defun open-collaboration--read-char (prompt &optional options)
  (if (fboundp 'read-char-from-minibuffer)
      (read-char-from-minibuffer prompt options)
    (read-key prompt)))
;; TODO (cite-sources): https://github.com/emacs-lsp/lsp-mode/blob/4cedcbf453e7f199cdcf457657fd50657db81c42/lsp-mode.el#L9257
(defun open-collaboration--interactively-select-room-folder ()
  (let* ((suggested
          ;; TODO (cite-sources): logic taken from https://github.com/emacs-lsp/lsp-mode/blob/4cedcbf453e7f199cdcf457657fd50657db81c42/lsp-mode.el#L4070
          (or
           (when (fboundp 'projectile-project-root)
             (condition-case nil
                 (projectile-project-root)
               (error nil)))
           (when (fboundp 'project-current)
             (when-let* ((project (project-current)))
               (if (fboundp 'project-root)
                   (project-root project)
                 (car (with-no-warnings (project-roots project))))))
           default-directory))
         (action (open-collaboration--read-char (format "Please select a directory to share to your collaboration room.

%s ==> Auto-detected project root %s
%s ==> Select directory interactively
%s ==> Current directory %s

Select action: "
                                                        (propertize "i" 'face 'success)
                                                        (propertize suggested 'face 'bold)
                                                        (propertize "I" 'face 'success)
                                                        (propertize "." 'face 'success)
                                                        (propertize default-directory 'face 'bold)))))
    (cl-case action
      (?i suggested)
      (?\r suggested)
      (?I (read-directory-name "Select folder to add as collaboration project" (or suggested default-directory) nil t))
      (?. default-directory)
      (t nil))))


;; Initializers
(defun open-collaboration-create-room ()
  "Create a new open collaboration room"
  (if-let ((room-folder (open-collaboration--interactively-select-room-folder)))
      (open-collaboration--login
       (lambda ()
         (open-collaboration--log 'info (buffer-string))
         (let* ((body (open-collaboration--parse-json-buffer))
                (token (gethash "pollToken" body)))
           (open-collaboration--handle-auth
            token 
            (gethash "auth" body)
            (lambda ()
              (open-collaboration--poll-for-login token room-folder))))))
    "No folder selected."))

(defun open-collaboration-join-room ()
  "Join an existing open collaboration room"
  (message "open-collaboration initialized"))

(defvar open-collaboration-create-room 'open-collaboration-create-room)
(defvar open-collaboration-join-room 'open-collaboration-join-room)

(provide 'open-collaboration)
;;; open-collaboration.el ends here
