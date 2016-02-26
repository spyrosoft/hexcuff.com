(ql:quickload :drakma)
(ql:quickload :jsown)

(defpackage :mailgun
	(:use :common-lisp)
	(:export :set-credentials :send-message :validate-email-address))

(in-package :mailgun)

(defvar *mailgun-credentials* (make-hash-table :test 'eq))

(defun set-credentials (&key api-key api-public-key from-email-address post-url (validate-email-url "https://api.mailgun.net/v3/address/validate"))
  "Set your Mailgun API credentials."
  (when api-key
    (setf (gethash 'api-key *mailgun-credentials*) api-key))
  (when api-public-key
    (setf (gethash 'api-public-key *mailgun-credentials*) api-public-key))
  (when from-email-address
    (setf (gethash 'from-email-address *mailgun-credentials*) from-email-address))
  (when post-url
    (setf (gethash 'post-url *mailgun-credentials*) post-url))
  (setf (gethash 'validate-email-url *mailgun-credentials*) validate-email-url)
  nil)

(defun validate-email-address (email-address)
  "Use Mailgun to validate a recipient email address."
  (let ((mailgun-response
         (map 'string #'code-char
              (drakma:http-request
               (gethash 'validate-email-url *mailgun-credentials*)
               :method :get
               :basic-authorization (list "api" (gethash 'api-public-key *mailgun-credentials*))
               :parameters (list (cons "address" email-address)))))
        (mailgun-parsed-response))
    (setq mailgun-parsed-response (jsown:parse mailgun-response))
    (handler-case (jsown:val mailgun-parsed-response "is_valid")
      (error nil "The JSON key 'is_valid' does not appear in the request response."))
    ))

(defun send-message (recipient-email-address email-subject email-message-body &key cc bcc)
  "Send an email after setting your Mailgun credentials."
  (unless (gethash 'api-key *mailgun-credentials*)
    (error "You need to set your credentials before send-message can be called."))
  (let ((mail-parameters '()))
    (push (cons "from" (gethash 'from-email-address *mailgun-credentials*)) mail-parameters)
    (push (cons "to" recipient-email-address) mail-parameters)
    (when cc
      (push (cons "cc" cc) mail-parameters))
    (when bcc
      (push (cons "bcc" bcc) mail-parameters))
    (push (cons "subject" email-subject) mail-parameters)
    (push (cons "text" email-message-body) mail-parameters)
    (drakma:http-request
     (gethash 'post-url *mailgun-credentials*)
     :method :post
     :basic-authorization (list "api" (gethash 'api-key *mailgun-credentials*))
     :parameters mail-parameters)
    ))