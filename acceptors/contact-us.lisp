(defun send-contact-us-email (name email message)
	(let ((contact-us-message ""))
		(when email
			(setq contact-us-message
            (concatenate 'string contact-us-message "From: " email "

")))
		(when name
			(setq contact-us-message
            (concatenate 'string contact-us-message "Name: " name "

")))
		(when message
			(setq contact-us-message
            (concatenate 'string contact-us-message "Message:

" message "

")))
    (handler-case
        (setq contact-us-message
              (concatenate 'string contact-us-message "IP Address: " (hunchentoot:remote-addr*)))
      (error nil nil))
    (mailgun:send-message *admin-from-email-address* "Contact Us Message" contact-us-message)))

(define-easy-handler (contact-us-form
                      :uri "/contact-us-ajax/"
                      :default-request-type :post)
    (name email message)
	(if (string-equal message "")
      "{\"success\":false, \"error\":\"Data for the message field is required.\"}"
      (progn
        (send-contact-us-email name email message)
        "{\"success\":true}"))
	)