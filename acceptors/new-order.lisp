(defvar *hex-cuffs-and-prices* (make-hash-table :test 'equal))
(setf (gethash "The Guy/Girl Magnet Hex" *hex-cuffs-and-prices*) "800")
(setf (gethash "The 304 - Stainless Hex" *hex-cuffs-and-prices*) "1400")
(setf (gethash "Steel Serrated Locking Hex" *hex-cuffs-and-prices*) "1200")
(setf (gethash "Steel Self Locking Black Hex" *hex-cuffs-and-prices*) "1200")

(defun invalid-input-exists (customer-email which-cuff slot-size which-ear quantity shopify-token)
  (let ((error-messages '()))
    (handler-case (mailgun:validate-email-address customer-email)
                  (error nil (push "The email address entered is invalid." error-messages)))
    (unless (gethash which-cuff *hex-cuffs-and-prices*)
      (push "The requested cuff has either been pulled from the catalog, or never existed." error-messages))
    (unless (or (equal slot-size "Small") (equal slot-size "Medium") (equal slot-size "Large"))
      (push "Slot sizes can only be one of the following: Small, Medium, Large." error-messages))
    (unless (or (equal which-ear "Left") (equal which-ear "Right"))
      (push "You must choose either the Left or the Right ear." error-messages))
    (handler-case (setq quantity (parse-integer quantity))
      (error nil (push "Quantity must be an integer." error-messages)))
    (when (< quantity 1)
      (push "Quantity must be at least 1." error-messages))
    (when (> quantity 100)
      (push "Orders are limited to 100. For bulk orders, please contact us via our contact page." error-messages))
    (unless shopify-token
      (push "The payment token is missing somehow. That's techno-babble for: Please fill out your order again and if the issue persists, let us know in a message via our contact form. Thank you." error-messages))
    error-messages
    ))

(defun payment-gateway-errors (payment-gateway-response-json)
  (let ((error-messages '()))
    (handler-case (jsown:val payment-gateway-response-json "id")
                  (error nil
                         (handler-case (push (jsown:val (jsown:val payment-gateway-response-json "error") "message") error-messages)
                                       (error nil (push "An error has occurred while obtaining your order number from the payment gateway. This likely means that the payment gateway is temporarily down. You have not been charged for your order. We recommend waiting and trying again. There isn't much else that can be done when this happens. Thank you for your patience." error-messages)))))
    error-messages
    ))

(defun create-a-payment (shopify-token which-cuff slot-size which-ear quantity)
  (let ((stripe-key
         (if (eq *live-or-dev* 'dev)
             *stripe-test-secret-key*
           *stripe-live-secret-key*))
        (product-description (concatenate 'string which-cuff ", Size: " slot-size ", Ear: " which-ear ", Quantity: " quantity))
        (cost-in-cents (gethash which-cuff *hex-cuffs-and-prices*)))
    (jsown:parse
     (flexi-streams:octets-to-string
      (drakma:http-request
       "https://api.stripe.com/v1/charges"
       :method :post
       :basic-authorization (list stripe-key "")
       :parameters (list (cons "amount" cost-in-cents)
                         (cons "currency" "usd")
                         (cons "source" shopify-token)
                         (cons "description" product-description)))))))

(defun send-order-confirmation-email (payment-gateway-response customer-email which-cuff slot-size which-ear quantity)
  (let ((shipping-data (jsown:val payment-gateway-response "source")))
    (let ((customer-name (jsown:val shipping-data "name"))
          (address (jsown:val shipping-data "address_line1"))
          (city (jsown:val shipping-data "address_city"))
          (state (jsown:val shipping-data "address_state"))
          (zip (jsown:val shipping-data "address_zip"))
          (order-total (format nil "~d" (/ (parse-integer (gethash which-cuff *hex-cuffs-and-prices*)) 100)))
          (order-id (jsown:val shipping-data "id"))
          (order-email-message (read-file-into-string "email-templates/new-order.txt")))
      (when (jsown:val shipping-data "address_line2")
        (setq address (concatenate 'string address "
" (jsown:val shipping-data "address_line2"))))
      (let ((message-search (list "CUSTOMER-NAME" "QUANTITY" "WHICH-CUFF" "SLOT-SIZE" "WHICH-EAR" "SHIPPING-ADDRESS" "SHIPPING-CITY" "SHIPPING-STATE" "SHIPPING-ZIP" "ORDER-TOTAL"))
            (message-replace (list customer-name quantity which-cuff slot-size which-ear address city state zip order-total)))
        (mapcar #'(lambda (search replace) (setq order-email-message (cl-ppcre:regex-replace search order-email-message replace))) message-search message-replace)
        )
      (mailgun:send-message customer-email (concatenate 'string "Receipt of hexcuff.com order #" order-id) order-email-message)
      (mailgun:send-message customer-email (concatenate 'string "Receipt of hexcuff.com order #" order-id) *orders-from-email-address*)
      )))

(define-easy-handler (new-order
                      :uri "/new-order-ajax/"
                      :default-request-type :post)
  (customer-email which-cuff slot-size which-ear quantity shopify-token)
  (let ((error-messages (invalid-input-exists customer-email which-cuff slot-size which-ear quantity shopify-token))
        (browser-response ""))
    (if error-messages
        (setq browser-response (jsown:to-json (jsown:new-js ("success" :false)
                                                            ("errors" error-messages))))
      (let* ((payment-gateway-response (create-a-payment shopify-token which-cuff slot-size which-ear quantity))
             (error-messages (payment-gateway-errors payment-gateway-response)))
        (if error-messages
            (setq browser-response (jsown:to-json (jsown:new-js ("success" :false)
                                                                ("errors" error-messages))))
          (progn
            (send-order-confirmation-email payment-gateway-response customer-email which-cuff slot-size which-ear quantity)
            (setq browser-response (jsown:to-json (jsown:new-js ("success" :true))))))))
    browser-response
    ))