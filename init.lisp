(ql:quickload :hunchentoot)

(defpackage :hexcuff
  (:use :common-lisp :hunchentoot))

(in-package :hexcuff)

;; Needed if you set :error-template-directory in the easy-acceptor
(setf hunchentoot::*show-lisp-errors-p* t)

(defvar hexcuff-server
  (make-instance 'hunchentoot:easy-acceptor
                 :document-root "web-root"
                 :error-template-directory "web-root/pages/error-templates/"
                 :access-log-destination "logs/access.log"
                 :message-log-destination "logs/error.log"
                 :port 8087))

(load "packages/mailgun.lisp")
(load "private/credentials.lisp")
(load "routes.lisp")

(hunchentoot:start hexcuff-server)