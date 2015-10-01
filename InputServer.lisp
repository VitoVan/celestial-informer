(setf sb-impl::*default-external-format* :UTF-8)
;;(declaim (optimize (debug 3)))
(ql:quickload '(hunchentoot cl-json cl-mongo uuid cl-smtp))
(defpackage celestial-informer-input
  (:use :cl :hunchentoot :json :cl-mongo :cl-smtp))
(in-package :celestial-informer-input)

(db.use "celestial-informer-input")

;;Time Convertion http://lisptips.com/post/11649360174/the-common-lisp-and-unix-epochs
(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))
(defun universal-to-unix-time(universal-time)
  (- universal-time *unix-epoch-difference*))
(defun unix-to-universal-time(unix-time)
  (+ unix-time *unix-epoch-difference*))
(defun get-unix-time()
  (universal-to-unix-time (get-universal-time)))

;;Email Server Configuration
(defvar *email-server* "smtp.example.com")
(defvar *email-account* "foo@example.com")
(defvar *email-pwd* "password")
(defvar *email-dname* "Zhang San")

;;Site Server Configuration
(defvar *server-path* "http://localhost:5003/")

(defun make-que-id()
  (concatenate 'string "QID-" (write-to-string (uuid:make-v4-uuid))))

(defun make-user-token()
  (concatenate 'string "UTK-" (write-to-string (uuid:make-v4-uuid))))

(defun db-get-que (qid)
  (car (docs (db.find "que"
                      (kv "qid" qid)))))

(defun db-push-message (title qid &key content)
  (and (db-get-que qid)
       (let* ((doc (add-element "status" 0
                                (add-element "atime" (get-unix-time)
                                             (add-element "title" title
                                                          (add-element "content" content
                                                                       (add-element "qid" qid (make-document))))))))
         (db.save "msg" doc))))

(defun db-push-que (uid)
  (let* ((qid (write-to-string (make-que-id)))
         (doc (add-element "atime" (get-unix-time)
                           (add-element "uid" uid
                                        (add-element "status" 0
                                                     (add-element "qid" qid (make-document)))))))
    (db.save "que" doc)
    qid))

(defun db-del-que (qid)
  (db.delete "que" (db-get-que qid)))

(defun db-get-user (uid &key token new-token)
  (car (docs (db.find "user"
                      (kv (kv "uid" uid)
                          (if token
                              (kv "token" token))
                          (if new-token
                              (kv "new-token" new-token)))))))

(defun db-push-user (uid)
  (and uid  (null (db-get-user uid))
       (let* ((doc (add-element "atime" (get-unix-time)
                                (add-element "uid" uid
                                             (add-element "token" (make-user-token)
                                                          (add-element "status" 0 (make-document)))))))
         (db.save "user" doc)
         doc)))

(defun db-new-token (uid)
  "generate new token for specified uid and set a delay for next generation, if the delay expired. And if the delay expired && last-token-time is 12hr before, then reset the delay to default (which is 60s), otherwise make it double"
  (and uid (let* ((user (db-get-user uid))
                  (new-token-delay (or (get-element "new-token-delay" user) 60))
                  (last-token-time (or (get-element "last-token-time" user) 0))
                  (delay-time-remain (- (+ last-token-time new-token-delay) (get-unix-time))))
             (if (<= delay-time-remain 0)
                 (let* ((new-token (make-user-token)))
                   (db.update
                    "user"
                    ($ "uid" uid)
                    (kv
                     ($set "new-token" new-token)
                     ($set "last-token-time" (get-unix-time))
                     ($set "new-token-delay" (if (> (- (get-unix-time) last-token-time) (* 60 60 12)) 60 (* new-token-delay 2)))))
                   new-token)
                 delay-time-remain))))

(defun db-use-token (uid token)
  "Use token: 1, provided token equals new-token, then clear new-token, set token as provided. 2, just same as old token 3, return user or nil"
  (let* ((use-new-token (db-get-user uid :new-token token))
         (use-old-token (db-get-user uid :token token)))
    (if use-new-token
        (db.update
         "user"
         ($ "uid" uid)
         (kv ($set "new-token" "") ($set "token" token))))
    (or use-new-token use-old-token)))

(defun get-param (key)
  (or (parameter key)
      (cookie-in key)))

; Start Hunchentoot
(setf *show-lisp-errors-p* t)
(setf *acceptor* (make-instance 'hunchentoot:easy-acceptor
                                :port 5003
                                :access-log-destination "log/input-access.log"
                                :message-log-destination "log/input-message.log"
                                :error-template-directory  "www/errors/"
                                :document-root "www/"))

(defun start-server ()
  (start *acceptor*)
  (format t "Server started at 5003"))

(defun validate-email(email)
  (cl-ppcre:scan-to-strings "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,4}"  email))

(defun ctl-user-add()
  (let* ((uid (validate-email (parameter "email")))
         (new-user (db-push-user uid)))
    (if new-user
        (car (multiple-value-list (get-element "token" new-user)))
        "already exists")))

;; Cookie stuff
(defvar *cookie-expires-time* (* 15 24 60 60))
(defun get-cookie-expires()
  (+ *cookie-expires-time* (get-universal-time)))

(defun ctl-user-login()
  "Login Controller: http://localhost:5003/user/login?email=aaa@bbb.com&token=UTK-6B2D2B86-B659-4C83-8FAA-79DA02ABA99D"
  (let* ((uid (validate-email (parameter "email")))
         (token (or (parameter "token") "FAKE-TOKEN"))
         (user (db-use-token uid token)))
    (if user
        (progn
          (set-cookie "uid" :value uid :expires (get-cookie-expires) :path "/")
          (set-cookie "token" :value token :expires (get-cookie-expires) :path "/")
          (redirect "/ques"))
        "login error")))

(defun send-login-email(email token)
  (handler-case
      (progn (send-email *email-server* *email-account*  email "Login Into Your Account"
                         (concatenate 'string *server-path* "user/login?email=" email "&token=" token)
                         :authentication '(:login *email-account* *email-pwd*)
                         :display-name *email-dname*)
             "mail sent")
    (error
        (condition)
      (format nil "~A" condition))))

(defun ctl-request-login()
  (let* ((uid (parameter "email"))
         (new-token (db-new-token uid)))
    (and uid (if (numberp new-token)
                 (concatenate 'string "Please wait for " (write-to-string new-token) " seconds")
                 (send-login-email uid new-token)))))

(setf *dispatch-table*
      (list
       (create-regex-dispatcher "^/user/add$" 'ctl-user-add)
       (create-regex-dispatcher "^/user/login$" 'ctl-user-login)
       (create-regex-dispatcher "^/user/req-login$" 'ctl-request-login)))

(start-server)
