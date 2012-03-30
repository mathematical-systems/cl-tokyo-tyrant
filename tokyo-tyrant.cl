(in-package "TOKYO-TYRANT")

(defconstant *ESUCCESS*    0 "error code: success")
(defconstant *EINVALID*    1 "error code: invalid operation")
(defconstant *ENOHOST*     2 "error code: host not found")
(defconstant *EREFUSED*    3 "error code: connection refused")
(defconstant *ESEND*       4 "error code: send error")
(defconstant *ERECV*       5 "error code: recv error")
(defconstant *EKEEP*       6 "error code: existing record")
(defconstant *ENOREC*      7 "error code: no record found")
(defconstant *EMISC*    9999 "error code: miscellaneous error")
(defconstant *XOLCKREC*    1 "scripting extension option: record locking")
(defconstant *XOLCKGLB*    2 "scripting extension option: global locking")
(defconstant *MONOULOG*    1 "versatile function option: omission of the update log")

(defclass RDB ()
  ((ecode :accessor ecode :initform *ESUCCESS* :documentation "the last happened error code")
   ;; (enc   :accessor enc   :initform nil)
   (sock  :accessor sock  :initform nil)
   (tout  :accessor tout  :initform 0)
   ))

(defmethod tt-recv ((rdb RDB) length)
  (let ((buf (make-string length)))
    (format t "~&tt-recv: length:~D" length)
    (read-sequence buf (sock rdb))
    (format t ",buf:~S~%" buf)
    buf))

(defmethod tt-recv-int32 ((rdb RDB))
  (unpack-N (tt-recv rdb 4)))

(defmethod tt-recv-int64 ((rdb RDB))
  (unpack-Q (tt-recv rdb 8)))

(defmethod tt-recv-code ((rdb RDB))
  (handler-case (read-byte (sock rdb))
    (error (condition)
      (declare (ignore condition))
      (setf (ecode rdb) *ERECV*)
      nil)))

(defmacro tt-recv-code-cond (rdb code &rest clauses)
  `(let ((,code (tokyo-tyrant::tt-recv-code ,rdb)))
     (when ,code
       (cond ,@clauses))))

(defmethod tt-send ((rdb RDB) fmt &rest fmt-args)
  (handler-case (progn
                  (apply #'format `(t ,(string+ "~&tt-send:" fmt "~%") ,@fmt-args))
                  (apply #'format `(,(sock rdb) ,fmt ,@fmt-args))
                  (force-output (sock rdb))
                  t)
    (error (condition)
      (declare (ignore condition))
      (setf (ecode rdb) *ESEND*)
      nil)))

(defmacro when-sock (rdb &rest then-forms)
  `(if (tokyo-tyrant::sock ,rdb)
       (progn
         ,@then-forms)
     (progn
       (setf (tokyo-tyrant::ecode ,rdb) tokyo-tyrant::*EINVALID*)
       nil)))

(defmacro unless-sock (rdb &rest else-forms)
  `(if (tokyo-tyrant::sock ,rdb)
       (progn
         (setf (tokyo-tyrant::ecode ,rdb) tokyo-tyrant::*EINVALID*)
         nil)
       (progn
         ,@else-forms)))

(defmethod tt-errmsg ((rdb RDB) &optional (ecode nil))
  "Get the message string corresponding to an error code.\
   `<i>ecode</i>' specifies the error code.  If it is not defined or negative, the last happened error code is specified.\
   The return value is the message string of the error code."
  (let ((e (or ecode (ecode rdb))))
    (cond
     ((eq e *ESUCCESS*) "success")
     ((eq e *EINVALID*) "invalid operation")
     ((eq e *ENOHOST*)  "host not found")
     ((eq e *EREFUSED*) "connection refused")
     ((eq e *ESEND*)    "send error")
     ((eq e *ERECV*)    "recv error")
     ((eq e *EKEEP*)    "existing record")
     ((eq e *ENOREC*)   "no record found")
     ((eq e *EMISC*)    "miscellaneous error")
     (t                 "unknown"))))

(defmethod tt-open ((rdb RDB) host &optional (port 0) (timeout 0))
  "Open a remote database connection.\
   `<i>host</i>' specifies the name or the address of the server.\
   `<i>port</i>' specifies the port number.  If it is not defined or not more than 0, UNIX domain socket is used and the path of the socket file is specified by the host parameter.\
   `<i>timeout</i>' specifies the timeout of each query in seconds.  If it is not defined or not more than 0, the timeout is not specified.\
   If successful, the return value is true, else, it is false."
  (unless-sock rdb
    (handler-case (let ((sock (if (> port 0)
                                  (make-socket :remote-host host :remote-port port :address-family :internet :format :bivalent)
                                (make-socket :remote-filename host :format :bivalent))))
                    (socket-connect sock)
                    (setf (sock rdb) sock)
                    (setf (tout rdb) timeout)
                    (setf (stream-external-format sock) 'ascii)
                    t)
      (excl:socket-error (condition)
        (setf (ecode rdb) (case (stream-error-identifier condition)
                           ((:host-unreachable)   *ENOHOST*)
                           ((:connection-refused) *EREFUSED*)
                           (t                     *EREFUSED*)))
        nil)
      (error (condition)
        (declare (ignore condition))
        (setf (ecode rdb) *EMISC*)
        (top-level.debug:zoom *standard-output*)
        nil))))

(defmethod tt-close ((rdb RDB))
   "Close the database connection.\
    If successful, the return value is true, else, it is false."
  (when-sock rdb
    (handler-case (progn
                    (close (sock rdb))
                    (setf (sock rdb) nil)
                    t)
      (error (condition)
        (declare (ignore condition))
        (setf (ecode rdb) *EMISC*)
        (setf (sock rdb) nil)
        nil))))

(defmethod tt-put ((rdb RDB) key value)
  "Store a record.\
   `<i>key</i>' specifies the key.\
   `<i>value</i>' specifies the value.\
   If successful, the return value is true, else, it is false.\
   If a record with the same key exists in the database, it is overwritten."
  (when-sock rdb
             (when (multiple-value-bind (key-ascii key-length) (string-ascii key)
                     (multiple-value-bind (value-ascii value-length) (string-ascii value)
                       (tt-send rdb "~C~C~A~A~A~A" (code-char #xC8) (code-char #x10) (pack-N key-length) (pack-N value-length) key-ascii value-ascii)))
               (tt-recv-code-cond rdb code
                                  ((zerop code)
                                   t)
                                  (t
                                   (setf (ecode rdb) *EMISC*)
                                   nil)))))

(defmethod tt-putkeep ((rdb RDB) key value)
  "Store a new record.\
   `<i>key</i>' specifies the key.\
   `<i>value</i>' specifies the value.\
   If successful, the return value is true, else, it is false.\
   If a record with the same key exists in the database, this method has no effect."
  (when-sock rdb
             (when (multiple-value-bind (key-ascii key-length) (string-ascii key)
                     (multiple-value-bind (value-ascii value-length) (string-ascii value)
                       (tt-send rdb "~C~C~A~A~A~A" (code-char #xC8) (code-char #x11) (pack-N key-length) (pack-N value-length) key-ascii value-ascii)))
               (tt-recv-code-cond rdb code
                                  ((zerop code)
                                   t)
                                  (t
                                   (setf (ecode rdb) *EKEEP*)
                                   nil)))))

(defmethod tt-putcat ((rdb RDB) key value)
  "Concatenate a value at the end of the existing record.\
   `<i>key</i>' specifies the key.\
   `<i>value</i>' specifies the value.\
   If successful, the return value is true, else, it is false.\
   If there is no corresponding record, a new record is created."
  (when-sock rdb
             (when (multiple-value-bind (key-ascii key-length) (string-ascii key)
                     (multiple-value-bind (value-ascii value-length) (string-ascii value)
                       (tt-send rdb "~C~C~A~A~A~A" (code-char #xC8) (code-char #x12) (pack-N key-length) (pack-N value-length) key-ascii value-ascii)))
               (tt-recv-code-cond rdb code
                                  ((zerop code)
                                   t)
                                  (t
                                   (setf (ecode rdb) *EMISC*)
                                   nil)))))

(defmethod tt-putshl ((rdb RDB) key value &optional (width 0))
  "Concatenate a value at the end of the existing record and shift it to the left.\
   `<i>key</i>' specifies the key.\
   `<i>value</i>' specifies the value.\
   `<i>width</i>' specifies the width of the record.\
   If successful, the return value is true, else, it is false.\
   If there is no corresponding record, a new record is created."
  (when-sock rdb
             (when (multiple-value-bind (key-ascii key-length) (string-ascii key)
                     (multiple-value-bind (value-ascii value-length) (string-ascii value)
                       (tt-send rdb "~C~C~A~A~A~A~A" (code-char #xC8) (code-char #x13) (pack-N key-length) (pack-N value-length) (pack-N width) key-ascii value-ascii)))
               (tt-recv-code-cond rdb code
                                  ((zerop code)
                                   t)
                                  (t
                                   (setf (ecode rdb) *EMISC*)
                                   nil)))))

(defmethod tt-putnr ((rdb RDB) key value)
  "Store a record without response from the server.\
   `<i>key</i>' specifies the key.\
   `<i>value</i>' specifies the value.\
   If successful, the return value is true, else, it is false.\
   If a record with the same key exists in the database, it is overwritten."
  (when-sock rdb
             (multiple-value-bind (key-ascii key-length) (string-ascii key)
               (multiple-value-bind (value-ascii value-length) (string-ascii value)
                 (tt-send rdb "~C~C~A~A~A~A" (code-char #xC8) (code-char #x18) (pack-N key-length) (pack-N value-length) key-ascii value-ascii)))))


(defmethod tt-out ((rdb RDB) key)
  "Remove a record.\
   `<i>key</i>' specifies the key.\
   If successful, the return value is true, else, it is false."
  (when-sock rdb
             (when (multiple-value-bind (key-ascii key-length) (string-ascii key)
                     (tt-send rdb "~C~C~A~A" (code-char #xC8) (code-char #x20) (pack-N key-length) key-ascii))
               (tt-recv-code-cond rdb code
                                  ((zerop code)
                                   t)
                                  (t
                                   (setf (ecode rdb) *ENOREC*)
                                   nil)))))

(defmethod tt-get ((rdb RDB) key)
  "Retrieve a record.\
   `<i>key</i>' specifies the key.\
   If successful, the return value is the value of the corresponding record.  `nil' is returned if no record corresponds."
  (when-sock rdb
             (when (multiple-value-bind (key-ascii key-length) (string-ascii key)
                     (tt-send rdb "~C~C~A~A" (code-char #xC8) (code-char #x30) (pack-N key-length) key-ascii))
               (tt-recv-code-cond rdb code
                                  ((zerop code)
                                   (or (let ((vsiz (tt-recv-int32 rdb)))
                                         (when (>= vsiz 0)
                                           (let ((vbuf (tt-recv rdb vsiz)))
                                             (when vbuf
                                               (string-external vbuf)))))
                                       (progn
                                         (setf (ecode rdb) *ERECV*)
                                         nil)))
                                  (t
                                   (setf (ecode rdb) *ENOREC*)
                                   nil)))))

(defmethod tt-mget ((rdb RDB) recs)
  "Retrieve records.\
   `<i>recs</i>' specifies a hash containing the retrieval keys.  As a result of this method, keys existing in the database have the corresponding values and keys not existing in the database are removed.\
   If successful, the return value is the number of retrieved records or nil on failure."
  (unless (hash-table-p recs) 
    (error "~S is not a valid argument to TT-MGET." recs))
  (when-sock rdb
             (when (tt-send rdb "~C~C~A~A"
                            (code-char #xC8)
                            (code-char #x31)
                            (pack-N (hash-table-count recs))
                            (loop for key being the hash-key of recs
                                collect (multiple-value-bind (key-ascii key-length) (string-ascii key)
                                          (format nil "~A~A" (pack-N key-length) key-ascii)) into keys
                                finally (return (apply #'string+ keys))))
               (tt-recv-code-cond rdb code
                                  ((not (zerop code))
                                   (setf (ecode rdb) *ENOREC*)
                                   nil)
                                  (t
                                   (or (let ((rnum (tt-recv-int32 rdb)))
                                         (when (>= rnum 0)
                                           (clrhash recs)
                                           (loop for i from 1 to rnum
                                               for ksiz = (tt-recv-int32 rdb)
                                               for vsiz = (tt-recv-int32 rdb)
                                               when (or (< ksiz 0) (< vsiz 0)) return nil
                                               for kbuf = (tt-recv rdb ksiz)
                                               for vbuf = (tt-recv rdb vsiz)
                                               unless (and kbuf vbuf) return nil
                                               do (setf (gethash (string-external kbuf) recs) (string-external vbuf))
                                               finally (return rnum))))
                                       (progn
                                         (setf (ecode rdb) *ERECV*)
                                         nil)))))))

(defmethod tt-vsiz ((rdb RDB) key)
  "Get the size of the value of a record.\
   `<i>key</i>' specifies the key.\
   If successful, the return value is the size of the value of the corresponding record, else, it is nil."
  (when-sock rdb
             (when (multiple-value-bind (key-ascii key-length) (string-ascii key)
                     (tt-send rdb "~C~C~A~A" (code-char #xC8) (code-char #x38) (pack-N key-length) key-ascii))
               (tt-recv-code-cond rdb code
                                  ((zerop code)
                                   (tt-recv-int32 rdb))
                                  (t
                                   (setf (ecode rdb) *ENOREC*)
                                   nil)))))

(defmethod tt-iterinit ((rdb RDB))
  "Initialize the iterator.\
   If successful, the return value is true, else, it is false.\
   The iterator is used in order to access the key of every record stored in a database."
  (when-sock rdb
             (when (tt-send rdb "~C~C" (code-char #xC8) (code-char #x50))
               (tt-recv-code-cond rdb code
                                  ((zerop code)
                                   t)
                                  (t
                                   (setf (ecode rdb) *EMISC*)
                                   nil)))))

(defmethod tt-iternext ((rdb RDB))
  "Get the next key of the iterator.\
   If successful, the return value is the next key, else, it is `nil'.  `nil' is returned when no record is to be get out of the iterator.\
   It is possible to access every record by iteration of calling this method.  It is allowed to update or remove records whose keys are fetched while the iteration.  However, it is not assured if updating the database is occurred while the iteration.  Besides, the order of this traversal access method is arbitrary, so it is not assured that the order of storing matches the one of the traversal access."
  (when-sock rdb
             (when (tt-send rdb "~C~C" (code-char #xC8) (code-char #x51))
               (tt-recv-code-cond rdb code
                                  ((zerop code)
                                   (or (let ((vsiz (tt-recv-int32 rdb)))
                                         (when (>= vsiz 0)
                                           (let ((vbuf (tt-recv rdb vsiz)))
                                             (when vbuf
                                               (string-external vbuf)))))
                                       (progn
                                         (setf (ecode rdb) *ERECV*)
                                         nil)))
                                  (t
                                   (setf (ecode rdb) *ENOREC*)
                                   nil)))))

(defmethod tt-fwmkeys ((rdb RDB) prefix &optional (max -1))
  "Get forward matching keys.\
   `<i>prefix</i>' specifies the prefix of the corresponding keys.\
   `<i>max</i>' specifies the maximum number of keys to be fetched.  If it is not defined or negative, no limit is specified.\
   The return value is an array of the keys of the corresponding records.  This method does never fail.  It returns an empty array even if no record corresponds.\
   Note that this method may be very slow because every key in the database is scanned."
  (when-sock rdb
             (when (multiple-value-bind (prefix-ascii prefix-length) (string-ascii prefix)
                     (tt-send rdb "~C~C~A~A~A" (code-char #xC8) (code-char #x58) (pack-N prefix-length) (pack-N max) prefix-ascii))
               (tt-recv-code-cond rdb code
                                  ((zerop code)
                                   (let ((knum (tt-recv-int32 rdb)))
                                     (when (>= knum 0)
                                       (loop for i from 1 to knum
                                           for ksiz = (tt-recv-int32 rdb)
                                           when (< ksiz 0) return (progn
                                                                    (setf (ecode rdb) *ERECV*)
                                                                    nil)
                                           for kbuf = (tt-recv rdb ksiz)
                                           unless kbuf return (progn
                                                                (setf (ecode rdb) *ERECV*)
                                                                nil)
                                           collect (string-external kbuf)))))
                                  (t
                                   (setf (ecode rdb) *ENOREC*)
                                   nil)))))

(defmethod tt-addint ((rdb RDB) key &optional (num 0))
  "Add an integer to a record.\
   `<i>key</i>' specifies the key.\
   `<i>num</i>' specifies the additional value.  If it is not defined, 0 is specified.\
   If successful, the return value is the summation value, else, it is `nil'.\
   If the corresponding record exists, the value is treated as an integer and is added to.  If no record corresponds, a new record of the additional value is stored.  Because records are stored in binary format, they should be processed with the `unpack' function with the `i' operator after retrieval."
  (when-sock rdb
             (when (multiple-value-bind (key-ascii key-length) (string-ascii key)
                     (tt-send rdb "~C~C~A~A~A" (code-char #xC8) (code-char #x60) (pack-N key-length) (pack-N num) key-ascii))
               (tt-recv-code-cond rdb code
                                  ((zerop code)
                                   (tt-recv-int32 rdb))
                                  (t
                                   (setf (ecode rdb) *EKEEP*)
                                   nil)))))

(defmethod tt-adddouble ((rdb RDB) key &optional (num 0))
  "Add a real number to a record.\
   `<i>key</i>' specifies the key.\
   `<i>num</i>' specifies the additional value.  If it is not defined, 0 is specified.\
   If successful, the return value is the summation value, else, it is `nil'.\
   If the corresponding record exists, the value is treated as a real number and is added to.  If no record corresponds, a new record of the additional value is stored.  Because records are stored in binary format, they should be processed with the `unpack' function with the `d' operator after retrieval."
  (when-sock rdb
             (let* ((integ (truncate num))
                    (fract (truncate (* (- num integ) 1000000000000))))
               (when (multiple-value-bind (key-ascii key-length) (string-ascii key)
                       (tt-send rdb "~C~C~A~A~A~A" (code-char #xC8) (code-char #x61) (pack-N key-length) (pack-Q integ) (pack-Q fract) key-ascii))
                 (tt-recv-code-cond rdb code
                                    ((zerop code)
                                     (let ((integ (tt-recv-int64 rdb))
                                           (fract (tt-recv-int64 rdb)))
                                       (+ integ (/ fract 1000000000000.0))))
                                    (t
                                     (setf (ecode rdb) *EKEEP*)
                                     nil))))))

(defmethod tt-ext ((rdb RDB) name &optional (key "") (value "") (opts 0))
  "Call a function of the script language extension.\
   `<i>name</i>' specifies the function name.\
   `<i>key</i>' specifies the key.  If it is not defined, an empty string is specified.\
   `<i>value</i>' specifies the value.  If it is not defined, an empty string is specified.\
   `<i>opts</i>' specifies options by bitwise-or: `*XOLCKREC*' for record locking, `*XOLCKGLB*' for global locking.  If it is not defined, no option is specified.\
   If successful, the return value is the value of the response or `nil' on failure."
  (when-sock rdb
             (when (multiple-value-bind (name-ascii name-length) (string-ascii name)
                     (multiple-value-bind (key-ascii key-length) (string-ascii key)
                       (multiple-value-bind (value-ascii value-length) (string-ascii value)
                         (tt-send rdb "~C~C~A~A~A~A~A~A~A" (code-char #xC8) (code-char #x68) (pack-N name-length) (pack-N opts) (pack-N key-length) (pack-N value-length) name-ascii key-ascii value-ascii))))
               (tt-recv-code-cond rdb code
                                  ((zerop code)
                                   (or (let ((vsiz (tt-recv-int32 rdb)))
                                         (when (>= vsiz 0)
                                           (let ((vbuf (tt-recv rdb vsiz)))
                                             (when vbuf
                                               (string-external vbuf)))))
                                       (progn
                                         (setf (ecode rdb) *ERECV*)
                                         nil)))
                                  (t
                                   (setf (ecode rdb) *EMISC*)
                                   nil)))))

(defmethod tt-sync ((rdb RDB))
  "Synchronize updated contents with the file and the device.\
   If successful, the return value is true, else, it is false."
  (when-sock rdb
             (when (tt-send rdb "~C~C" (code-char #xC8) (code-char #x70))
               (tt-recv-code-cond rdb code
                                  ((zerop code)
                                   t)
                                  (t
                                   (setf (ecode rdb) *EMISC*)
                                   nil)))))

(defmethod tt-optimize ((rdb RDB) &optional (params ""))
  "Optimize the storage.\
   `<i>params</i>' specifies the string of the tuning parameters.  If it is not defined, it is not used.\
   If successful, the return value is true, else, it is false."
  (when-sock rdb
             (when (multiple-value-bind (params-ascii params-length) (string-ascii params)
                     (tt-send rdb "~C~C~A~A" (code-char #xC8) (code-char #x71) (pack-N params-length) params-ascii))
               (tt-recv-code-cond rdb code
                                  ((zerop code)
                                   t)
                                  (t
                                   (setf (ecode rdb) *EMISC*)
                                   nil)))))

(defmethod tt-vanish ((rdb RDB))
  "Remove all records.\
   If successful, the return value is true, else, it is false."
  (when-sock rdb
             (when (tt-send rdb "~C~C" (code-char #xC8) (code-char #x72))
               (tt-recv-code-cond rdb code
                                  ((zerop code)
                                   t)
                                  (t
                                   (setf (ecode rdb) *EMISC*)
                                   nil)))))

(defmethod tt-copy ((rdb RDB) path)
  "Copy the database file.
   `<i>path</i>' specifies the path of the destination file.  If it begins with `@', the trailing substring is executed as a command line.\
   If successful, the return value is true, else, it is false.  False is returned if the executed command returns non-zero code.\
   The database file is assured to be kept synchronized and not modified while the copying or executing operation is in progress.  So, this method is useful to create a backup file of the database file."
  (when-sock rdb
             (when (multiple-value-bind (path-ascii path-length) (string-ascii path)
                     (tt-send rdb "~C~C~A~A" (code-char #xC8) (code-char #x73) (pack-N path-length) path-ascii))
               (tt-recv-code-cond rdb code
                                  ((zerop code)
                                   t)
                                  (t
                                   (setf (ecode rdb) *EMISC*)
                                   nil)))))

(defmethod tt-rnum ((rdb RDB))
  "Get the number of records.\
   The return value is the number of records or 0 if the object does not connect to any database server."
  (or (when-sock rdb
                 (when (tt-send rdb "~C~C" (code-char #xC8) (code-char #x80))
                   (tt-recv-code-cond rdb code
                                      ((zerop code)
                                       (or (let ((rv (tt-recv-int64 rdb)))
                                             (when (>= rv 0)
                                               rv))
                                           (progn
                                             (setf (ecode rdb) *ERECV*)
                                             nil)))
                                      (t
                                       (setf (ecode rdb) *EMISC*)
                                       nil))))
      0))

(defmethod tt-size ((rdb RDB))
  "Get the size of the database.\
   The return value is the size of the database or 0 if the object does not connect to any database server."
  (or (when-sock rdb
                 (when (tt-send rdb "~C~C" (code-char #xC8) (code-char #x81))
                   (tt-recv-code-cond rdb code
                                      ((zerop code)
                                       (or (let ((rv (tt-recv-int64 rdb)))
                                             (when (>= rv 0)
                                               rv))
                                           (progn
                                             (setf (ecode rdb) *ERECV*)
                                             nil)))
                                      (t
                                       (setf (ecode rdb) *EMISC*)
                                       nil))))
      0))

(defmethod tt-stat ((rdb RDB))
  "Get the status string of the database server.\
   The return value is the status message of the database or `nil' if the object does not connect to any database server.  The message format is TSV.  The first field of each line means the parameter name and the second field means the value."
  (when-sock rdb
             (when (tt-send rdb "~C~C" (code-char #xC8) (code-char #x88))
               (tt-recv-code-cond rdb code
                                  ((zerop code)
                                   (or (let ((ssiz (tt-recv-int32 rdb)))
                                         (when (>= ssiz 0)
                                           (let ((sbuf (tt-recv rdb ssiz)))
                                             (when sbuf
                                               (string-external sbuf)))))
                                       (progn
                                         (setf (ecode rdb) *ERECV*)
                                         nil)))
                                  (t
                                   (setf (ecode rdb) *EMISC*)
                                   nil)))))

;;; NOTE: if successful, and the result is empty,     the return value is t,
;;;       if successful, and the result is not empty, the return value is an array of the result,
;;;       `nil' is returned on failure.

(defmethod tt-misc ((rdb RDB) name &optional (args nil) (opts 0))
  "Call a versatile function for miscellaneous operations.\
   `<i>name</i>' specifies the name of the function.  All databases support 'putlist', 'outlist', and 'getlist'.  'putlist' is to store records.  It receives keys and values one after the other, and returns an empty list.  'outlist' is to remove records.  It receives keys, and returns an empty array.  'getlist' is to retrieve records.  It receives keys, and returns keys and values of corresponding records one after the other.  Table database supports 'setindex', 'search', and 'genuid'.\
   `<i>args</i>' specifies an array containing arguments.  If it is not defined, no argument is specified.\
   `<i>opts</i>' specifies options by bitwise-or: `*MONOULOG*' for omission of the update log.  If it is not defined, no option is specified.\
   If successful, the return value is an array of the result.  `nil' is returned on failure."
  (unless (listp args)
    (error "~S is not a valid argument to TT-MISC." args))
  (when-sock rdb
             (when (multiple-value-bind (name-ascii name-length) (string-ascii name)
                     (tt-send rdb "~C~C~A~A~A~A~A"
                              (code-char #xC8)
                              (code-char #x90)
                              (pack-N name-length)
                              (pack-N opts)
                              (pack-N (length args))
                              name-ascii
                              (loop for arg in args
                                  collect (multiple-value-bind (arg-ascii arg-length) (string-ascii arg)
                                            (format nil "~A~A" (pack-N arg-length) arg-ascii)) into strings
                                  finally (return (apply #'string+ strings)))))
               (tt-recv-code-cond rdb code
                                  ((zerop code)
                                   (or (let ((rnum (tt-recv-int32 rdb)))
                                         (if (zerop rnum)
                                             t
                                           (loop for i from 1 to rnum
                                               for esiz = (tt-recv-int32 rdb)
                                               when (< esiz 0) return nil
                                               for ebuf = (tt-recv rdb esiz)
                                               unless ebuf return nil
                                               collect (string-external ebuf))))
                                       (progn
                                         (setf (ecode rdb) *ERECV*)
                                         nil)))
                                  (t
                                   (tt-recv-int32 rdb) ;; NOTE: This tt-recv-int32 is must.
                                   (setf (ecode rdb) *EMISC*)
                                   nil)))))

(defmethod tt-store ((rdb RDB) key value)
  "Hash-compatible method.\
   Alias of `put'."
  (tt-put rdb key value))

(defmethod tt-delete ((rdb RDB) key)
  "Hash-compatible method.\
   Alias of `out'."
  (tt-out rdb key))

(defmethod tt-fetch ((rdb RDB) key)
  "Hash-compatible method.\
   Alias of `get'."
  (tt-get rdb key))

(defmethod tt-has-keyp ((rdb RDB) key)
  "Hash-compatible method.\
   Check existence of a key."
  (>= (tt-vsiz rdb key) 0))

(defmethod tt-has-valuep ((rdb RDB) value)
  "Hash-compatible method.\
   Check existence of a value."
  (when (tt-iterinit rdb)
    (loop for tkey = (tt-iternext rdb)
        while tkey
        for tvalue = (tt-get rdb tkey)
        while tvalue
        thereis (equal value tvalue))))

(defmethod tt-clear ((rdb RDB))
  "Hash-compatible method.\
   Alias of `vanish'."
  (tt-vanish rdb))

(defmethod tt-length ((rdb RDB))
  "Hash-compatible method.\
   Alias of `rnum'."
  (tt-rnum rdb))

(defmethod tt-emptyp ((rdb RDB))
  "Hash-compatible method.\
   Alias of `rnum < 1'."
  (< (tt-rnum rdb) 1))

(defmethod tt-each ((rdb RDB) proc)
  "Hash-compatible method.\
   Iterator of pairs of the key and the value."
  (when (tt-iterinit rdb)
    (loop for key = (tt-iternext rdb)
        while key
        for value = (tt-get rdb key)
        while value
        do (funcall proc key value))))

(defmethod tt-each-keys ((rdb RDB) proc)
  "Hash-compatible method.\
   Iterator of the keys."
  (when (tt-iterinit rdb)
    (loop for key = (tt-iternext rdb)
        while key
        do (funcall proc key))))

(defmethod tt-each-values ((rdb RDB) proc)
  "Hash-compatible method.\
   Iterator of the values."
  (when (tt-iterinit rdb)
    (loop for key = (tt-iternext rdb)
        while key
        for value = (tt-get rdb key)
        while value
        do (funcall proc value))))

(defmethod tt-keys ((rdb RDB))
  "Hash-compatible method.\
   Get an array of all keys."
  (when (tt-iterinit rdb)
    (loop for key = (tt-iternext rdb)
        while key
        collect key)))

(defmethod tt-values ((rdb RDB))
  "Hash-compatible method.\
   Get an array of all keys."
  (when (tt-iterinit rdb)
    (loop for key = (tt-iternext rdb)
        while key
        for value = (tt-get rdb key)
        while value
        collect value)))

(defconstant *ITLEXICAL*       0 "index type: lexical string")
(defconstant *ITDECIMAL*       1 "index type: decimal string")
(defconstant *ITTOKEN*         2 "index type: token inverted index")
(defconstant *ITQGRAM*         3 "index type: q-gram inverted index")
(defconstant *ITOPT*        9998 "index type: optimize")
(defconstant *ITVOID*       9999 "index type: void")
(defconstant *ITKEEP* (ash 1 24) "index type: keep existing index")

;;; This class inherits the class 'RDB'.  All methods are specific to servers of the table database.
(defclass RDBTBL (RDB)
  ()
  )

(defmethod tt-put ((rdbtbl RDBTBL) pkey cols)
  "Store a record.\
   `<i>pkey</i>' specifies the primary key.\
   `<i>cols</i>' specifies a hash containing columns.\
   If successful, the return value is true, else, it is false.\
   If a record with the same key exists in the database, it is overwritten."
  (unless (hash-table-p cols)
    (error "~S is not a valid argument to TT-PUT." cols))
  (if (tt-misc rdbtbl "put" (cons pkey (hash-table-to-key-value-list cols)) 0)
      t
    nil))

(defmethod tt-putkeep ((rdbtbl RDBTBL) pkey cols)
  "Store a new record.\
   `<i>pkey</i>' specifies the primary key.\
   `<i>cols</i>' specifies a hash containing columns.\
   If successful, the return value is true, else, it is false.\
   If a record with the same key exists in the database, this method has no effect."
  (unless (hash-table-p cols)
    (error "~S is not a valid argument to TT-PUTKEEP." cols))
  (if (tt-misc rdbtbl "putkeep" (cons pkey (hash-table-to-key-value-list cols)) 0)
      t
    (when (eq (ecode rdbtbl) *EMISC*)
      (setf (ecode rdbtbl) *EKEEP*)
      nil)))

(defmethod tt-putcat ((rdbtbl RDBTBL) pkey cols)
  "Concatenate columns of the existing record.\
   `<i>pkey</i>' specifies the primary key.\
   `<i>cols</i>' specifies a hash containing columns.\
   If successful, the return value is true, else, it is false.\
   If there is no corresponding record, a new record is created."
  (unless (hash-table-p cols)
    (error "~S is not a valid argument to TT-PUTCAT." cols))
  (if (tt-misc rdbtbl "putcat" (cons pkey (hash-table-to-key-value-list cols)) 0)
      t
    nil))

(defmethod tt-out  ((rdbtbl RDBTBL) pkey)
  "Remove a record.\
   `<i>pkey</i>' specifies the primary key.\
   If successful, the return value is true, else, it is false."
  (if (tt-misc rdbtbl "out" (list pkey) 0)
      t
    (when (eq (ecode rdbtbl) *EMISC*)
      (setf (ecode rdbtbl) *ENOREC*)
      nil)))

(defmethod tt-get ((rdbtbl RDBTBL) pkey)
  "Retrieve a record.\
   `<i>pkey</i>' specifies the primary key.\
   If successful, the return value is a hash of the columns of the corresponding record.  `nil' is returned if no record corresponds."
  (let ((rv (tt-misc rdbtbl "get" (list pkey))))
    (if rv
        (loop with cols = (make-hash-table :test 'equal)
            for l on rv by #'cddr
            do (setf (gethash (first l) cols) (second l))
            finally (return cols))
      (when (eq (ecode rdbtbl) *EMISC*)
        (setf (ecode rdbtbl) *ENOREC*)
        nil))))

(defmethod tt-mget ((rdbtbl RDBTBL) recs)
  "Retrieve records.\
   `<i>recs</i>' specifies a hash containing the retrieval keys.  As a result of this method, keys existing in the database have the corresponding columns and keys not existing in the database are removed.\
   If successful, the return value is the number of retrieved records or -1 on failure.\
   Due to the protocol restriction, this method can not handle records with binary columns including the '\0' chracter."
  (let ((rv (call-next-method)))
    (if (< rv 0)
        -1
      (progn
        (loop for pkey being the hash-key using (hash-value value) of recs
            for cols = (make-hash-table :test 'equal)
            for cary = (excl:split-re " " value)
            do (progn
                 (loop for l on cary by #'cddr
                     do (setf (gethash (first l) cols) (second l)))
                 (setf (gethash pkey recs) cols)))
        rv))))

(defmethod tt-setindex ((rdbtbl RDBTBL) name type)
  "Set a column index.\
   `<i>name</i>' specifies the name of a column.  If the name of an existing index is specified, the index is rebuilt.  An empty string means the primary key.\
   `<i>type</i>' specifies the index type: `*ITLEXICAL*' for lexical string, `*ITDECIMAL*' for decimal string, `*ITTOKEN*' for token inverted index, `*ITQGRAM*' for q-gram inverted index.  If it is `*ITOPT*', the index is optimized.  If it is `*ITVOID*', the index is removed.  If `*ITKEEP*' is added by bitwise-or and the index exists, this method merely returns failure.\
   If successful, the return value is true, else, it is false."
  (let ((rv (tt-misc rdbtbl "setindex" (list name (write-to-string type)) 0)))
    (if rv
        t
      nil)))

(defmethod tt-genuid ((rdbtbl RDBTBL))
  "Generate a unique ID number.\
   The return value is the new unique ID number or -1 on failure."
  (let ((rv (tt-misc rdbtbl "genuid" '() 0)))
    (if rv
        (car rv)
      -1)))

(defconstant *QCSTREQ*           0 "query condition: string is equal to")
(defconstant *QCSTRINC*          1 "query condition: string is included in")
(defconstant *QCSTRBW*           2 "query condition: string begins with")
(defconstant *QCSTREW*           3 "query condition: string ends with")
(defconstant *QCSTRAND*          4 "query condition: string includes all tokens in")
(defconstant *QCSTROR*           5 "query condition: string includes at least one token in")
(defconstant *QCSTROREQ*         6 "query condition: string is equal to at least one token in")
(defconstant *QCSTRRX*           7 "query condition: string matches regular expressions of")
(defconstant *QCNUMEQ*           8 "query condition: number is equal to")
(defconstant *QCNUMGT*           9 "query condition: number is greater than")
(defconstant *QCNUMGE*          10 "query condition: number is greater than or equal to")
(defconstant *QCNUMLT*          11 "query condition: number is less than")
(defconstant *QCNUMLE*          12 "query condition: number is less than or equal to")
(defconstant *QCNUMBT*          13 "query condition: number is between two tokens of")
(defconstant *QCNUMOREQ*        14 "query condition: number is equal to at least one token in")
(defconstant *QCFTSPH*          15 "query condition: full-text search with the phrase of")
(defconstant *QCFTSAND*         16 "query condition: full-text search with all tokens in")
(defconstant *QCFTSOR*          17 "query condition: full-text search with at least one token in")
(defconstant *QCFTSEX*          18 "query condition: full-text search with the compound expression of")
(defconstant *QCNEGATE* (ash 1 24) "query condition: negation flag")
(defconstant *QCNOIDX*  (ash 1 25) "query condition: no index flag")
(defconstant *QOSTRASC*          0 "order type: string ascending")
(defconstant *QOSTRDESC*         1 "order type: string descending")
(defconstant *QONUMASC*          2 "order type: number ascending")
(defconstant *QONUMDESC*         3 "order type: number descending")
(defconstant *MSUNION*           0 "set operation type: union")
(defconstant *MSISECT*           1 "set operation type: intersection")
(defconstant *MSDIFF*            2 "set operation type: difference")

;;; This class is a helper for the class "RDBTBL".
(defclass RDBQRY (RDBTBL)
  ((rdb   :accessor rdb   :initform nil)
   (args  :accessor args  :initform nil)
   (hint  :accessor hint  :initform nil)
   )
  )

(defmethod tt-qry-initialize ((rdbqry RDBQRY) (rdbtbl RDBTBL))
  "Create a query object.\
   `<i>rdb</i>' specifies the remote database object.\
   The return value is the new query object."
  (unless (eq (type-of rdbtbl) 'RDBTBL)
    (error "~S is not a valid argument to tt-qry-initialize." rdbtbl))
  (setf (rdb  rdbqry) rdbtbl)
  (setf (args rdbqry) '("hint"))
  rdbqry)

(defmethod tt-qry-addcond ((rdbqry RDBQRY) name op expr)
  "Add a narrowing condition.\
   `<i>name</i>' specifies the name of a column.  An empty string means the primary key.\
   `<i>op</i>' specifies an operation type: `*QCSTREQ*' for string which is equal to the expression, `*QCSTRINC*' for string which is included in the expression, `*QCSTRBW*' for string which begins with the expression, `*QCSTREW*' for string which ends with the expression, `*QCSTRAND*' for string which includes all tokens in the expression, `*QCSTROR*' for string which includes at least one token in the expression, `*QCSTROREQ*' for string which is equal to at least one token in the expression, `*QCSTRRX*' for string which matches regular expressions of the expression, `*QCNUMEQ*' for number which is equal to the expression, `*QCNUMGT*' for number which is greater than the expression, `*QCNUMGE*' for number which is greater than or equal to the expression, `*QCNUMLT*' for number which is less than the expression, `*QCNUMLE*' for number which is less than or equal to the expression, `*QCNUMBT*' for number which is between two tokens of the expression, `*QCNUMOREQ*' for number which is equal to at least one token in the expression, `*QCFTSPH*' for full-text search with the phrase of the expression, `*QCFTSAND*' for full-text search with all tokens in the expression, `*QCFTSOR*' for full-text search with at least one token in the expression, `*QCFTSEX*' for full-text search with the compound expression.  All operations can be flagged by bitwise-or: `*QCNEGATE*' for negation, `*QCNOIDX*' for using no index.\
   `<i>expr</i>' specifies an operand exression.\
   The return value is always `nil'."
  (push (string-join-by-nullchar "addcond" name op expr) (args rdbqry))
  nil)

(defmethod tt-qry-setorder ((rdbqry RDBQRY) name &optional (type *QOSTRASC*))
  "Set the order of the result.\
   `<i>name</i>' specifies the name of a column.  An empty string means the primary key.\
   `<i>type</i>' specifies the order type: `*QOSTRASC*' for string ascending, `*QOSTRDESC*' for string descending, `*QONUMASC*' for number ascending, `*QONUMDESC*' for number descending.  If it is not defined, `*QOSTRASC*' is specified.\
   The return value is always `nil'."
  (push (string-join-by-nullchar "setorder" name type) (args rdbqry))
  nil)

(defmethod tt-qry-setlimit ((rdbqry RDBQRY) &optional (max -1) (skip -1))
  "Set the maximum number of records of the result.\
   `<i>max</i>' specifies the maximum number of records of the result.  If it is not defined or negative, no limit is specified.\
   `<i>skip</i>' specifies the maximum number of records of the result.  If it is not defined or not more than 0, no record is skipped.\
   The return value is always `nil'."
  (push (string-join-by-nullchar "setlimit" max skip) (args rdbqry))
  nil)

(defmethod tt-qry-search ((rdbqry RDBQRY))
  "Execute the search.\
   The return value is an array of the primary keys of the corresponding records.  This method does never fail.  It returns an empty array even if no record corresponds."
  (setf (hint rdbqry) "")
  (let ((rv (tt-misc (rdb rdbqry) "search" (args rdbqry) *MONOULOG*)))
    (when rv
      (tt-qry-popmeta rdbqry rv))))

(defmethod tt-qry-searchout ((rdbqry RDBQRY))
  "Remove each corresponding record.\
   If successful, the return value is true, else, it is false."
  (setf (hint rdbqry) "")
  (let ((rv (tt-misc (rdb rdbqry) "search" (cons "out" (args rdbqry)) 0)))
    (and rv (tt-qry-popmeta rdbqry rv) t)))

(defmethod tt-qry-searchget ((rdbqry RDBQRY) &optional (names nil))
  "Get records corresponding to the search.\
   `<i>names</i>' specifies an array of column names to be fetched.  An empty string means the primary key.  If it is not defined, every column is fetched.\
   The return value is an array of column hashes of the corresponding records.  This method does never fail.  It returns an empty list even if no record corresponds.\
   Due to the protocol restriction, this method can not handle records with binary columns including the '\0' chracter."
   (when (and names (not (listp names)))
     (error "~S is not a valid argument to tt-qry-searchget." names))
   (setf (hint rdbqry) "")
   (let ((rv (tt-misc (rdb rdbqry)
                      "search"
                      (cons (if names
                                (apply #'string-join-by-nullchar (cons "get" names))
                              "get")
                            (args rdbqry))
                      *MONOULOG*)))
     (when rv
       (loop for str in (tt-qry-popmeta rdbqry rv)
           collect (loop with cols = (make-hash-table :test 'equal)
                       with cary = (excl:split-re " " str)
                       for l on cary by #'cddr
                       do (setf (gethash (first l) cols) (second l))
                       finally (return cols))))))

(defmethod tt-qry-searchcount ((rdbqry RDBQRY))
  "Get the count of corresponding records.\
   The return value is the count of corresponding records or 0 on failure."
  (setf (hint rdbqry) "")
  (let ((rv (tt-misc (rdb rdbqry) "search" (cons "count" (args rdbqry)) *MONOULOG*)))
    (or (when rv
          (let ((rv (tt-qry-popmeta rdbqry rv)))
            (when rv
              (parse-integer (car rv)))))
        0)))

(defmethod tt-qry-metasearch ((rdbqry RDBQRY) others &optional (type *MSUNION*))
  "Retrieve records with multiple query objects and get the set of the result.\
   `<i>others</i>' specifies an array of the query objects except for the self object.\
   `<i>type</i>' specifies a set operation type: `*MSUNION*' for the union set, `*MSISECT*' for the intersection set, `*MSDIFF*' for the difference set.  If it is not defined, `*MSUNION*' is specified.\
   The return value is an array of the primary keys of the corresponding records.  This method does never fail.  It returns an empty array even if no record corresponds.\
   If the first query object has the order setting, the result array is sorted by the order."
  (unless (listp others)
    (error "~S is not a valid argument to tt-qry-metasearch." others))
  (setf (hint rdbqry) "")
  (let ((rv (tt-misc (rdb rdbqry) "metasearch"
                     (cons (string-join-by-nullchar "mstype" type)
                           (append (loop for other in others
                                       when (eq (type-of other) 'RDBQRY)
                                       append (cons "next" (args other)))
                                   (args rdbqry)))
                     *MONOULOG*)))
    (when rv
      (tt-qry-popmeta rdbqry rv))))

;;; NOTE my tt-qry-popmeta work without a side-effect,
;;; so I guess more proper name is tt-qry-remove-meta.

(defmethod tt-qry-popmeta ((rdbqry RDBQRY) res)
  "Pop meta data from the result list."
  (let ((re (excl:compile-re "^  \\[\\[HINT\\]\\]$" :multiple-lines t)))
    (loop for pkey in res
        append (if (excl:match-re re pkey)
                   (progn
                     (setf (hint rdbqry) (excl:replace-re pkey re ""))
                     nil)
                 (list pkey)))))
