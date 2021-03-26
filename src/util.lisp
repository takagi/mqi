(in-package :mqi)

(defun call-with-transaction (func)
  (mito:execute-sql "BEGIN")
  (prog1
      (handler-bind ((error #'(lambda (e)
                                (declare (ignore e))
                                (mito:execute-sql "ROLLBACK"))))
        (funcall func))
    (mito:execute-sql "COMMIT")))

(defmacro with-transaction (&body body)
  `(call-with-transaction #'(lambda () ,@body)))
