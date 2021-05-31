(in-package :mqi)

(defun call-with-transaction (func)
  (mito:execute-sql "BEGIN")
  (let (success-p)
    (unwind-protect
         (progn
           (funcall func)
           (mito:execute-sql "COMMIT")
           (setf success-p t))
      (unless success-p
        (mito:execute-sql "ROLLBACK")))))

(defmacro with-transaction (&body body)
  `(call-with-transaction #'(lambda () ,@body)))
