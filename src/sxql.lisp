(in-package :mqi.sxql)

;;
;; SXQL function interface

(defun expand-op (object)
  (if (and (listp object)
           (keywordp (car object)))
      (apply #'sxql::make-op (car object) (mapcar #'expand-op (cdr object)))
      object))

(defun expand-expression (expressions)
  (cond
    ((not (listp expressions)) expressions)
    ((and (symbolp (car expressions))
          (not (keywordp (car expressions))))
     expressions)
    (t (mapcar #'expand-op expressions))))

(defun make-from-clause (&rest statements)
  (apply #'sxql:make-clause :from (mapcar #'expand-op statements)))

(defun make-join-clause (table &key (kind :inner) on using)
  (apply #'sxql:make-clause :join (expand-op table)
                            :kind kind
                            `(,@(when on
                                  `(:on ,(if (and (listp on)
                                                  (keywordp (car on)))
                                             (expand-op on)
                                             on)))
                              ,@(when using
                                  `(:using ,using)))))

(defun make-where-clause (expression)
  (sxql:make-clause :where (if (and (listp expression)
                                    (keywordp (car expression)))
                               (expand-op expression)
                               expression)))

(defun make-order-by-clause (&rest expressions)
  (apply #'sxql:make-clause :order-by (expand-expression expressions)))

(defun make-limit-clause (count1 &optional count2)
  (sxql:limit count1 count2))

(defun make-for-update-clause ()
  (sxql:make-clause :for-update))

(defun make-select-statement (fields &rest clauses)
  (let ((fields1 (optima:match fields
                   ((or (list* (type keyword) _)
                        (list* (list* (type keyword) _) _))
                    (apply #'sxql:make-clause :fields
                           (mapcar #'expand-op fields)))
                   ((type keyword) (sxql:fields fields))
                   ((or (type symbol)
                        (list* (type symbol) _))
                    (sxql::convert-if-fields-clause fields))
                   (otherwise fields))))
    (apply #'sxql:make-statement :select fields1 clauses)))
