(in-package :mqi)

(defun rel-column-name (table column)
  (values
   (intern
    (format nil "~:@(~A.~A~)" table column)
    :keyword)))

(defun find-child-column-slots (class slot)
  (mapcar #'(lambda (slot-name)
              (mito.class.table:find-slot-by-name class slot-name
                                                  :test #'string=))
          (mito.class.table:find-child-columns class slot)))

(defun table-column-referencing (class table-name)
  (check-type table-name symbol)
  (car
   (member table-name
    (mito.class:table-column-slots class)
    :key #'mito.class.column:table-column-type)))

(defun compute-joins-spec (class referenced)
  (let* ((referenced-name (class-name referenced))
         (slot (table-column-referencing class referenced-name))
         (children (find-child-column-slots class slot)))
    (when children
      `(:and ,@(loop for child in children
                  collect
                     `(:= ,(rel-column-name
                            (mito.class.table:table-name class)
                            (mito.class.column:table-column-name child))
                          ,(rel-column-name
                            (mito.class.table:table-name referenced)
                            (c2mop:slot-definition-name
                             (mito.class.column:table-column-references-column
                              child)))))))))

(defun relation-from-table-class (relation)
  (mito.util:ensure-class
   (from-table
    (relation-from relation))))

(defun relation-to-sql (relation)
  (let* ((from (relation-from relation))
         (joins (relation-joins relation))
         (where (relation-where relation))
         (limit (relation-limit relation))
         (lock (relation-lock relation))
         (maximum (relation-maximum relation))
         (order (relation-order relation))
         (class (mito.util:ensure-class (from-table from)))
         (fields :*)
         (clauses (list
                   (mqi.sxql:make-from-clause
                    (sxql:make-sql-symbol 
                     (mito.class.table:table-name class))))))
    (when joins
      (let* ((table (mito.util:ensure-class (joins-table joins)))
             (spec (or (joins-spec joins)
                       (compute-joins-spec class table)
                       (compute-joins-spec table class)))
             (table-name (alexandria:make-keyword
                          (mito.class.table:table-name table))))
        (push (mqi.sxql:make-join-clause table-name :on spec) clauses)
        (setf fields (rel-column-name
                      (mito.class.table:table-name class)
                      "*"))))
    (when where
      (let ((condition (where-condition where)))
        (push (mqi.sxql:make-where-clause condition) clauses)))
    (when order
      (let ((columns (order-columns order)))
        (push (apply #'mqi.sxql:make-order-by-clause columns) clauses)))
    (when limit
      (let ((num (limit-num limit)))
        (push (mqi.sxql:make-limit-clause num) clauses)))
    (when lock
      (push (mqi.sxql:make-for-update-clause) clauses))
    (when maximum
      (let ((column (maximum-column maximum)))
        (when joins
          (setf column (rel-column-name
                        (mito.class.table:table-name class)
                        column)))
        (setf fields `((:as (:max ,column) :mqi_max)))))
    (apply #'mqi.sxql:make-select-statement fields (nreverse clauses))))

(defun take-all (relation
                 &key ((:connection mito:*connection*) mito:*connection*)
                 &aux (relation (ensure-relation relation)))
  (let ((class (relation-from-table-class relation))
        (sql (relation-to-sql relation)))
    (mito.dao:select-by-sql class sql)))

(defun take (relation
             &key ((:connection mito:*connection*) mito:*connection*)
             &aux (relation (ensure-relation relation)))
  (car (take-all (apply-limit 1 relation))))

(defun find-by (fields-and-values relation
                &key ((:connection mito:*connection*) mito:*connection*)
                &aux (relation (ensure-relation relation)))
  (loop for (field value) on fields-and-values by #'cddr
     do (setf relation (apply-where `(:= ,field ,value) relation)))
  (take relation))

(defun find (value relation
             &key ((:connection mito:*connection*) mito:*connection*)
             &aux (relation (ensure-relation relation)))
  (let* ((class (relation-from-table-class relation))
         (primary-key (mito.class:table-primary-key class)))
    (unless (= 1 (length primary-key))
      (error "Composite primary key is not supported. Use FIND-BY instead."))
    (find-by `(,(car primary-key) ,value) relation)))

(defun maximum (column relation
                &key ((:connection mito:*connection*) mito:*connection*)
                &aux (relation (ensure-relation relation)))
  (let ((sql (relation-to-sql
              (apply-maximum column relation))))
    (getf (car (mito:retrieve-by-sql sql))
          :mqi-max)))
