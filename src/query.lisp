(in-package :mqi)

(defun relation-from-table-class (relation)
  (mito.util:ensure-class
   (from-table
    (relation-from relation))))

(defun take-all (relation
                 &key ((:connection mito:*connection*) mito:*connection*)
                 &aux (relation (ensure-relation relation)))
  (let ((class (relation-from-table-class relation))
        (sql (relation-to-sql relation)))
    (values (mito.dao:select-by-sql class sql))))

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

(defun count (relation
              &key ((:connection mito:*connection*) mito:*connection*)
              &aux (relation (ensure-relation relation)))
  (let ((sql (relation-to-sql
              (apply-count relation))))
    (getf (car (mito:retrieve-by-sql sql))
          :--mqi-count)))

(defun maximum (column relation
                &key ((:connection mito:*connection*) mito:*connection*)
                &aux (relation (ensure-relation relation)))
  (let ((sql (relation-to-sql
              (apply-maximum column relation))))
    (getf (car (mito:retrieve-by-sql sql))
          :--mqi-max)))

(defun pluck (columns relation
              &key ((:connection mito:*connection*) mito:*connection*)
              &aux (single-p (not (consp columns)))
                   (relation (ensure-relation relation)))
  (let ((sql (relation-to-sql
              (apply-select columns relation))))
    (if single-p
        (loop for row in (mito:retrieve-by-sql sql)
           collect (cadr row))
        (loop for row in (mito:retrieve-by-sql sql)
           collect
             (loop for (column value) on row by #'cddr
                collect value)))))
