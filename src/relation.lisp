(in-package :mqi)

(defun ensure-relation (relation)
  (if (symbolp relation)
      `((:from ,relation))
      relation))

;;
;; FROM clause

(defun relation-from (relation)
  (car (member :from relation :key #'car)))

(defun from-table (clause)
  (optima:match clause
    ((list :from table) table)))

;;
;; JOINS clause

(defun relation-joins (relation)
  (car (member :joins relation :key #'car)))

(defun joins-table (clause)
  (optima:match clause
    ((list :joins table) table)))

(defun joins-spec (clause)
  (declare (ignore clause))
  nil)

(defun apply-joins (table relation)
  (setf relation (ensure-relation relation))
  (append `((:joins ,table))
   (remove :joins relation :key #'car)))

(defmacro joins (table relation)
  `(apply-joins ,table ,relation))

;;
;; WHERE clause

(defun relation-where (relation)
  (car (member :where relation :key #'car)))

(defun where-condition (clause)
  (optima:ematch clause
    ((list :where condition) condition)))

(defun apply-where (condition relation)
  (setf relation (ensure-relation relation))
  (let ((where (relation-where relation)))
    (if where
        (let ((relation0 (remove :where relation :key #'car))
              (condition0 (where-condition where)))
          (append `((:where (:and ,condition0 ,condition))) relation0))
        (append `((:where ,condition)) relation))))

(defun expand-op (x)
  (if (and (listp x)
           (keywordp (car x)))
      `(list ,@(mapcar #'expand-op x))
      x))

(defmacro where (condition relation)
  `(apply-where ,(expand-op condition) ,relation))

;;
;; SELECT clause

(defun relation-select (relation)
  (car (member :select relation :key #'car)))

(defun select-columns (clause)
  (optima:ematch clause
    ((list* :select columns) columns)))

(defun apply-select (columns relation)
  (setf relation (ensure-relation relation))
  (setf columns (alexandria:ensure-list columns))
  (append `((:select ,@columns))
   (remove :select relation :key #'car)))

(defmacro select (columns relation)
  `(apply-select ',columns ,relation))

;;
;; LIMIT clause

(defun relation-limit (relation)
  (car (member :limit relation :key #'car)))

(defun limit-num (clause)
  (optima:ematch clause
    ((list :limit n) n)))

(defun apply-limit (num relation)
  (setf relation (ensure-relation relation))
  (append `((:limit ,num))
   (remove :limit relation :key #'car)))

(defmacro limit (num relation)
  `(apply-limit ,num ,relation))

;;
;; LOCK clause

(defun relation-lock (relation)
  (car (member :lock relation :key #'car)))

(defun apply-lock (relation)
  (setf relation (ensure-relation relation))
  (if (null (relation-lock relation))
      (append `((:lock)) relation)
      relation))

(defmacro lock (relation)
  `(apply-lock ,relation))

;;
;; COMPUTE clause

(defun relation-compute (relation)
  (car (member :compute relation :key #'car)))

(defun compute-tag (clause)
  (optima:ematch clause
    ((list* :compute tag _) tag)))

(defun apply-count (relation)
  (setf relation (ensure-relation relation))
  (append `((:compute :count))
   (remove :compute relation :key #'car)))

(defun compute-maximum-column (clause)
  (optima:ematch clause
    ((list :compute :maximum column) column)))

(defun apply-maximum (column relation)
  (setf relation (ensure-relation relation))
  (append `((:compute :maximum ,column))
   (remove :compute relation :key #'car)))

;;
;; ORDER clause

(defun relation-order (relation)
  (car (member :order relation :key #'car)))

(defun order-columns (clause)
  (optima:ematch clause
    ((list* :order columns) columns)))

(defun apply-order (columns relation)
  (setf relation (ensure-relation relation))
  (setf columns (alexandria:ensure-list columns))
  (let ((order (relation-order relation)))
    (if order
        (let ((relation0 (remove :order relation :key #'car))
              (columns0 (order-columns order)))
          (append `((:order ,@columns0 ,@columns)) relation0))
        (append `((:order ,@columns)) relation))))

(defmacro order (columns relation)
  `(apply-order ',columns ,relation))
