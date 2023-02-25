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
  nil)

(defun apply-joins (table relation)
  (setf relation (ensure-relation relation))
  (setf relation (remove :joins relation :key #'car))
  (append `((:joins ,table)) relation))

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
;; LIMIT clause

(defun relation-limit (relation)
  (car (member :limit relation :key #'car)))

(defun limit-num (clause)
  (optima:ematch clause
    ((list :limit n) n)))

(defun apply-limit (num relation)
  (setf relation (ensure-relation relation))
  (setf relation (remove :limit relation :key #'car))
  (append `((:limit ,num)) relation))

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
;; MAXIMUM clause

(defun relation-maximum (relation)
  (car (member :maximum relation :key #'car)))

(defun maximum-column (clause)
  (optima:ematch clause
    ((list :maximum column) column)))

(defun apply-maximum (column relation)
  (setf relation (ensure-relation relation))
  (setf relation (remove :maximum relation :key #'car))
  (append `((:maximum ,column)) relation))

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
