(defpackage mqi.sxql
  (:use :cl)
  (:export :make-from-clause
           :make-join-clause
           :make-where-clause
           :make-order-by-clause
           :make-limit-clause
           :make-for-update-clause
           :make-select-statement))

(defpackage mqi
  (:use :cl)
  (:shadow :count
           :find)
  (:export ;; Relation
           :apply-joins
           :joins
           :apply-where
           :where
           :apply-select
           :select
           :apply-limit
           :limit
           :apply-lock
           :lock
           :apply-order
           :order
           ;; Query
           :take-all
           :take
           :find-by
           :find
           :count
           :maximum
           :pluck
           ;; Util
           :call-with-transaction
           :with-transaction))
