(in-package :mqi-test)

(plan nil)

;;
;; FROM clause

(subtest "RELATION-FROM"
  (let ((relation '((:limit 1)
                    (:from foo))))
    (is (mqi::relation-from relation)
        '(:from foo))))

(subtest "FROM-TABLE"
  (is (mqi::from-table '(:from foo))
      'foo))

;;
;; JOINS clause

;;
;; WHERE clause

(subtest "RELATION-WHERE"
  (let ((relation '((:limit 1)
                    (:where (:= :foo 1)))))
    (is (mqi::relation-where relation)
        '(:where (:= :foo 1)))))

(subtest "WHERE-CONDITION"
  (let ((clause '(:where (:= :foo 1))))
    (is (mqi::where-condition clause)
        '(:= :foo 1))))

(subtest "APPLY-WHERE"
  (let ((relation (mqi:apply-where '(:= :foo 1) 'foo)))
    (is (mqi::relation-where relation)
        '(:where (:= :foo 1))))
  (let ((relation (mqi:apply-where '(:= :bar 2)
                   (mqi:apply-where '(:= :foo 1) 'foo))))
    (is (mqi::relation-where relation)
        '(:where (:and (:= :foo 1) (:= :bar 2))))))

(subtest "WHERE"
  (let ((relation (mqi:where (:= :foo 1) 'foo)))
    (is (mqi::relation-where relation)
        '(:where (:= :foo 1))))
  (let ((relation (mqi:where (:= :bar 2)
                   (mqi:where (:= :foo 1) 'foo))))
    (is (mqi::relation-where relation)
        '(:where (:and (:= :foo 1) (:= :bar 2)))))
  (let ((relation (mqi:where (:and (:= :foo 1)
                                   (:= :bar 2)) 'foo)))
    (is (mqi::relation-where relation)
        '(:where (:and (:= :foo 1) (:= :bar 2))))))

;;
;; SELECT clause

(subtest "RELATION-SELECT"
  (let ((r '((:select :age)
             (:from foo))))
    (is (mqi::relation-select r)
        '(:select :age))))

(subtest "SELECT-COLUMNS"
  (let ((c '(:select :age)))
    (is (mqi::select-columns c)
        '(:age)))
  (let ((c '(:select :name :age)))
    (is (mqi::select-columns c)
        '(:name :age))))

(subtest "APPLY-SELECT"
  (let ((r (mqi:apply-select :age 'foo)))
    (is (mqi::relation-select r)
        '(:select :age)))
  (let ((r (mqi:apply-select '(:name :age) 'foo)))
    (is (mqi::relation-select r)
        '(:select :name :age)))
  (let ((r (mqi:apply-select :age
            (mqi:apply-select :name 'foo))))
    (is (mqi::relation-select r)
        '(:select :age))))

(subtest "SELECT"
  (let ((r (mqi:select :age 'foo)))
    (is (mqi::relation-select r)
        '(:select :age)))
  (let ((r (mqi:select (:name :age) 'foo)))
    (is (mqi::relation-select r)
        '(:select :name :age)))
  (let ((r (mqi:select :age
            (mqi:select :name 'foo))))
    (is (mqi::relation-select r)
        '(:select :age))))

;;
;; LIMIT clause

(subtest "RELATION-LIMIT"
  (let ((relation '((:limit 1)
                    (:from foo))))
    (is (mqi::relation-limit relation)
        '(:limit 1))))

(subtest "LIMIT-NUM"
  (let ((clause '(:limit 1)))
    (is (mqi::limit-num clause)
        1)))

(subtest "APPLY-LIMIT"
  (let ((relation (mqi:apply-limit 1 'foo)))
    (is (mqi::relation-limit relation)
        '(:limit 1)))
  (let ((relation (mqi:apply-limit 2
                   (mqi:apply-limit 1 'foo))))
    (is (mqi::relation-limit relation)
        '(:limit 2))))

(subtest "LIMIT"
  (let ((relation (mqi:limit 1 'foo)))
    (is (mqi::relation-limit relation)
        '(:limit 1)))
  (let ((relation (mqi:limit 2
                   (mqi:limit 1 'foo))))
    (is (mqi::relation-limit relation)
        '(:limit 2))))

;;
;; LOCK clause

(subtest "RELATION-LOCK"
  (let ((relation '((:lock)
                    (:from foo))))
    (is (mqi::relation-lock relation)
        '(:lock))))

(subtest "APPLY-LOCK"
  (let ((relation (mqi:apply-lock 'foo)))
    (is (mqi::relation-lock relation)
        '(:lock)))
  (let ((relation (mqi:apply-lock
                   (mqi:apply-lock 'foo))))
    (is (count :lock relation :key #'car)
        1)))

(subtest "LOCK"
  (let ((relation (mqi:lock 'foo)))
    (is (mqi::relation-lock relation)
        '(:lock))))

;;
;; COMPUTE clause

(subtest "RELATION-COMPUTE"
  (let ((relation '((:compute :maximum :age)
                    (:from foo))))
    (is (mqi::relation-compute relation)
        '(:compute :maximum :age))))

(subtest "COMPUTE-TAG"
  (let ((clause '(:compute :count)))
    (is (mqi::compute-tag clause)
        :count)))

(subtest "APPLY-COUNT"
  (let ((relation (mqi::apply-count 'foo)))
    (is (mqi::relation-compute relation)
        '(:compute :count)))
  (let ((relation (mqi::apply-count
                   (mqi::apply-maximum :age 'foo))))
    (is (length relation)
        2)
    (is (mqi::relation-compute relation)
        '(:compute :count))))

(subtest "COMPUTE-MAXIMUM-COLUMN"
  (let ((clause '(:compute :maximum :age)))
    (is (mqi::compute-maximum-column clause)
        :age)))

(subtest "APPLY-MAXIMUM"
  (let ((relation (mqi::apply-maximum :age 'foo)))
    (is (mqi::relation-compute relation)
        '(:compute :maximum :age)))
  (let ((relation (mqi::apply-maximum :height
                   (mqi::apply-maximum :age 'foo))))
    (is (mqi::relation-compute relation)
        '(:compute :maximum :height)))
  (let ((relation (mqi::apply-maximum :age
                   (mqi::apply-count 'foo))))
    (is (length relation)
        2)
    (is (mqi::relation-compute relation)
        '(:compute :maximum :age))))

;;
;; ORDER clause

(subtest "RELATION-ORDER"
  (let ((r '((:order :age)
             (:from foo))))
    (is (mqi::relation-order r)
        '(:order :age))))

(subtest "ORDER-COLUMNS"
  (let ((c '(:order :age)))
    (is (mqi::order-columns c)
        '(:age)))
  (let ((c '(:order :name (:desc :age))))
    (is (mqi::order-columns c)
        '(:name (:desc :age)))))

(subtest "APPLY-ORDER"
  (let ((r (mqi:apply-order :age 'foo)))
    (is (mqi::relation-order r)
        '(:order :age)))
  (let ((r (mqi:apply-order '(:name :age) 'foo)))
    (is (mqi::relation-order r)
        '(:order :name :age)))
  (let ((r (mqi:apply-order :age
            (mqi:apply-order :name 'foo))))
    (is (mqi::relation-order r)
        '(:order :name :age))))

(subtest "ORDER"
  (let ((r (mqi:order :age 'foo)))
    (is (mqi::relation-order r)
        '(:order :age)))
  (let ((r (mqi:order (:name :age) 'foo)))
    (is (mqi::relation-order r)
        '(:order :name :age)))
  (let ((r (mqi::order :age
            (mqi::order :name 'foo))))
    (is (mqi::relation-order r)
        '(:order :name :age))))

(finalize)
