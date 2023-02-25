(in-package :mqi-test)

(plan nil)

(defclass foo () ()
  (:metaclass mito:dao-table-class))

(defclass bar ()
  ((foo :col-type foo))
  (:metaclass mito:dao-table-class))

(defun relation-to-sql-string (relation
                               &aux (relation (mqi::ensure-relation relation)))
  (sxql:yield (mqi::relation-to-sql relation)))

(subtest "FROM clause"
  (is (relation-to-sql-string (mqi::ensure-relation 'foo))
      "SELECT * FROM foo"))

(subtest "JOINS clause"
  (is (relation-to-sql-string (mqi:joins 'bar 'foo))
      "SELECT foo.* FROM foo INNER JOIN bar ON (bar.foo_id = foo.id)")
  (is (relation-to-sql-string (mqi:joins 'foo 'bar))
      "SELECT bar.* FROM bar INNER JOIN foo ON (bar.foo_id = foo.id)")
  (is (relation-to-sql-string (mqi::apply-maximum :x
                               (mqi:joins 'bar 'foo)))
      "SELECT MAX(foo.x) AS __mqi_max FROM foo INNER JOIN bar ON (bar.foo_id = foo.id)"))

(subtest "WHERE clause"
  (is (relation-to-sql-string (mqi:where (:= :foo 1) 'foo))
      "SELECT * FROM foo WHERE (foo = ?)"))

(subtest "ORDER clause"
  )

(subtest "LIMIT clause"
  )

(subtest "LOCK clause"
  )

(subtest "MAXIMUM clause"
  )

(finalize)
