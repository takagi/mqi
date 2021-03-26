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

(subtest "SELECT clause"
  (is (relation-to-sql-string (mqi:select :age 'foo))
      "SELECT age FROM foo")
  (is (relation-to-sql-string (mqi:select (:name :age) 'foo))
      "SELECT name, age FROM foo")
  (is (relation-to-sql-string (mqi:select :age
                               (mqi:joins 'bar 'foo)))
      "SELECT foo.age FROM foo INNER JOIN bar ON (bar.foo_id = foo.id)")
  (is (relation-to-sql-string (mqi:select :age
                               (mqi:joins 'foo 'bar)))
      "SELECT bar.age FROM bar INNER JOIN foo ON (bar.foo_id = foo.id)"))

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
  (is (relation-to-sql-string (mqi::apply-maximum :foo 'foo))
      "SELECT MAX(foo) AS __mqi_max FROM foo")
  (is (relation-to-sql-string (mqi::apply-maximum :foo
                               (mqi:joins 'bar 'foo)))
      "SELECT MAX(foo.foo) AS __mqi_max FROM foo INNER JOIN bar ON (bar.foo_id = foo.id)"))

(subtest "COUNT clause"
  (is (relation-to-sql-string (mqi::apply-count 'foo))
      "SELECT COUNT(*) AS __mqi_count FROM foo")
  (is (relation-to-sql-string (mqi::apply-count
                               (mqi:joins 'bar 'foo)))
      "SELECT COUNT(*) AS __mqi_count FROM foo INNER JOIN bar ON (bar.foo_id = foo.id)"))

(finalize)
