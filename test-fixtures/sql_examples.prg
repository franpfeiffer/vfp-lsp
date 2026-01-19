* SQL Examples in Visual FoxPro
* Demonstrates embedded SQL support

* Simple SELECT
SELECT * FROM customers

* SELECT with specific columns
SELECT id, name, email FROM customers

* SELECT with WHERE clause
SELECT name, total ;
    FROM orders ;
    WHERE status = "ACTIVE"

* SELECT with JOIN
SELECT c.name, o.total, o.orderdate ;
    FROM customers c ;
    INNER JOIN orders o ON c.id = o.custid

* LEFT JOIN
SELECT c.name, ISNULL(o.total, 0) AS total ;
    FROM customers c ;
    LEFT OUTER JOIN orders o ON c.id = o.custid

* Multiple JOINs
SELECT c.name, o.orderdate, p.productname ;
    FROM customers c ;
    INNER JOIN orders o ON c.id = o.custid ;
    INNER JOIN orderitems oi ON o.id = oi.orderid ;
    INNER JOIN products p ON oi.productid = p.id

* SELECT with GROUP BY and HAVING
SELECT custid, COUNT(*) AS ordercount, SUM(total) AS totalspent ;
    FROM orders ;
    GROUP BY custid ;
    HAVING SUM(total) > 1000

* SELECT with ORDER BY
SELECT name, total ;
    FROM orders ;
    ORDER BY total DESC, name ASC

* SELECT INTO CURSOR
SELECT * FROM customers ;
    WHERE active = .T. ;
    INTO CURSOR csrActiveCustomers NOFILTER READWRITE

* SELECT INTO TABLE
SELECT * FROM customers ;
    WHERE region = "WEST" ;
    INTO TABLE WestCustomers

* SELECT INTO ARRAY
SELECT name FROM customers ;
    INTO ARRAY aNames

* SELECT with DISTINCT
SELECT DISTINCT city, state ;
    FROM customers ;
    ORDER BY state, city

* SELECT with TOP
SELECT TOP 10 name, total ;
    FROM orders ;
    ORDER BY total DESC

* SELECT with UNION
SELECT name, "Customer" AS type FROM customers ;
UNION ALL ;
SELECT name, "Vendor" AS type FROM vendors

* SELECT with subquery
SELECT name FROM customers ;
    WHERE id IN (SELECT DISTINCT custid FROM orders WHERE total > 500)

* SELECT with BETWEEN
SELECT * FROM orders ;
    WHERE orderdate BETWEEN {^2024-01-01} AND {^2024-12-31}

* SELECT with LIKE
SELECT * FROM customers ;
    WHERE name LIKE "Smith%"

* SELECT with IS NULL
SELECT * FROM customers ;
    WHERE email IS NULL

* Aggregate functions
SELECT ;
    COUNT(*) AS total_orders, ;
    SUM(total) AS total_amount, ;
    AVG(total) AS avg_amount, ;
    MIN(total) AS min_order, ;
    MAX(total) AS max_order ;
    FROM orders

* INSERT statement
INSERT INTO customers (name, email, phone) ;
    VALUES ("John Doe", "john@example.com", "555-1234")

* INSERT from SELECT
INSERT INTO archive_orders ;
    SELECT * FROM orders WHERE orderdate < {^2023-01-01}

* UPDATE statement
UPDATE customers ;
    SET status = "INACTIVE", ;
        modifieddate = DATETIME() ;
    WHERE lastorderdate < {^2023-01-01}

* UPDATE with subquery
UPDATE orders ;
    SET discount = 0.10 ;
    WHERE custid IN (SELECT id FROM customers WHERE vip = .T.)

* DELETE statement
DELETE FROM orders ;
    WHERE status = "CANCELLED"

* DELETE with subquery
DELETE FROM orderitems ;
    WHERE orderid IN (SELECT id FROM orders WHERE status = "DELETED")

* CREATE TABLE
CREATE TABLE NewCustomers ;
    (id I AUTOINC, ;
     name C(50) NOT NULL, ;
     email C(100), ;
     phone C(20), ;
     active L DEFAULT .T., ;
     created T DEFAULT DATETIME())

* CREATE CURSOR
CREATE CURSOR csrTemp ;
    (field1 C(20), field2 N(10,2), field3 D)

* ALTER TABLE
ALTER TABLE customers ;
    ADD COLUMN website C(100)

ALTER TABLE customers ;
    DROP COLUMN fax

* CREATE INDEX
INDEX ON name TAG name_idx

INDEX ON UPPER(lastname + firstname) TAG fullname_idx

* DROP TABLE
DROP TABLE TempData

RETURN
