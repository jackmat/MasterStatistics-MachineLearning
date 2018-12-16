# -*- coding: utf-8 -*-
"""
Created on Thu May  3 15:21:53 2018

@author: Carles
"""
import mysql.connector
import pandas as pd

import MySQLdb

# Open database connection
db_connection = MySQLdb.connect(host = '127.0.0.1',
                            port = 3306, 
                            db='masterthesispractice', 
                            user='root', 
                            passwd='')
# prepare a cursor object using cursor() method
db_cursor = db_connection.cursor()

# =============================================================================
# 
# db_connection = mysql.connector.connect(host = '127.0.0.1',
#                             port = 3306, 
#                             database='masterthesispractice', 
#                             user='root', 
#                             password='')
# 
# db_cursor = db_connection.cursor(buffered=True)
# 
# ## Evaluating all variables in the dataset
# =============================================================================
# 1) List all employees, i.e. all tuples in the jbemployee relation. 

query = """            
    Select *
    FROM jbemployee
    
            """
df = pd.read_sql(query, db_connection)
df

# 2) List the name of all departments in alphabetical order. 

query = """            
    Select name
    FROM jbdept
    ORDER BY name ASC
    
            """
df = pd.read_sql(query, db_connection)
df


# 3) What parts are not in store, i.e. qoh = 0? (qoh = Quantity On Hand) 

query = """            
    Select jbparts.name
    FROM jbparts
    WHERE qoh = "0"
    
            """
df = pd.read_sql(query, db_connection)
df


# 4) Which employees have a salary between 9000 (included) and 10000 (included)? 
query = """            
    Select *
    FROM jbemployee
    WHERE SALARY BETWEEN 9000 and 10000
    
            """
df = pd.read_sql(query, db_connection)
df


# 5) What was the age of each employee when they started working (startyear)? 
query = """            
    Select name, (startyear- birthyear) AS startyear
    FROM jbemployee
    
    
            """
df = pd.read_sql(query, db_connection)
df

# 6) Which employees have a last name ending with “son”? 
query = """            
    Select name
    FROM jbemployee
    WHERE name LIKE "%son,%"
    
            """
df = pd.read_sql(query, db_connection)
df

# 7) Which items (note items, not parts) have been delivered by a supplier 
# called Fisher-Price? Formulate this query using a subquery in the where-clause. 

query = """            
    SELECT name
    from jbitem
    where jbitem.supplier =     
    (select id from 
    jbsupplier 
    where name = "Fisher-Price") 
    
            """
df = pd.read_sql(query, db_connection)
df


# 8) Formulate the same query as above, but without a subquery. 

query = """            
    SELECT jbitem.name
    from jbitem, jbsupplier
    where jbsupplier.name = "Fisher-Price"  AND jbitem.supplier = jbsupplier.id     
        
            """
df = pd.read_sql(query, db_connection)
df


# 9) Show all cities that have suppliers located in them. 
# Formulate this query using a subquery in the where-clause. 

query = """            
    SELECT jbcity.name
    FROM jbcity
    WHERE jbcity.id IN 
    (SELECT jbsupplier.city
    from jbsupplier)
             
            """
df = pd.read_sql(query, db_connection)
df



# 10) What is the name and color of the parts that are heavier than a card reader? 
# Formulate this query using a subquery in the where-clause. 
#(The SQL query must not contain the weight as a constant.) 
query = """            
    SELECT jbparts.name, jbparts.color
    FROM jbparts
    WHERE jbparts.weight >
    (SELECT jbparts.weight
    from jbparts
    where jbparts.name = "card reader")
             
            """
df = pd.read_sql(query, db_connection)
df




# 11) Formulate the same query as above, but without a subquery. (The query must not contain the weight as a constant.) 

query = """            
        SELECT A.name,A.color 
        FROM jbparts as A,jbparts as B 
        WHERE A.weight > B.weight 
        AND B.name='card reader';
             
            """
df = pd.read_sql(query, db_connection)
df


# 12) What is the average weight of black parts? 

query = """            
        SELECT AVG(jbparts.weight)
        FROM jbparts 
        WHERE jbparts.color = "black"
             
            """
df = pd.read_sql(query, db_connection)
df


# 13) What is the total weight of all parts that each supplier in Massachusetts (“Mass”) has delivered? 
# Retrieve the name and the total weight for each of these suppliers. 
# Do not forget to take the quantity of delivered parts into account. 
# Note that one row should be returned for each supplier

        
var1, var2, var3 = "jbsupplier", "jbparts", "jbcity"
query = """            
         
        SELECT jbsupplier.name, SUM(jbsupply.quan*jbparts.weight), jbsupplier.city, jbcity.state

        FROM %s, %s, %s, jbsupply
        
        WHERE jbcity.state = "Mass" AND
        jbcity.id = jbsupplier.city AND
        jbparts.id = jbsupply.part AND
        jbsupply.supplier = jbsupplier.id
        
        GROUP BY jbsupplier.id
        
            """ , var1, var2, var3
            
db_cursor.execute(*query)            

df = pd.read_sql(query, db_connection)
df


# =============================================================================
# 14) Create a new relation (a table), with the same attributes as the table items 
# using the CREATE TABLE syntax where you define every attribute explicitly 
# (i.e. not as a copy of another table). 
# Then fill the table with all items that cost less than the average price for items.
# Remember to define primary and foreign keys in your table! 

query = """            
    DROP TABLE IF EXISTS jbitemuavg CASCADE,
    CREATE TABLE jbitemuavg (
    id int,
    name varchar(255),
    dept int NOT NULL,
    price int,
    qoh int,
    supplier int NOT NULL, 
    PRIMARY KEY (id), 
    KEY fk_item_copy_dept (dept), 
    KEY fk_item_copy_supplier (supplier), 
    CONSTRAINT fk_item_copy_dept FOREIGN KEY(dept) REFERENCES jbdept(id), 
    CONSTRAINT fk_item_copy_supplier FOREIGN KEY(supplier) REFERENCES jbsupplier(id))
    
"""
query2 = """
    INSERT INTO jbitemuavg 
    SELECT * FROM jbitem
    WHERE jbitem.price <  (SELECT AVG(price) from jbitem)
    
            """
db_cursor.execute(query, multi=True)    
db_cursor.execute(query2)    
db_connection.commit()
        

# 15) Create a view that contains the items that cost less than the average price 
#for items.  
query = """            
         
        SELECT *
        FROM jbitemuavg
        
        
            """
df = pd.read_sql(query, db_connection)
df



# 16) What is the difference between a table and a view? 
# One is static and the other is dynamic. 
# Which is which and what do we mean by static respectively dynamic? 

# 17) Create a view, using only the implicit join notation, i.e. only use where
# statements but no inner join, right join or left join statements, 
# that calculates the total cost of each debit, by considering price and quantity 
# of each bought item. (To be used for charging customer accounts). 
# The view should contain the sale identifier (debit) and total cost.  

query = """            
         
        SELECT *
        FROM jbitemuavg
        
        
            """
df = pd.read_sql(query, db_connection)
df




# 18) Do the same as in (17), using only the explicit join notation, i.e. using only left, right or inner joins but no where statement. 
# Motivate why you use the join you do (left, right or inner), and why this is the correct one (unlike the others). 
# 19) Oh no! An earthquake!  
# a) Remove all suppliers in Los Angeles from the table jbsupplier. 
# This will not work right away (you will receive error code 23000) which you will have 
# to solve by deleting some other related tuples. However, do not delete more tuples from other tables than necessary and do not change the structure of the tables, 
# i.e. do not remove foreign keys. Also, remember that you are only allowed to use 
# “Los Angeles” as a constant in your queries, not “199” or “900”. 
# b) Explain what you did and why. 
# 
# 20) An employee has tried to find out which suppliers that have delivered items that have been sold. 
# He has created a view and a query that shows the number of items sold from a supplier.
#   mysql> CREATE VIEW jbsale_supply(supplier, item, quantity) AS  -> SELECT jbsupplier.name, jbitem.name, jbsale.quantity      
#   -> FROM jbsupplier, jbitem, jbsale     -> WHERE jbsupplier.id = jbitem.supplier      -> AND jbsale.item = jbitem.id; Query OK, 0 rows affected (0.01 sec) 
#  mysql> SELECT supplier, sum(quantity) AS sum 
#  FROM jbsale_supply     
#  -> GROUP BY supplier; +--------------+---------------+ | supplier     | sum(quantity) | +--------------+---------------+ | Cannon       |             6 | | Levi-Strauss |             1 | | Playskool    |             2 | | White Stag   |             4 | | Whitman's    |             2 | +--------------+---------------+ 5 rows in set (0.00 sec) The employee would also like include the suppliers which has delivered some items, although for whom no items have been sold so far. In other words he wants to list all suppliers, which has supplied any item, as well as the number of these 
# 
# 
# 
# =============================================================================
