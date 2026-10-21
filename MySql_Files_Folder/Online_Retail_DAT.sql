CREATE DATABASE IF NOT EXISTS online_retail_analytics;
USE online_retail_analytics;

CREATE TABLE IF NOT EXISTS raw_online_retail (
    InvoiceNo   VARCHAR(30),
    StockCode   VARCHAR(50),
    Description TEXT,
    Quantity    VARCHAR(30),
    InvoiceDate VARCHAR(40),
    UnitPrice   VARCHAR(40),
    CustomerID  VARCHAR(30),
    Country     VARCHAR(100)
);
-- It Is used to remove all recods(raws) of the table
-- TRUNCATE TABLE raw_online_retail;

-- Best way to load large csv data 
LOAD DATA LOCAL INFILE
'C:/Users/vasav/OneDrive/Desktop/DA_PROJECTS/Project_2/online_retail_utf8.csv'
INTO TABLE raw_online_retail
CHARACTER SET utf8mb4
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 LINES
(InvoiceNo, StockCode, Description, Quantity,
 InvoiceDate, UnitPrice, CustomerID, Country);
--  SET GLOBAL local_infile = 1;
--  SHOW GLOBAL VARIABLES LIKE 'local_infile';
 
SELECT * 
 FROM 
 raw_online_retail;
 
 SELECT COUNT(*) AS total_rows
 FROM 
 raw_online_retail;
 
SELECT CustomerID
 FROM 
 raw_online_retail
 ORDER BY CustomerID DESC;
 
--   Verify columns
DESCRIBE raw_online_retail;
