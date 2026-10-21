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
