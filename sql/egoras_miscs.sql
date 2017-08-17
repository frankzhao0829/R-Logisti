/*---------------------------------------------------------------------------*\
|                                CPX PHASE 1                                  |
\*---------------------------------------------------------------------------*/

-- Check available Version & Region
SELECT DISTINCT [Version], [Profit Center L0 Name] FROM TestEgoras

-- Row Count by Version + Region of Raw Egoras Data
SELECT [Version]
, [Profit Center L0 Name]
, COUNT(*) FROM Egoras
WHERE [Profit Center L0 Name] = 'EMEA'
GROUP BY [Version]
, [Profit Center L0 Name]

-- Count distinct Sales Order ID/Source System Key
SELECT COUNT(DISTINCT [Sales Order ID]) FROM Egoras WHERE [Profit Center L0 Name] = 'EMEA' AND [Version] = 18
SELECT COUNT(DISTINCT [Sales Order Source System Key]) FROM Egoras WHERE [Profit Center L0 Name] = 'EMEA' AND [Version] = 18

-- Row Count by Version + Region of Deduped Egoras Data
SELECT [Version]
, [Profit Center L0 Name]
, COUNT(*) FROM
[dbo].[Egoras_SHP_DTL]
GROUP BY [Version]
, [Profit Center L0 Name]

-- Check leading 0 for certain columns exists or not
SELECT DISTINCT [Version] FROM Egoras WHERE [Profit Center L0 Name] = 'Americas' AND LEN([Sales Order Line Item ID]) < 6
SELECT DISTINCT [Version] FROM Egoras WHERE [Profit Center L0 Name] = 'Asia Pacific' AND LEN([Shipment ID]) < 10

-- Shipment ID length can be larger than 10 (12, with 0 left padding)
SELECT TOP 100 [Shipment ID] FROM TestEgoras WHERE [Profit Center L0 Name] = 'Americas'  AND [Version] = '17' AND LEN([Shipment ID]) > 10

-- CHECK DISK SPACE USAGE
-- CAN CHECK IT FROM THE OBJECT BROSWER AS WELL
USE [cpx]
GO
sp_spaceused



/*---------------------------------------------------------------------------*\
|                                CPX PHASE 2                                  |
\*---------------------------------------------------------------------------*/

SELECT DISTINCT LEN([SHIPMENT ID]) FROM [dbo].[Egoras_Week36_Week43] WHERE [Profit Center L0 Name] = 'EMEA'

SELECT TOP 100 * FROM [dbo].[Egoras_Week36_Week43] WHERE [Profit Center L0 Name] = 'EMEA'


-- V20 AMS/APJ ROW COUNT DIFFERENT
SELECT * FROM [dbo].[Egoras_SHP_DTL_Week17_Week35] a WHERE [Version] = 20 AND NOT EXISTS (SELECT * FROM [dbo].[Egoras_SHP_DTL] b WHERE
a.[Sales Order Source System Key] = b.[Sales Order Source System Key] AND
a.[Shipment ID] = b.[Shipment ID] AND
a.[Shipment Line Item ID] = b.[Shipment Line Item ID] AND
b.[Version] = 20)
-- 7700196997 | 000160
-- 303014944  | 000250

SELECT * FROM [dbo].[Egoras] WHERE [Shipment ID] = '7700196997' AND [Shipment Line Item ID] = '000160'

SELECT [Profit Center L0 Name], [Version], COUNT(*) FROM [dbo].[Egoras_SHP_DTL] GROUP BY [Profit Center L0 Name], [Version]

---- QADTA INTERSECTION TRAINING & TESTING FOR CUSTOMER REVIEW
---- TARGETING RE TO DEL

-- Check Records of 2016 for Quote data
SELECT [Profit_Center_L0_Name]
, COUNT(*) AS ROW_COUNT
, MAX([DELIVERY]) AS MAX_DEL
, MIN([DELIVERY]) AS MIN_DEL
FROM [cpx].[dbo].[QADTA_Sep_Week_38_2016]
WHERE [DELIVERY] > '2016-01-01'
GROUP BY [Profit_Center_L0_Name]

-- Check the intersection between Quote and Egoras data
SELECT a.[Profit_Center_L0_Name]
, b.[Supplier Complexity Group]
, COUNT(*) AS HLI_COUNT
, COUNT(DISTINCT a.[DG_RE_DE]) AS DG_COUNT
, MAX(a.[DELIVERY]) AS MAX_DEL
, MIN(a.[DELIVERY]) AS MIN_DEL
FROM [cpx].[dbo].[QADTA_Sep_Week_38_2016] a
INNER JOIN
[dbo].[Region_HLI_CLEAN] b ON
a.[Profit_Center_L0_Name] = b.[Profit Center L0 Name] AND
a.[DG_RE_DE] = b.[DG_RE-DE]
WHERE b.[ASAP Flag] = 'Y'
--AND a.[SPECIAL_CODE_QUOTE] > 0
GROUP BY a.[Profit_Center_L0_Name], b.[Supplier Complexity Group]
ORDER BY a.[Profit_Center_L0_Name]

-- Get the Egoras Intersections with QADTA data
SELECT * FROM [dbo].[Region_HLI_CLEAN] aa WHERE EXISTS
(
	SELECT * FROM [cpx].[dbo].[QADTA_Sep_Week_38_2016] a
	INNER JOIN
	[dbo].[Region_HLI_CLEAN] b ON
	a.[Profit_Center_L0_Name] = b.[Profit Center L0 Name] AND
	a.[DG_RE_DE] = b.[DG_RE-DE]
	WHERE b.[ASAP Flag] = 'Y'
	AND b.[Profit Center L0 Name] = 'Asia Pacific'
	AND aa.[Sales Order ID] = b.[Sales Order ID]
	AND aa.[Sales Order Line Item ID] = b.[Sales Order Line Item ID]
)

-------------------------------------------------------------------------------
-- Extract SRT Infomation from QADTA Data
SELECT DISTINCT [Profit_Center_L0_Name] AS [Profit Center L0 Name]
, [DG_RE_DE] AS [DG_RE-DE]
, [SRT]
, [SRT_PADDING_VALUE]
FROM [dbo].[QADTA_Sep_Week_38_2016]
WHERE SRT IS NOT NULL AND SRT_PADDING_VALUE IS NOT NULL


-------------------------------------------------------------------------------
-- Check if there are multiple SRT/SRT_PADDING_VALUE for same
-- [qv SLS_QTN_ID] + [qv SLS_QTN_VRSN_SQN_NR]
SELECT * FROM [dbo].[QADTA_Sep_Week_38_2016] aa WHERE EXISTS
(
SELECT [qv SLS_QTN_VRSN_SQN_NR]
, [qv SLS_QTN_ID]
FROM [dbo].[QADTA_Sep_Week_38_2016] a
WHERE a.[Profit_Center_L0_Name] = aa.[Profit_Center_L0_Name]
AND a.[DG_RE_DE] = aa.[DG_RE_DE]
GROUP BY [qv SLS_QTN_VRSN_SQN_NR]
, [qv SLS_QTN_ID]
HAVING COUNT(DISTINCT SRT) > 1
)

-------------------------------------------------------------------------------
-- Refined SRT/SRT_PADDING_VALUE Extract
WITH CTE AS (
SELECT [Profit_Center_L0_Name]
, [DG_RE_DE]
, [qv SLS_QTN_ID]
, [qv SLS_QTN_VRSN_SQN_NR]
, [SRT]
, [SRT_PADDING_VALUE]
, ROW_NUMBER() OVER (PARTITION BY [Profit_Center_L0_Name]
								, [DG_RE_DE]
								, [qv SLS_QTN_ID]
								ORDER BY [qv SLS_QTN_VRSN_SQN_NR] DESC) AS [ROW_SEQ_NO]
FROM [dbo].[QADTA_Sep_Week_38_2016]
)
SELECT [Profit_Center_L0_Name] AS [Profit Center L0 Name]
, [DG_RE_DE] AS [DG_RE-DE]
, [SRT]
, [SRT_PADDING_VALUE], [SRT] + [SRT_PADDING_VALUE] AS [QUOTED_EDT]
FROM CTE WHERE [ROW_SEQ_NO] = 1
-------------------------------------------------------------------------------
-- Dedup multi-version sales orders
-- Sales Order ID & SLS_QTN_ID are almost one to one mapping (with 1 exception)
WITH CTE AS (
SELECT t.*
, ROW_NUMBER() OVER (PARTITION BY [Profit_Center_L0_Name]
								, [DG_RE_DE]
								, [qv SLS_QTN_ID]
								ORDER BY [qv SLS_QTN_VRSN_SQN_NR] DESC) AS [ROW_SEQ_NO]
FROM [dbo].[QADTA_Sep_Week_38_2016] t
)
SELECT * FROM CTE WHERE [ROW_SEQ_NO] = 1

-------------------------------------------------------------------------------
-- Extract DG in QADTA but not in Egoras for Q3 & Q4 in FY16
SELECT [Profit_Center_L0_Name]
, [DG_RE_DE]
FROM [dbo].[QADTA_Sep_Week_38_2016] aa 
WHERE NOT EXISTS
(
SELECT * FROM [dbo].[Egoras_SHP_DTL] bb 
WHERE aa.[Profit_Center_L0_Name] = bb.[Profit Center L0 Name]
AND aa.[DG_RE_DE] = bb.[DG_RE-DE]
)
AND ([Delivery_Quarter] = 'Q3' OR [Delivery_Quarter] = 'Q4')
AND [Delivery_Fiscal_Month] like '%16'
ORDER BY 1, 2

-------------------------------------------------------------------------------
-- Extract DG in Egoras but not in QADTA Data
SELECT DISTINCT [Profit Center L0 Name], [DG_RE-DE]
FROM [dbo].[Region_HLI_CLEAN] aa
WHERE NOT EXISTS
(
SELECT * FROM [dbo].[QADTA_Sep_Week_38_2016] bb
WHERE bb.[Profit_Center_L0_Name] = aa.[Profit Center L0 Name]
AND bb.[DG_RE_DE] = aa.[DG_RE-DE]
)
ORDER BY 1, 2

-------------------------------------------------------------------------------
-- Check RE-DE Distribution by Suppler + Plant Code
SELECT DISTINCT [Profit Center L0 Name]
, [Supplier]
, [Plant Code]
, COUNT(*) AS [RowCount]
, AVG([RE to Del]) AS [avREtoDel]
, STDEVP([RE to Del]) AS [sdREtoDel]
FROM cpx.[dbo].[Region_HLI_CLEAN]
WHERE [RE to Del] IS NOT NULL
GROUP BY [Profit Center L0 Name]
, [Supplier]
, [Plant Code]
ORDER BY 1, 2, 5

-- Check the Egoras Line Item ID to Topmost Line Item ID(HLI ID) Mappings
SELECT TOP 100 * FROM [dbo].[Egoras_HLI_Mappings]
SELECT DISTINCT LEN([Topmost Parent Line Item ID]) FROM [dbo].[Egoras_HLI_Mappings]

-------------------------------------------------------------------------------
SELECT TOP 100 * FROM [dbo].[QADTA_Sep_Week_38_2016]

SELECT COUNT(*) FROM [dbo].[QADTA_Sep_Week_38_2016]

SELECT COUNT(*) FROM
(SELECT DISTINCT [Profit_Center_L0_Name], [DG_RE_DE] FROM [dbo].[QADTA_Sep_Week_38_2016]) a

-------------------------------------------------------------------------------
WITH QADTA_INTERSECT AS (
SELECT a.* FROM [cpx].[dbo].[QADTA_Sep_Week_38_2016] a
WHERE EXISTS (
SELECT * FROM [dbo].[Region_HLI_CLEAN] b
WHERE a.[Profit_Center_L0_Name] = b.[Profit Center L0 Name] 
AND a.[DG_RE_DE] = b.[DG_RE-DE]
AND b.[ASAP Flag] = 'Y'
AND b.[Profit Center L0 Name] = 'Americas'
)
)
SELECT Delivery_Quarter
, COUNT(DISTINCT SALES_ORDER_ID)
FROM QADTA_INTERSECT
GROUP BY Delivery_Quarter


/*---------------------------------------------------------------------------*\
|                                CPX PHASE 3                                  |
\*---------------------------------------------------------------------------*/

SELECT * FROM [dbo].[HPE_Quote_Item_Detail] WHERE qvsls_qtn_id in (
SELECT [qvSLS_QTN_ID] FROM [dbo].[HPE_Quote_Headers] WHERE [ORIG_ASSET] = 'Watson')
AND [qiSPLR_RSPNS_TM] = 'UP'
--AND qvsls_qtn_id = '220075342'
--ORDER BY qvsls_qtn_id, CAST(sqiSLS_QTN_ITM_SQN_NR AS INT)

-------------------------------------------------------------------------------
-- Check R Services in SQL Server
use cpx
exec sp_execute_external_script  @language =N'R',  
@script=N'OutputDataSet<-InputDataSet',    
@input_data_1 =N'select 1 as hello'  
with result sets (([hello] int not null));  
go  

-- Enable external scripts
sp_configure 'external scripts enabled', 1;  
RECONFIGURE;

GO
INSERT INTO [dbo].[EMEAShippingPointMappings]
VALUES ('0198', '5200', 'NonEMEASupplier')
, ('0153', '5200', 'NonEMEASupplier')
, ('0157', '5200', 'NonEMEASupplier')
, ('0154', '32FN', 'NonEMEASupplier')
, ('G100', 'G142', 'OEM-CZ')
GO

SELECT DISTINCT [Sales Order Source System Key] FROM [dbo].[Region_HLI_CLEAN] WHERE [Profit Center L0 Name] = 'Americas' AND [Focus Account Group] LIKE 'FA%'

SELECT MIN([DELIVERY]), MAX([DELIVERY]) FROM [dbo].[QADTA_Sep_Week_38_2016]
SELECT MIN([DELIVERY]), MAX([DELIVERY]) FROM [dbo].[QADTA_Jan_Week_15_2017]

SELECT COUNT(*) FROM (
SELECT DG_RE_DE, COUNT(*) AS ITEMS_COUNT, MIN([DELIVERY]) AS DELIVERY_DATE FROM [dbo].[QADTA_Sep_Week_38_2016]
GROUP BY DG_RE_DE HAVING MIN(DELIVERY) >= CONVERT(DATETIME, '2015-09-28', 120)
EXCEPT
SELECT DG_RE_DE, COUNT(*) AS ITEMS_COUNT, MIN([DELIVERY]) AS DELIVERY_DATE FROM [dbo].[QADTA_Jan_Week_15_2017]
GROUP BY DG_RE_DE HAVING MIN(DELIVERY) < CONVERT(DATETIME, '2016-09-18', 120)
--EXCEPT
--SELECT DG_RE_DE, COUNT(*) AS ITEMS_COUNT, MIN([DELIVERY]) AS DELIVERY_DATE FROM [dbo].[QADTA_Sep_Week_38_2016]
--GROUP BY DG_RE_DE HAVING MIN(DELIVERY) >= CONVERT(DATETIME, '2015-09-28', 120)
) a

-- Check which Hashvalue will cause different Lead Time
SELECT [Region], [ProductID], COUNT(DISTINCT([LeadTime])), MAX([SKUHashValue]) FROM [dbo].[SKU_Lead_Time_20170320] GROUP BY [Region], [ProductID]
HAVING COUNT(DISTINCT([LeadTime])) > 1

-- Add computation columns to separate Product ID and Localization Hash Value
ALTER TABLE [cpx].[dbo].[SKU_Lead_Time_20170320] ADD ProductID AS (CASE WHEN CHARINDEX('#', [ProductSKU]) > 0 THEN LEFT([ProductSKU], CHARINDEX('#', [ProductSKU]) - 1) ELSE [ProductSKU] END)
ALTER TABLE [cpx].[dbo].[SKU_Lead_Time_20170320] ADD SKUHashValue AS (CASE WHEN CHARINDEX('#', [ProductSKU]) > 0 THEN RIGHT([ProductSKU], 4) ELSE '' END)

SELECT DISTINCT [SKUHashValue] FROM [cpx].[dbo].[SKU_Lead_Time_20170320]

-- ADD ProductSKU & SKUHashValue columns to Egoras_SHP_DTL
ALTER TABLE [dbo].[Egoras_SHP_DTL] ADD [ProductSKU] [nvarchar](50) NULL, [SKUHashValue] [nvarchar](10) NULL

GO
-- Update two columns based on data from Vertica DB
UPDATE [dbo].[Egoras_SHP_DTL] SET [ProductSKU] = COALESCE(b.[prod_id], a.[Product ID]),
[SKUHashValue] = b.[hashoption]
FROM [dbo].[Egoras_SHP_DTL] a
LEFT JOIN [dbo].[Vertica_ProductSKU_Hash0D1] b
ON a.[Sales Order Source System Key] = b.[SRC_SYS_KY]
AND a.[Shipment ID] = b.[SHIP_ID]
AND a.[Shipment Line Item ID] = b.[SHIP_LN_ITM_ID]

GO

SELECT COUNT(*) FROM [dbo].[Egoras_SHP_DTL] WHERE ProductSKU IS NOT NULL AND CONCAT([Product ID], '#0D1') <> ProductSKU

UPDATE [dbo].[Egoras_SHP_DTL] SET [ProductSKU] = [Product ID] WHERE [ProductSKU] IS NULL

-- SPECIAL SYS 2027
SELECT COUNT(*) FROM [dbo].[Egoras_SHP_DTL] WHERE LEN([Shipment Line Item ID]) = 3
SELECT COUNT(*) FROM [dbo].[Egoras_SHP_DTL] WHERE LEN([Shipment ID]) = 12
SELECT COUNT(*) FROM [dbo].[Egoras_SHP_DTL] WHERE [Sales Order Source System Key] = '2027'


-- Clean up [dbo].[SKU_Lead_Time_Hist_WsA]
DELETE FROM [dbo].[SKU_Lead_Time_Hist_WsA] WHERE [TimeStamp] IS NULL 
DELETE FROM [dbo].[SKU_Lead_Time_Hist_WsA] WHERE [LeadTime] = -1

-- Add [StartTimeStamp] & [EndTimeStamp]
ALTER TABLE [dbo].[SKU_Lead_Time_Hist_WsA] ADD [StartTimeStamp] [datetime] NULL, [EndTimeStamp] [datetime] NULL

UPDATE [dbo].[SKU_Lead_Time_Hist_WsA] SET StartTimeStamp = a.[TimeStamp],
EndTimeStamp = b.[EndTimeStamp]
FROM [dbo].[SKU_Lead_Time_Hist_WsA] a
LEFT JOIN
(SELECT [Region], [ProductNumber], [TimeStamp], LEAD([TimeStamp], 1, CONVERT(datetime, '2019-01-01 00:00:00', 120)) OVER (PARTITION BY [Region], [ProductNumber] ORDER BY [TimeStamp] ASC) AS [EndTimeStamp]
FROM [dbo].[SKU_Lead_Time_Hist_WsA]) b ON a.[Region] = b.[Region] AND a.[ProductNumber] = b.[ProductNumber] AND a.[TimeStamp] = b.[TimeStamp]

-- Validate the Start and End Time Stamp
SELECT TOP 100 [Region], [ProductNumber], [TimeStamp], [LeadTime], [StartTimeStamp], [EndTimeStamp] FROM [dbo].[SKU_Lead_Time_Hist_WsA] ORDER BY [Region], [ProductNumber], [TimeStamp]


-- Set NULL of [Value] to '?'
UPDATE [dbo].[SKU_Lead_Time_Hist_WsA] SET [Value] = '?' WHERE [Value] IS NULL

INSERT INTO [dbo].[SKU_Lead_Time_Hist_WsA]
SELECT b.[MessageType], a.[EndTimeStamp], b.[Status], b.[Country], b.[ProductNumber], b.[Region], b.[ALTProductNumber], b.[Code],
b.[Value], b.[RecoveryDate], b.[LeadTime], b.[#agg], a.[StartTimeStamp], a.[EndTimeStamp] FROM
(SELECT [Region], [ProductNumber], CONVERT(datetime, '1999-12-31 00:00:00', 120) AS [StartTimeStamp], MIN([TimeStamp]) AS [EndTimeStamp]
FROM [dbo].[SKU_Lead_Time_Hist_WsA] GROUP BY [Region], [ProductNumber]) a
INNER JOIN [dbo].[SKU_Lead_Time_Hist_WsA] b
ON a.[Region] = b.[Region] AND a.[ProductNumber] = b.[ProductNumber] AND a.[EndTimeStamp] = b.[TimeStamp]



-- UPDATE LeadTime & ConstraintFlag from [dbo].[SKU_Lead_Time_Hist_WsA] to [dbo].[Egoras]
ALTER TABLE [dbo].[Egoras_SHP_DTL] ADD [LeadTime] [INT] NULL
ALTER TABLE [dbo].[Egoras_SHP_DTL] ADD [ConstraintFlag] [nvarchar](10) NULL

UPDATE [dbo].[Egoras_SHP_DTL] SET 
[LeadTime] = b.[LeadTime],
[ConstraintFlag] = b.[Value]
FROM [dbo].[Egoras_SHP_DTL] a LEFT JOIN [dbo].[SKU_Lead_Time_Hist_WsA] b
ON a.[ProductSKU] = b.ProductNumber
AND CASE a.[Profit Center L0 Name] WHEN 'Americas' THEN 'NA' WHEN 'EMEA' THEN 'EU' ELSE 'EU' END = b.[Region]
AND a.[Create] >= b.[StartTimeStamp] AND a.[Create] < b.[EndTimeStamp]

SELECT [Profit Center L0 Name], COUNT(*) FROM [dbo].[Egoras_SHP_DTL] WHERE LeadTime IS NULL GROUP BY [Profit Center L0 Name]


CREATE TABLE [dbo].[Region_HLI_CLEAN_Extra_SRT](
	[Sales Order ID] [nvarchar](20) NOT NULL,
	[HLI_ID] [nvarchar](20) NOT NULL,
	[MaxSRTLeadTime] [int] NULL,
	[ConstraintFlag] [nvarchar](10) NULL,
	[CountOf0D1] [int] NULL,
	[Hash0D1Flag] [nvarchar](5) NULL
)

GO

SELECT COUNT(*) FROM [dbo].[Region_HLI_CLEAN] WHERE [Sales Order ID] IN (SELECT FORMAT(CAST([SAP Order Number] AS BIGINT), 'd10') FROM [dbo].[eORders_FY-17])
SELECT COUNT(*) FROM [dbo].[Region_HLI_CLEAN] WHERE [Sales Order ID] IN (SELECT [SAP Order Number] FROM [dbo].[eORders_FY-17])


GO

-- VIEW FOR HLI ALL COLUMNS
CREATE VIEW [Region_HLI_CLEAN_ALL_v] AS (
SELECT a.*, b.[MaxSRTLeadTime], b.[ConstraintFlag], b.[CountOf0D1], b.[Hash0D1Flag] FROM [dbo].[Region_HLI_CLEAN] a LEFT JOIN [dbo].[Region_HLI_CLEAN_Extra_SRT] b
ON a.[Sales Order ID] = b.[Sales Order ID] AND a.[HLI_ID] = b.[HLI_ID]
)

GO

-- SUBSET OF HLI ALL FOR ECOMMERCE ANALYSIS
CREATE VIEW [eOrders_HLI_CLEAN_v] AS (
SELECT a.*, b.Country AS eOrderCountry FROM [Region_HLI_CLEAN_ALL_v] a INNER JOIN [dbo].[eORders_FY-17] b ON a.[Sales Order ID] = b.[SAP Order Number]
)

GO

-- VIEW FOR QADTA HLI ALL INTERSECTION DATA
CREATE VIEW QADTA_HLI_ALL_INTERSECT_v
AS
WITH CTE AS
(
SELECT * FROM (
SELECT a.*, ROW_NUMBER() OVER(PARTITION BY [Profit_Center_L0_Name], [DG_RE_DE], [SALES_ORDER_ID]
								ORDER BY [qvSLS_QTN_VRSN_SQN_NR] DESC) AS rn
FROM [dbo].[QADTA_Jan_Week_15_2017] a) aa WHERE rn = 1
)
SELECT a.*, b.SRT, b.SRT_PADDING_VALUE, b.QUOTED_EDT FROM [dbo].[Region_HLI_CLEAN_ALL_v] a INNER JOIN CTE b
ON a.[DG_RE-DE] = b.[DG_RE_DE]
AND a.[Profit Center L0 Name] = b.[Profit_Center_L0_Name]
AND a.[Sales Order ID] = FORMAT(CAST(b.[SALES_ORDER_ID] AS BIGINT), 'd10')
AND a.[ASAP Flag] = 'Y'

GO

-- CHECK THE EQUALITY BETWEEN SRT FROM QADTA AND WSA IN HLI LEVEL
SELECT [Profit Center L0 Name]
, [Supplier Complexity Group]
, SUM(CASE WHEN SRT = MaxSRTLeadTime THEN 1 ELSE 0 END) AS [#EqualSRT]
, COUNT(*) AS [#TotalLines] FROM [dbo].[QADTA_HLI_ALL_INTERSECT_v]
GROUP BY [Profit Center L0 Name], [Supplier Complexity Group]

-- CHECK THE EQUALITY BETWEEN SRT FROM QADTA AND WSA IN DG LEVEL
SELECT [Supplier Complexity Group], SUM(SRTEqualFlag), COUNT(*)
FROM
(SELECT [Supplier Complexity Group]
, [DG_RE-DE]
, MAX(SRT) AS DG_SRT
, MAX(MaxSRTLeadTIme) AS MAX_Hist_SRT_DG
, CASE WHEN MAX(SRT) = MAX(MaxSRTLeadTime) THEN 1 ELSE 0 END AS [SRTEqualFlag]
FROM [dbo].[QADTA_HLI_ALL_INTERSECT_v] GROUP BY [Supplier Complexity Group], [DG_RE-DE]
HAVING MAX(MaxSRTLeadTIme) IS NOT NULL
) a GROUP BY [Supplier Complexity Group]


-- UPDATE Ctry_Rgn FOR EMEA HLI DATA AS THEY HAVE TOO MANY LEVELS
UPDATE [dbo].[Region_HLI_CLEAN] SET Ctry_Rgn = 'Western Asia' WHERE Ctry_Rgn = 'IL' AND [Profit Center L0 Name] = 'EMEA'
UPDATE [dbo].[Region_HLI_CLEAN] SET Ctry_Rgn = 'Western Europe' WHERE Ctry_Rgn = 'AT' AND [Profit Center L0 Name] = 'EMEA'
UPDATE [dbo].[Region_HLI_CLEAN] SET Ctry_Rgn = 'Western Asia' WHERE Ctry_Rgn = 'TR' AND [Profit Center L0 Name] = 'EMEA'
UPDATE [dbo].[Region_HLI_CLEAN] SET Ctry_Rgn = 'Northern Europe' WHERE Ctry_Rgn = 'FI' AND [Profit Center L0 Name] = 'EMEA'
UPDATE [dbo].[Region_HLI_CLEAN] SET Ctry_Rgn = 'Southern Europe' WHERE Ctry_Rgn = 'PT' AND [Profit Center L0 Name] = 'EMEA'

-- DELETE ADDED ROWS
DELETE FROM [dbo].[SKU_Lead_Time_Hist_WsA] WHERE [TimeStamp] = CONVERT(datetime, '1999-12-31 00:00:00', 120)

-- DELETE ALL JOINED VALUES
UPDATE [dbo].[Egoras_SHP_DTL] SET [LeadTime] = NULL, [ConstraintFlag] = NULL

UPDATE [dbo].[Egoras_SHP_DTL] SET 
[LeadTime] = b.[LeadTime],
[ConstraintFlag] = b.[Value]
FROM [dbo].[Egoras_SHP_DTL] a LEFT JOIN [dbo].[SKU_Lead_Time_Hist_WsA] b
ON a.[ProductSKU] = b.ProductNumber
AND CASE a.[Profit Center L0 Name] WHEN 'Americas' THEN 'NA' WHEN 'EMEA' THEN 'EU' ELSE 'EU' END = b.[Region]
AND a.[Create] >= b.[StartTimeStamp] AND a.[Create] < b.[EndTimeStamp]
