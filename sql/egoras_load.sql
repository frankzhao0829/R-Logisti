-- Sample script to load raw csv/tsv files to Egoras table locally
-- No need to add the version no. to the raw data files as done in the remote upload
-- For remote raw data load, see egoras_remote_load.md for details

CREATE VIEW TempEgoras_v AS
SELECT
[Shipment Date],
[Profit Center L0 Name],
[Profit Center L1 Name],
[Profit Center L2 Name],
[Sales Order ID],
[Sales Order Line Item ID],
[Sales Order Source System Key],
[Sales Product Type Descr],
[Legacy Sales Order ID],
[Legacy Sales Order Line Item ID],
[GA AMID 4 NAME],
[GA AMID 2 ID],
[GA AMID 2 NAME],
[Sales Order Type Category Code],
[SNP Group Source Code],
[Global Business Unit Name],
[Higher Level Item Flag],
[OSS Higher Level Item],
[OSS Status On Item],
[Fulfillment OSS Status On Item],
[Parent Sales Order Item ID],
[Plant Code],
[Product Descr],
[Product Family Descr],
[Product ID],
[Product Line ID],
[Route Code],
[Sales Order Header Complete Delivery Flag],
[Base Quantity],
[Secured Position Net USD Amount],
[Ship To ISO Country Name],
[Shipment ID],
[OSS Shipment No],
[OSS SAP DeliveryNo],
[Shipment Line Item ID],
[Shipment to ISO Country Code],
[Shipping Receiving Point Code],
[Sold To Customer Name],
[Supplier],
[Supplying Division Sub Entity Code],
[Team],
[Focus Account Group],
[Unit Quantity],
[HPS Segment],
[ISS vs BCS Split],
[ASAP Flag],
[Business Area Code],
[Complexity Groups],
[Solution Order (Y N)],
[Coordination of Delivery Code],
[Delivery Priority Code],
[Delivery Priority Group],
[Derived End Customer Name],
[Distribution Center Code],
[B2B Sales Order ID],
[B2B Sales Order Line Item ID],
[B2B Concatenated Sales Order User Status Code],
[B2B Concatenated Sales Order Line Item User Status Code],
[Concatenated Sales Order User Status Code],
[Concatenated Sales Order Line Item User Status Code],
[B2B Shipment Date],
[B2B Shipment ID],
[B2B Shipment Line Item Identifier],
[Estimated End To End Transit Day No],
[Clean Order Date],
[Fulflmt Clean Order Date],
[Fulflmt ASAP Flag],
[Fulflmt Delivery Priority Code],
[Fulflmt Higher Level Item Flag],
[Fulflmt Plant Code],
[Fulflmt Route Code],
[Fulflmt Sales Order Header Distribution Center Code],
[Fulflmt Sales Order Header Complete Delivery Flag],
[Fulflmt Coordination of Delivery Code],
[Fulflmt Shipment Date],
[Fulflmt Shipping Receiving Point Code],
[B2B Sales Order Header Source System Key],
[GA AMID 4 ID],
[Supplier Complexity Group],
[Sales Document Item Category Code],
[Fulflmt Sales Document Item Category Code],
[Sales Organization Code],
[Special Process],
[HP Receive Date],
[Sales Order Detail Create Timestamp],
[Sales Order Detail Create Timestamp Local],
[Fulflmt Sales Order Detail Create Date],
[Create],
[Clean Item Date Time],
[Clean Item Date Time Local],
[Fulflmt Clean Item Date Time],
[Fulflmt Clean Item Date Time Local],
[Clean Header Date Time],
[Clean Header Date Time Local],
[Fulflmt Clean Header Date Time],
[Fulflmt Clean Header Date Time Local],
[Clean],
[Production Start Date],
[Production Start Date Local],
[Drop to Factory Timestamp],
[Drop to Factory Timestamp Local],
[Fulflmt Drop to Factory Timestamp],
[Fulflmt Drop to Factory Timestamp Local],
[Fulflmt Production Start Date],
[Fulflmt Production Start Date Local],
[Production Start],
[Factory Ship Date Time],
[Factory Ship Date Time Local],
[Fulflmt Factory Ship Date Time],
[Fulflmt Factory Ship Date Time Local],
[Factory Ship],
[OSS Cust Ship Date],
[OSS Cust Ship Date Local],
[Fulflmt OSS Cust Ship Date],
[Fulflmt OSS Cust Ship Date Local],
[Customer Ship],
[Special Type of Order],
[Hub Receive Date],
[Hub Receive Date Local],
[Fulflmt Hub Receive Date],
[Fulflmt Hub Receive Date Local],
[Hub Receive],
[OSS POD Date],
[Fulflmt OSS POD Date],
[Delivery],
[Customer Requested Delivery Date],
[Fulflmt Requested Delivery Date],
[Actual Customer Requested Delivery Date],
[First Planned Fact Ship Date],
[Fulflmt First Planned Fact Ship Date],
[First Planned Cust Ship],
[Fulflmt First Planned Cust Ship],
[First Planned Delivery Date],
[Fulflmt First Planned Delivery Date],
[Planned Factory Ship Date],
[Fulflmt Planned Factory Ship Date],
[First Touchpoint Scheduled Ship Timestamp],
[Last Touchpoint Scheduled Ship Timestamp],
[Planned Cust Ship Date],
[Planned Delivery Date],
[Production Done Date],
[Production Done Date Local],
[Fulflmt Production Done Date],
[Fulflmt Production Done Date Local],
[Production Done],
[Release2Fulfillment Date],
[Release2Fulfillment Date Local],
[Fulflmt Release2Fulfillment Date],
[Fulflmt Release2Fulfillment Date Local],
[SCOUT Delivered Date Time],
[Fulflmt SCOUT Delivered Date Time],
[SCOUT Latest Carrier Status Code],
[Fulflmt SCOUT Latest Carrier Status Code],
[SCOUT Latest Carrier Status Desc],
[Fulflmt SCOUT Latest Carrier Status Desc],
[SCOUT Latest Carr Status Reason Code],
[Fulflmt SCOUT Latest Carr Status Reason Code],
[SCOUT Latest Carr Status Reason Desc],
[Fulflmt SCOUT Latest Carr Status Reason Desc],
[SCOUT Pod Submission Date],
[Fulflmt SCOUT Pod Submission Date],
[DTFC -3 0],
[DTFC -x 0],
[DTCRD 0 0],
[DTCRD -x 0],
[DTACRD -x 0],
[STFC -x 0],
[STFC -2 0],
[STFC -3 0],
[STFA -x 0],
[STFA -2 0],
[STFA -3 0],
[RE to CR],
[RE to CL],
[RE to FS],
[Re to CS],
[RE to Del],
[CL to BT],
[CL to FS],
[BT to FGI],
[FGI to FS],
[FS to CS],
[BT to FS],
[FS to Hub R],
[Hub R to Hub S],
[CS to Del],
[CR to CL],
[Order_RE-CR],
[Order_CR-CL],
[Order_RE-CL],
[DG_RE-CR],
[DG_RE-CL],
[DG_RE-FS],
[DG_RE-CS],
[DG_RE-DE],
[DG_CR-CL],
[DG_CL-BT],
[DG_CL-FGI],
[DG_CL-FS],
[DG_BT-FGI],
[DG_BT-FS],
[DG_BT-CS],
[DG_FGI-FS],
[DG_FS-HR],
[DG_FS-CS],
[DG_HR-CS],
[DG_CS-DE],
[DG_FAck-FS],
[DG_LAck-FS],
[DG_FCS-CS],
[DG_LCS-CS],
[DG_FCD-DE],
[DG_LCD-DE],
[DG_CRD-DE],
[DG_ACRD-DE]
FROM
[dbo].[egoras]

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week17.csv' WITH (FIRSTROW=2, FIELDTERMINATOR = ',')

UPDATE [dbo].[egoras] 
SET [Version] = 1617
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week18.csv' WITH (FIRSTROW=2, FIELDTERMINATOR = ',')

UPDATE [dbo].[egoras] 
SET [Version] = 1618
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week19.csv' WITH (FIRSTROW=2, FIELDTERMINATOR = ',')

UPDATE [dbo].[egoras] 
SET [Version] = 1619
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week20.csv' WITH (FIRSTROW=2, FIELDTERMINATOR = ',')

UPDATE [dbo].[egoras] 
SET [Version] = 1620
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week21.csv' WITH (FIRSTROW=2, FIELDTERMINATOR = ',')

UPDATE [dbo].[egoras] 
SET [Version] = 1621
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week22.csv' WITH (FIRSTROW=2, FIELDTERMINATOR = ',')

UPDATE [dbo].[egoras] 
SET [Version] = 1622
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week23.csv' WITH (FIRSTROW=2, FIELDTERMINATOR = ',')

UPDATE [dbo].[egoras] 
SET [Version] = 1623
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week24.csv' WITH (FIRSTROW=2, FIELDTERMINATOR = ',')

UPDATE [dbo].[egoras] 
SET [Version] = 1624
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week25.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1625
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week26.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1626
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week27.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1627
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week28.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1628
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week29.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1629
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week30.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1630
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week31.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1631
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week32.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1632
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week33.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1633
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week34.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1634
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week35.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1635
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week36.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1636
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week37.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1637
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week38.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1638
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week39.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1639
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week40.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1640
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week41.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1641
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week42.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1642
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week43.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1643
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week44.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1644
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week45.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1645
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week46.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1646
WHERE [Version] IS NULL

GO


BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week47.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1647
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week48.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1648
WHERE [Version] IS NULL

GO


BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week49.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1649
WHERE [Version] IS NULL

GO


BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week50.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1650
WHERE [Version] IS NULL

GO


BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week51.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1651
WHERE [Version] IS NULL

GO


BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week52.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1652
WHERE [Version] IS NULL

GO


BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/APJ_Speed_Predictability_2016_Week53.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1653
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/EMEA_Speed_Predictability_2017_Week1.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1701
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/EMEA_Speed_Predictability_2017_Week2.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1702
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/EMEA_Speed_Predictability_2017_Week3.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1703
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/EMEA_Speed_Predictability_2017_Week4.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1704
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/EMEA_Speed_Predictability_2017_Week5.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1705
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/EMEA_Speed_Predictability_2017_Week6.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1706
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/EMEA_Speed_Predictability_2017_Week7.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1707
WHERE [Version] IS NULL

GO

BULK INSERT [dbo].[TempEgoras_v] FROM '/opt/mount1/EMEA_Speed_Predictability_2017_Week8.txt' WITH (FIRSTROW=2, FIELDTERMINATOR = '\t')

UPDATE [dbo].[egoras] 
SET [Version] = 1708
WHERE [Version] IS NULL

GO

--SELECT TOP 100 * FROM [dbo].[egoras]
--SELECT count(*) FROM [dbo].[egoras] WHERE [Version] = '18'