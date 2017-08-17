/************   Create Table [dbo].[Egoras_SHP_DTL] **************************************/
USE [cpx]
GO

/****** Object:  Table [dbo].[Egoras_SHP_DTL]    Script Date: 8/29/2016 10:53:52 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[Egoras_SHP_DTL](
	[Version] [smallint] NULL,
	[Shipment Date] [datetime] NULL,
	[Profit Center L0 Name] [nvarchar](20) NULL,
	[Profit Center L1 Name] [nvarchar](20) NULL,
	[Profit Center L2 Name] [nvarchar](50) NULL,
	[Sales Order ID] [nvarchar](20) NULL,
	[Sales Order Line Item ID] [nvarchar](20) NOT NULL,
	[Sales Order Source System Key] [nvarchar](20) NOT NULL,
	[Sales Product Type Descr] [nvarchar](20) NULL,
	[Legacy Sales Order ID] [nvarchar](20) NULL,
	[Legacy Sales Order Line Item ID] [nvarchar](20) NULL,
	[GA AMID 4 NAME] [nvarchar](max) NULL,
	[GA AMID 2 ID] [nvarchar](20) NULL,
	[GA AMID 2 NAME] [nvarchar](max) NULL,
	[Sales Order Type Category Code] [nvarchar](20) NULL,
	[SNP Group Source Code] [nvarchar](20) NULL,
	[Global Business Unit Name] [nvarchar](20) NULL,
	[Higher Level Item Flag] [nvarchar](1) NULL,
	[OSS Higher Level Item] [nvarchar](20) NULL,
	[OSS Status On Item] [nvarchar](50) NULL,
	[Fulfillment OSS Status On Item] [nvarchar](50) NULL,
	[Parent Sales Order Item ID] [nvarchar](20) NULL,
	[Plant Code] [nvarchar](10) NULL,
	[Product Descr] [nvarchar](255) NULL,
	[Product Family Descr] [nvarchar](255) NULL,
	[Product ID] [nvarchar](20) NULL,
	[Product Line ID] [nvarchar](10) NULL,
	[Route Code] [nvarchar](10) NULL,
	[Sales Order Header Complete Delivery Flag] [nvarchar](10) NULL,
	[Base Quantity] [float] NULL,
	[Secured Position Net USD Amount] [float] NULL,
	[Ship To ISO Country Name] [nvarchar](40) NULL,
	[Shipment ID] [nvarchar](20) NOT NULL,
	[OSS Shipment No] [nvarchar](20) NULL,
	[OSS SAP DeliveryNo] [nvarchar](20) NULL,
	[Shipment Line Item ID] [nvarchar](20) NOT NULL,
	[Shipment to ISO Country Code] [nvarchar](20) NULL,
	[Shipping Receiving Point Code] [nvarchar](20) NULL,
	[Sold To Customer Name] [nvarchar](max) NULL,
	[Supplier] [nvarchar](50) NULL,
	[Supplying Division Sub Entity Code] [nvarchar](10) NULL,
	[Team] [nvarchar](20) NULL,
	[Focus Account Group] [nvarchar](50) NULL,
	[Unit Quantity] [float] NULL,
	[HPS Segment] [nvarchar](50) NULL,
	[ISS vs BCS Split] [nvarchar](10) NULL,
	[ASAP Flag] [nvarchar](1) NULL,
	[Business Area Code] [nvarchar](20) NULL,
	[Complexity Groups] [nvarchar](20) NULL,
	[Solution Order (Y N)] [nvarchar](1) NULL,
	[Coordination of Delivery Code] [nvarchar](20) NULL,
	[Delivery Priority Code] [nvarchar](20) NULL,
	[Delivery Priority Group] [nvarchar](max) NULL,
	[Derived End Customer Name] [nvarchar](max) NULL,
	[Distribution Center Code] [nvarchar](20) NULL,
	[B2B Sales Order ID] [nvarchar](20) NULL,
	[B2B Sales Order Line Item ID] [nvarchar](20) NULL,
	[B2B Concatenated Sales Order User Status Code] [nvarchar](max) NULL,
	[B2B Concatenated Sales Order Line Item User Status Code] [nvarchar](max) NULL,
	[Concatenated Sales Order User Status Code] [nvarchar](max) NULL,
	[Concatenated Sales Order Line Item User Status Code] [nvarchar](max) NULL,
	[B2B Shipment Date] [datetime] NULL,
	[B2B Shipment ID] [nvarchar](20) NULL,
	[B2B Shipment Line Item Identifier] [nvarchar](20) NULL,
	[Estimated End To End Transit Day No] [bigint] NULL,
	[Clean Order Date] [datetime] NULL,
	[Fulflmt Clean Order Date] [datetime] NULL,
	[Fulflmt ASAP Flag] [nvarchar](1) NULL,
	[Fulflmt Delivery Priority Code] [nvarchar](20) NULL,
	[Fulflmt Higher Level Item Flag] [nvarchar](1) NULL,
	[Fulflmt Plant Code] [nvarchar](20) NULL,
	[Fulflmt Route Code] [nvarchar](20) NULL,
	[Fulflmt Sales Order Header Distribution Center Code] [nvarchar](20) NULL,
	[Fulflmt Sales Order Header Complete Delivery Flag] [nvarchar](1) NULL,
	[Fulflmt Coordination of Delivery Code] [nvarchar](20) NULL,
	[Fulflmt Shipment Date] [datetime] NULL,
	[Fulflmt Shipping Receiving Point Code] [nvarchar](20) NULL,
	[B2B Sales Order Header Source System Key] [nvarchar](20) NULL,
	[GA AMID 4 ID] [nvarchar](20) NULL,
	[Supplier Complexity Group] [nvarchar](50) NULL,
	[Sales Document Item Category Code] [nvarchar](10) NULL,
	[Fulflmt Sales Document Item Category Code] [nvarchar](10) NULL,
	[Sales Organization Code] [nvarchar](10) NULL,
	[Special Process] [nvarchar](max) NULL,
	[HP Receive Date] [datetime] NULL,
	[Sales Order Detail Create Timestamp] [datetime] NULL,
	[Sales Order Detail Create Timestamp Local] [datetime] NULL,
	[Fulflmt Sales Order Detail Create Date] [datetime] NULL,
	[Create] [datetime] NULL,
	[Clean Item Date Time] [datetime] NULL,
	[Clean Item Date Time Local] [datetime] NULL,
	[Fulflmt Clean Item Date Time] [datetime] NULL,
	[Fulflmt Clean Item Date Time Local] [datetime] NULL,
	[Clean Header Date Time] [datetime] NULL,
	[Clean Header Date Time Local] [datetime] NULL,
	[Fulflmt Clean Header Date Time] [datetime] NULL,
	[Fulflmt Clean Header Date Time Local] [datetime] NULL,
	[Clean] [datetime] NULL,
	[Production Start Date] [datetime] NULL,
	[Production Start Date Local] [datetime] NULL,
	[Drop to Factory Timestamp] [datetime] NULL,
	[Drop to Factory Timestamp Local] [datetime] NULL,
	[Fulflmt Drop to Factory Timestamp] [datetime] NULL,
	[Fulflmt Drop to Factory Timestamp Local] [datetime] NULL,
	[Fulflmt Production Start Date] [datetime] NULL,
	[Fulflmt Production Start Date Local] [datetime] NULL,
	[Production Start] [datetime] NULL,
	[Factory Ship Date Time] [datetime] NULL,
	[Factory Ship Date Time Local] [datetime] NULL,
	[Fulflmt Factory Ship Date Time] [datetime] NULL,
	[Fulflmt Factory Ship Date Time Local] [datetime] NULL,
	[Factory Ship] [datetime] NULL,
	[OSS Cust Ship Date] [datetime] NULL,
	[OSS Cust Ship Date Local] [datetime] NULL,
	[Fulflmt OSS Cust Ship Date] [datetime] NULL,
	[Fulflmt OSS Cust Ship Date Local] [datetime] NULL,
	[Customer Ship] [datetime] NULL,
	[Special Type of Order] [nvarchar](max) NULL,
	[Hub Receive Date] [datetime] NULL,
	[Hub Receive Date Local] [datetime] NULL,
	[Fulflmt Hub Receive Date] [nvarchar](max) NULL,
	[Fulflmt Hub Receive Date Local] [nvarchar](max) NULL,
	[Hub Receive] [datetime] NULL,
	[OSS POD Date] [datetime] NULL,
	[Fulflmt OSS POD Date] [nvarchar](max) NULL,
	[Delivery] [datetime] NULL,
	[Customer Requested Delivery Date] [datetime] NULL,
	[Fulflmt Requested Delivery Date] [datetime] NULL,
	[Actual Customer Requested Delivery Date] [nvarchar](max) NULL,
	[First Planned Fact Ship Date] [datetime] NULL,
	[Fulflmt First Planned Fact Ship Date] [datetime] NULL,
	[First Planned Cust Ship] [datetime] NULL,
	[Fulflmt First Planned Cust Ship] [datetime] NULL,
	[First Planned Delivery Date] [datetime] NULL,
	[Fulflmt First Planned Delivery Date] [datetime] NULL,
	[Planned Factory Ship Date] [datetime] NULL,
	[Fulflmt Planned Factory Ship Date] [datetime] NULL,
	[First Touchpoint Scheduled Ship Timestamp] [nvarchar](max) NULL,
	[Last Touchpoint Scheduled Ship Timestamp] [nvarchar](max) NULL,
	[Planned Cust Ship Date] [datetime] NULL,
	[Planned Delivery Date] [datetime] NULL,
	[Production Done Date] [datetime] NULL,
	[Production Done Date Local] [datetime] NULL,
	[Fulflmt Production Done Date] [datetime] NULL,
	[Fulflmt Production Done Date Local] [datetime] NULL,
	[Production Done] [datetime] NULL,
	[Release2Fulfillment Date] [datetime] NULL,
	[Release2Fulfillment Date Local] [datetime] NULL,
	[Fulflmt Release2Fulfillment Date] [datetime] NULL,
	[Fulflmt Release2Fulfillment Date Local] [datetime] NULL,
	[SCOUT Delivered Date Time] [datetime] NULL,
	[Fulflmt SCOUT Delivered Date Time] [nvarchar](max) NULL,
	[SCOUT Latest Carrier Status Code] [nvarchar](9) NULL,
	[Fulflmt SCOUT Latest Carrier Status Code] [nvarchar](10) NULL,
	[SCOUT Latest Carrier Status Desc] [nvarchar](255) NULL,
	[Fulflmt SCOUT Latest Carrier Status Desc] [nvarchar](255) NULL,
	[SCOUT Latest Carr Status Reason Code] [nvarchar](20) NULL,
	[Fulflmt SCOUT Latest Carr Status Reason Code] [nvarchar](10) NULL,
	[SCOUT Latest Carr Status Reason Desc] [nvarchar](255) NULL,
	[Fulflmt SCOUT Latest Carr Status Reason Desc] [nvarchar](255) NULL,
	[SCOUT Pod Submission Date] [datetime] NULL,
	[Fulflmt SCOUT Pod Submission Date] [nvarchar](max) NULL,
	[DTFC -3 0] [float] NULL,
	[DTFC -x 0] [float] NULL,
	[DTCRD 0 0] [float] NULL,
	[DTCRD -x 0] [float] NULL,
	[DTACRD -x 0] [nvarchar](max) NULL,
	[STFC -x 0] [float] NULL,
	[STFC -2 0] [float] NULL,
	[STFC -3 0] [float] NULL,
	[STFA -x 0] [float] NULL,
	[STFA -2 0] [float] NULL,
	[STFA -3 0] [float] NULL,
	[RE to CR] [float] NULL,
	[RE to CL] [float] NULL,
	[RE to FS] [float] NULL,
	[Re to CS] [float] NULL,
	[RE to Del] [float] NULL,
	[CL to BT] [float] NULL,
	[CL to FS] [float] NULL,
	[BT to FGI] [float] NULL,
	[FGI to FS] [float] NULL,
	[FS to CS] [float] NULL,
	[BT to FS] [float] NULL,
	[FS to Hub R] [float] NULL,
	[Hub R to Hub S] [float] NULL,
	[CS to Del] [float] NULL,
	[CR to CL] [float] NULL,
	[Order_RE-CR] [nvarchar](50) NULL,
	[Order_CR-CL] [nvarchar](50) NULL,
	[Order_RE-CL] [nvarchar](50) NULL,
	[DG_RE-CR] [nvarchar](50) NULL,
	[DG_RE-CL] [nvarchar](50) NULL,
	[DG_RE-FS] [nvarchar](50) NULL,
	[DG_RE-CS] [nvarchar](50) NULL,
	[DG_RE-DE] [nvarchar](50) NULL,
	[DG_CR-CL] [nvarchar](50) NULL,
	[DG_CL-BT] [nvarchar](50) NULL,
	[DG_CL-FGI] [nvarchar](50) NULL,
	[DG_CL-FS] [nvarchar](50) NULL,
	[DG_BT-FGI] [nvarchar](50) NULL,
	[DG_BT-FS] [nvarchar](50) NULL,
	[DG_BT-CS] [nvarchar](50) NULL,
	[DG_FGI-FS] [nvarchar](50) NULL,
	[DG_FS-HR] [nvarchar](50) NULL,
	[DG_FS-CS] [nvarchar](50) NULL,
	[DG_HR-CS] [nvarchar](50) NULL,
	[DG_CS-DE] [nvarchar](50) NULL,
	[DG_FAck-FS] [nvarchar](50) NULL,
	[DG_LAck-FS] [nvarchar](50) NULL,
	[DG_FCS-CS] [nvarchar](50) NULL,
	[DG_LCS-CS] [nvarchar](50) NULL,
	[DG_FCD-DE] [nvarchar](50) NULL,
	[DG_LCD-DE] [nvarchar](50) NULL,
	[DG_CRD-DE] [nvarchar](50) NULL,
	[DG_ACRD-DE] [nvarchar](max) NULL,
	CONSTRAINT Primary_Key PRIMARY KEY ([Sales Order Source System Key], [Shipment ID], [Shipment Line Item ID])    
) 

GO



/*************************************************************************************************************************/
/****  Keep the latest obs with [Sales Order Source System Key], [Shipment ID], [Shipment Line Item ID] as primary key ***/
/*************************************************************************************************************************/


/****** Script for SelectTopNRows command from SSMS  ******/
insert INTO dbo.Egoras_SHP_DTL
(
 [Version]
,[Shipment Date]
,[Profit Center L0 Name]
,[Profit Center L1 Name]
,[Profit Center L2 Name]
,[Sales Order ID]
,[Sales Order Line Item ID]
,[Sales Order Source System Key]
,[Sales Product Type Descr]
,[Legacy Sales Order ID]
,[Legacy Sales Order Line Item ID]
,[GA AMID 4 NAME]
,[GA AMID 2 ID]
,[GA AMID 2 NAME]
,[Sales Order Type Category Code]
,[SNP Group Source Code]
,[Global Business Unit Name]
,[Higher Level Item Flag]
,[OSS Higher Level Item]
,[OSS Status On Item]
,[Fulfillment OSS Status On Item]
,[Parent Sales Order Item ID]
,[Plant Code]
,[Product Descr]
,[Product Family Descr]
,[Product ID]
,[Product Line ID]
,[Route Code]
,[Sales Order Header Complete Delivery Flag]
,[Base Quantity]
,[Secured Position Net USD Amount]
,[Ship To ISO Country Name]
,[Shipment ID]
,[OSS Shipment No]
,[OSS SAP DeliveryNo]
,[Shipment Line Item ID]
,[Shipment to ISO Country Code]
,[Shipping Receiving Point Code]
,[Sold To Customer Name]
,[Supplier]
,[Supplying Division Sub Entity Code]
,[Team]
,[Focus Account Group]
,[Unit Quantity]
,[HPS Segment]
,[ISS vs BCS Split]
,[ASAP Flag]
,[Business Area Code]
,[Complexity Groups]
,[Solution Order (Y N)]
,[Coordination of Delivery Code]
,[Delivery Priority Code]
,[Delivery Priority Group]
,[Derived End Customer Name]
,[Distribution Center Code]
,[B2B Sales Order ID]
,[B2B Sales Order Line Item ID]
,[B2B Concatenated Sales Order User Status Code]
,[B2B Concatenated Sales Order Line Item User Status Code]
,[Concatenated Sales Order User Status Code]
,[Concatenated Sales Order Line Item User Status Code]
,[B2B Shipment Date]
,[B2B Shipment ID]
,[B2B Shipment Line Item Identifier]
,[Estimated End To End Transit Day No]
,[Clean Order Date]
,[Fulflmt Clean Order Date]
,[Fulflmt ASAP Flag]
,[Fulflmt Delivery Priority Code]
,[Fulflmt Higher Level Item Flag]
,[Fulflmt Plant Code]
,[Fulflmt Route Code]
,[Fulflmt Sales Order Header Distribution Center Code]
,[Fulflmt Sales Order Header Complete Delivery Flag]
,[Fulflmt Coordination of Delivery Code]
,[Fulflmt Shipment Date]
,[Fulflmt Shipping Receiving Point Code]
,[B2B Sales Order Header Source System Key]
,[GA AMID 4 ID]
,[Supplier Complexity Group]
,[Sales Document Item Category Code]
,[Fulflmt Sales Document Item Category Code]
,[Sales Organization Code]
,[Special Process]
,[HP Receive Date]
,[Sales Order Detail Create Timestamp]
,[Sales Order Detail Create Timestamp Local]
,[Fulflmt Sales Order Detail Create Date]
,[Create]
,[Clean Item Date Time]
,[Clean Item Date Time Local]
,[Fulflmt Clean Item Date Time]
,[Fulflmt Clean Item Date Time Local]
,[Clean Header Date Time]
,[Clean Header Date Time Local]
,[Fulflmt Clean Header Date Time]
,[Fulflmt Clean Header Date Time Local]
,[Clean]
,[Production Start Date]
,[Production Start Date Local]
,[Drop to Factory Timestamp]
,[Drop to Factory Timestamp Local]
,[Fulflmt Drop to Factory Timestamp]
,[Fulflmt Drop to Factory Timestamp Local]
,[Fulflmt Production Start Date]
,[Fulflmt Production Start Date Local]
,[Production Start]
,[Factory Ship Date Time]
,[Factory Ship Date Time Local]
,[Fulflmt Factory Ship Date Time]
,[Fulflmt Factory Ship Date Time Local]
,[Factory Ship]
,[OSS Cust Ship Date]
,[OSS Cust Ship Date Local]
,[Fulflmt OSS Cust Ship Date]
,[Fulflmt OSS Cust Ship Date Local]
,[Customer Ship]
,[Special Type of Order]
,[Hub Receive Date]
,[Hub Receive Date Local]
,[Fulflmt Hub Receive Date]
,[Fulflmt Hub Receive Date Local]
,[Hub Receive]
,[OSS POD Date]
,[Fulflmt OSS POD Date]
,[Delivery]
,[Customer Requested Delivery Date]
,[Fulflmt Requested Delivery Date]
,[Actual Customer Requested Delivery Date]
,[First Planned Fact Ship Date]
,[Fulflmt First Planned Fact Ship Date]
,[First Planned Cust Ship]
,[Fulflmt First Planned Cust Ship]
,[First Planned Delivery Date]
,[Fulflmt First Planned Delivery Date]
,[Planned Factory Ship Date]
,[Fulflmt Planned Factory Ship Date]
,[First Touchpoint Scheduled Ship Timestamp]
,[Last Touchpoint Scheduled Ship Timestamp]
,[Planned Cust Ship Date]
,[Planned Delivery Date]
,[Production Done Date]
,[Production Done Date Local]
,[Fulflmt Production Done Date]
,[Fulflmt Production Done Date Local]
,[Production Done]
,[Release2Fulfillment Date]
,[Release2Fulfillment Date Local]
,[Fulflmt Release2Fulfillment Date]
,[Fulflmt Release2Fulfillment Date Local]
,[SCOUT Delivered Date Time]
,[Fulflmt SCOUT Delivered Date Time]
,[SCOUT Latest Carrier Status Code]
,[Fulflmt SCOUT Latest Carrier Status Code]
,[SCOUT Latest Carrier Status Desc]
,[Fulflmt SCOUT Latest Carrier Status Desc]
,[SCOUT Latest Carr Status Reason Code]
,[Fulflmt SCOUT Latest Carr Status Reason Code]
,[SCOUT Latest Carr Status Reason Desc]
,[Fulflmt SCOUT Latest Carr Status Reason Desc]
,[SCOUT Pod Submission Date]
,[Fulflmt SCOUT Pod Submission Date]
,[DTFC -3 0]
,[DTFC -x 0]
,[DTCRD 0 0]
,[DTCRD -x 0]
,[DTACRD -x 0]
,[STFC -x 0]
,[STFC -2 0]
,[STFC -3 0]
,[STFA -x 0]
,[STFA -2 0]
,[STFA -3 0]
,[RE to CR]
,[RE to CL]
,[RE to FS]
,[Re to CS]
,[RE to Del]
,[CL to BT]
,[CL to FS]
,[BT to FGI]
,[FGI to FS]
,[FS to CS]
,[BT to FS]
,[FS to Hub R]
,[Hub R to Hub S]
,[CS to Del]
,[CR to CL]
,[Order_RE-CR]
,[Order_CR-CL]
,[Order_RE-CL]
,[DG_RE-CR]
,[DG_RE-CL]
,[DG_RE-FS]
,[DG_RE-CS]
,[DG_RE-DE]
,[DG_CR-CL]
,[DG_CL-BT]
,[DG_CL-FGI]
,[DG_CL-FS]
,[DG_BT-FGI]
,[DG_BT-FS]
,[DG_BT-CS]
,[DG_FGI-FS]
,[DG_FS-HR]
,[DG_FS-CS]
,[DG_HR-CS]
,[DG_CS-DE]
,[DG_FAck-FS]
,[DG_LAck-FS]
,[DG_FCS-CS]
,[DG_LCS-CS]
,[DG_FCD-DE]
,[DG_LCD-DE]
,[DG_CRD-DE]
,[DG_ACRD-DE]
)
select [Version]
	,[Shipment Date]
	,[Profit Center L0 Name]
	,[Profit Center L1 Name]
	,[Profit Center L2 Name]
	,[Sales Order ID]
	,[Sales Order Line Item ID]
	,[Sales Order Source System Key]
	,[Sales Product Type Descr]
	,[Legacy Sales Order ID]
	,[Legacy Sales Order Line Item ID]
	,[GA AMID 4 NAME]
	,[GA AMID 2 ID]
	,[GA AMID 2 NAME]
	,[Sales Order Type Category Code]
	,[SNP Group Source Code]
	,[Global Business Unit Name]
	,[Higher Level Item Flag]
	,[OSS Higher Level Item]
	,[OSS Status On Item]
	,[Fulfillment OSS Status On Item]
	,[Parent Sales Order Item ID]
	,[Plant Code]
	,[Product Descr]
	,[Product Family Descr]
	,[Product ID]
	,[Product Line ID]
	,[Route Code]
	,[Sales Order Header Complete Delivery Flag]
	,[Base Quantity]
	,[Secured Position Net USD Amount]
	,[Ship To ISO Country Name]
	,[Shipment ID]
	,[OSS Shipment No]
	,[OSS SAP DeliveryNo]
	,[Shipment Line Item ID]
	,[Shipment to ISO Country Code]
	,[Shipping Receiving Point Code]
	,[Sold To Customer Name]
	,[Supplier]
	,[Supplying Division Sub Entity Code]
	,[Team]
	,[Focus Account Group]
	,[Unit Quantity]
	,[HPS Segment]
	,[ISS vs BCS Split]
	,[ASAP Flag]
	,[Business Area Code]
	,[Complexity Groups]
	,[Solution Order (Y N)]
	,[Coordination of Delivery Code]
	,[Delivery Priority Code]
	,[Delivery Priority Group]
	,[Derived End Customer Name]
	,[Distribution Center Code]
	,[B2B Sales Order ID]
	,[B2B Sales Order Line Item ID]
	,[B2B Concatenated Sales Order User Status Code]
	,[B2B Concatenated Sales Order Line Item User Status Code]
	,[Concatenated Sales Order User Status Code]
	,[Concatenated Sales Order Line Item User Status Code]
	,[B2B Shipment Date]
	,[B2B Shipment ID]
	,[B2B Shipment Line Item Identifier]
	,[Estimated End To End Transit Day No]
	,[Clean Order Date]
	,[Fulflmt Clean Order Date]
	,[Fulflmt ASAP Flag]
	,[Fulflmt Delivery Priority Code]
	,[Fulflmt Higher Level Item Flag]
	,[Fulflmt Plant Code]
	,[Fulflmt Route Code]
	,[Fulflmt Sales Order Header Distribution Center Code]
	,[Fulflmt Sales Order Header Complete Delivery Flag]
	,[Fulflmt Coordination of Delivery Code]
	,[Fulflmt Shipment Date]
	,[Fulflmt Shipping Receiving Point Code]
	,[B2B Sales Order Header Source System Key]
	,[GA AMID 4 ID]
	,[Supplier Complexity Group]
	,[Sales Document Item Category Code]
	,[Fulflmt Sales Document Item Category Code]
	,[Sales Organization Code]
	,[Special Process]
	,[HP Receive Date]
	,[Sales Order Detail Create Timestamp]
	,[Sales Order Detail Create Timestamp Local]
	,[Fulflmt Sales Order Detail Create Date]
	,[Create]
	,[Clean Item Date Time]
	,[Clean Item Date Time Local]
	,[Fulflmt Clean Item Date Time]
	,[Fulflmt Clean Item Date Time Local]
	,[Clean Header Date Time]
	,[Clean Header Date Time Local]
	,[Fulflmt Clean Header Date Time]
	,[Fulflmt Clean Header Date Time Local]
	,[Clean]
	,[Production Start Date]
	,[Production Start Date Local]
	,[Drop to Factory Timestamp]
	,[Drop to Factory Timestamp Local]
	,[Fulflmt Drop to Factory Timestamp]
	,[Fulflmt Drop to Factory Timestamp Local]
	,[Fulflmt Production Start Date]
	,[Fulflmt Production Start Date Local]
	,[Production Start]
	,[Factory Ship Date Time]
	,[Factory Ship Date Time Local]
	,[Fulflmt Factory Ship Date Time]
	,[Fulflmt Factory Ship Date Time Local]
	,[Factory Ship]
	,[OSS Cust Ship Date]
	,[OSS Cust Ship Date Local]
	,[Fulflmt OSS Cust Ship Date]
	,[Fulflmt OSS Cust Ship Date Local]
	,[Customer Ship]
	,[Special Type of Order]
	,[Hub Receive Date]
	,[Hub Receive Date Local]
	,[Fulflmt Hub Receive Date]
	,[Fulflmt Hub Receive Date Local]
	,[Hub Receive]
	,[OSS POD Date]
	,[Fulflmt OSS POD Date]
	,[Delivery]
	,[Customer Requested Delivery Date]
	,[Fulflmt Requested Delivery Date]
	,[Actual Customer Requested Delivery Date]
	,[First Planned Fact Ship Date]
	,[Fulflmt First Planned Fact Ship Date]
	,[First Planned Cust Ship]
	,[Fulflmt First Planned Cust Ship]
	,[First Planned Delivery Date]
	,[Fulflmt First Planned Delivery Date]
	,[Planned Factory Ship Date]
	,[Fulflmt Planned Factory Ship Date]
	,[First Touchpoint Scheduled Ship Timestamp]
	,[Last Touchpoint Scheduled Ship Timestamp]
	,[Planned Cust Ship Date]
	,[Planned Delivery Date]
	,[Production Done Date]
	,[Production Done Date Local]
	,[Fulflmt Production Done Date]
	,[Fulflmt Production Done Date Local]
	,[Production Done]
	,[Release2Fulfillment Date]
	,[Release2Fulfillment Date Local]
	,[Fulflmt Release2Fulfillment Date]
	,[Fulflmt Release2Fulfillment Date Local]
	,[SCOUT Delivered Date Time]
	,[Fulflmt SCOUT Delivered Date Time]
	,[SCOUT Latest Carrier Status Code]
	,[Fulflmt SCOUT Latest Carrier Status Code]
	,[SCOUT Latest Carrier Status Desc]
	,[Fulflmt SCOUT Latest Carrier Status Desc]
	,[SCOUT Latest Carr Status Reason Code]
	,[Fulflmt SCOUT Latest Carr Status Reason Code]
	,[SCOUT Latest Carr Status Reason Desc]
	,[Fulflmt SCOUT Latest Carr Status Reason Desc]
	,[SCOUT Pod Submission Date]
	,[Fulflmt SCOUT Pod Submission Date]
	,[DTFC -3 0]
	,[DTFC -x 0]
	,[DTCRD 0 0]
	,[DTCRD -x 0]
	,[DTACRD -x 0]
	,[STFC -x 0]
	,[STFC -2 0]
	,[STFC -3 0]
	,[STFA -x 0]
	,[STFA -2 0]
	,[STFA -3 0]
	,[RE to CR]
	,[RE to CL]
	,[RE to FS]
	,[Re to CS]
	,[RE to Del]
	,[CL to BT]
	,[CL to FS]
	,[BT to FGI]
	,[FGI to FS]
	,[FS to CS]
	,[BT to FS]
	,[FS to Hub R]
	,[Hub R to Hub S]
	,[CS to Del]
	,[CR to CL]
	,[Order_RE-CR]
	,[Order_CR-CL]
	,[Order_RE-CL]
	,[DG_RE-CR]
	,[DG_RE-CL]
	,[DG_RE-FS]
	,[DG_RE-CS]
	,[DG_RE-DE]
	,[DG_CR-CL]
	,[DG_CL-BT]
	,[DG_CL-FGI]
	,[DG_CL-FS]
	,[DG_BT-FGI]
	,[DG_BT-FS]
	,[DG_BT-CS]
	,[DG_FGI-FS]
	,[DG_FS-HR]
	,[DG_FS-CS]
	,[DG_HR-CS]
	,[DG_CS-DE]
	,[DG_FAck-FS]
	,[DG_LAck-FS]
	,[DG_FCS-CS]
	,[DG_LCS-CS]
	,[DG_FCD-DE]
	,[DG_LCD-DE]
	,[DG_CRD-DE]
	,[DG_ACRD-DE] 
from (
		SELECT [Version]
			  ,[Shipment Date]
			  ,[Profit Center L0 Name]
			  ,[Profit Center L1 Name]
			  ,[Profit Center L2 Name]
			  ,[Sales Order ID]
			  ,[Sales Order Line Item ID]
			  ,[Sales Order Source System Key]
			  ,[Sales Product Type Descr]
			  ,[Legacy Sales Order ID]
			  ,[Legacy Sales Order Line Item ID]
			  ,[GA AMID 4 NAME]
			  ,[GA AMID 2 ID]
			  ,[GA AMID 2 NAME]
			  ,[Sales Order Type Category Code]
			  ,[SNP Group Source Code]
			  ,[Global Business Unit Name]
			  ,[Higher Level Item Flag]
			  ,[OSS Higher Level Item]
			  ,[OSS Status On Item]
			  ,[Fulfillment OSS Status On Item]
			  ,[Parent Sales Order Item ID]
			  ,[Plant Code]
			  ,[Product Descr]
			  ,[Product Family Descr]
			  ,[Product ID]
			  ,[Product Line ID]
			  ,[Route Code]
			  ,[Sales Order Header Complete Delivery Flag]
			  ,[Base Quantity]
			  ,[Secured Position Net USD Amount]
			  ,[Ship To ISO Country Name]
			  ,[Shipment ID]
			  ,[OSS Shipment No]
			  ,[OSS SAP DeliveryNo]
			  ,[Shipment Line Item ID]
			  ,[Shipment to ISO Country Code]
			  ,[Shipping Receiving Point Code]
			  ,[Sold To Customer Name]
			  ,[Supplier]
			  ,[Supplying Division Sub Entity Code]
			  ,[Team]
			  ,[Focus Account Group]
			  ,[Unit Quantity]
			  ,[HPS Segment]
			  ,[ISS vs BCS Split]
			  ,[ASAP Flag]
			  ,[Business Area Code]
			  ,[Complexity Groups]
			  ,[Solution Order (Y N)]
			  ,[Coordination of Delivery Code]
			  ,[Delivery Priority Code]
			  ,[Delivery Priority Group]
			  ,[Derived End Customer Name]
			  ,[Distribution Center Code]
			  ,[B2B Sales Order ID]
			  ,[B2B Sales Order Line Item ID]
			  ,[B2B Concatenated Sales Order User Status Code]
			  ,[B2B Concatenated Sales Order Line Item User Status Code]
			  ,[Concatenated Sales Order User Status Code]
			  ,[Concatenated Sales Order Line Item User Status Code]
			  ,[B2B Shipment Date]
			  ,[B2B Shipment ID]
			  ,[B2B Shipment Line Item Identifier]
			  ,[Estimated End To End Transit Day No]
			  ,[Clean Order Date]
			  ,[Fulflmt Clean Order Date]
			  ,[Fulflmt ASAP Flag]
			  ,[Fulflmt Delivery Priority Code]
			  ,[Fulflmt Higher Level Item Flag]
			  ,[Fulflmt Plant Code]
			  ,[Fulflmt Route Code]
			  ,[Fulflmt Sales Order Header Distribution Center Code]
			  ,[Fulflmt Sales Order Header Complete Delivery Flag]
			  ,[Fulflmt Coordination of Delivery Code]
			  ,[Fulflmt Shipment Date]
			  ,[Fulflmt Shipping Receiving Point Code]
			  ,[B2B Sales Order Header Source System Key]
			  ,[GA AMID 4 ID]
			  ,[Supplier Complexity Group]
			  ,[Sales Document Item Category Code]
			  ,[Fulflmt Sales Document Item Category Code]
			  ,[Sales Organization Code]
			  ,[Special Process]
			  ,[HP Receive Date]
			  ,[Sales Order Detail Create Timestamp]
			  ,[Sales Order Detail Create Timestamp Local]
			  ,[Fulflmt Sales Order Detail Create Date]
			  ,[Create]
			  ,[Clean Item Date Time]
			  ,[Clean Item Date Time Local]
			  ,[Fulflmt Clean Item Date Time]
			  ,[Fulflmt Clean Item Date Time Local]
			  ,[Clean Header Date Time]
			  ,[Clean Header Date Time Local]
			  ,[Fulflmt Clean Header Date Time]
			  ,[Fulflmt Clean Header Date Time Local]
			  ,[Clean]
			  ,[Production Start Date]
			  ,[Production Start Date Local]
			  ,[Drop to Factory Timestamp]
			  ,[Drop to Factory Timestamp Local]
			  ,[Fulflmt Drop to Factory Timestamp]
			  ,[Fulflmt Drop to Factory Timestamp Local]
			  ,[Fulflmt Production Start Date]
			  ,[Fulflmt Production Start Date Local]
			  ,[Production Start]
			  ,[Factory Ship Date Time]
			  ,[Factory Ship Date Time Local]
			  ,[Fulflmt Factory Ship Date Time]
			  ,[Fulflmt Factory Ship Date Time Local]
			  ,[Factory Ship]
			  ,[OSS Cust Ship Date]
			  ,[OSS Cust Ship Date Local]
			  ,[Fulflmt OSS Cust Ship Date]
			  ,[Fulflmt OSS Cust Ship Date Local]
			  ,[Customer Ship]
			  ,[Special Type of Order]
			  ,[Hub Receive Date]
			  ,[Hub Receive Date Local]
			  ,[Fulflmt Hub Receive Date]
			  ,[Fulflmt Hub Receive Date Local]
			  ,[Hub Receive]
			  ,[OSS POD Date]
			  ,[Fulflmt OSS POD Date]
			  ,[Delivery]
			  ,[Customer Requested Delivery Date]
			  ,[Fulflmt Requested Delivery Date]
			  ,[Actual Customer Requested Delivery Date]
			  ,[First Planned Fact Ship Date]
			  ,[Fulflmt First Planned Fact Ship Date]
			  ,[First Planned Cust Ship]
			  ,[Fulflmt First Planned Cust Ship]
			  ,[First Planned Delivery Date]
			  ,[Fulflmt First Planned Delivery Date]
			  ,[Planned Factory Ship Date]
			  ,[Fulflmt Planned Factory Ship Date]
			  ,[First Touchpoint Scheduled Ship Timestamp]
			  ,[Last Touchpoint Scheduled Ship Timestamp]
			  ,[Planned Cust Ship Date]
			  ,[Planned Delivery Date]
			  ,[Production Done Date]
			  ,[Production Done Date Local]
			  ,[Fulflmt Production Done Date]
			  ,[Fulflmt Production Done Date Local]
			  ,[Production Done]
			  ,[Release2Fulfillment Date]
			  ,[Release2Fulfillment Date Local]
			  ,[Fulflmt Release2Fulfillment Date]
			  ,[Fulflmt Release2Fulfillment Date Local]
			  ,[SCOUT Delivered Date Time]
			  ,[Fulflmt SCOUT Delivered Date Time]
			  ,[SCOUT Latest Carrier Status Code]
			  ,[Fulflmt SCOUT Latest Carrier Status Code]
			  ,[SCOUT Latest Carrier Status Desc]
			  ,[Fulflmt SCOUT Latest Carrier Status Desc]
			  ,[SCOUT Latest Carr Status Reason Code]
			  ,[Fulflmt SCOUT Latest Carr Status Reason Code]
			  ,[SCOUT Latest Carr Status Reason Desc]
			  ,[Fulflmt SCOUT Latest Carr Status Reason Desc]
			  ,[SCOUT Pod Submission Date]
			  ,[Fulflmt SCOUT Pod Submission Date]
			  ,[DTFC -3 0]
			  ,[DTFC -x 0]
			  ,[DTCRD 0 0]
			  ,[DTCRD -x 0]
			  ,[DTACRD -x 0]
			  ,[STFC -x 0]
			  ,[STFC -2 0]
			  ,[STFC -3 0]
			  ,[STFA -x 0]
			  ,[STFA -2 0]
			  ,[STFA -3 0]
			  ,[RE to CR]
			  ,[RE to CL]
			  ,[RE to FS]
			  ,[Re to CS]
			  ,[RE to Del]
			  ,[CL to BT]
			  ,[CL to FS]
			  ,[BT to FGI]
			  ,[FGI to FS]
			  ,[FS to CS]
			  ,[BT to FS]
			  ,[FS to Hub R]
			  ,[Hub R to Hub S]
			  ,[CS to Del]
			  ,[CR to CL]
			  ,[Order_RE-CR]
			  ,[Order_CR-CL]
			  ,[Order_RE-CL]
			  ,[DG_RE-CR]
			  ,[DG_RE-CL]
			  ,[DG_RE-FS]
			  ,[DG_RE-CS]
			  ,[DG_RE-DE]
			  ,[DG_CR-CL]
			  ,[DG_CL-BT]
			  ,[DG_CL-FGI]
			  ,[DG_CL-FS]
			  ,[DG_BT-FGI]
			  ,[DG_BT-FS]
			  ,[DG_BT-CS]
			  ,[DG_FGI-FS]
			  ,[DG_FS-HR]
			  ,[DG_FS-CS]
			  ,[DG_HR-CS]
			  ,[DG_CS-DE]
			  ,[DG_FAck-FS]
			  ,[DG_LAck-FS]
			  ,[DG_FCS-CS]
			  ,[DG_LCS-CS]
			  ,[DG_FCD-DE]
			  ,[DG_LCD-DE]
			  ,[DG_CRD-DE]
			  ,[DG_ACRD-DE]
			  ,ROW_NUMBER() OVER(Partition BY [Sales Order Source System Key]
										,[Shipment ID]
										,[Shipment Line Item ID] 
								ORDER BY	[Sales Order Source System Key]
										,[Shipment ID]
										,[Shipment Line Item ID]
										,Version desc) AS Row 
		  FROM [cpx].[dbo].[Egoras] where [Profit Center L0 Name] = 'EMEA') as a
where Row = 1
