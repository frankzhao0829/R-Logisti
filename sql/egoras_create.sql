-- Table Schema for Raw Egoras files (csv/tsv) with Version number (week No.)
-- Applied to table Egoras/TestEgoras/Egoras_SHP_DTL
-- CAUTION: nvarchar length setting is done manually, and may cause error if the value is longer than the settings


CREATE TABLE [dbo].[Egoras](
	[Version] [smallint] NULL,
	[Shipment Date] [datetime] NULL,
	[Profit Center L0 Name] [nvarchar](20) NULL,
	[Profit Center L1 Name] [nvarchar](20) NULL,
	[Profit Center L2 Name] [nvarchar](50) NULL,
	[Sales Order ID] [nvarchar](20) NULL,
	[Sales Order Line Item ID] [nvarchar](20) NULL,
	[Sales Order Source System Key] [nvarchar](20) NULL,
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
	[Shipment ID] [nvarchar](20) NULL,
	[OSS Shipment No] [nvarchar](20) NULL,
	[OSS SAP DeliveryNo] [nvarchar](20) NULL,
	[Shipment Line Item ID] [nvarchar](20) NULL,
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
	[DG_ACRD-DE] [nvarchar](max) NULL
)
