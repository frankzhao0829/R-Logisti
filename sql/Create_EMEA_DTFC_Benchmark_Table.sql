USE [cpx]
GO

/****** Object:  Table [dbo].[EMEA_DTFC_Benchmark]    Script Date: 2/27/2017 5:18:45 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

SET ANSI_PADDING ON
GO

CREATE TABLE [dbo].[EMEA_DTFC_Benchmark](
	[Legacy Sales Order ID] [varchar](12) NULL,
	[Sales Order ID] [varchar](50) NULL,
	[Shipment ID] [varchar](50) NULL,
	[Complexity Groups] [varchar](50) NULL,
	[Supplier Complexity Group] [varchar](50) NULL,
	[Plant Code] [varchar](10) NULL,
	[Coordination of Delivery Code] [varchar](50) NULL,
	[Distribution Center Code] [varchar](10) NULL,
	[Delivery Priority Code] [varchar](50) NULL,
	[DG_FCD-DE] [varchar](50) NULL,
	[ASAP Flag] [varchar](1) NULL,
	[Higher Level Item Flag] [varchar](1) NULL,
	[Ship To ISO Country Name] [varchar](50) NULL,
	[Route Code] [varchar](8) NULL,
	[SCOUT Latest Carr Status Reason Desc] [varchar](50) NULL,
	[Hold Map] [varchar](50) NULL,
	[Hold] [varchar](255) NULL,
	[Count Items] [varchar](50) NULL,
	[Create] [datetime] NULL,
	[Clean] [datetime] NULL,
	[Factory Ship] [datetime] NULL,
	[Shipment Date] [datetime] NULL,
	[Delivery] [datetime] NULL,
	[First Planned Factory Shipdate] [datetime] NULL,
	[Last Planned Factory Shipdate] [datetime] NULL,
	[First Planned Customer Shipdate] [datetime] NULL,
	[Last Planned Customer Shipdate] [datetime] NULL,
	[First Planned Delivery Date] [datetime] NULL,
	[Last Planned Delivery Date] [datetime] NULL,
	[STFA (-x 0)] [smallint] NULL,
	[STFA (-2 0)] [smallint] NULL,
	[STFC (-x 0)] [smallint] NULL,
	[STFC (-3 0)] [smallint] NULL,
	[DTFC (-x 0)] [smallint] NULL,
	[DTFC (-3 0)] [smallint] NULL,
	[Dataset DTFC Available & ASAP] [bit] NULL,
	[Subregion] [varchar](50) NULL,
	[Supplier] [varchar](20) NULL,
	[Complexity] [varchar](100) NULL,
	[Coordination] [smallint] NULL,
	[Clean to Ship Leadtime] [smallint] NULL,
	[Hub Leadtime] [smallint] NULL,
	[Logistics Leadtime] [real] NULL,
	[Avg Mfg Leadtime] [real] NULL,
	[Avg Hub Leadtime] [real] NULL,
	[Avg Logistics Leadtime] [real] NULL,
	[Dataset counted] [bit] NULL
) ON [PRIMARY]

GO

SET ANSI_PADDING OFF
GO

