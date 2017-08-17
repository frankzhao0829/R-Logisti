USE [cpx]
GO

/****** Object:  Table [dbo].[EMEA_SIM_BENCHMARK]    Script Date: 2/27/2017 5:20:01 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[EMEA_SIM_BENCHMARK](
	[Legacy Sales Order ID] [nvarchar](255) NULL,
	[Sales Order ID] [nvarchar](255) NULL,
	[Shipment ID] [nvarchar](255) NULL,
	[Complexity Groups] [nvarchar](255) NULL,
	[Supplier Complexity Group] [nvarchar](255) NULL,
	[Plant Code] [nvarchar](255) NULL,
	[Coordination of Delivery Code] [nvarchar](255) NULL,
	[Distribution Center Code] [nvarchar](255) NULL,
	[Delivery Priority Code] [nvarchar](255) NULL,
	[DG_FCD-DE] [nvarchar](255) NULL,
	[ASAP Flag] [nvarchar](255) NULL,
	[Higher Level Item Flag] [nvarchar](255) NULL,
	[Ship To ISO Country Name] [nvarchar](255) NULL,
	[Route Code] [nvarchar](255) NULL,
	[SCOUT Latest Carr Status Reason Desc] [nvarchar](255) NULL,
	[Hold Map] [nvarchar](255) NULL,
	[Hold] [nvarchar](255) NULL,
	[Count Items] [float] NULL,
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
	[STFA (x/0)] [float] NULL,
	[STFA (2/0)] [float] NULL,
	[STFC (x/0)] [float] NULL,
	[STFC (3/0)] [float] NULL,
	[DTFC (x/0)] [float] NULL,
	[DTFC (3/0)] [float] NULL,
	[Dataset DTFC Available & ASAP] [bit] NOT NULL,
	[Subregion] [nvarchar](255) NULL,
	[Supplier] [nvarchar](255) NULL,
	[Complexity] [nvarchar](255) NULL,
	[Coordination] [nvarchar](255) NULL,
	[Clean to Ship Leadtime] [nvarchar](255) NULL,
	[Hub Leadtime] [nvarchar](255) NULL,
	[Logistics Leadtime] [nvarchar](255) NULL,
	[Avg Mfg Leadtime] [nvarchar](255) NULL,
	[Avg Hub Leadtime] [nvarchar](255) NULL,
	[Avg Logistics Leadtime] [nvarchar](255) NULL,
	[Dataset counted] [bit] NOT NULL
) ON [PRIMARY]

GO

