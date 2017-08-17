USE [cpx]
GO

/****** Object:  Table [dbo].[SKU_Lead_Time_20170320]    Script Date: 3/30/2017 5:46:06 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

SET ANSI_PADDING ON
GO

CREATE TABLE [dbo].[SKU_Lead_Time_20170320](
	[Region] [varchar](255) NULL,
	[Country] [varchar](255) NULL,
	[ProductSKU] [varchar](255) NULL,
	[LeadTime] [int] NULL,
	[UpdatedDT] [datetime] NULL,
	[ProductID]  AS (case when charindex('#',[ProductSKU])>(0) then left([ProductSKU],charindex('#',[ProductSKU])-(1)) else [ProductSKU] end),
	[SKUHashValue]  AS (case when charindex('#',[ProductSKU])>(0) then right([ProductSKU],(4)) else '' end)
) ON [PRIMARY]

GO

SET ANSI_PADDING OFF
GO


