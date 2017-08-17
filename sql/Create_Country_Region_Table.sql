USE [cpx]
GO

/****** Object:  Table [dbo].[Country_Region]    Script Date: 2/25/2017 2:29:39 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

SET ANSI_PADDING ON
GO

CREATE TABLE [dbo].[Country_Region](
	[Ship To ISO Country Name] [varchar](50) NULL,
	[Shipment to ISO Country Code] [varchar](50) NULL,
	[Region] [varchar](50) NULL
) ON [PRIMARY]

GO

SET ANSI_PADDING OFF
GO

