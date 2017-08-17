USE [cpx]
GO

/****** Object:  Table [dbo].[EMEAShippingPointMappings]    Script Date: 2/22/2017 9:29:55 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[EMEAShippingPointMappings](
	[Plant Code] [nvarchar](255) NULL,
	[Shipping Receiving Point Code] [nvarchar](255) NULL,
	[Shipping Point] [nvarchar](255) NULL
) ON [PRIMARY]

GO


