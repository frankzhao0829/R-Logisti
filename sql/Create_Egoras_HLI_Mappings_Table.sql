USE [cpx]
GO

/****** Object:  Table [dbo].[Egoras_HLI_Mappings]    Script Date: 2/25/2017 2:30:11 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[Egoras_HLI_Mappings](
	[Profit Center L0 Name] [nvarchar](20) NULL,
	[Sales Order ID] [nvarchar](20) NULL,
	[Sales Order Line Item ID] [nvarchar](20) NULL,
	[Topmost Parent Line Item ID] [nvarchar](20) NULL
) ON [PRIMARY]

GO

