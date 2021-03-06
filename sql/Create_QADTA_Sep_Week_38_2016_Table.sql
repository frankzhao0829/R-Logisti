USE [cpx]
GO

/****** Object:  Table [dbo].[QADTA_Sep_Week_38_2016]    Script Date: 2/22/2017 9:29:08 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

SET ANSI_PADDING ON
GO

CREATE TABLE [dbo].[QADTA_Sep_Week_38_2016](
	[Delivery_Quarter] [varchar](2) NULL,
	[Delivery_Fiscal_Month] [varchar](20) NULL,
	[Delivery_Week] [varchar](255) NULL,
	[DG_RE_DE] [varchar](50) NULL,
	[Profit_Center_L0_Name] [varchar](20) NULL,
	[Profit_Center_L2_Name] [varchar](20) NULL,
	[SALES_ORDER_ID] [varchar](50) NULL,
	[GA_AMID_4_NAME] [varchar](255) NULL,
	[DELIVERY] [datetime] NULL,
	[RE_TO_DEL] [smallint] NULL,
	[qv SLS_QTN_ID] [varchar](50) NULL,
	[qv SLS_QTN_VRSN_SQN_NR] [smallint] NULL,
	[ASSET_QUOTE_NR] [varchar](20) NULL,
	[ASSET_QUOTE_NO_VRSN] [varchar](20) NULL,
	[COMPLETION_TS] [datetime] NULL,
	[OPPORTUNITY_ID] [varchar](50) NULL,
	[SRT] [smallint] NULL,
	[SRT_PADDING_VALUE] [smallint] NULL,
	[QUOTED_EDT] [smallint] NULL,
	[SPECIAL_CODE_QUOTE] [smallint] NULL,
	[Diff_RE_TO_DEL_QUOTED_EDT] [int] NULL,
	[Abs_Diff_RE_TO_DEL_QUOTED_EDT] [int] NULL,
	[First_Planned_Delivery_Date] [datetime] NULL,
	[Customer_Requested_Delivery_Date] [datetime] NULL,
	[SFDC_Status] [varchar](50) NULL,
	[Diff_First_Planned_Del_DT_HP_Receive_DT] [int] NULL,
	[Country_CD] [varchar](20) NULL,
	[ASSET_INSTANCE] [varchar](50) NULL,
	[Difference (Z-S)] [int] NULL
) ON [PRIMARY]

GO

SET ANSI_PADDING OFF
GO


