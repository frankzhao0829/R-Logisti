USE [cpx]
GO

/****** Object:  Table [dbo].[HPE_Quotes]    Script Date: 3/2/2017 2:58:34 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

SET ANSI_PADDING ON
GO

CREATE TABLE [dbo].[HPE_Quote_Headers](
	[qvSLS_QTN_ID] [nvarchar](255) NULL,
	[qvSLS_QTN_VRSN_SQN_NR] [nvarchar](255) NULL,
	[ORIG_ASSET] [nvarchar](255) NULL,
	[ASSET_QUOTE_NR] [nvarchar](255) NULL,
	[ASSET_QUOTE_VRSN] [nvarchar](255) NULL,
	[MODIFIED_TS] [nvarchar](255) NULL,
	[MODIFIED_PERSON_ID] [nvarchar](255) NULL,
	[CREATION_PERSON_ID] [nvarchar](255) NULL,
	[PRICE_LIST_TYPE] [nvarchar](255) NULL,
	[SFDC_STATUS] [nvarchar](255) NULL,
	[OPPORTUNITY_ID] [nvarchar](255) NULL,
	[REQUEST_ID] [nvarchar](255) NULL,
	[Deal_Nr] [nvarchar](255) NULL,
	[sub_total_amt] [nvarchar](255) NULL,
	[total_tax_amt] [nvarchar](255) NULL,
	[SHIPPING_AND_HANDLING_AMT] [nvarchar](255) NULL,
	[TOTAL_AMT] [nvarchar](255) NULL,
	[TOTAL_LIST_PRICE_AMT] [nvarchar](255) NULL,
	[DISCOUNT_PCT] [nvarchar](255) NULL,
	[DISCOUNT_AMT] [nvarchar](255) NULL,
	[PAYMENT_TERM] [nvarchar](255) NULL,
	[PRMRY_QUOTE_URL] [nvarchar](max) NULL,
	[REGION_CD] [nvarchar](255) NULL,
	[AMP_ID] [nvarchar](255) NULL,
	[GROUP_CONTRACT_NR] [nvarchar](255) NULL,
	[SERVICE_AGRMNT_ID] [nvarchar](255) NULL,
	[SALES_ORG] [nvarchar](255) NULL,
	[MULTIYEAR_IND] [nvarchar](255) NULL,
	[BILLING_SCHDL] [nvarchar](255) NULL,
	[EXPIRING_CONTRACT_NR] [nvarchar](255) NULL,
	[CONTRACT_START_DATE] [nvarchar](255) NULL,
	[CONTRACT_END_DATE] [nvarchar](255) NULL,
	[TERMS_IN_MONTHS] [nvarchar](255) NULL,
	[DEAL_PHASE] [nvarchar](255) NULL,
	[LAST_PRICED_DT] [nvarchar](255) NULL,
	[DELIVERY_SERVICES] [nvarchar](255) NULL,
	[DELIVERY_SPEED] [nvarchar](255) NULL,
	[PA_CAC] [nvarchar](255) NULL,
	[PA_NR] [nvarchar](255) NULL,
	[ROUND_DIGIT] [nvarchar](255) NULL,
	[SPECIAL_HANDLING] [nvarchar](255) NULL,
	[DELIVERY_TERMS] [nvarchar](255) NULL,
	[TITLE_PASSES_AT] [nvarchar](255) NULL,
	[SIGNIFICANT_DIGITS] [nvarchar](255) NULL,
	[BMI_ID] [nvarchar](255) NULL,
	[NAME] [nvarchar](255) NULL,
	[PRICE_GEO] [nvarchar](255) NULL,
	[ASSET_QUOTE_NR_AND_VRSN] [nvarchar](255) NULL,
	[COMPLETION_TS] [nvarchar](255) NULL,
	[Tax_Rate] [nvarchar](255) NULL,
	[Total_Regulatory_Fee] [nvarchar](255) NULL,
	[AIRPACKED_TOTALWEIGHT] [nvarchar](255) NULL,
	[ORG_ID] [nvarchar](255) NULL,
	[TOTAL_PRICE_PREV_CONTRACT] [nvarchar](255) NULL,
	[PROFILE_ID] [nvarchar](255) NULL,
	[CUST_SPCEF_QT_NR] [nvarchar](255) NULL,
	[NO_SRVC_RSN_CD] [nvarchar](255) NULL,
	[APPL_CODE_VERTICAL] [nvarchar](255) NULL,
	[COMMENCE_DATE_TIME] [nvarchar](255) NULL,
	[COMMERCIALITY_CODE] [nvarchar](255) NULL,
	[PARTNER_ID] [nvarchar](255) NULL,
	[ASSET_INSTANCE] [nvarchar](255) NULL,
	[Pdf_template_id] [nvarchar](255) NULL,
	[SRT_PADDING_VALUE] [nvarchar](255) NULL,
	[LIST_PRICE_ONLY_QUOTE_FL] [nvarchar](255) NULL,
	[PA_DISCOUNT_GEO] [nvarchar](255) NULL,
	[PA_EXPIRY_TS] [nvarchar](255) NULL,
	[REQUEST_F_QUOTE_RCVD_TS] [nvarchar](255) NULL,
	[PRICE_TERM_CD] [nvarchar](255) NULL,
	[DESCRIPTION] [nvarchar](255) NULL,
	[CREATION_TS] [nvarchar](255) NULL,
	[EFFECTIVE_TS] [nvarchar](255) NULL,
	[EXPIRY_TS] [nvarchar](255) NULL,
	[COUNTRY_CD] [nvarchar](255) NULL,
	[CURRENCY_CD] [nvarchar](255) NULL,
	[LANG_CD] [nvarchar](255) NULL,
	[SLS_QTN_VRSN_TYP_CD] [nvarchar](255) NULL,
	[SLS_CHNL_CD] [nvarchar](255) NULL,
	[CSTM_CNFGN_IND] [nvarchar](255) NULL,
	[HAND_OFF_TS] [nvarchar](255) NULL,
	[RQST_F_ORD_RCVD_TS] [nvarchar](255) NULL,
	[SLS_QTN_VRSN_STTS_CD] [nvarchar](255) NULL,
	[CUST_RQST_TS] [nvarchar](255) NULL,
	[ACCEPT_QUOTE_DT] [nvarchar](255) NULL,
	[DEAL_VRSN] [nvarchar](255) NULL,
	[QT_CNCLD_RSN_CD] [nvarchar](255) NULL,
	[QT_CNCLD_CMMNT] [nvarchar](255) NULL,
	[NO_SRVC_FL] [nvarchar](255) NULL,
	[LOW_SRVC_FL] [nvarchar](255) NULL,
	[ESCALATED_PRICE_BY] [nvarchar](255) NULL,
	[ESCALATED_PRICE_BY_EMAIL] [nvarchar](255) NULL,
	[LEADBU] [nvarchar](255) NULL,
	[CUSTOMERNAME] [nvarchar](255) NULL,
	[CUSTOMERMDCPORGID] [nvarchar](255) NULL,
	[CUSTOMERMDCPSITEID] [nvarchar](255) NULL,
	[CHANNELPARTNERNAMERESELLER] [nvarchar](255) NULL,
	[CHANELPARTNERIDRESELLER] [nvarchar](255) NULL,
	[DISTRIBUTORNAME] [nvarchar](255) NULL,
	[MDCPCUSTOMERSEGMENT] [nvarchar](255) NULL,
	[CUSTOMERENGAGEMENT] [nvarchar](255) NULL,
	[PREESCALATEDDISCOUNT] [nvarchar](255) NULL,
	[ESCALATEDDISCOUNT] [nvarchar](255) NULL,
	[PREESCALATEDTOTALQUOTEVALUE] [nvarchar](255) NULL,
	[ESCALATEDTOTALQUOTEVALUE] [nvarchar](255) NULL,
	[FULFILLMENT] [nvarchar](255) NULL,
	[TENANT_CD] [nvarchar](255) NULL,
	[customer_type] [nvarchar](255) NULL,
	[city] [nvarchar](255) NULL,
	[cityarea] [nvarchar](255) NULL,
	[state] [nvarchar](255) NULL,
	[street] [nvarchar](255) NULL,
	[street2] [nvarchar](255) NULL,
	[street3] [nvarchar](255) NULL,
	[postalcode] [nvarchar](255) NULL,
	[country] [nvarchar](255) NULL,
	[ORIGINATING_APP] [nvarchar](255) NULL,
	[AbsolutlyNoSupportFlag] [nvarchar](255) NULL,
	[PenRateProxyPercent] [nvarchar](255) NULL,
	[Distributor_on_Behalf_flag] [nvarchar](255) NULL,
	[SalesEmpApprover] [nvarchar](255) NULL,
	[ARUBA_QUOTE_FL] [nvarchar](255) NULL,
	[QuoteOwner] [nvarchar](255) NULL
) ON [PRIMARY]

GO

SET ANSI_PADDING OFF
GO

