USE [cpx]
GO

/****** Object:  View [dbo].[QADTA_Intersect_HLI_NEW_v]    Script Date: 2/25/2017 1:54:15 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

/* order by CountLineItems DESC,a.[SALES_ORDER_ID]  */
CREATE VIEW [dbo].[QADTA_Intersect_HLI_NEW_v]
AS
SELECT b.[Profit Center L0 Name], b.[DG_RE-DE], b.[Sales Order ID], b.[Sales Order Line Item ID], a.SPECIAL_CODE_QUOTE, b.[GA AMID 4 NAME], b.[GA AMID 2 ID], b.[GA AMID 2 NAME], 
             b.[Sales Order Type Category Code], b.[SNP Group Source Code], b.[Global Business Unit Name], b.[Plant Code], b.[Product Descr], b.[Product Family Descr], b.[Product ID], b.[Product Line ID], b.[Route Code], 
             b.[Sales Order Header Complete Delivery Flag], b.[Base Quantity], b.[Shipment to ISO Country Code], b.[Shipping Receiving Point Code], b.Supplier, b.[Supplying Division Sub Entity Code], b.Team, 
             b.[Focus Account Group], b.[HPS Segment], b.[ISS vs BCS Split], b.[ASAP Flag], b.[Business Area Code], b.[Complexity Groups], b.[Solution Order (Y N)], b.[Coordination of Delivery Code], 
             b.[Delivery Priority Code], b.[Delivery Priority Group], b.[Supplier Complexity Group], b.[Sales Document Item Category Code], b.[Sales Organization Code], b.[Special Process], 
             b.[Estimated End To End Transit Day No], b.[HP Receive Date], b.Clean, b.[Production Start], b.[Factory Ship], b.[Customer Ship], b.[Hub Receive Date], b.[Hub Receive], b.Delivery, 
             b.[First Planned Fact Ship Date], b.[First Planned Cust Ship], b.[First Planned Delivery Date], b.HLI_ID, b.ITM_COUNT, b.ConfigText, b.MultiShipFlag, b.Ctry_Rgn, COUNT(DISTINCT a.Country_CD) 
             AS CountCtryCd, COUNT(DISTINCT b.[Shipment Line Item ID]) AS CountLineItems, COUNT(DISTINCT a.GA_AMID_4_NAME) AS CountCustomers, MIN(a.DELIVERY) AS MinDel, MAX(a.DELIVERY) AS MaxDel, 
             MAX(a.SRT) AS SRT, MAX(a.SRT_PADDING_VALUE) AS SRTPAD, MIN(a.QUOTED_EDT) AS minquoted_edt, MAX(a.QUOTED_EDT) AS QUOTED_EDT, MIN(a.RE_TO_DEL) AS MinRE_to_Del, MAX(a.RE_TO_DEL) 
             AS RE_to_Del, MIN(a.Diff_RE_TO_DEL_QUOTED_EDT) AS mindiff, MAX(a.Diff_RE_TO_DEL_QUOTED_EDT) AS maxdiff, MAX(a.First_Planned_Delivery_Date) AS MaxFC, MAX(a.Delivery_Quarter) AS MaxDelQ, 
             MAX(a.Delivery_Fiscal_Month) AS MaxDelM, MAX(a.Delivery_Week) AS MaxDelW, b.[Secured Position Net USD Amount], b.[Ship To ISO Country Name], b.[Distribution Center Code]
FROM   dbo.QADTA_Sep_Week_38_2016 AS a INNER JOIN
             dbo.Region_HLI_CLEAN AS b ON a.SALES_ORDER_ID = CAST(b.[Sales Order ID] AS float) AND a.DG_RE_DE = b.[DG_RE-DE] AND a.Profit_Center_L0_Name = b.[Profit Center L0 Name]
GROUP BY b.[Profit Center L0 Name], b.[DG_RE-DE], b.[Sales Order ID], b.[Sales Order Line Item ID], a.SPECIAL_CODE_QUOTE, b.[GA AMID 4 NAME], b.[GA AMID 2 ID], b.[GA AMID 2 NAME], 
             b.[Sales Order Type Category Code], b.[SNP Group Source Code], b.[Global Business Unit Name], b.[Plant Code], b.[Product Descr], b.[Product Family Descr], b.[Product ID], b.[Product Line ID], b.[Route Code], 
             b.[Sales Order Header Complete Delivery Flag], b.[Base Quantity], b.[Shipment to ISO Country Code], b.[Shipping Receiving Point Code], b.Supplier, b.[Supplying Division Sub Entity Code], b.Team, 
             b.[Focus Account Group], b.[HPS Segment], b.[ISS vs BCS Split], b.[ASAP Flag], b.[Business Area Code], b.[Complexity Groups], b.[Solution Order (Y N)], b.[Coordination of Delivery Code], 
             b.[Delivery Priority Code], b.[Delivery Priority Group], b.[Supplier Complexity Group], b.[Sales Document Item Category Code], b.[Sales Organization Code], b.[Special Process], 
             b.[Estimated End To End Transit Day No], b.[HP Receive Date], b.Clean, b.[Production Start], b.[Factory Ship], b.[Customer Ship], b.[Hub Receive Date], b.[Hub Receive], b.Delivery, 
             b.[First Planned Fact Ship Date], b.[First Planned Cust Ship], b.[First Planned Delivery Date], b.HLI_ID, b.ITM_COUNT, b.ConfigText, b.MultiShipFlag, b.Ctry_Rgn, b.[Secured Position Net USD Amount], 
             b.[Ship To ISO Country Name], b.[Distribution Center Code]

GO

EXEC sys.sp_addextendedproperty @name=N'MS_DiagramPane1', @value=N'[0E232FF0-B466-11cf-A24F-00AA00A3EFFF, 1.00]
Begin DesignProperties = 
   Begin PaneConfigurations = 
      Begin PaneConfiguration = 0
         NumPanes = 4
         Configuration = "(H (1[40] 4[20] 2[20] 3) )"
      End
      Begin PaneConfiguration = 1
         NumPanes = 3
         Configuration = "(H (1 [50] 4 [25] 3))"
      End
      Begin PaneConfiguration = 2
         NumPanes = 3
         Configuration = "(H (1 [50] 2 [25] 3))"
      End
      Begin PaneConfiguration = 3
         NumPanes = 3
         Configuration = "(H (4 [30] 2 [40] 3))"
      End
      Begin PaneConfiguration = 4
         NumPanes = 2
         Configuration = "(H (1 [56] 3))"
      End
      Begin PaneConfiguration = 5
         NumPanes = 2
         Configuration = "(H (2 [66] 3))"
      End
      Begin PaneConfiguration = 6
         NumPanes = 2
         Configuration = "(H (4 [50] 3))"
      End
      Begin PaneConfiguration = 7
         NumPanes = 1
         Configuration = "(V (3))"
      End
      Begin PaneConfiguration = 8
         NumPanes = 3
         Configuration = "(H (1[56] 4[18] 2) )"
      End
      Begin PaneConfiguration = 9
         NumPanes = 2
         Configuration = "(H (1 [75] 4))"
      End
      Begin PaneConfiguration = 10
         NumPanes = 2
         Configuration = "(H (1[66] 2) )"
      End
      Begin PaneConfiguration = 11
         NumPanes = 2
         Configuration = "(H (4 [60] 2))"
      End
      Begin PaneConfiguration = 12
         NumPanes = 1
         Configuration = "(H (1) )"
      End
      Begin PaneConfiguration = 13
         NumPanes = 1
         Configuration = "(V (4))"
      End
      Begin PaneConfiguration = 14
         NumPanes = 1
         Configuration = "(V (2))"
      End
      ActivePaneConfig = 0
   End
   Begin DiagramPane = 
      Begin Origin = 
         Top = 0
         Left = 0
      End
      Begin Tables = 
         Begin Table = "a"
            Begin Extent = 
               Top = 9
               Left = 57
               Bottom = 205
               Right = 487
            End
            DisplayFlags = 280
            TopColumn = 8
         End
         Begin Table = "b"
            Begin Extent = 
               Top = 9
               Left = 544
               Bottom = 205
               Right = 1122
            End
            DisplayFlags = 280
            TopColumn = 217
         End
      End
   End
   Begin SQLPane = 
   End
   Begin DataPane = 
      Begin ParameterDefaults = ""
      End
   End
   Begin CriteriaPane = 
      Begin ColumnWidths = 12
         Column = 1440
         Alias = 900
         Table = 1170
         Output = 720
         Append = 1400
         NewValue = 1170
         SortType = 1350
         SortOrder = 1410
         GroupBy = 1350
         Filter = 1350
         Or = 1350
         Or = 1350
         Or = 1350
      End
   End
End
' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'VIEW',@level1name=N'QADTA_Intersect_HLI_NEW_v'
GO

EXEC sys.sp_addextendedproperty @name=N'MS_DiagramPaneCount', @value=1 , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'VIEW',@level1name=N'QADTA_Intersect_HLI_NEW_v'
GO


