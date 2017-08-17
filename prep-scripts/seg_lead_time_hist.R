
REGION <- 'APJ'
#FILE <- paste0('data/transformed/', REGION, '_HLI_CLEAN.rds')
FILE <- paste0('data/transformed/', REGION, '_QADTA_HLI.rds')

################################################################################
############################## LOADING PACKAGES ################################
################################################################################

library(ggplot2)
library(dplyr)

################################################################################
################################# DATA LOAD ####################################
################################################################################

## LOAD EMEA High Level Clean Data
higherLevelData <- readRDS(FILE)

################################################################################
#############################  PREPARE INPUT DATA ##############################
################################################################################

# Train Data Selection & RENAME the columns
RFDataSet <- higherLevelData %>%
  #filter(`Supplier Complexity Group` %in% TARGETCMPLXTY) %>%
  filter(`ASAP Flag` == 'Y') %>%
  select(SalesOrderID = `Sales Order ID`,
         SOLineItemID = `Sales Order Line Item ID`,
         DeliveryGroupID = `DG_RE-DE`,
         ProductID = `Product ID`,
         ProdFamilyDescr = `Product Family Descr`,
         RouteCode = `Route Code`,
         SalesProdType = `Sales Product Type Descr`,
         SNPCode = `SNP Group Source Code`,
         GBU = `Global Business Unit Name`,
         PlantCode = `Plant Code`,
         AMID2ID = `GA AMID 2 ID`,
         ShipToCountryCode = `Shipment to ISO Country Code`,
         ProdLineID = `Product Line ID`,
         SOHeaderCompleteFlag = `Sales Order Header Complete Delivery Flag`,
         BaseQuantity = `Base Quantity`,
         NetUSDAmount = `Secured Position Net USD Amount`,
         ShipRePointCode = `Shipping Receiving Point Code`,
         Supplier,
         SupDivEntityCode = `Supplying Division Sub Entity Code`,
         Team,
         FocusAccountGroup = `Focus Account Group`,
         UnitQuantity = `Unit Quantity`,
         HPSSeg = `HPS Segment`,
         ISSBCSSplit = `ISS vs BCS Split`,
         SolutionOrder = `Solution Order (Y N)`,
         CoordDelCode = `Coordination of Delivery Code`,
         DelPriorityCode = `Delivery Priority Code`,
         DisCenterCode = `Distribution Center Code`,
         EstE2EDayNo = `Estimated End To End Transit Day No`,
         SalesDocItemCatCode = `Sales Document Item Category Code`,
         SalesOrgCode = `Sales Organization Code`,
         ComplexityGroups = `Complexity Groups`,
         SupplierComplexityGroup = `Supplier Complexity Group`,
         HPREDate = `HP Receive Date`,
         FPDelDate = `First Planned Delivery Date`,
         Clean,
         ProdStart = `Production Start`,
         FShip = `Factory Ship`,
         CShip = `Customer Ship`,
         FPCustDate = `First Planned Cust Ship`,
         Delivery,
         ITM_COUNT,
         ConfigText,
         SRT,
         SRT_PADDING_VALUE,
         QUOTED_EDT,
         Ctry_Rgn
  )

## Calculate lead time in diffrent segs

# US Holiday, Sat/Sun as non-working days
cal <- bizdays::create.calendar("US/ANBIMA", weekdays=c("saturday", "sunday"))

RFDataSet <- RFDataSet %>%
  mutate(ATPREtoDel  = bizdays::bizdays(HPREDate , FPDelDate , cal),
         ATPCLtoDel  = bizdays::bizdays(Clean    , FPDelDate , cal),
         ATPCLtoCS   = bizdays::bizdays(Clean    , FPCustDate, cal),
         calcREtoDel = bizdays::bizdays(HPREDate , Delivery  , cal),
         calcREtoFS  = bizdays::bizdays(HPREDate , FShip     , cal),
         calcREtoCL  = bizdays::bizdays(HPREDate , Clean     , cal),
         calcCLtoDel = bizdays::bizdays(Clean    , Delivery  , cal),
         calcCLtoFS  = bizdays::bizdays(Clean    , FShip     , cal),
         calcCLtoCS  = bizdays::bizdays(Clean    , CShip     , cal),
         calcCLtoBT  = bizdays::bizdays(Clean    , ProdStart , cal),
         calcBTtoCS  = bizdays::bizdays(ProdStart, CShip     , cal),
         calcBTtoFS  = bizdays::bizdays(ProdStart, FShip     , cal),
         calcCStoDel = bizdays::bizdays(CShip    , Delivery  , cal),
         calcFStoDel = bizdays::bizdays(FShip    , Delivery  , cal))

# Filter out invalid values
# CAUTION: may affect the derived engineering features calculation
RFDataSet <- RFDataSet %>%
  filter(ATPREtoDel >= 0 &
           ATPCLtoDel  >= 0 &
           ATPCLtoCS   >= 0 &
           calcREtoDel >= 0 &
           calcREtoFS  >= 0 &
           calcREtoCL  >= 0 &
           calcCLtoDel >= 0 &
           calcCLtoFS  >= 0 &
           calcCLtoCS  >= 0 &
           calcCLtoBT  >= 0 &
           calcBTtoCS  >= 0 &
           calcBTtoFS  >= 0 &
           calcCStoDel >= 0 &
           (!is.na(ITM_COUNT)) & #TODO: ITM_COUNT in DB has NULL value (5)
           calcFStoDel >= 0)

df <- RFDataSet

df_REtoDel <- data.frame(Target="RE To Del", LT=df$calcREtoDel)
df_REtoFS  <- data.frame(Target="RE To FS" , LT=df$calcREtoFS)
df_FStoDel <- data.frame(Target="FS To Del", LT=df$calcFStoDel)

ggdf <- rbind(df_REtoDel, df_REtoFS, df_FStoDel)

df_by_target_q80 <- ggdf %>% group_by(Target) %>% summarise_at(vars(LT80=LT), quantile, probs=0.8)

png(filename = paste0("LT_Hist_",REGION, ".png"), type = 'cairo',
    width = 2000,
    height = 1500,
    res = 300)

ggplot(ggdf, aes(x=LT, color=Target, fill=Target)) +
  geom_histogram(binwidth = 1) + facet_grid(Target ~ .) +
  geom_vline(data = df_by_target_q80, aes(xintercept = LT80), color = "red", linetype="dashed") +
  geom_text(data = df_by_target_q80, aes(x = LT80 + 8, label=paste("LT = ", LT80), y=800), color='red') +
  labs(title=paste("Lead Time Histogram Plot for", REGION), x="Lead Time", y="count")

dev.off()
