## Last Modified @ 2017-03-03 05:39:17  GMT ------------------------------

## For Microsoft R Open
#checkpoint::checkpoint('2016-10-31')

################################################################################
################################## CONSTANTS ###################################
################################################################################

#RFPKG <- 'ranger'
#RFPKG <- 'randomForest'
RFPKG <- 'quantregForest'

if (RFPKG=='quantregForest') Quantile <- 0.75 else Quantile <- NULL

#TARGETCMPLXTY <- c('CTO', 'Complex CTO', 'Configured Rack', 'Rack Solution')
REGION <- 'EMEA'
FILE <- paste0('data/transformed/', REGION, '_HLI_CLEAN.rds')

################################################################################
############################## LOADING PACKAGES ################################
################################################################################

library(dplyr)
library(RFPKG, character.only = TRUE)
library(ggplot2)
library(doParallel)
library(foreach)
library(bizdays)

source('R/FcstComp.R')
source('R/RFModel.R')
source('R/GetWeekNo.R')

################################################################################
############################## HELPER FUNCTIONS ################################
################################################################################

dup <- function(x) duplicated(x) | duplicated(x, fromLast = T)
pull <- function(x,y) {x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]}
contain <- function(x, v) { names(x)[sapply(x, function(y) if(is.na(v)) sum(is.na(y)) > 0 else v %in% y)] }

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
           calcFStoDel >= 0)

################################################################################
##################  REMAP PLANT CODE / SRPC TO SHIPPINGPOINT ###################
################################################################################

## FOR EMEA ONLY
if (REGION == 'EMEA') {

  EMEASPMappings <- readRDS('data/transformed/EMEASPMappings.rds')

  # Check if all Plant Code/SRPC exist in the mapping file
  stopifnot(RFDataSet %>% select(PlantCode, ShipRePointCode) %>%
              distinct() %>% anti_join(EMEASPMappings) %>% nrow() == 0)

  RFDataSet <- RFDataSet %>% inner_join(EMEASPMappings)

}

################################################################################
#######################  ADD DELIVERY PRIORITY CODE NEW ########################
################################################################################

# --TODOTODO--: CHECK IF QUERY FROM DB, DISABLE THE OPERATION
DelCodeMap <- setNames(
  c('71', '72', '74', '85', '86', '81', '99', '99', '69', '69', '99', '99', '99', '99', '99', '99', '99'),
  c('21', '22', '24', '35', '36', '41', '23', '27', '48', '68', '58', '11', '12', '13', '14', '20', '50')
)

AMSDelCodeMap <- setNames(
  c('17', '18', '19', '21', '23', '25', '25', '25', '25', '25'),
  c('07', '08', '09', '11', '13', '15', '05', '01', '02', '03')
)

if (REGION %in% c('EMEA', 'APJ')) {
  RFDataSet <- RFDataSet %>%
    mutate(DelPriorityCodeNew = ifelse(DelPriorityCode %in% names(DelCodeMap),
                                       DelCodeMap[DelPriorityCode],
                                       DelPriorityCode))
}

if (REGION == 'AMS') {
  RFDataSet <- RFDataSet %>%
    mutate(DelPriorityCodeNew = ifelse(DelPriorityCode %in% names(AMSDelCodeMap),
                                       AMSDelCodeMap[DelPriorityCode],
                                       DelPriorityCode))
}

################################################################################
############################# TRAIN & TEST SPLIT ###############################
################################################################################

set.seed(42)
nobs <- nrow(RFDataSet)

# ============================================================================ #
# Method 1: 70/30 Split

# SPLIT IN HLI LEVEL
#train <- sample(nobs, 0.70*nobs)
#test <- which(! 1:nobs %in% train )

# SPLIT IN DG LEVEL
DG <- unique(RFDataSet$DeliveryGroupID)
deliverygroups <- data.frame(DeliveryGroupID = DG, randgroup = runif(length(DG)))

RFDataSet <- merge(RFDataSet, deliverygroups, by="DeliveryGroupID")
train <- which(RFDataSet$randgroup <= 0.7)
test <- which(RFDataSet$randgroup > 0.7)

# ============================================================================ #
## Method 2: Delivery Date Week No. Split

## This works as this week number for 2016: http://www.epochconverter.com/weeks/2016
#train <- which((as.integer(strftime(as.Date(RFDataSet$Delivery), format="%U"))) %in% 18:30)
#train <- which((as.integer(strftime(as.Date(RFDataSet$Delivery), format="%U"))) < 31)
#test  <- which((as.integer(strftime(as.Date(RFDataSet$Delivery), format="%U"))) %in% 31:33)
#test <- which(! 1:nobs %in% train )

################################################################################
############################ ENGINEERING FEATURES ##############################
################################################################################

## Mean Lead Time Segmentation in Plant/Product ID

## --TODOTODO--: IS THIS FOR EMEA ONLY ????
DelCodeAGroup <- c('11', '12', '13', '14')
DelCodeBGroup <- c('21', '22', '24', '35', '36', '41', '23', '27', '48', '68', '58')

if (REGION == 'EMEA') {
  meanLTShippingPointProduct <- RFDataSet[train, ] %>%
    group_by(SupplierComplexityGroup, ShippingPoint, ProdFamilyDescr) %>%
    summarise(MeanCLtoFS = mean(calcCLtoFS, na.rm = TRUE),
              MeanCLtoCS = mean(calcCLtoCS, na.rm = TRUE),
              MeanCLtoBT = mean(calcCLtoBT, na.rm = TRUE),
              MeanREtoCL = mean(calcREtoCL, na.rm = TRUE),
              MeanREtoFS = mean(calcREtoFS, na.rm = TRUE),
              MeanBTtoFS = mean(calcBTtoFS, na.rm = TRUE),
              MeanBTtoCS = mean(calcBTtoCS, na.rm = TRUE),
              CountDelCodeA = sum(ifelse(DelPriorityCode %in% DelCodeAGroup, 1, 0)),
              CountDelCodeB = sum(ifelse(DelPriorityCode %in% DelCodeBGroup, 1, 0)))

  meanLTShippingPoint <- RFDataSet[train, ] %>%
    group_by(SupplierComplexityGroup, ShippingPoint) %>%
    summarise(TEMPMeanCLtoFS = mean(calcCLtoFS, na.rm = TRUE),
              TEMPMeanCLtoCS = mean(calcCLtoCS, na.rm = TRUE),
              TEMPMeanCLtoBT = mean(calcCLtoBT, na.rm = TRUE),
              TEMPMeanREtoCL = mean(calcREtoCL, na.rm = TRUE),
              TEMPMeanREtoFS = mean(calcREtoFS, na.rm = TRUE),
              TEMPMeanBTtoFS = mean(calcBTtoFS, na.rm = TRUE),
              TEMPMeanBTtoCS = mean(calcBTtoCS, na.rm = TRUE),
              TEMPCountDelCodeA = sum(ifelse(DelPriorityCode %in% DelCodeAGroup, 1, 0)),
              TEMPCountDelCodeB = sum(ifelse(DelPriorityCode %in% DelCodeBGroup, 1, 0)))

  RFDataSet <- RFDataSet %>% left_join(meanLTShippingPointProduct) %>%
    left_join(meanLTShippingPoint) %>%
    mutate(MeanCLtoFS = ifelse(is.na(MeanCLtoFS), TEMPMeanCLtoFS, MeanCLtoFS),
           MeanCLtoCS = ifelse(is.na(MeanCLtoCS), TEMPMeanCLtoCS, MeanCLtoCS),
           MeanCLtoBT = ifelse(is.na(MeanCLtoBT), TEMPMeanCLtoBT, MeanCLtoBT),
           MeanREtoCL = ifelse(is.na(MeanREtoCL), TEMPMeanREtoCL, MeanREtoCL),
           MeanREtoFS = ifelse(is.na(MeanREtoFS), TEMPMeanREtoFS, MeanREtoFS),
           MeanBTtoFS = ifelse(is.na(MeanBTtoFS), TEMPMeanBTtoFS, MeanBTtoFS),
           MeanBTtoCS = ifelse(is.na(MeanBTtoCS), TEMPMeanBTtoCS, MeanBTtoCS),
           CountDelCodeA = ifelse(is.na(CountDelCodeA), TEMPCountDelCodeA, CountDelCodeA),
           CountDelCodeB = ifelse(is.na(CountDelCodeB), TEMPCountDelCodeB, CountDelCodeB)) %>%
    select(-starts_with('TEMP'))

  meanLTShippingPointCountryCustomer <- RFDataSet[train, ] %>%
    group_by(SupplierComplexityGroup, ShippingPoint, ShipToCountryCode, RouteCode) %>%
    summarise(MeanFStoDel = mean(calcFStoDel, na.rm = TRUE),
              MeanCStoDel = mean(calcCStoDel, na.rm = TRUE))

  meanLTShippingPointCountry <- RFDataSet[train, ] %>%
    group_by(SupplierComplexityGroup, ShippingPoint, ShipToCountryCode) %>%
    summarise(TEMPMeanFStoDel = mean(calcFStoDel, na.rm = TRUE),
              TEMPMeanCStoDel = mean(calcCStoDel, na.rm = TRUE))

  meanLTShippingPointFStoDel <- RFDataSet[train, ] %>%
    group_by(SupplierComplexityGroup, ShippingPoint) %>%
    summarise(TEMP2MeanFStoDel = mean(calcFStoDel, na.rm = TRUE),
              TEMP2MeanCStoDel = mean(calcCStoDel, na.rm = TRUE))

  RFDataSet <- RFDataSet %>% left_join(meanLTShippingPointCountryCustomer) %>%
    left_join(meanLTShippingPointCountry) %>%
    left_join(meanLTShippingPointFStoDel) %>%
    mutate(MeanFStoDel = ifelse(is.na(MeanFStoDel), TEMPMeanFStoDel, MeanFStoDel),
           MeanCStoDel = ifelse(is.na(MeanCStoDel), TEMPMeanCStoDel, MeanCStoDel),
           MeanFStoDel = ifelse(is.na(MeanFStoDel), TEMP2MeanFStoDel, MeanFStoDel),
           MeanCStoDel = ifelse(is.na(MeanCStoDel), TEMP2MeanCStoDel, MeanCStoDel)) %>%
    select(-starts_with('TEMP'))

} else {
  ## FOR AMS/APJ AS SHIPPING POINT MAPPING IS NOT AVAILABLE
  meanLTPlantProduct <- RFDataSet[train, ] %>%
    group_by(SupplierComplexityGroup, PlantCode, ProdFamilyDescr) %>%
    summarise(MeanCLtoFS = mean(calcCLtoFS, na.rm = TRUE),
              MeanCLtoCS = mean(calcCLtoCS, na.rm = TRUE),
              MeanCLtoBT = mean(calcCLtoBT, na.rm = TRUE),
              MeanREtoCL = mean(calcREtoCL, na.rm = TRUE),
              MeanREtoFS = mean(calcREtoFS, na.rm = TRUE),
              MeanBTtoFS = mean(calcBTtoFS, na.rm = TRUE),
              MeanBTtoCS = mean(calcBTtoCS, na.rm = TRUE),
              CountDelCodeA = sum(ifelse(DelPriorityCode %in% DelCodeAGroup, 1, 0)),
              CountDelCodeB = sum(ifelse(DelPriorityCode %in% DelCodeBGroup, 1, 0)))

  meanLTPlant <- RFDataSet[train, ] %>%
    group_by(SupplierComplexityGroup, PlantCode) %>%
    summarise(TEMPMeanCLtoFS = mean(calcCLtoFS, na.rm = TRUE),
              TEMPMeanCLtoCS = mean(calcCLtoCS, na.rm = TRUE),
              TEMPMeanCLtoBT = mean(calcCLtoBT, na.rm = TRUE),
              TEMPMeanREtoCL = mean(calcREtoCL, na.rm = TRUE),
              TEMPMeanREtoFS = mean(calcREtoFS, na.rm = TRUE),
              TEMPMeanBTtoFS = mean(calcBTtoFS, na.rm = TRUE),
              TEMPMeanBTtoCS = mean(calcBTtoCS, na.rm = TRUE),
              TEMPCountDelCodeA = sum(ifelse(DelPriorityCode %in% DelCodeAGroup, 1, 0)),
              TEMPCountDelCodeB = sum(ifelse(DelPriorityCode %in% DelCodeBGroup, 1, 0)))

  RFDataSet <- RFDataSet %>% left_join(meanLTPlantProduct) %>%
    left_join(meanLTPlant) %>%
    mutate(MeanCLtoFS = ifelse(is.na(MeanCLtoFS), TEMPMeanCLtoFS, MeanCLtoFS),
           MeanCLtoCS = ifelse(is.na(MeanCLtoCS), TEMPMeanCLtoCS, MeanCLtoCS),
           MeanCLtoBT = ifelse(is.na(MeanCLtoBT), TEMPMeanCLtoBT, MeanCLtoBT),
           MeanREtoCL = ifelse(is.na(MeanREtoCL), TEMPMeanREtoCL, MeanREtoCL),
           MeanREtoFS = ifelse(is.na(MeanREtoFS), TEMPMeanREtoFS, MeanREtoFS),
           MeanBTtoFS = ifelse(is.na(MeanBTtoFS), TEMPMeanBTtoFS, MeanBTtoFS),
           MeanBTtoCS = ifelse(is.na(MeanBTtoCS), TEMPMeanBTtoCS, MeanBTtoCS),
           CountDelCodeA = ifelse(is.na(CountDelCodeA), TEMPCountDelCodeA, CountDelCodeA),
           CountDelCodeB = ifelse(is.na(CountDelCodeB), TEMPCountDelCodeB, CountDelCodeB)) %>%
    select(-starts_with('TEMP'))

  meanLTShipRePointCountryCustemer <- RFDataSet[train, ] %>%
    group_by(SupplierComplexityGroup, ShipRePointCode, ShipToCountryCode, RouteCode) %>%
    summarise(MeanFStoDel = mean(calcFStoDel, na.rm = TRUE),
              MeanCStoDel = mean(calcCStoDel, na.rm = TRUE))

  meanLTShipRePointCountry <- RFDataSet[train, ] %>%
    group_by(SupplierComplexityGroup, ShipRePointCode, ShipToCountryCode) %>%
    summarise(TEMPMeanFStoDel = mean(calcFStoDel, na.rm = TRUE),
              TEMPMeanCStoDel = mean(calcCStoDel, na.rm = TRUE))

  meanLTShipRePoint <- RFDataSet[train, ] %>%
    group_by(SupplierComplexityGroup, ShipRePointCode) %>%
    summarise(TEMP2MeanFStoDel = mean(calcFStoDel, na.rm = TRUE),
              TEMP2MeanCStoDel = mean(calcCStoDel, na.rm = TRUE))

  # Impute the missing using average shipping Point Code + Country
  RFDataSet <- RFDataSet %>% left_join(meanLTShipRePointCountryCustemer) %>%
    left_join(meanLTShipRePointCountry) %>%
    left_join(meanLTShipRePoint) %>%
    mutate(MeanFStoDel = ifelse(is.na(MeanFStoDel), TEMPMeanFStoDel, MeanFStoDel),
           MeanCStoDel = ifelse(is.na(MeanCStoDel), TEMPMeanCStoDel, MeanCStoDel),
           MeanFStoDel = ifelse(is.na(MeanFStoDel), TEMP2MeanFStoDel, MeanFStoDel),
           MeanCStoDel = ifelse(is.na(MeanCStoDel), TEMP2MeanCStoDel, MeanCStoDel)) %>%
    select(-starts_with('TEMP'))

}

# Mean Lead Time by SCG, in case there are NA values due to train/test split
meanLTSCG <- RFDataSet[train, ] %>%
  group_by(SupplierComplexityGroup) %>%
  summarise(TEMP3MeanCLtoFS = mean(calcCLtoFS, na.rm = TRUE),
            TEMP3MeanCLtoCS = mean(calcCLtoCS, na.rm = TRUE),
            TEMP3MeanCLtoBT = mean(calcCLtoBT, na.rm = TRUE),
            TEMP3MeanREtoCL = mean(calcREtoCL, na.rm = TRUE),
            TEMP3MeanREtoFS = mean(calcREtoFS, na.rm = TRUE),
            TEMP3MeanBTtoFS = mean(calcBTtoFS, na.rm = TRUE),
            TEMP3MeanBTtoCS = mean(calcBTtoCS, na.rm = TRUE),
            TEMP3CountDelCodeA = sum(ifelse(DelPriorityCode %in% DelCodeAGroup, 1, 0)),
            TEMP3CountDelCodeB = sum(ifelse(DelPriorityCode %in% DelCodeBGroup, 1, 0)),
            TEMP3MeanFStoDel = mean(calcFStoDel, na.rm = TRUE),
            TEMP3MeanCStoDel = mean(calcCStoDel, na.rm = TRUE))

RFDataSet <- RFDataSet %>% left_join(meanLTSCG) %>%
  mutate(MeanCLtoFS = ifelse(is.na(MeanCLtoFS), TEMP3MeanCLtoFS, MeanCLtoFS),
         MeanCLtoCS = ifelse(is.na(MeanCLtoCS), TEMP3MeanCLtoCS, MeanCLtoCS),
         MeanCLtoBT = ifelse(is.na(MeanCLtoBT), TEMP3MeanCLtoBT, MeanCLtoBT),
         MeanREtoCL = ifelse(is.na(MeanREtoCL), TEMP3MeanREtoCL, MeanREtoCL),
         MeanREtoFS = ifelse(is.na(MeanREtoFS), TEMP3MeanREtoFS, MeanREtoFS),
         MeanBTtoFS = ifelse(is.na(MeanBTtoFS), TEMP3MeanBTtoFS, MeanBTtoFS),
         MeanBTtoCS = ifelse(is.na(MeanBTtoCS), TEMP3MeanBTtoCS, MeanBTtoCS),
         CountDelCodeA = ifelse(is.na(CountDelCodeA), TEMP3CountDelCodeA, CountDelCodeA),
         CountDelCodeB = ifelse(is.na(CountDelCodeB), TEMP3CountDelCodeB, CountDelCodeB),
         MeanFStoDel = ifelse(is.na(MeanFStoDel), TEMP3MeanFStoDel, MeanFStoDel),
         MeanCStoDel = ifelse(is.na(MeanCStoDel), TEMP3MeanCStoDel, MeanCStoDel)) %>%
  select(-starts_with('TEMP'))

# WEEK No. From/To Clean Date To/From Quarter End
RFDataSet <- RFDataSet %>%
  mutate(WksAftMonBeg = pmin(GetWksAftMonBeg(HPREDate), 4),
         WksBefMonEnd = pmin(GetWksBefMonEnd(HPREDate), 4),
         WksAftQtrBeg = pmin(GetWksAftQtrBeg(HPREDate), 7),
         WksBefQtrEnd = pmin(GetWksBefQtrEnd(HPREDate), 7))

if (RFPKG == 'quantregForest') {
  # Define new Product Line features by GB to get around factor level limits
  RFDataSet <- RFDataSet %>% mutate(PL_HPS  = ifelse(GBU == 'HPS' , ProdLineID, "x0"))
  RFDataSet <- RFDataSet %>% mutate(PL_HPSD = ifelse(GBU == 'HPSD', ProdLineID, "x0"))
  RFDataSet <- RFDataSet %>% mutate(PL_HPN  = ifelse(GBU == 'HPN' , ProdLineID, "x0"))
}

################################################################################
############################## IMPUTE NA VALUES ################################
################################################################################

RFDataSet %>% contain(NA) %>% length()
## [1] "AMID2ID"           "Team"              "FocusAccountGroup" "HPSSeg"
## [5] "ISSBCSSplit"       "CoordDelCode"      "SalesOrgCode"

# --TODOTODO--: Imputation can be improved
# Impute NA with a fix value '?'
RFDataSet[is.na(RFDataSet)] <- '?'

length(unique(RFDataSet$SalesOrderID))

################################################################################
############################### LSA TEXT MINING ################################
################################################################################

#LSADATADIR <- 'data/transformed/LSA_SCG_20/'
#
#FILES <- list.files(LSADATADIR)
#LSA <- do.call('rbind', lapply(paste0(LSADATADIR, FILES), read.csv))
#
#LSA <- LSA %>% mutate(SalesOrderID = gsub('_.*', '', X),
#                      SOLineItemID = gsub('.*_', '', X)) %>%
#  select(-X)
#
## Set LSA weights of those missing line items to 0
#missingLSASet <- RFDataSet %>% anti_join(LSA) %>% select(SalesOrderID, SOLineItemID)
#
#LSA <- LSA %>% full_join(missingLSASet)
#LSA[is.na(LSA)] <- 0

#saveRDS(LSA, 'data/transformed/EMEA_LSA_SCG_TFIDF_20.rds')

#LSA <- readRDS('data/transformed/QADTA_LSA_SCG_SP_20.rds')

#RFDataSet %>% anti_join(LSA) %>% nrow()

#RFDataSet <- RFDataSet %>% left_join(LSA)

################################################################################
######################## TARGET LIST AND GROUP SETTING #########################
################################################################################

# FOR ORDER: CL TO DEL
#targetList <- c('calcCLtoDel', 'calcCLtoFS', 'calcCLtoBT', 'calcBTtoFS', 'calcFStoDel')

# FOR ORDER: CL TO CS
#targetList <- c('calcCLtoCS', 'calcCLtoBT', 'calcBTtoCS')

# FOR ORDER: RE TO DEL
targetList <- c('calcREtoDel', 'calcREtoCL', 'calcCLtoBT', 'calcBTtoCS', 'calcCStoDel')

# Unseg List - no data segmentation
unsegList <- rep('ALL', nrow(RFDataSet))

# Build models by SupplierComplexityGroup
segList <- as.character(RFDataSet$SupplierComplexityGroup)

# ADD grouping to dataframe for convenience (MeanLT calculation for example)
RFDataSet$segList <- segList

################################################################################
####################### MODEL BUILD PARAMETERS SETTING #########################
################################################################################

## SET INPUT PARAMETERS HERE

baseCatInput <- c('GBU'
                  #, 'SNPCode'
                  #, 'SalesProdType'
                  , 'PlantCode'
                  , 'SOHeaderCompleteFlag'
                  , 'ShipRePointCode'
                  , 'Supplier'
                  #, 'SupDivEntityCode'
                  #, 'Team'
                  #, 'FocusAccountGroup'
                  , 'HPSSeg'
                  , 'ISSBCSSplit'
                  , 'SolutionOrder'
                  #, 'CoordDelCode'
                  #, 'DisCenterCode'
                  #, 'SalesDocItemCatCode'
                  , 'SalesOrgCode'
                  , 'SupplierComplexityGroup'
                  , 'Ctry_Rgn'
                  #, 'DelPriorityCodeNew'
)

## TODO quantregForest specific request for cat levels
if (RFPKG == 'quantregForest') {
  baseCatInput <- c(baseCatInput, 'PL_HPS', 'PL_HPSD', 'PL_HPN')
} else {
  baseCatInput <- c(baseCatInput, 'ProdLineID')
}

# Add ShippingPoint input for EMEA data
if (REGION == 'EMEA') {
  baseCatInput <- c(baseCatInput, 'ShippingPoint')
}

engCatInput <- NULL

baseNumInput <- c('BaseQuantity'
                  , 'NetUSDAmount'
                  , 'EstE2EDayNo'
                  , 'ITM_COUNT'
                  #, 'UnitQuantity'
)

engNumInput <- list()

engNumInput$calcREtoDel <- c('MeanCLtoBT'
                         , 'MeanREtoCL'
                         , 'MeanBTtoCS'
                         #, 'MeanCLtoCS'
                         #, 'MeanCLtoFS'
                         #, 'MeanBTtoFS'
                         #, 'MeanFStoDel'
                         , 'MeanCStoDel'
                         #, 'CountDelCodeA'
                         #, 'CountDelCodeB'
                         , 'WksAftMonBeg'
                         , 'WksBefMonEnd'
                         , 'WksAftQtrBeg'
                         , 'WksBefQtrEnd'
)

engNumInput$calcREtoCL  <- c('MeanREtoCL'
                             , 'WksAftMonBeg'
                             , 'WksBefMonEnd'
                             , 'WksAftQtrBeg'
                             , 'WksBefQtrEnd'
)
engNumInput$calcCLtoBT  <- c('MeanCLtoBT'
                             , 'WksAftMonBeg'
                             , 'WksBefMonEnd'
                             , 'WksAftQtrBeg'
                             , 'WksBefQtrEnd'
)
engNumInput$calcBTtoCS  <- c('MeanBTtoCS'
                             , 'WksAftMonBeg'
                             , 'WksBefMonEnd'
                             , 'WksAftQtrBeg'
                             , 'WksBefQtrEnd'
)
engNumInput$calcCStoDel <- c('MeanCStoDel'
                             , 'WksAftMonBeg'
                             , 'WksBefMonEnd'
                             , 'WksAftQtrBeg'
                             , 'WksBefQtrEnd'
)

# TEXT MINING WEIGHT INPUT
tmNumInput <- paste0('C_', 1:20)

## !!CHANGE CATEGORICAL INPUTS TO FACTOR
## TODO - Move to a separate place
RFDataSet[c(baseCatInput, engCatInput)] <- lapply(RFDataSet[c(baseCatInput, engCatInput)], factor)

baseInput <- c(baseCatInput, baseNumInput)          # Base Predictors

engInput <- list()

engInput$calcREtoDel <- c(baseInput, engCatInput, engNumInput$calcREtoDel)
engInput$calcREtoCL  <- c(baseInput, engCatInput, engNumInput$calcREtoCL)
engInput$calcCLtoBT  <- c(baseInput, engCatInput, engNumInput$calcCLtoBT)
engInput$calcBTtoCS  <- c(baseInput, engCatInput, engNumInput$calcBTtoCS)
engInput$calcCStoDel <- c(baseInput, engCatInput, engNumInput$calcCStoDel)

tmInput   <- c(engInput, tmNumInput)                # TM + Eng + Base Predictors

################################################################################
################################ RUN THE MODEL #################################
################################################################################

## SITUATIONS FOR QUOTE TIME PREDICTIONS
## 1. baseInput + NO SEG (DATA GROUP)
## 2. baseInput + SCG SEG
## 3. engInput  + NO SEG (DATA GROUP)
## 4. engInput  + SCG SEG
## 5. tmInput   + SCG SEG


# ============================================================================ #
## S1 - BASE UNSEGMENTED MODEL

rfBASEALL <- ModelBuild(RFPKG = RFPKG,
                        data = RFDataSet[train, ],
                        input = baseInput,
                        targetList = targetList,
                        group = unsegList[train])

## RF VARIABLE IMPORTANCE

VarImp(RFPKG = RFPKG, rfList = rfBASEALL$ALL, filename = 'VarImp_BASEALL.png')

## S1 - Predictions

prBASEALLTrain <- ModelPredict(RFPKG = RFPKG,
                               modelList = rfBASEALL,
                               data = RFDataSet[train, ],
                               targetList = targetList,
                               group = unsegList[train], Quantile)

prBASEALLTest <- ModelPredict(RFPKG = RFPKG,
                              modelList = rfBASEALL,
                              data = RFDataSet[test, ],
                              targetList = targetList,
                              group = unsegList[test], Quantile)

#saveRDS(rfBASEALL, 'rfBASEALL.rds')
#rm(rfBASEALL)
#rfBASEALL <- readRDS('rfBASEALL.rds')

# ============================================================================ #
# S2 - BASE SEGMENTED BY SupplierComplexityGroup MODEL

rfBASESCG <- ModelBuild(RFPKG = RFPKG,
                        data = RFDataSet[train, ],
                        input = baseInput,
                        targetList = targetList,
                        group = segList[train])

## RF VARIABLE IMPORTANCE

lapply(names(rfBASESCG), function(nm, myList) {
  VarImp(RFPKG = RFPKG, rfList = myList[[nm]], filename = paste0('VarImp_BASESCG_', nm, '.png'))
}, myList = rfBASESCG)

## S2 - Predictions

prBASESCGTrain <- ModelPredict(RFPKG = RFPKG,
                               modelList = rfBASESCG,
                               data = RFDataSet[train, ],
                               targetList = targetList,
                               group = segList[train], Quantile)

prBASESCGTest <- ModelPredict(RFPKG = RFPKG,
                              modelList = rfBASESCG,
                              data = RFDataSet[test, ],
                              targetList = targetList,
                              group = segList[test], Quantile)

#saveRDS(rfBASESCG, 'rfBASESCG.rds')
#rm(rfBASESCG)
#rfBASESCG <- readRDS('rfBASESCG.rds')

# ============================================================================ #
## S3 - ENGINEERED UNSEGMENTED MODEL

rfENGALL <- ModelBuild(RFPKG = RFPKG,
                       data  = RFDataSet[train, ],
                       input = engInput,
                       targetList = targetList,
                       group = unsegList[train])

## RF VARIABLE IMPORTANCE

VarImp(RFPKG = RFPKG, rfList = rfENGALL$ALL, filename = 'VarImp_ENGALL.png')

## S3 - Predictions

prENGALLTrain <- ModelPredict(RFPKG = RFPKG,
                              modelList = rfENGALL,
                              data = RFDataSet[train, ],
                              targetList = targetList,
                              group = unsegList[train], Quantile)

prENGALLTest <- ModelPredict(RFPKG = RFPKG,
                             modelList = rfENGALL,
                             data = RFDataSet[test, ],
                             targetList = targetList,
                             group = unsegList[test], Quantile)

#saveRDS(rfENGALL, 'rfENGALL.rds')
#rm(rfENGALL)
#rfENGALL <- readRDS('rfENGALL.rds')

# ============================================================================ #
## S4 - ENGINEERED SEGMENTED BY SupplierComplexityGroup MODEL

rfENGSCG <- ModelBuild(RFPKG = RFPKG,
                       data = RFDataSet[train, ],
                       input = engInput,
                       targetList = targetList,
                       group = segList[train])

lapply(names(rfENGSCG), function(nm, myList) {
  VarImp(RFPKG = RFPKG, rfList = myList[[nm]], filename = paste0('VarImp_ENGSCG_', nm, '.png'))
}, myList = rfENGSCG)

## S4 - Predictions

prENGSCGTrain <- ModelPredict(RFPKG = RFPKG,
                              modelList = rfENGSCG,
                              data = RFDataSet[train, ],
                              targetList = targetList,
                              group = segList[train], Quantile)

prENGSCGTest <- ModelPredict(RFPKG = RFPKG,
                             modelList = rfENGSCG,
                             data = RFDataSet[test, ],
                             targetList = targetList,
                             group = segList[test], Quantile)

#saveRDS(rfENGSCG, 'rfENGSCG.rds')
#rm(rfENGSCG)
#rfENGSCG <- readRDS('rfENGSCG.rds')

# ============================================================================ #
## S5 - TEXTMINING SEGMENTED BY GBU MODEL

#rfTMSCG <- ModelBuild(RFPKG = RFPKG,
#                      data = RFDataSet[train, ],
#                      input = tmInput,
#                      targetList = targetList,
#                      group = segList[train])
#
#lapply(names(rfTMSCG), function(nm, myList) {
#  VarImp(RFPKG = RFPKG, rfList = myList[[nm]], filename = paste0('VarImp_TMSCG_', nm, '.png'))
#}, myList = rfTMSCG)
#
### S5 - Predictions
#
#prTMSCGTrain <- ModelPredict(RFPKG = RFPKG,
#                             modelList = rfTMSCG,
#                             data = RFDataSet[train, ],
#                             targetList = targetList,
#                             group = segList[train])
#
#prTMSCGTest <- ModelPredict(RFPKG = RFPKG,
#                            modelList = rfTMSCG,
#                            data = RFDataSet[test, ],
#                            targetList = targetList,
#                            group = segList[test])

#saveRDS(rfTMSCG, 'rfTMSCG')
#rm(rfTMSCG)
#rfTMSCG <- readRDS('rfTMSCG.rds')

################################################################################
######################## BUILD PREDICTIONS DATA FRAME ##########################
################################################################################

## IF TARGET LIST CHANGED, CHANGE HERE CORRESPONDINGLY!!!!!!!!
## BIG DATA FRAME CONTAINS ALL THE PREDICTIONS

# ============================================================================ #
## GENERATE MEAN LT MODEL FOR SEGMENTED
## USE THE TRAIN AVERAGE FOR BOTH TRAINING AND TESTING DATASET

segMeanLT <- RFDataSet[train, ] %>%
  group_by(segList) %>%
  summarise(MeanLT = ceiling(mean(calcREtoDel))) %>% ungroup()

RFDataSet <- RFDataSet %>% inner_join(segMeanLT)
## Should be joined by "segList"

# ============================================================================ #

PaddingFactor <- 0.0

dfTRAIN <- data.frame(
  SalesOrderID      = RFDataSet[train, ]$SalesOrderID,
  SOLineItemID      = RFDataSet[train, ]$SOLineItemID,
  DeliveryGroupID   = RFDataSet[train, ]$DeliveryGroupID,
  GroupList         = RFDataSet[train, ]$segList,
  Supplier          = RFDataSet[train, ]$Supplier,
  RF_BASE_ALL_UNSEG = ceiling(prBASEALLTrain$calcREtoDel) + PaddingFactor,
  RF_BASE_ALL_SEG   = ceiling(prBASEALLTrain$calcREtoCL + prBASEALLTrain$calcCLtoBT + prBASEALLTrain$calcBTtoCS + prBASEALLTrain$calcCStoDel) + PaddingFactor,
  RF_BASE_SCG_UNSEG = ceiling(prBASESCGTrain$calcREtoDel) + PaddingFactor,
  RF_BASE_SCG_SEG   = ceiling(prBASESCGTrain$calcREtoCL + prBASESCGTrain$calcCLtoBT + prBASESCGTrain$calcBTtoCS + prBASESCGTrain$calcCStoDel) + PaddingFactor,
  RF_ENG_ALL_UNSEG  = ceiling(prENGALLTrain$calcREtoDel) + PaddingFactor,
  RF_ENG_ALL_SEG    = ceiling(prENGALLTrain$calcREtoCL + prENGALLTrain$calcCLtoBT + prENGALLTrain$calcBTtoCS + prENGALLTrain$calcCStoDel) + PaddingFactor,
  RF_ENG_SCG_UNSEG  = ceiling(prENGSCGTrain$calcREtoDel) + PaddingFactor,
  RF_ENG_SCG_SEG    = ceiling(prENGSCGTrain$calcREtoCL + prENGSCGTrain$calcCLtoBT + prENGSCGTrain$calcBTtoCS + prENGSCGTrain$calcCStoDel) + PaddingFactor,
  #RF_TM_SCG_UNSEG  = ceiling(prTMSCGTrain$calcREtoDel) + PaddingFactor,
  #RF_TM_SCG_SEG    = ceiling(prTMSCGTrain$calcREtoCL + prTMSCGTrain$calcCLtoBT + prTMSCGTrain$calcBTtoCS + prTMSCGTrain$calcCStoDel) + PaddingFactor,
  ATP               = RFDataSet[train, ]$ATPREtoDel,
  ALLMeanLT         = ceiling(mean(RFDataSet[train, ]$calcREtoDel)),
  SCGMeanLT         = RFDataSet[train, ]$MeanLT,
  Target            = RFDataSet[train, ]$calcREtoDel
)

dfTEST <- data.frame(
  SalesOrderID      = RFDataSet[test, ]$SalesOrderID,
  SOLineItemID      = RFDataSet[test, ]$SOLineItemID,
  DeliveryGroupID   = RFDataSet[test, ]$DeliveryGroupID,
  GroupList         = RFDataSet[test, ]$segList,
  Supplier          = RFDataSet[test, ]$Supplier,
  RF_BASE_ALL_UNSEG = ceiling(prBASEALLTest$calcREtoDel) + PaddingFactor,
  RF_BASE_ALL_SEG   = ceiling(prBASEALLTest$calcREtoCL + prBASEALLTest$calcCLtoBT + prBASEALLTest$calcBTtoCS + prBASEALLTest$calcCStoDel) + PaddingFactor,
  RF_BASE_SCG_UNSEG = ceiling(prBASESCGTest$calcREtoDel) + PaddingFactor,
  RF_BASE_SCG_SEG   = ceiling(prBASESCGTest$calcREtoCL + prBASESCGTest$calcCLtoBT + prBASESCGTest$calcBTtoCS + prBASESCGTest$calcCStoDel) + PaddingFactor,
  RF_ENG_ALL_UNSEG  = ceiling(prENGALLTest$calcREtoDel) + PaddingFactor,
  RF_ENG_ALL_SEG    = ceiling(prENGALLTest$calcREtoCL + prENGALLTest$calcCLtoBT + prENGALLTest$calcBTtoCS + prENGALLTest$calcCStoDel) + PaddingFactor,
  RF_ENG_SCG_UNSEG  = ceiling(prENGSCGTest$calcREtoDel) + PaddingFactor,
  RF_ENG_SCG_SEG    = ceiling(prENGSCGTest$calcREtoCL + prENGSCGTest$calcCLtoBT + prENGSCGTest$calcBTtoCS + prENGSCGTest$calcCStoDel) + PaddingFactor,
  #RF_TM_SCG_UNSEG  = ceiling(prTMSCGTest$calcREtoDel) + PaddingFactor,
  #RF_TM_SCG_SEG    = ceiling(prTMSCGTest$calcREtoCL + prTMSCGTest$calcCLtoBT + prTMSCGTest$calcBTtoCS + prTMSCGTest$calcCStoDel) + PaddingFactor,
  ATP               = RFDataSet[test, ]$ATPREtoDel,
  ALLMeanLT         = ceiling(mean(RFDataSet[train, ]$calcREtoDel)), # IMPORTANT
  SCGMeanLT         = RFDataSet[test, ]$MeanLT,
  Target            = RFDataSet[test, ]$calcREtoDel
)

write.csv(dfTRAIN, paste0(REGION, '_TRAIN_Predictions.csv'), row.names = F)
write.csv(dfTEST , paste0(REGION, '_TEST_Predictions.csv'), row.names = F)

################################################################################
############################# EVALUATE THE MODEL ###############################
################################################################################

## Evaluate models built using ModelEvaluate Function

# ============================================================================ #
## TRAINING DATA
# ONLY LEAD TIME SEGMENTED MODELS ARE SELECTED
out <- ModelEvaluate(predicted = dfTRAIN %>%
                       select(contains('SEG'),
                              ATP,
                              ALLMeanLT,
                              SCGMeanLT
                       ),
                     dfTRAIN$Target,
                     segList[train])

RECPlot(out$REC, ncol = 3, nrow = 3, filename = 'REC_TRAIN.png')
write.csv(out$ACC, 'ACC_TRAIN.csv', row.names = F)
# ============================================================================ #
## TESTING DATA
# ONLY LEAD TIME SEGMENTED MODELS ARE SELECTED
out <- ModelEvaluate(predicted = dfTEST %>%
                       select(contains('SEG'),
                              ATP,
                              ALLMeanLT,
                              SCGMeanLT
                       ),
                     dfTEST$Target,
                     segList[test])

out$ACC

RECPlot(out$REC, ncol = 3, nrow = 3, filename = 'REC_TEST.png')
write.csv(out$ACC, 'ACC_TEST.csv', row.names = F)

# ============================================================================ #
## Actual LT VS Fitted LT Plots

modelstoplot <- c('RF_BASE_SCG_UNSEG'
                  ,'RF_BASE_SCG_SEG'
                  ,'RF_ENG_ALL_UNSEG'
                  ,'RF_ENG_ALL_SEG'
                  ,'RF_ENG_SCG_UNSEG'
                  ,'RF_ENG_SCG_SEG')

# Model Plots
for (i in 1:length(modelstoplot)) {
  png(filename = paste0(modelstoplot[i],"_Plot_Test.png"), type = 'cairo',
      width = 2000,
      height = 2000,
      res = 250)
  plot_colorByDensity(dfTEST[[modelstoplot[i]]],
                      dfTEST$Target,
                      xlab=paste0("Fitted Quantile RF model (Q=",Quantile*100,"th Percentile)"),
                      ylab="RE to Del LT",
                      main=paste0("Actual Lead Time vs Model ",
                                  modelstoplot[i],"\n (Test Data)"))
  dev.off()

  png(filename = paste0(modelstoplot[i],"_Plot_Train.png"), type = 'cairo',
      width = 2000,
      height = 2000,
      res = 250)

  plot_colorByDensity(dfTRAIN[[modelstoplot[i]]],
                      dfTRAIN$Target,
                      xlab=paste0("Fitted Quantile RF model (Q=",Quantile*100,"th Percentile)"),
                      ylab="RE to Del LT",
                      main=paste0("Actual Lead Time vs Model ",
                                  modelstoplot[i],"\n (Training Data)"))

  dev.off()
}

################################  END OF SCRIPT ################################
