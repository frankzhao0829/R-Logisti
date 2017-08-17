## Last Modified @ 2017-01-09 06:52:37  GMT ------------------------------

################################################################################
################################## CONSTANTS ###################################
################################################################################

#RFPKG <- 'ranger'
#RFPKG <- 'randomForest'
RFPKG <- 'quantregForest'

if (RFPKG=='quantregForest') Quantile <- 0.75 else Quantile <- NULL

#TARGETCMPLXTY <- c('CTO', 'Complex CTO', 'Configured Rack', 'Rack Solution')
REGION <- 'AMS'
FILE <- paste0('data/transformed/', REGION, '_QADTA_HLI.rds')

# REFER TO SHIP TO ISO COUNTRY NAME
#INCLDCOUNTRIES <- c('US', 'CA')
#INCLDCOUNTRIES <- c('CA')
#INCLDCOUNTRIES <- c('BR') # USE THIS FOR BRAZIL, CREATE SIMILAR LIST FOR OTHER SEG

# NEW PARAMETER TO SELECT X% OF TOTAL TRAIN DATA
TRAINPCT <- 1

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
source('R/GenHierCatGroupFeat.R')
source('R/TransClassInputs.R')

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
  #filter(`Supplier Complexity Group` %in% TARGETCMPLXTY) %>%   # Filter On CMPLXTY
  #filter(`Shipment to ISO Country Code` %in% INCLDCOUNTRIES) %>%    # Filter on Country
  filter(!is.na(`DG_RE-DE`)) %>%
  filter(`ASAP Flag` == 'Y') %>%
  select(SalesOrderID = `Sales Order ID`,
         SOLineItemID = `Sales Order Line Item ID`,
         DeliveryGroupID = `DG_RE-DE`,
         ProductID = `Product ID`,
         ProdFamilyDescr = `Product Family Descr`,
         RouteCode = `Route Code`,
         SalesProdType = `Sales Product Type Descr`,
         SNPCode = `SNP Group Source Code`,
         #GBU = `Global Business Unit Name`,
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
         FPShipDate = `First Planned Fact Ship Date`,
         Delivery,
         ITM_COUNT,
         ConfigText,
         SRT,
         SRT_PADDING_VALUE,
         QUOTED_EDT,
         MaxSRTLeadTime,
         ConstraintFlag,
         CountOf0D1,
         Hash0D1Flag,
         Ctry_Rgn
  )

## Calculate lead time in diffrent segs

# US Holiday, Sat/Sun as non-working days
cal <- bizdays::create.calendar("US/ANBIMA", weekdays=c("saturday", "sunday"))

RFDataSet <- RFDataSet %>%
  mutate(ATPREtoDel  = bizdays::bizdays(HPREDate , FPDelDate , cal),
         ATPCLtoDel  = bizdays::bizdays(Clean    , FPDelDate , cal),
         ATPCLtoFS   = bizdays::bizdays(Clean    , FPShipDate, cal),
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
           ATPCLtoFS   >= 0 &
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
           !is.na(MaxSRTLeadTime) &
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
###########################  REMAP GBU TO OLD NAMES ############################
################################################################################

PL_GBU_MAP <- readRDS(sprintf('data/transformed/%s_PL_GBU_MAP.rds', REGION))

RFDataSet %>% semi_join(PL_GBU_MAP, by = c('ProdLineID' = 'PL')) %>% nrow()

RFDataSet <- RFDataSet %>% left_join(PL_GBU_MAP, by = c('ProdLineID' = 'PL'))

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
#DG <- unique(RFDataSet$DeliveryGroupID)
#deliverygroups <- data.frame(DeliveryGroupID = DG, randgroup = runif(length(DG)))
#
#RFDataSet <- merge(RFDataSet, deliverygroups, by="DeliveryGroupID")
#train <- which(RFDataSet$randgroup <= 0.7)
#test <- which(RFDataSet$randgroup > 0.7)

# ============================================================================ #
## Method 2: Delivery Date Week No. Split

## This works as this week number for 2016: http://www.epochconverter.com/weeks/2016
#train <- which((as.integer(strftime(as.Date(RFDataSet$Delivery), format="%U"))) %in% 18:30)
#train <- which((as.integer(strftime(as.Date(RFDataSet$Delivery), format="%U"))) < 31)
#test  <- which((as.integer(strftime(as.Date(RFDataSet$Delivery), format="%U"))) %in% 31:33)
#test <- which(! 1:nobs %in% train )


# ============================================================================ #
## Method 3: Out Of Time Split, X% of OOTrain as train and OOTest as test

OOTrain <- which(RFDataSet$HPREDate >= '2016-05-01' & RFDataSet$HPREDate < '2016-11-01')
OOTest  <- which(RFDataSet$HPREDate >= '2016-11-01' & RFDataSet$HPREDate < '2017-01-01')

## Sampling a X pecent of total OOT Training Data
train <- sample(OOTrain, TRAINPCT * length(OOTrain))

# Use all OOT Testing Data
test <- OOTest
#test <- OOTest[RFDataSet[OOTest, 'ProdTrainCount'] >= 10]

ProductTrainCount <- RFDataSet[train, ] %>%
  group_by(ProductID) %>% summarise(ProdTrainCount = n())

RFDataSet <- RFDataSet %>% left_join(ProductTrainCount, by = 'ProductID') %>%
  mutate(ProdTrainCount = ifelse(is.na(ProdTrainCount), 0, ProdTrainCount))

################################################################################
############################ ENGINEERING FEATURES ##############################
################################################################################

## Mean Lead Time Segmentation in Plant/Product ID

## --TODOTODO--: IS THIS FOR EMEA ONLY ????
DelCodeAGroup <- c('11', '12', '13', '14')
DelCodeBGroup <- c('21', '22', '24', '35', '36', '41', '23', '27', '48', '68', '58')

## Mean Lead Time Segmentation in Plant/Product ID
if (REGION == 'EMEA') {
  FSMeanLTGroup <- c('SupplierComplexityGroup', 'ShippingPoint', 'ProdFamilyDescr')
  DelMeanLTGroup <- c('SupplierComplexityGroup', 'ShippingPoint', 'ShipToCountryCode', 'RouteCode')
} else {
  FSMeanLTGroup <- c('SupplierComplexityGroup', 'PlantCode', 'ProdFamilyDescr')
  DelMeanLTGroup <- c('SupplierComplexityGroup', 'ShipRePointCode', 'ShipToCountryCode', 'RouteCode')
}

RFDataSet <- RFDataSet %>%
  mutate(MeanREtoCL = GenHierCatGroupFeat(., group = FSMeanLTGroup, f = train, y = 'calcREtoCL', mean, na.rm = TRUE),
         MeanCLtoBT = GenHierCatGroupFeat(., group = FSMeanLTGroup, f = train, y = 'calcCLtoBT', mean, na.rm = TRUE),
         MeanBTtoFS = GenHierCatGroupFeat(., group = FSMeanLTGroup, f = train, y = 'calcBTtoFS', mean, na.rm = TRUE),
         MeanREtoFS = GenHierCatGroupFeat(., group = FSMeanLTGroup, f = train, y = 'calcREtoFS', mean, na.rm = TRUE),
         MeanFStoDel = GenHierCatGroupFeat(., group = DelMeanLTGroup, f = train, y = 'calcFStoDel', mean, na.rm = TRUE))

## WEEK No. From/To Clean Date To/From Quarter End
RFDataSet <- RFDataSet %>%
  mutate(WksAftMonBeg = pmin(GetWksAftMonBeg(HPREDate), 4),
         WksBefMonEnd = pmin(GetWksBefMonEnd(HPREDate), 4),
         WksAftQtrBeg = pmin(GetWksAftQtrBeg(HPREDate), 7),
         WksBefQtrEnd = pmin(GetWksBefQtrEnd(HPREDate), 7))

# Define new Product Line features by GB to get around factor level limits
RFDataSet <- RFDataSet %>% mutate(PL_CMPUT  = ifelse(GBU == 'Compute'  , ProdLineID, "x0"))
RFDataSet <- RFDataSet %>% mutate(PL_STRGE  = ifelse(GBU == 'Storage'  , ProdLineID, "x0"))
RFDataSet <- RFDataSet %>% mutate(PL_DCNET  = ifelse(GBU == 'DC Net'   , ProdLineID, "x0"))
RFDataSet <- RFDataSet %>% mutate(PL_ARUBA  = ifelse(GBU == 'HPE Aruba', ProdLineID, "x0"))

## Transfer extra levels into one group to get around facto level limits
#ExcessLvlCols <- c('ShipRePointCode')
#MAXNOLVL <- 20
#for (nn in ExcessLvlCols) {
#  SRPCTrans <- TransClassInputs(RFDataSet[train,], nn, MAXNOLVL, prefix = '')
#  RFDataSet[train,][[nn]] <- SRPCTrans$output$ShipRePointCode
#  RFDataSet[test,][[nn]] <- as.character(ApplyClassTrans(RFDataSet[test,], SRPCTrans$mapping)[[nn]])
#}

################################################################################
############################## IMPUTE NA VALUES ################################
################################################################################

RFDataSet %>% contain(NA) %>% length()

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

# FOR QADTA: RE TO DEL
targetList <- c('calcREtoDel', 'calcREtoFS', 'calcFStoDel')

# Unseg List - no data segmentation
unsegList <- rep('ALL', nrow(RFDataSet))

# Build models by SupplierComplexityGroup
#segList <- as.character(RFDataSet$SupplierComplexityGroup)

# Grouped SCG seglist

SCGNewMap <- setNames(
  c('PPS Option', 'BTO', 'Complex CTO', 'CTO', 'HPN Option', 'HighCTO'        , 'HighCTO'      ),
  c('PPS Option', 'BTO', 'Complex CTO', 'CTO', 'HPN Option', 'Configured Rack', 'Rack Solution')
)

segList <- SCGNewMap[RFDataSet$SupplierComplexityGroup]

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
                  , 'PL_CMPUT'
                  , 'PL_STRGE'
                  , 'PL_DCNET'
                  , 'PL_ARUBA'
                  , 'SOHeaderCompleteFlag'
                  , 'ShipRePointCode'
                  , 'Supplier'
                  #, 'SupDivEntityCode'
                  #, 'Team'
                  #, 'FocusAccountGroup'
                  , 'HPSSeg'
                  #, 'ISSBCSSplit'
                  , 'SolutionOrder'
                  #, 'CoordDelCode'
                  #, 'DisCenterCode'
                  #, 'SalesDocItemCatCode'
                  #, 'SalesOrgCode'
                  , 'SupplierComplexityGroup'
                  , 'Ctry_Rgn'
                  , 'ConstraintFlag'
                  , 'Hash0D1Flag'
                  #, 'DelPriorityCodeNew'
)

# Add ShippingPoint input for EMEA data
if (REGION == 'EMEA') {
  baseCatInput <- c(baseCatInput, 'ShippingPoint')
}

engCatInput <- NULL

baseNumInput <- c('BaseQuantity'
                  , 'NetUSDAmount'
                  #, 'EstE2EDayNo'
                  , 'ITM_COUNT'
                  , 'SRT'
                  , 'SRT_PADDING_VALUE'
                  , 'QUOTED_EDT'
                  , 'CountOf0D1'
                  , 'MaxSRTLeadTime'
                  #, 'UnitQuantity'
)

engNumInput <- list()

engNumInput$calcREtoDel <- c('MeanCLtoBT'
                             , 'MeanREtoCL'
                             , 'MeanBTtoFS'
                             , 'MeanREtoFS'
                             , 'MeanFStoDel'
                             , 'WksAftMonBeg'
                             , 'WksBefMonEnd'
                             , 'WksAftQtrBeg'
                             , 'WksBefQtrEnd'
)

engNumInput$calcREtoFS  <- c('MeanCLtoBT'
                             , 'MeanREtoCL'
                             , 'MeanBTtoFS'
                             , 'MeanREtoFS'
                             , 'WksAftMonBeg'
                             , 'WksBefMonEnd'
                             , 'WksAftQtrBeg'
                             , 'WksBefQtrEnd'
)

engNumInput$calcFStoDel <- c('MeanFStoDel'
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
engInput$calcREtoFS  <- c(baseInput, engCatInput, engNumInput$calcREtoFS)
engInput$calcFStoDel <- c(baseInput, engCatInput, engNumInput$calcFStoDel)

tmInput   <- c(engInput, tmNumInput)                # TM + Eng + Base Predictors

################################################################################
######################## CREATE DIRECTORIES FOR RESULTS ########################
################################################################################

OUTPUTDIR <- file.path(getwd(), format(as.POSIXlt(Sys.time(), "GMT"), '%Y%m%d%H%M'))

MODELDIR <- file.path(OUTPUTDIR, 'models')
PREDTDIR <- file.path(OUTPUTDIR, 'predictions')
METRCDIR <- file.path(OUTPUTDIR, 'metrics')

dir.create(OUTPUTDIR)
dir.create(MODELDIR)
dir.create(PREDTDIR)
dir.create(METRCDIR)

################################################################################
################################ RUN THE MODEL #################################
################################################################################

## SITUATIONS FOR QUOTE TIME PREDICTIONS
## 1. baseInput + NO SEG (DATA GROUP)
## 2. baseInput + SCG SEG
## 3. engInput  + NO SEG (DATA GROUP)
## 4. engInput  + SCG SEG
## 5. tmInput   + SCG SEG


print("Start Model Build ...")

# ============================================================================ #
## S1 - BASE UNSEGMENTED MODEL

rfBASEALL <- ModelBuild(RFPKG = RFPKG,
                        data = RFDataSet[train, ],
                        input = baseInput,
                        targetList = targetList,
                        group = unsegList[train])

## RF VARIABLE IMPORTANCE

VarImp(RFPKG = RFPKG, rfList = rfBASEALL$ALL,
       filename = file.path(METRCDIR, 'VarImp_BASEALL.png'))

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

saveRDS(rfBASEALL, file.path(MODELDIR, 'rfBASEALL.rds'))
rm(rfBASEALL)
gc()

# ============================================================================ #
# S2 - BASE SEGMENTED BY SupplierComplexityGroup MODEL

rfBASESCG <- ModelBuild(RFPKG = RFPKG,
                        data = RFDataSet[train, ],
                        input = baseInput,
                        targetList = targetList,
                        group = segList[train])

## RF VARIABLE IMPORTANCE

lapply(names(rfBASESCG), function(nm, myList) {
  VarImp(RFPKG = RFPKG, rfList = myList[[nm]],
         filename = file.path(METRCDIR, paste0('VarImp_BASESCG_', nm, '.png')))
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

saveRDS(rfBASESCG, file.path(MODELDIR, 'rfBASESCG.rds'))
rm(rfBASESCG)
gc()

# ============================================================================ #
## S3 - ENGINEERED UNSEGMENTED MODEL

rfENGALL <- ModelBuild(RFPKG = RFPKG,
                       data  = RFDataSet[train, ],
                       input = engInput,
                       targetList = targetList,
                       group = unsegList[train])

## RF VARIABLE IMPORTANCE

VarImp(RFPKG = RFPKG, rfList = rfENGALL$ALL,
       filename = file.path(METRCDIR, 'VarImp_ENGALL.png'))

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

saveRDS(rfENGALL, file.path(MODELDIR, 'rfENGALL.rds'))
rm(rfENGALL)
gc()

# ============================================================================ #
## S4 - ENGINEERED SEGMENTED BY SupplierComplexityGroup MODEL

rfENGSCG <- ModelBuild(RFPKG = RFPKG,
                       data = RFDataSet[train, ],
                       input = engInput,
                       targetList = targetList,
                       group = segList[train])

lapply(names(rfENGSCG), function(nm, myList) {
  VarImp(RFPKG = RFPKG, rfList = myList[[nm]],
         filename = file.path(METRCDIR, paste0('VarImp_ENGSCG_', nm, '.png')))
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

prENGSCGTest80 <- ModelPredict(RFPKG = RFPKG,
                               modelList = rfENGSCG,
                               data = RFDataSet[test, ],
                               targetList = targetList,
                               group = segList[test], .80)

prENGSCGTest70 <- ModelPredict(RFPKG = RFPKG,
                               modelList = rfENGSCG,
                               data = RFDataSet[test, ],
                               targetList = targetList,
                               group = segList[test], .70)

Q2 <- .60 #will use this to add smaller FS to Del lead time in SEG model later.
prENGSCGTest2 <- ModelPredict(RFPKG = RFPKG,
                              modelList = rfENGSCG,
                              data = RFDataSet[test, ],
                              targetList = targetList,
                              group = segList[test], Q2)


saveRDS(rfENGSCG, file.path(MODELDIR, 'rfENGSCG.rds'))
rm(rfENGSCG)
gc()

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

QTarget <- Quantile + 0.05
segMeanLT <- RFDataSet[train, ] %>%
  group_by(segList) %>%
  summarise(QTargetLT = ceiling(as.numeric(quantile(calcREtoDel, QTarget)))) %>% ungroup()

RFDataSet <- RFDataSet %>% inner_join(segMeanLT)
## Should be joined by "segList"

# ============================================================================ #

PaddingFactor <- 0.0

dfTRAIN <- data.frame(
  SalesOrderID      = RFDataSet[train, ]$SalesOrderID,
  SOLineItemID      = RFDataSet[train, ]$SOLineItemID,
  HPREDate          = RFDataSet[train, ]$HPREDate,
  DeliveryGroupID   = RFDataSet[train, ]$DeliveryGroupID,
  ProdTrainCount    = RFDataSet[train, ]$ProdTrainCount,
  GroupList         = RFDataSet[train, ]$segList,
  SCG               = RFDataSet[train, ]$SupplierComplexityGroup,
  Supplier          = RFDataSet[train, ]$Supplier,
  RF_BASE_ALL_UNSEG = ceiling(prBASEALLTrain$calcREtoDel) + PaddingFactor,
  RF_BASE_ALL_SEG   = ceiling(prBASEALLTrain$calcREtoFS + prBASEALLTrain$calcFStoDel) + PaddingFactor,
  RF_BASE_SCG_UNSEG = ceiling(prBASESCGTrain$calcREtoDel) + PaddingFactor,
  RF_BASE_SCG_SEG   = ceiling(prBASESCGTrain$calcREtoFS + prBASESCGTrain$calcFStoDel) + PaddingFactor,
  RF_ENG_ALL_UNSEG  = ceiling(prENGALLTrain$calcREtoDel) + PaddingFactor,
  RF_ENG_ALL_SEG    = ceiling(prENGALLTrain$calcREtoFS + prENGALLTrain$calcFStoDel) + PaddingFactor,
  RF_ENG_SCG_UNSEG  = ceiling(prENGSCGTrain$calcREtoDel) + PaddingFactor,
  RF_ENG_SCG_SEG    = ceiling(prENGSCGTrain$calcREtoFS + prENGSCGTrain$calcFStoDel) + PaddingFactor,
  #RF_TM_SCG_UNSEG  = ceiling(prTMSCGTrain$calcREtoDel) + PaddingFactor,
  #RF_TM_SCG_SEG    = ceiling(prTMSCGTrain$calcREtoFS + prTMSCGTrain$calcFStoDel) + PaddingFactor,
  QUOTED_EDT        = RFDataSet[train, ]$QUOTED_EDT,
  ATP               = RFDataSet[train, ]$ATPREtoDel,
  ALLQTargetLT      = ceiling(as.numeric(quantile(RFDataSet[train, ]$calcREtoDel, QTarget))),
  SCGQTargetLT      = RFDataSet[train, ]$QTargetLT,
  Target            = RFDataSet[train, ]$calcREtoDel
)

dfTEST <- data.frame(
  SalesOrderID      = RFDataSet[test, ]$SalesOrderID,
  SOLineItemID      = RFDataSet[test, ]$SOLineItemID,
  HPREDate          = RFDataSet[test, ]$HPREDate,
  DeliveryGroupID   = RFDataSet[test, ]$DeliveryGroupID,
  ProdTrainCount    = RFDataSet[test, ]$ProdTrainCount,
  GroupList         = RFDataSet[test, ]$segList,
  SCG               = RFDataSet[test, ]$SupplierComplexityGroup,
  Supplier          = RFDataSet[test, ]$Supplier,
  RF_BASE_ALL_UNSEG = ceiling(prBASEALLTest$calcREtoDel) + PaddingFactor,
  RF_BASE_ALL_SEG   = ceiling(prBASEALLTest$calcREtoFS + prBASEALLTest$calcFStoDel) + PaddingFactor,
  RF_BASE_SCG_UNSEG = ceiling(prBASESCGTest$calcREtoDel) + PaddingFactor,
  RF_BASE_SCG_SEG   = ceiling(prBASESCGTest$calcREtoFS + prBASESCGTest$calcFStoDel) + PaddingFactor,
  RF_ENG_ALL_UNSEG  = ceiling(prENGALLTest$calcREtoDel) + PaddingFactor,
  RF_ENG_ALL_SEG    = ceiling(prENGALLTest$calcREtoFS + prENGALLTest$calcFStoDel) + PaddingFactor,
  RF_ENG_SCG_UNSEG  = ceiling(prENGSCGTest$calcREtoDel) + PaddingFactor,
  RF_ENG_SCG_UNSEG80 = ceiling(prENGSCGTest80$calcREtoDel) + PaddingFactor,
  RF_ENG_SCG_UNSEG70 = ceiling(prENGSCGTest70$calcREtoDel) + PaddingFactor,
  RF_ENG_SCG_SEG    = ceiling(prENGSCGTest$calcREtoFS + prENGSCGTest$calcFStoDel) + PaddingFactor,
  RF_ENG_SCG_SEG80  = ceiling(prENGSCGTest80$calcREtoFS + prENGSCGTest80$calcFStoDel) + PaddingFactor,
  RF_ENG_SCG_SEG70  = ceiling(prENGSCGTest70$calcREtoFS + prENGSCGTest70$calcFStoDel) + PaddingFactor,
  RF_ENG_SCG_SEG2   = ceiling(prENGSCGTest$calcREtoFS + prENGSCGTest2$calcFStoDel) + PaddingFactor,
  #RF_TM_SCG_UNSEG  = ceiling(prTMSCGTest$calcREtoDel) + PaddingFactor,
  #RF_TM_SCG_SEG    = ceiling(prTMSCGTest$calcREtoFS + prTMSCGTest$calcFStoDel) + PaddingFactor,
  QUOTED_EDT        = RFDataSet[test, ]$QUOTED_EDT,
  ATP               = RFDataSet[test, ]$ATPREtoDel,
  ALLQTargetLT      = ceiling(as.numeric(quantile(RFDataSet[train, ]$calcREtoDel, QTarget))), # IMPORTANT
  SCGQTargetLT      = RFDataSet[test, ]$QTargetLT,
  Target            = RFDataSet[test, ]$calcREtoDel
)

write.csv(dfTRAIN, file.path(PREDTDIR, paste0(REGION, '_TRAIN_Predictions.csv')), row.names = F)
write.csv(dfTEST , file.path(PREDTDIR, paste0(REGION, '_TEST_Predictions.csv')), row.names = F)

## ROLLUP TO DG

# ============================================================================ #
## TRAINING DATA

dfTRAINDG <- dfTRAIN %>% group_by(DeliveryGroupID) %>%
  summarise(MaxGroupList      = max(as.character(GroupList)),
            MinGroupList      = min(as.character(GroupList)),
            MaxSCGList        = max(as.character(SCG)),
            MinSCGList        = min(as.character(SCG)),
            HPREDate          = max(as.character(HPREDate)),
            ProdTrainCount    = min(ProdTrainCount),
            RF_BASE_ALL_UNSEG = max(RF_BASE_ALL_UNSEG),
            RF_BASE_ALL_SEG   = max(RF_BASE_ALL_SEG),
            RF_BASE_SCG_UNSEG = max(RF_BASE_SCG_UNSEG),
            RF_BASE_SCG_SEG   = max(RF_BASE_SCG_SEG),
            RF_ENG_ALL_UNSEG  = max(RF_ENG_ALL_UNSEG),
            RF_ENG_ALL_SEG    = max(RF_ENG_ALL_SEG),
            RF_ENG_SCG_UNSEG  = max(RF_ENG_SCG_UNSEG),
            RF_ENG_SCG_SEG    = max(RF_ENG_SCG_SEG),
            QUOTED_EDT        = max(QUOTED_EDT),
            ATP               = max(ATP),
            ALLQTargetLT      = max(ALLQTargetLT),
            SCGQTargetLT      = max(SCGQTargetLT),
            Target            = max(Target)
  ) %>% ungroup()

# ============================================================================ #
## TESTING DATA

dfTESTDG <- dfTEST %>% group_by(DeliveryGroupID) %>%
  summarise(MaxGroupList      = max(as.character(GroupList)),
            MinGroupList      = min(as.character(GroupList)),
            MaxSCGList        = max(as.character(SCG)),
            MinSCGList        = min(as.character(SCG)),
            HPREDate          = max(as.character(HPREDate)),
            ProdTrainCount    = min(ProdTrainCount),
            RF_BASE_ALL_UNSEG = max(RF_BASE_ALL_UNSEG),
            RF_BASE_ALL_SEG   = max(RF_BASE_ALL_SEG),
            RF_BASE_SCG_UNSEG = max(RF_BASE_SCG_UNSEG),
            RF_BASE_SCG_SEG   = max(RF_BASE_SCG_SEG),
            RF_ENG_ALL_UNSEG  = max(RF_ENG_ALL_UNSEG),
            RF_ENG_ALL_SEG    = max(RF_ENG_ALL_SEG),
            RF_ENG_SCG_UNSEG  = max(RF_ENG_SCG_UNSEG),
            RF_ENG_SCG_SEG    = max(RF_ENG_SCG_SEG),
            RF_ENG_SCG_UNSEG80  = max(RF_ENG_SCG_UNSEG80),
            RF_ENG_SCG_UNSEG70  = max(RF_ENG_SCG_UNSEG70),
            RF_ENG_SCG_SEG80    = max(RF_ENG_SCG_SEG80),
            RF_ENG_SCG_SEG70    = max(RF_ENG_SCG_SEG70),
            RF_ENG_SCG_SEG2    = max(RF_ENG_SCG_SEG2),
            QUOTED_EDT        = max(QUOTED_EDT),
            ATP               = max(ATP),
            ALLQTargetLT      = max(ALLQTargetLT),
            SCGQTargetLT      = max(SCGQTargetLT),
            Target            = max(Target)
  ) %>% ungroup()

write.csv(dfTRAINDG, file.path(PREDTDIR, paste0(REGION, '_TRAIN_DG_Predictions.csv')), row.names = F)
write.csv(dfTESTDG , file.path(PREDTDIR, paste0(REGION, '_TEST_DG_Predictions.csv')), row.names = F)

################################################################################
############################# EVALUATE THE MODEL ###############################
################################################################################

## Evaluate the model - DG level

# ============================================================================ #
## TRAINING DATA
outMaxDG <- ModelEvaluate(predicted = dfTRAINDG %>%
                            select(contains('SEG'), QUOTED_EDT, ATP, ALLQTargetLT, SCGQTargetLT),
                          dfTRAINDG$Target,
                          dfTRAINDG$MaxSCGList)  # based on segList

outMinDG <- ModelEvaluate(predicted = dfTRAINDG %>%
                            select(contains('SEG'), QUOTED_EDT, ATP, ALLQTargetLT, SCGQTargetLT),
                          dfTRAINDG$Target,
                          dfTRAINDG$MinSCGList)  # based on segList

write.csv(outMaxDG$ACC, file.path(METRCDIR, paste0(REGION, '_DG_ACC_TRAIN_MAX_SCG.csv')), row.names = F)
write.csv(outMinDG$ACC, file.path(METRCDIR, paste0(REGION, '_DG_ACC_TRAIN_MIN_SCG.csv')), row.names = F)

# ============================================================================ #
## TESTING DATA
outMaxDG <- ModelEvaluate(predicted = dfTESTDG %>%
                            select(contains('SEG'), QUOTED_EDT, ATP, ALLQTargetLT, SCGQTargetLT),
                          dfTESTDG$Target,
                          dfTESTDG$MaxSCGList)  # based on segList

outMinDG <- ModelEvaluate(predicted = dfTESTDG %>%
                            select(contains('SEG'), QUOTED_EDT,ATP, ALLQTargetLT, SCGQTargetLT),
                          dfTESTDG$Target,
                          dfTESTDG$MinSCGList)  # based on segList

write.csv(outMaxDG$ACC, file.path(METRCDIR, paste0(REGION, '_DG_ACC_TEST_MAX_SCG.csv')), row.names = F)
write.csv(outMinDG$ACC, file.path(METRCDIR, paste0(REGION, '_DG_ACC_TEST_MIN_SCG.csv')), row.names = F)

# ============================================================================ #
## TESTING DATA WITH SEGMENTED BY ProdTrainCount
outTrainCountMaxDG <- ModelEvaluate(predicted = dfTESTDG %>%
                                      select(contains('SEG'), QUOTED_EDT, ATP, ALLQTargetLT, SCGQTargetLT),
                                    dfTESTDG$Target,
                                    paste0(dfTESTDG$MaxSCGList,
                                           ifelse(dfTESTDG$ProdTrainCount > 10, '_COUNT>10', '_COUNT<=10')),
                                    QTarget)
outTrainCountMinDG <- ModelEvaluate(predicted = dfTESTDG %>%
                                      select(contains('SEG'), QUOTED_EDT, ATP, ALLQTargetLT, SCGQTargetLT),
                                    dfTESTDG$Target,
                                    paste0(dfTESTDG$MinSCGList,
                                           ifelse(dfTESTDG$ProdTrainCount > 10, '_COUNT>10', '_COUNT<=10')),
                                    QTarget)

write.csv(outTrainCountMaxDG$ACC, file.path(METRCDIR, paste0(REGION, '_DG_ACC_TEST_MAX_SCG_TCOUNT.csv')), row.names = F)
write.csv(outTrainCountMinDG$ACC, file.path(METRCDIR, paste0(REGION, '_DG_ACC_TEST_MIN_SCG_TCOUNT.csv')), row.names = F)

# ============================================================================ #
## TESTING DATA WITH SEGMENTED BY Receive Date
outSegMonMaxDG1 <- ModelEvaluate(predicted = dfTESTDG %>%
                                   select(contains('SEG'), QUOTED_EDT, ATP, ALLQTargetLT, SCGQTargetLT),
                                 dfTESTDG$Target,
                                 substr(dfTESTDG$HPREDate, 1, 7),
                                 QTarget)
outSegMonMaxDG2 <- ModelEvaluate(predicted = dfTESTDG %>%
                                   select(contains('SEG'), QUOTED_EDT, ATP, ALLQTargetLT, SCGQTargetLT),
                                 dfTESTDG$Target,
                                 paste0(dfTESTDG$MaxSCGList, substr(dfTESTDG$HPREDate, 1, 7)),
                                 QTarget)

outSegMonMinDG1 <- ModelEvaluate(predicted = dfTESTDG %>%
                                   select(contains('SEG'), QUOTED_EDT, ATP, ALLQTargetLT, SCGQTargetLT),
                                 dfTESTDG$Target,
                                 substr(dfTESTDG$HPREDate, 1, 7),
                                 QTarget)

outSegMonMinDG2 <- ModelEvaluate(predicted = dfTESTDG %>%
                                   select(contains('SEG'), QUOTED_EDT, ATP, ALLQTargetLT, SCGQTargetLT),
                                 dfTESTDG$Target,
                                 paste0(dfTESTDG$MinSCGList, substr(dfTESTDG$HPREDate, 1, 7)),
                                 QTarget)

write.csv(rbind(outSegMonMaxDG1$ACC, outSegMonMaxDG2$ACC),
          file.path(METRCDIR, paste0(REGION, '_DG_ACC_TEST_MAX_SCG_SEGMON.csv')),
          row.names = F)
write.csv(rbind(outSegMonMinDG1$ACC, outSegMonMinDG2$ACC),
          file.path(METRCDIR, paste0(REGION, '_DG_ACC_TEST_MIN_SCG_SEGMON.csv')),
          row.names = F)

## EVALUATE THE MODEL - HLI level

# ============================================================================ #
## TRAINING DATA
out <- ModelEvaluate(predicted = dfTRAIN %>%
                       select(contains('SEG'), QUOTED_EDT, ATP, ALLQTargetLT, SCGQTargetLT),
                     dfTRAIN$Target, dfTRAIN$SCG, QTarget)

outSP <- ModelEvaluate(predicted = dfTRAIN %>%
                         select(contains('SCG_UNSEG'), QUOTED_EDT, ATP, ALLQTargetLT, SCGQTargetLT),
                       observed  = dfTRAIN$Target,
                       group     = paste(dfTRAIN$SCG, dfTRAIN$Supplier, sep = '_'), QTarget)

RECPlot(out$REC, ncol = 3, nrow = 3, filename = file.path(METRCDIR, paste0(REGION, '_REC_TRAIN.png')))
write.csv(rbind(out$ACC, outSP$ACC), file.path(METRCDIR, paste0(REGION, '_ACC_TRAIN.csv')), row.names = F)

# ============================================================================ #
## TESTING DATA
out <- ModelEvaluate(predicted = dfTEST %>%
                       select(contains('SEG'), QUOTED_EDT, ATP, ALLQTargetLT, SCGQTargetLT),
                     dfTEST$Target, dfTEST$SCG, QTarget)

outSP <- ModelEvaluate(predicted = dfTEST %>%
                         select(contains('SCG_UNSEG'), QUOTED_EDT, ATP, ALLQTargetLT, SCGQTargetLT),
                       observed  = dfTEST$Target,
                       group     = paste(dfTEST$GroupList, dfTEST$Supplier, sep = '_'), QTarget)

RECPlot(out$REC, ncol = 3, nrow = 3, filename = file.path(METRCDIR, paste0(REGION, '_REC_TEST.png')))
write.csv(rbind(out$ACC, outSP$ACC), file.path(METRCDIR, paste0(REGION, '_ACC_TEST.csv')), row.names = F)

# ============================================================================ #
## TESTING DATA WITH SEGMENTED BY ProdTrainCount
outTrainCount <- ModelEvaluate(predicted = dfTEST %>%
                                 select(contains('SEG'), QUOTED_EDT, ATP, ALLQTargetLT, SCGQTargetLT),
                               dfTEST$Target, paste0(dfTEST$SCG, ifelse(dfTEST$ProdTrainCount > 10, '_COUNT>10', '_COUNT<=10')), QTarget)

write.csv(outTrainCount$ACC, file.path(METRCDIR, paste0(REGION, '_ACC_TEST_TCOUNT.csv')), row.names = F)

# ============================================================================ #
## TESTING DATA WITH SEGMENTED BY HPREDate
## VERIFY THE SEASONAL INFLUENCE

outSegMon1 <- ModelEvaluate(predicted = dfTEST %>%
                              select(contains('SEG'), QUOTED_EDT, ATP, ALLQTargetLT, SCGQTargetLT),
                            dfTEST$Target,
                            substr(dfTEST$HPREDate, 1, 7), QTarget)

outSegMon2 <- ModelEvaluate(predicted = dfTEST %>%
                              select(contains('SEG'), QUOTED_EDT, ATP, ALLQTargetLT, SCGQTargetLT),
                            dfTEST$Target,
                            paste0(dfTEST$SCG, substr(dfTEST$HPREDate, 1, 7)), QTarget)

write.csv(rbind(outSegMon1$ACC, outSegMon2$ACC),
          file.path(METRCDIR, paste0(REGION, '_ACC_TEST_SEGMON.csv')), row.names = F)

# ============================================================================ #
## Actual LT VS Fitted LT Plots

modelstoplot <- c('RF_BASE_SCG_UNSEG'
                  ,'RF_BASE_SCG_SEG'
                  ,'RF_ENG_ALL_UNSEG'
                  ,'RF_ENG_ALL_SEG'
                  ,'RF_ENG_SCG_UNSEG'
                  ,'RF_ENG_SCG_SEG')

# QUOTED_EDT Plots
png(filename = file.path(METRCDIR, "WsA_SRT_Plot_Test.png"), type = 'cairo',
    width = 2000,
    height = 2000,
    res = 250)

plot_colorByDensity(dfTEST$QUOTED_EDT,
                    dfTEST$Target,
                    xlab="Quoted EDT",
                    ylab="RE to Del LT",
                    main="Lead Time vs Quoted EDT (Test Data)")

dev.off()

png(filename = file.path(METRCDIR, "WsA_SRT_Plot_Train.png"), type = 'cairo',
    width = 2000,
    height = 2000,
    res = 250)

plot_colorByDensity(dfTRAIN$QUOTED_EDT,
                    dfTRAIN$Target,xlab="Quoted EDT",
                    ylab="RE to Del LT",
                    main="Lead Time vs Quoted EDT (Training Data)")

dev.off()

# Model Plots
for (i in 1:length(modelstoplot)) {
  png(filename = file.path(METRCDIR, paste0(modelstoplot[i],"_Plot_Test.png")), type = 'cairo',
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

  png(filename = file.path(METRCDIR, paste0(modelstoplot[i],"_Plot_Train.png")), type = 'cairo',
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

#################################################################################
##################### MODEL EVALUATION ON THE SEGMENTED LT ######################
#################################################################################
#
### COMPARE SGE MODEL RETOFS & FSTODEL WITH SRT & SRT_PADDING_VALUE
#
#dfTRAINREtoFS <- data.frame(
#  SalesOrderID      = RFDataSet[train, ]$SalesOrderID,
#  SOLineItemID      = RFDataSet[train, ]$SOLineItemID,
#  DeliveryGroupID   = RFDataSet[train, ]$DeliveryGroupID,
#  GroupList         = RFDataSet[train, ]$segList,
#  Supplier          = RFDataSet[train, ]$Supplier,
#  RF_BASE_ALL_SEG   = ceiling(prBASEALLTrain$calcREtoFS) + PaddingFactor,
#  RF_BASE_SCG_SEG   = ceiling(prBASESCGTrain$calcREtoFS) + PaddingFactor,
#  RF_ENG_ALL_SEG    = ceiling(prENGALLTrain$calcREtoFS) + PaddingFactor,
#  RF_ENG_SCG_SEG    = ceiling(prENGSCGTrain$calcREtoFS) + PaddingFactor,
#  SRT               = RFDataSet[train, ]$SRT,
#  Target            = RFDataSet[train, ]$calcREtoFS
#)
#
#dfTESTREtoFS <- data.frame(
#  SalesOrderID      = RFDataSet[test, ]$SalesOrderID,
#  SOLineItemID      = RFDataSet[test, ]$SOLineItemID,
#  DeliveryGroupID   = RFDataSet[test, ]$DeliveryGroupID,
#  GroupList         = RFDataSet[test, ]$segList,
#  Supplier          = RFDataSet[test, ]$Supplier,
#  RF_BASE_ALL_SEG   = ceiling(prBASEALLTest$calcREtoFS) + PaddingFactor,
#  RF_BASE_SCG_SEG   = ceiling(prBASESCGTest$calcREtoFS) + PaddingFactor,
#  RF_ENG_ALL_SEG    = ceiling(prENGALLTest$calcREtoFS) + PaddingFactor,
#  RF_ENG_SCG_SEG    = ceiling(prENGSCGTest$calcREtoFS) + PaddingFactor,
#  SRT               = RFDataSet[test, ]$SRT,
#  Target            = RFDataSet[test, ]$calcREtoFS
#)
#
#dfTRAINFStoDel <- data.frame(
#  SalesOrderID      = RFDataSet[train, ]$SalesOrderID,
#  SOLineItemID      = RFDataSet[train, ]$SOLineItemID,
#  DeliveryGroupID   = RFDataSet[train, ]$DeliveryGroupID,
#  GroupList         = RFDataSet[train, ]$segList,
#  Supplier          = RFDataSet[train, ]$Supplier,
#  RF_BASE_ALL_SEG   = ceiling(prBASEALLTrain$calcFStoDel) + PaddingFactor,
#  RF_BASE_SCG_SEG   = ceiling(prBASESCGTrain$calcFStoDel) + PaddingFactor,
#  RF_ENG_ALL_SEG    = ceiling(prENGALLTrain$calcFStoDel) + PaddingFactor,
#  RF_ENG_SCG_SEG    = ceiling(prENGSCGTrain$calcFStoDel) + PaddingFactor,
#  SRT_PADDING_VALUE = RFDataSet[train, ]$SRT_PADDING_VALUE,
#  Target            = RFDataSet[train, ]$calcFStoDel
#)
#
#dfTESTFStoDel <- data.frame(
#  SalesOrderID      = RFDataSet[test, ]$SalesOrderID,
#  SOLineItemID      = RFDataSet[test, ]$SOLineItemID,
#  DeliveryGroupID   = RFDataSet[test, ]$DeliveryGroupID,
#  GroupList         = RFDataSet[test, ]$segList,
#  Supplier          = RFDataSet[test, ]$Supplier,
#  RF_BASE_ALL_SEG   = ceiling(prBASEALLTest$calcFStoDel) + PaddingFactor,
#  RF_BASE_SCG_SEG   = ceiling(prBASESCGTest$calcFStoDel) + PaddingFactor,
#  RF_ENG_ALL_SEG    = ceiling(prENGALLTest$calcFStoDel) + PaddingFactor,
#  RF_ENG_SCG_SEG    = ceiling(prENGSCGTest$calcFStoDel) + PaddingFactor,
#  SRT_PADDING_VALUE = RFDataSet[test, ]$SRT_PADDING_VALUE,
#  Target            = RFDataSet[test, ]$calcFStoDel
#)
#
#write.csv(dfTRAINREtoFS, paste0(REGION, '_REtoFS_TRAIN_Predictions.csv'), row.names = F)
#write.csv(dfTESTREtoFS , paste0(REGION, '_REtoFS_TEST_Predictions.csv'), row.names = F)
#write.csv(dfTRAINFStoDel, paste0(REGION, '_FStoDel_TRAIN_Predictions.csv'), row.names = F)
#write.csv(dfTESTFStoDel , paste0(REGION, '_FStoDel_TEST_Predictions.csv'), row.names = F)
#
#################################################################################
######################### EVALUATE THE MODEL - RE To FS #########################
#################################################################################
#
## ============================================================================ #
### TRAINING DATA
## ONLY LEAD TIME SEGMENTED MODELS ARE SELECTED
#out <- ModelEvaluate(predicted = dfTRAINREtoFS %>% select(contains('SEG'), SRT),
#                     observed  = dfTRAINREtoFS$Target,
#                     group     = segList[train])
#
#outSP <- ModelEvaluate(predicted = dfTRAINREtoFS %>% select(contains('SEG'), SRT),
#                       observed  = dfTRAINREtoFS$Target,
#                       group     = paste(dfTRAINREtoFS$GroupList, dfTRAINREtoFS$Supplier, sep = '_'))
#
#RECPlot(out$REC, ncol = 3, nrow = 3, filename = paste0(REGION, '_REtoFS_REC_TRAIN.png'))
#write.csv(rbind(out$ACC, outSP$ACC), paste0(REGION, '_REtoFS_ACC_TRAIN.csv'), row.names = F)
#
## ============================================================================ #
### TESTING DATA
## ONLY LEAD TIME SEGMENTED MODELS ARE SELECTED
#out <- ModelEvaluate(predicted = dfTESTREtoFS %>% select(contains('SEG'), SRT),
#                     observed  = dfTESTREtoFS$Target,
#                     group     = segList[test])
#
#outSP <- ModelEvaluate(predicted = dfTESTREtoFS %>% select(contains('SEG'), SRT),
#                       observed  = dfTESTREtoFS$Target,
#                       group     = paste(dfTESTREtoFS$GroupList, dfTESTREtoFS$Supplier, sep = '_'))
#
#RECPlot(out$REC, ncol = 3, nrow = 3, filename = paste0(REGION, '_REtoFS_REC_TEST.png'))
#write.csv(rbind(out$ACC, outSP$ACC), paste0(REGION, '_REtoFS_ACC_TEST.csv'), row.names = F)
#
## Show the metrics for testing data
#out$ACC
#
#################################################################################
######################## EVALUATE THE MODEL - FS To Del #########################
#################################################################################
#
## ============================================================================ #
### TRAINING DATA
## ONLY LEAD TIME SEGMENTED MODELS ARE SELECTED
#out <- ModelEvaluate(predicted = dfTRAINFStoDel %>% select(contains('SEG'), SRT_PADDING_VALUE),
#                     observed  = dfTRAINFStoDel$Target,
#                     group     = segList[train])
#
#outSP <- ModelEvaluate(predicted = dfTRAINFStoDel %>% select(contains('SEG'), SRT_PADDING_VALUE),
#                       observed  = dfTRAINFStoDel$Target,
#                       group     = paste(dfTRAINFStoDel$GroupList, dfTRAINFStoDel$Supplier, sep = '_'))
#
#RECPlot(out$REC, ncol = 3, nrow = 3, filename = paste0(REGION, '_FStoDel_REC_TRAIN.png'))
#write.csv(rbind(out$ACC, outSP$ACC), paste0(REGION, '_FStoDel_ACC_TRAIN.csv'), row.names = F)
#
## ============================================================================ #
### TESTING DATA
## ONLY LEAD TIME SEGMENTED MODELS ARE SELECTED
#out <- ModelEvaluate(predicted = dfTESTFStoDel %>% select(contains('SEG'), SRT_PADDING_VALUE),
#                     observed  = dfTESTFStoDel$Target,
#                     group     = segList[test])
#
#outSP <- ModelEvaluate(predicted = dfTESTFStoDel %>% select(contains('SEG'), SRT_PADDING_VALUE),
#                       observed  = dfTESTFStoDel$Target,
#                       group     = paste(dfTESTFStoDel$GroupList, dfTESTFStoDel$Supplier, sep = '_'))
#
#RECPlot(out$REC, ncol = 3, nrow = 3, filename = paste0(REGION, '_FStoDel_REC_TEST.png'))
#write.csv(rbind(out$ACC, outSP$ACC), paste0(REGION, '_FStoDel_ACC_TEST.csv'), row.names = F)
#
## Show the metrics for testing data
#out$ACC


################################  END OF SCRIPT #################################
