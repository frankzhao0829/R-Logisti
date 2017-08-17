## Last Modified @ 2017-04-11 06:52:37  GMT ------------------------------

################################################################################
################################## CONSTANTS ###################################
################################################################################

#RFPKG <- 'ranger'
#RFPKG <- 'randomForest'
RFPKG <- 'quantregForest'

if (RFPKG=='quantregForest') Quantile <- 0.90 else Quantile <- NULL

#TARGETCMPLXTY <- c('CTO', 'Complex CTO', 'Configured Rack', 'Rack Solution')

L0NAME <- setNames(c('Asia Pacific', 'Americas', 'EMEA'),
                   c('APJ'         , 'AMS'     , 'EMEA'))

REGIONLIST <- c('AMS', 'APJ', 'EMEA')
FILE <- paste0('data/transformed/', REGIONLIST, '_ECOM_HLI.rds')

# REFER TO SHIP TO ISO COUNTRY NAME
#INCLDCOUNTRIES <- c('US', 'CA')
#INCLDCOUNTRIES <- c('CA')
#INCLDCOUNTRIES <- c('BR') # USE THIS FOR BRAZIL, CREATE SIMILAR LIST FOR OTHER SEG

# NEW PARAMETER TO SELECT X% OF TOTAL TRAIN DATA
TRAINPCT <- 1

# TRAIN TEST SPLIT PARAMS
TRAINSTARTDATE <- '2016-04-01'
TRAINENDDATE   <- '2016-10-01'
TESTENDDATE    <- '2017-02-01'

################################################################################
############################## LOADING PACKAGES ################################
################################################################################

#options( java.parameters = "-Xmx3g")
#library(RJDBC)
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

higherLevelData <- do.call(rbind, lapply(FILE, readRDS))

# Connect to DB
#drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver", "/srv/drivers/sqljdbc41.jar")
#dbhandle <- dbConnect(drv, "jdbc:sqlserver://ec4t00951;databaseName=cpx", 'sa', 'Sasa@1234')
#sqlECOM <- "SELECT * FROM [dbo].[eOrders_HLI_CLEAN_OSS_v]"
#higherLevelData  <- dbGetQuery(dbhandle, sqlECOM)

################################################################################
#############################  PREPARE INPUT DATA ##############################
################################################################################

# Train Data Selection & RENAME the columns
RFDataSet <- higherLevelData %>%
  #filter(`Supplier Complexity Group` %in% TARGETCMPLXTY) %>%   # Filter On CMPLXTY
  #filter(`Shipment to ISO Country Code` %in% INCLDCOUNTRIES) %>%    # Filter on Country
  filter(!is.na(`DG_RE-DE`)) %>%
  filter(`ASAP Flag` == 'Y') %>%
  select(Version,
         SubRegion = `Profit Center L2 Name`,
         Region = `Profit Center L0 Name`,
         SalesOrderID = `Sales Order ID`,
         SOLineItemID = `Sales Order Line Item ID`,
         HLI_ID,
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
         EUSRTLeadTime = EUMaxSRT,
         EUConstraintFlag = EUConstraintFlag,
         NASRTLeadTime = NAMaxSRT,
         NAConstraintFlag = NAConstraintFlag,
         CountOf0D1,
         Hash0D1Flag,
         Ctry_Rgn,
         MATL_TYPE_CD,
         MDL_ID,
         TYPE_DN,
         NEW_PL,
         NEW_GBU,
         PR_PROD_FMLY,
         NEW_SLS_ORG
  )

## Calculate lead time in diffrent segs

# US Holiday, Sat/Sun as non-working days
cal <- bizdays::create.calendar("US/ANBIMA", weekdays=c("saturday", "sunday"))

RFDataSet <- RFDataSet %>%
  mutate(ATPREtoFS   = bizdays::bizdays(HPREDate , FPShipDate, cal),
         calcREtoCL  = bizdays::bizdays(HPREDate , Clean     , cal),
         calcREtoBT  = bizdays::bizdays(HPREDate , ProdStart , cal),
         calcREtoFS  = bizdays::bizdays(HPREDate , FShip     , cal),
         calcCLtoBT  = bizdays::bizdays(Clean    , ProdStart , cal),
         calcCLtoFS  = bizdays::bizdays(Clean    , FShip     , cal),
         calcBTtoFS  = bizdays::bizdays(ProdStart, FShip     , cal))

RFDataSet <- RFDataSet %>%
  filter(ATPREtoFS    >= 0 &
           calcREtoCL >= 0 &
           calcREtoBT >= 0 &
           calcREtoFS >= 0 &
           calcCLtoBT >= 0 &
           calcCLtoFS >= 0 &
           calcBTtoFS >= 0)

################################################################################
###################### SRT VALUE IMPUTATION AND FILTERING ######################
################################################################################

# Impute missing SRT using Alt SRT
# Use SRTLeadTime for null check because ConstraintFlag may be null even if SRT is not null

RFDataSet <- RFDataSet %>%
  mutate(EUSRTLeadTime = ifelse(is.na(EUSRTLeadTime), NASRTLeadTime, EUSRTLeadTime))
RFDataSet <- RFDataSet %>%
  mutate(EUConstraintFlag = ifelse(is.na(EUSRTLeadTime), NAConstraintFlag, EUConstraintFlag))

RFDataSet <- RFDataSet %>%
  mutate(NASRTLeadTime = ifelse(is.na(NASRTLeadTime), EUSRTLeadTime, NASRTLeadTime))
RFDataSet <- RFDataSet %>%
  mutate(NAConstraintFlag = ifelse(is.na(NASRTLeadTime), EUConstraintFlag, NAConstraintFlag))

# Filter out invalid values
# CAUTION: may affect the derived engineering features calculation
RFDataSet <- RFDataSet %>% filter(!is.na(EUSRTLeadTime))

################################################################################
##################  REMAP PLANT CODE / SRPC TO SHIPPINGPOINT ###################
################################################################################

## FOR EMEA ONLY
EMEASPMappings <- readRDS('data/transformed/EMEASPMappings.rds')

EMEASPMappings$Region <- 'EMEA'

RFDataSet <- RFDataSet %>% left_join(EMEASPMappings)

################################################################################
###########################  REMAP GBU TO OLD NAMES ############################
################################################################################

PL_GBU_MAP <- do.call(rbind, lapply(sprintf('data/transformed/%s_PL_GBU_MAP.rds', REGIONLIST), readRDS)) %>% distinct()

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

RFDataSet <- RFDataSet %>%
  mutate(DelPriorityCodeNew =
           ifelse(Region == 'Americas',
                  ifelse(DelPriorityCode %in% names(AMSDelCodeMap),
                         AMSDelCodeMap[DelPriorityCode],
                         DelPriorityCode),
                  ifelse(DelPriorityCode %in% names(DelCodeMap),
                         DelCodeMap[DelPriorityCode],
                         DelPriorityCode)))

################################################################################
####################### RECODE HPN PL & Plant Code #############################
################################################################################

RFDataSet <-
  RFDataSet %>% mutate(ProdLineID = ifelse(SupplierComplexityGroup == 'HPN Option' &
                                             ProdLineID %in% c('34', '35'), '6H', ProdLineID))
RFDataSet <-
  RFDataSet %>% mutate(PlantCode = ifelse(SupplierComplexityGroup == 'HPN Option' &
                                            PlantCode %in% c('8O00', '6303'), '01KA', PlantCode))

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

OOTrain <- which(RFDataSet$HPREDate >= TRAINSTARTDATE & RFDataSet$HPREDate < TRAINENDDATE)
OOTest  <- which(RFDataSet$HPREDate >= TRAINENDDATE   & RFDataSet$HPREDate < TESTENDDATE)

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
#FSMeanLTGroup <- c('SupplierComplexityGroup', 'PlantCode', 'ProdFamilyDescr')
FSMeanLTGroup <- c('SupplierComplexityGroup', 'ProdFamilyDescr')

RFDataSet <- RFDataSet %>%
  mutate(MeanREtoCL = GenHierCatGroupFeat(., group = FSMeanLTGroup, f = train, y = 'calcREtoCL', mean, na.rm = TRUE),
         MeanREtoBT = GenHierCatGroupFeat(., group = FSMeanLTGroup, f = train, y = 'calcREtoBT', mean, na.rm = TRUE),
         MeanCLtoBT = GenHierCatGroupFeat(., group = FSMeanLTGroup, f = train, y = 'calcCLtoBT', mean, na.rm = TRUE),
         MeanBTtoFS = GenHierCatGroupFeat(., group = FSMeanLTGroup, f = train, y = 'calcBTtoFS', mean, na.rm = TRUE),
         MeanREtoFS = GenHierCatGroupFeat(., group = FSMeanLTGroup, f = train, y = 'calcREtoFS', mean, na.rm = TRUE))

#EngFeatUSP <- "dbo.uspGetECOMProdSCGPlantMeanLT"
EngFeatUSP <- "[dbo].[uspGetAllProdSCGPlantMaxProdMeanLT]"

## THE SQL TRAIN PERIOD MUST MATCH WITH THE MODEL SETTINGS
sqlUSP <- paste0("EXEC "                        , EngFeatUSP,
                 " @trainStartDate = '"         , TRAINSTARTDATE,
                 " 00:00:00', @trainEndDate = '", TRAINENDDATE,
                 " 00:00:00'")

#MeanLT <- dbGetQuery(dbhandle, sqlUSP)
MeanLT <- readRDS(paste0('data/transformed/SQLMeanLT', gsub('-', '', TRAINSTARTDATE), '_', gsub('-', '', TRAINENDDATE), '.rds'))

#saveRDS(MeanLT, paste0('data/transformed/SQLMeanLT', gsub('-', '', TRAINSTARTDATE), '_', gsub('-', '', TRAINENDDATE), '.rds'))

#dbDisconnect(dbhandle)

RFDataSet <- RFDataSet %>% left_join(MeanLT)

RFDataSet <- RFDataSet %>% mutate(MaxMeanREtoFS = ifelse(is.na(MaxMeanREtoFS), MeanREtoFS, MaxMeanREtoFS))
RFDataSet <- RFDataSet %>% mutate(MaxMeanREtoCL = ifelse(is.na(MaxMeanREtoCL), MeanREtoCL, MaxMeanREtoCL))
RFDataSet <- RFDataSet %>% mutate(MaxMeanREtoBT = ifelse(is.na(MaxMeanREtoBT), MeanREtoBT, MaxMeanREtoBT))
RFDataSet <- RFDataSet %>% mutate(MaxMeanCLtoBT = ifelse(is.na(MaxMeanCLtoBT), MeanCLtoBT, MaxMeanCLtoBT))
RFDataSet <- RFDataSet %>% mutate(MaxMeanBTtoFS = ifelse(is.na(MaxMeanBTtoFS), MeanBTtoFS, MaxMeanBTtoFS))

## WEEK No. From/To Clean Date To/From Quarter End
RFDataSet <- RFDataSet %>%
  mutate(WksAftMonBeg = pmin(GetWksAftMonBeg(HPREDate), 4),
         WksBefMonEnd = pmin(GetWksBefMonEnd(HPREDate), 4),
         WksAftQtrBeg = pmin(GetWksAftQtrBeg(HPREDate), 7),
         WksBefQtrEnd = pmin(GetWksBefQtrEnd(HPREDate), 7))

# Define new Product Line features by GB to get around factor level limits
RFDataSet <- RFDataSet %>% mutate(PL_CMPUT = ifelse(GBU == 'Compute'  , ProdLineID, "x0"))
RFDataSet <- RFDataSet %>% mutate(PL_STRGE = ifelse(GBU == 'Storage'  , ProdLineID, "x0"))
RFDataSet <- RFDataSet %>% mutate(PL_DCNET = ifelse(GBU == 'DC Net'   , ProdLineID, "x0"))
RFDataSet <- RFDataSet %>% mutate(PL_ARUBA = ifelse(GBU == 'HPE Aruba', ProdLineID, "x0"))

RFDataSet <- RFDataSet %>% mutate(NEW_PL_CMPUT = ifelse(NEW_GBU == 'Compute'   , NEW_PL, "x0"))
RFDataSet <- RFDataSet %>% mutate(NEW_PL_STRGE = ifelse(NEW_GBU == 'Storage'   , NEW_PL, "x0"))
RFDataSet <- RFDataSet %>% mutate(NEW_PL_SLTNS = ifelse(NEW_GBU == 'Solutions' , NEW_PL, "x0"))
RFDataSet <- RFDataSet %>% mutate(NEW_PL_NTWKG = ifelse(NEW_GBU == 'Networking', NEW_PL, "x0"))

RFDataSet <- RFDataSet %>% mutate(I_EMEA = ifelse(Region == 'EMEA',         1, 0))
RFDataSet <- RFDataSet %>% mutate(I_AMS  = ifelse(Region == 'Americas',     1, 0))
RFDataSet <- RFDataSet %>% mutate(I_APJ  = ifelse(Region == 'Asia Pacific', 1, 0))

################################################################################
############################## IMPUTE NA VALUES ################################
################################################################################

RFDataSet %>% contain(NA)

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
targetList <- c('calcREtoFS', 'calcREtoBT', 'calcBTtoFS')

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

# ============================================================================ #
# CATEGORICAL INPUTS FOR BOTH BASE AND ENG

baseCatInput <- c('GBU'
                  #, 'SNPCode'
                  #, 'SalesProdType'
                  , 'Region'
                  , 'PlantCode'
                  #, 'PL_CMPUT'
                  #, 'PL_STRGE'
                  #, 'PL_DCNET'
                  #, 'PL_ARUBA'
                  , 'SOHeaderCompleteFlag'
                  #, 'ShipRePointCode'
                  , 'ShippingPoint'
                  #, 'Supplier'
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
                  #, 'Ctry_Rgn'
                  , 'EUConstraintFlag'
                  , 'NAConstraintFlag'
                  , 'Hash0D1Flag'
                  , 'I_EMEA'
                  , 'I_AMS'
                  , 'I_APJ'
                  , 'NEW_GBU'
                  , 'NEW_PL_CMPUT'
                  , 'NEW_PL_STRGE'
                  , 'NEW_PL_SLTNS'
                  , 'NEW_PL_NTWKG'
                  , 'PR_PROD_FMLY'
                  , 'MATL_TYPE_CD'
                  , 'NEW_SLS_ORG'
                  #, 'DelPriorityCodeNew'
)

engCatInput <- NULL   # NO CAT ENG

## !!CHANGE CATEGORICAL INPUTS TO FACTOR
RFDataSet[c(baseCatInput, engCatInput)] <- lapply(RFDataSet[c(baseCatInput, engCatInput)], factor)


## SPLIT CAT INPUTS WITH MORE THAN 34 LEVELS (FOR BASE ONLY NOW)
NNMAX <- 34

tt <- unlist(lapply(RFDataSet[, baseCatInput], function(x) if(is.factor(x)) length(levels(x)) > NNMAX))

highCols <- names(tt[tt])

for (col in highCols) {
  temp <- as.data.frame(do.call(cbind, lapply(REGIONLIST, function(x) {
    return(ifelse(RFDataSet$Region == L0NAME[x], as.character(RFDataSet[[col]]), 'x0'))
  })))

  names(temp) <- paste(col, REGIONLIST, sep = '_')
  RFDataSet <- RFDataSet %>% bind_cols(temp)
}

baseCatInput <- setdiff(baseCatInput, highCols)
baseCatInput <- c(baseCatInput, unlist(lapply(highCols, function(x) paste(x, REGIONLIST, sep = '_'))))

# ============================================================================ #

baseNumInput <-
  c(
    'BaseQuantity'
    , 'NetUSDAmount'
    #, 'EstE2EDayNo'
    , 'ITM_COUNT'
    , 'CountOf0D1'
    , 'EUSRTLeadTime'
    , 'NASRTLeadTime'
    , 'UnitQuantity'
  )

#engNumInput <- c('MeanREtoCL'
#                 , 'MeanREtoBT'
#                 , 'MeanCLtoBT'
#                 , 'MeanBTtoFS'
#                 , 'MeanREtoFS'
#                 , 'MaxMeanCLtoBT'
#                 , 'MaxMeanREtoCL'
#                 , 'MaxMeanREtoBT'
#                 , 'MaxMeanBTtoFS'
#                 , 'MaxMeanREtoFS'
#                 , 'Num_Units'
#                 , 'Num_Options'
#                 , 'Num_Shipments'
#                 , 'WksAftMonBeg'
#                 , 'WksBefMonEnd'
#                 , 'WksAftQtrBeg'
#                 , 'WksBefQtrEnd'
#)

engNumInput <- list()

engNumInput$calcREtoFS <-
  c(
    #, 'MeanREtoFS'
    'MaxMeanREtoFS'
    , 'Num_Units'
    , 'Num_Options'
    #, 'Num_Shipments'
    #, 'WksAftMonBeg'
    #, 'WksBefMonEnd'
    #, 'WksAftQtrBeg'
    #, 'WksBefQtrEnd'
  )

engNumInput$calcREtoBT <-
  c(
    #, 'MeanREtoBT'
    'MaxMeanREtoBT'
    , 'Num_Units'
    , 'Num_Options'
    #, 'Num_Shipments'
    #, 'WksAftMonBeg'
    #, 'WksBefMonEnd'
    #, 'WksAftQtrBeg'
    #, 'WksBefQtrEnd'
  )

engNumInput$calcBTtoFS <-
  c(
    #, 'MeanBTtoFS'
    'MaxMeanBTtoFS'
    , 'Num_Units'
    , 'Num_Options'
    #, 'Num_Shipments'
    #, 'WksAftMonBeg'
    #, 'WksBefMonEnd'
    #, 'WksAftQtrBeg'
    #, 'WksBefQtrEnd'
  )

# ============================================================================ #

# TEXT MINING WEIGHT INPUT
tmNumInput <- paste0('C_', 1:20)

baseInput <- c(baseCatInput, baseNumInput)          # Base Predictors

#engInput  <- c(baseInput, engCatInput, engNumInput)

# TRY REMOVE Ctry_Rgn for engineering inputs
#engInput <- setdiff(engInput, c('Ctry_Rgn', 'Ctry_Rgn_AMS', 'Ctry_Rgn_APJ', 'Ctry_Rgn_EMEA'))

engInput <- list()
engInput$calcREtoFS <- c(baseInput, engCatInput, engNumInput$calcREtoFS)
engInput$calcREtoBT <- c(baseInput, engCatInput, engNumInput$calcREtoBT)
engInput$calcBTtoFS <- c(baseInput, engCatInput, engNumInput$calcBTtoFS)

#tmInput   <- c(engInput, tmNumInput)                # TM + Eng + Base Predictors

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
############# FIXED QUANTILE LT MODELS BASED ON DIFFERENT SEGMENTS #############
################################################################################

# ============================================================================ #
## GENERATE MEAN LT MODEL FOR SEGMENTED
## USE THE TRAIN AVERAGE FOR BOTH TRAINING AND TESTING DATASET

QTarget <- Quantile

segMeanLT <- RFDataSet[train, ] %>%
  group_by(segList) %>%
  summarise(QTargetLT = round(as.numeric(quantile(calcREtoFS, QTarget)))) %>% ungroup()

segMeanLT2 <- RFDataSet[train, ] %>%
  group_by(ProductID) %>%
  summarise(QTargetLT2 = round(as.numeric(quantile(calcREtoFS, QTarget)))) %>% ungroup()

segMeanLT3 <- RFDataSet[train, ] %>%
  group_by(ProdFamilyDescr) %>%
  summarise(QTargetLT3 = round(as.numeric(quantile(calcREtoFS, QTarget)))) %>% ungroup()

RFDataSet <- RFDataSet %>% inner_join(segMeanLT)
RFDataSet <- RFDataSet %>% left_join(segMeanLT2)
RFDataSet <- RFDataSet %>% left_join(segMeanLT3)

RFDataSet <-
  RFDataSet %>% mutate(QTargetLT2 = ifelse(is.na(QTargetLT2), QTargetLT3, QTargetLT2))
RFDataSet <-
  RFDataSet %>% mutate(QTargetLT2 = ifelse(is.na(QTargetLT2), QTargetLT , QTargetLT2))
RFDataSet <-
  RFDataSet %>% mutate(QTargetLT3 = ifelse(is.na(QTargetLT3), QTargetLT , QTargetLT3))

#QTarget <- Quantile + 0.05

segMeanLTB <- RFDataSet[train, ] %>%
  group_by(segList) %>%
  summarise(QTargetLTB = round(as.numeric(quantile(calcREtoFS, QTarget)))) %>% ungroup()

segMeanLTB2 <- RFDataSet[train, ] %>%
  group_by(ProductID, AMID2ID) %>%
  summarise(QTargetLTB2 = round(as.numeric(quantile(calcREtoFS, QTarget)))) %>% ungroup()

segMeanLTB3 <- RFDataSet[train, ] %>%
  group_by(ProdFamilyDescr, AMID2ID) %>%
  summarise(QTargetLTB3 = round(as.numeric(quantile(calcREtoFS, QTarget)))) %>% ungroup()

RFDataSet <- RFDataSet %>% inner_join(segMeanLTB)
RFDataSet <- RFDataSet %>% left_join(segMeanLTB2)
RFDataSet <- RFDataSet %>% left_join(segMeanLTB3)

RFDataSet <-
  RFDataSet %>% mutate(QTargetLTB2 = ifelse(is.na(QTargetLTB2), QTargetLTB3, QTargetLT2 ))
RFDataSet <-
  RFDataSet %>% mutate(QTargetLTB2 = ifelse(is.na(QTargetLTB2), QTargetLTB , QTargetLTB2))
RFDataSet <-
  RFDataSet %>% mutate(QTargetLTB3 = ifelse(is.na(QTargetLTB3), QTargetLTB , QTargetLT3 ))

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

prBASEALLTest85 <- ModelPredict(RFPKG = RFPKG,
                                modelList = rfBASEALL,
                                data = RFDataSet[test, ],
                                targetList = targetList,
                                group = unsegList[test], 0.85)

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

prBASESCGTest85 <- ModelPredict(RFPKG = RFPKG,
                                modelList = rfBASESCG,
                                data = RFDataSet[test, ],
                                targetList = targetList,
                                group = segList[test], 0.85)

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

prENGALLTest85 <- ModelPredict(RFPKG = RFPKG,
                               modelList = rfENGALL,
                               data = RFDataSet[test, ],
                               targetList = targetList,
                               group = unsegList[test], 0.85)

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

prENGSCGTest85 <- ModelPredict(RFPKG = RFPKG,
                               modelList = rfENGSCG,
                               data = RFDataSet[test, ],
                               targetList = targetList,
                               group = segList[test], .85)

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

PaddingFactor <- 0.0

dfTRAINALL <- data.frame(
  Region            = RFDataSet[train, ]$Region,
  SalesOrderID      = RFDataSet[train, ]$SalesOrderID,
  SOLineItemID      = RFDataSet[train, ]$SOLineItemID,
  HPREDate          = RFDataSet[train, ]$HPREDate,
  DeliveryGroupID   = RFDataSet[train, ]$DeliveryGroupID,
  ProdTrainCount    = RFDataSet[train, ]$ProdTrainCount,
  GroupList         = RFDataSet[train, ]$segList,
  SCG               = RFDataSet[train, ]$SupplierComplexityGroup,
  Supplier          = RFDataSet[train, ]$Supplier,
  RF_BASE_ALL_UNSEG = ceiling(prBASEALLTrain[[targetList[1]]]) + PaddingFactor,
  RF_BASE_ALL_SEG   = ceiling(Reduce('+', prBASEALLTrain[targetList[-1]])) + PaddingFactor,
  RF_BASE_SCG_UNSEG = ceiling(prBASESCGTrain[[targetList[1]]]) + PaddingFactor,
  RF_BASE_SCG_SEG   = ceiling(Reduce('+', prBASESCGTrain[targetList[-1]])) + PaddingFactor,
  RF_ENG_ALL_UNSEG  = ceiling(prENGALLTrain[[targetList[1]]]) + PaddingFactor,
  RF_ENG_ALL_SEG    = ceiling(Reduce('+', prENGALLTrain[targetList[-1]])) + PaddingFactor,
  RF_ENG_SCG_UNSEG  = ceiling(prENGSCGTrain[[targetList[1]]]) + PaddingFactor,
  RF_ENG_SCG_SEG    = ceiling(Reduce('+', prENGSCGTrain[targetList[-1]])) + PaddingFactor,
  #RF_TM_SCG_UNSEG  = ceiling(prTMSCGTrain[targetList[1]]) + PaddingFactor,
  #RF_TM_SCG_SEG    = ceiling(Reduce('+', prTMSCGTrain[targetList[-1]])) + PaddingFactor,
  WsAEUSRT          = RFDataSet[train, ]$EUSRTLeadTime,
  WsANASRT          = RFDataSet[train, ]$NASRTLeadTime,
  ATP               = RFDataSet[train, ]$ATPREtoFS,
  ALLQTargetLT      = ceiling(as.numeric(quantile(RFDataSet[train, ]$calcREtoFS, QTarget))),
  SCGQTargetLT      = RFDataSet[train, ]$QTargetLT,
  ProdIDQTargetLT   = RFDataSet[train, ]$QTargetLT2,
  ProdFMQTargetLT   = RFDataSet[train, ]$QTargetLT3,
  SCGQTargetLTB     = RFDataSet[train, ]$QTargetLTB,
  ProdIDQTargetLTB  = RFDataSet[train, ]$QTargetLTB2,
  ProdFMQTargetLTB  = RFDataSet[train, ]$QTargetLTB3,
  Target            = RFDataSet[train, ]$calcREtoFS
)

dfTESTALL <- data.frame(
  Region             = RFDataSet[test, ]$Region,
  SalesOrderID       = RFDataSet[test, ]$SalesOrderID,
  SOLineItemID       = RFDataSet[test, ]$SOLineItemID,
  HPREDate           = RFDataSet[test, ]$HPREDate,
  DeliveryGroupID    = RFDataSet[test, ]$DeliveryGroupID,
  ProdTrainCount     = RFDataSet[test, ]$ProdTrainCount,
  GroupList          = RFDataSet[test, ]$segList,
  SCG                = RFDataSet[test, ]$SupplierComplexityGroup,
  Supplier           = RFDataSet[test, ]$Supplier,
  RF_BASE_ALL_UNSEG  = ceiling(prBASEALLTest[[targetList[1]]]) + PaddingFactor,
  RF_BASE_ALL_SEG    = ceiling(Reduce('+', prBASEALLTest[targetList[-1]])) + PaddingFactor,
  RF_BASE_ALL_SEG85  = ceiling(prBASEALLTest[[targetList[2]]] + prBASEALLTest85[[targetList[3]]]) + PaddingFactor,
  RF_BASE_SCG_UNSEG  = ceiling(prBASESCGTest[[targetList[1]]]) + PaddingFactor,
  RF_BASE_SCG_SEG    = ceiling(Reduce('+', prBASESCGTest[targetList[-1]])) + PaddingFactor,
  RF_BASE_SCG_SEG85  = ceiling(prBASESCGTest[[targetList[2]]] + prBASESCGTest85[[targetList[3]]]) + PaddingFactor,
  RF_ENG_ALL_UNSEG   = ceiling(prENGALLTest[[targetList[1]]]) + PaddingFactor,
  RF_ENG_ALL_SEG     = ceiling(Reduce('+', prENGALLTest[targetList[-1]])) + PaddingFactor,
  RF_ENG_ALL_SEG85   = ceiling(prENGALLTest[[targetList[2]]] + prENGALLTest85[[targetList[3]]]) + PaddingFactor,
  RF_ENG_SCG_UNSEG   = ceiling(prENGSCGTest[[targetList[1]]]) + PaddingFactor,
  RF_ENG_SCG_SEG     = ceiling(Reduce('+', prENGSCGTest[targetList[-1]])) + PaddingFactor,
  RF_ENG_SCG_SEG85   = ceiling(prENGSCGTest[[targetList[2]]] + prENGSCGTest85[[targetList[3]]]) + PaddingFactor,
  RF_ENG_SCG_UNSEG80 = ceiling(prENGSCGTest80[[targetList[1]]]) + PaddingFactor,
  RF_ENG_SCG_UNSEG70 = ceiling(prENGSCGTest70[[targetList[1]]]) + PaddingFactor,
  #RF_TM_SCG_UNSEG   = ceiling(prTMSCGTest$calcREtoFS) + PaddingFactor,
  #RF_TM_SCG_SEG     = ceiling(Reduce('+', prTMSCGTest[targetList[-1]])) + PaddingFactor,
  WsAEUSRT           = RFDataSet[test, ]$EUSRTLeadTime,
  WsANASRT           = RFDataSet[test, ]$NASRTLeadTime,
  ATP                = RFDataSet[test, ]$ATPREtoFS,
  ALLQTargetLT       = ceiling(as.numeric(quantile(RFDataSet[train, ]$calcREtoFS, QTarget))), # IMPORTANT
  SCGQTargetLT       = RFDataSet[test, ]$QTargetLT,
  ProdIDQTargetLT    = RFDataSet[test, ]$QTargetLT2,
  ProdFMQTargetLT    = RFDataSet[test, ]$QTargetLT3,
  SCGQTargetLTB      = RFDataSet[test, ]$QTargetLTB,
  ProdIDQTargetLTB   = RFDataSet[test, ]$QTargetLTB2,
  ProdFMQTargetLTB   = RFDataSet[test, ]$QTargetLTB3,
  Target             = RFDataSet[test, ]$calcREtoFS
)

write.csv(dfTRAINALL, file.path(PREDTDIR, 'TRAIN_Predictions.csv'), row.names = F)
write.csv(dfTESTALL , file.path(PREDTDIR, 'TEST_Predictions.csv'), row.names = F)

## ROLLUP TO DG

# ============================================================================ #
## TRAINING DATA

dfTRAINDGALL <- dfTRAINALL %>% group_by(Region, DeliveryGroupID) %>%
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
            WsAEUSRT          = max(WsAEUSRT),
            WsANASRT          = max(WsANASRT),
            ATP               = max(ATP),
            ALLQTargetLT      = max(ALLQTargetLT),
            SCGQTargetLT      = max(SCGQTargetLT),
            ProdIDQTargetLT   = max(ProdIDQTargetLT),
            ProdFMQTargetLT   = max(ProdFMQTargetLT),
            SCGQTargetLTB     = max(SCGQTargetLTB),
            ProdIDQTargetLTB  = max(ProdIDQTargetLTB),
            ProdFMQTargetLTB  = max(ProdFMQTargetLTB),
            Target            = max(Target)
  ) %>% ungroup()

# ============================================================================ #
## TESTING DATA

dfTESTDGALL <- dfTESTALL %>% group_by(Region, DeliveryGroupID) %>%
  summarise(MaxGroupList       = max(as.character(GroupList)),
            MinGroupList       = min(as.character(GroupList)),
            MaxSCGList         = max(as.character(SCG)),
            MinSCGList         = min(as.character(SCG)),
            HPREDate           = max(as.character(HPREDate)),
            ProdTrainCount     = min(ProdTrainCount),
            RF_BASE_ALL_UNSEG  = max(RF_BASE_ALL_UNSEG),
            RF_BASE_ALL_SEG    = max(RF_BASE_ALL_SEG),
            RF_BASE_ALL_SEG85  = max(RF_BASE_ALL_SEG85),
            RF_BASE_SCG_UNSEG  = max(RF_BASE_SCG_UNSEG),
            RF_BASE_SCG_SEG    = max(RF_BASE_SCG_SEG),
            RF_BASE_SCG_SEG85  = max(RF_BASE_SCG_SEG85),
            RF_ENG_ALL_UNSEG   = max(RF_ENG_ALL_UNSEG),
            RF_ENG_ALL_SEG     = max(RF_ENG_ALL_SEG),
            RF_ENG_ALL_SEG85   = max(RF_ENG_ALL_SEG85),
            RF_ENG_SCG_UNSEG   = max(RF_ENG_SCG_UNSEG),
            RF_ENG_SCG_SEG     = max(RF_ENG_SCG_SEG),
            RF_ENG_SCG_SEG85   = max(RF_ENG_SCG_SEG85),
            RF_ENG_SCG_UNSEG80 = max(RF_ENG_SCG_UNSEG80),
            RF_ENG_SCG_UNSEG70 = max(RF_ENG_SCG_UNSEG70),
            WsAEUSRT           = max(WsAEUSRT),
            WsANASRT           = max(WsANASRT),
            ATP                = max(ATP),
            ALLQTargetLT       = max(ALLQTargetLT),
            SCGQTargetLT       = max(SCGQTargetLT),
            ProdIDQTargetLT    = max(ProdIDQTargetLT),
            ProdFMQTargetLT    = max(ProdFMQTargetLT),
            SCGQTargetLTB      = max(SCGQTargetLTB),
            ProdIDQTargetLTB   = max(ProdIDQTargetLTB),
            ProdFMQTargetLTB   = max(ProdFMQTargetLTB),
            Target             = max(Target)
  ) %>% ungroup()

write.csv(dfTRAINDGALL, file.path(PREDTDIR, 'TRAIN_DG_Predictions.csv'), row.names = F)
write.csv(dfTESTDGALL , file.path(PREDTDIR, 'TEST_DG_Predictions.csv'), row.names = F)

################################################################################
############################# EVALUATE THE MODEL ###############################
################################################################################

## Evaluate the model - DG level

for (REGION in REGIONLIST) {

  dfTRAINDG <- dfTRAINDGALL %>% filter(Region == L0NAME[REGION])
  dfTESTDG  <- dfTESTDGALL %>% filter(Region == L0NAME[REGION])
  dfTRAIN   <- dfTRAINALL %>% filter(Region == L0NAME[REGION])
  dfTEST    <- dfTESTALL %>% filter(Region == L0NAME[REGION])

  # ============================================================================ #
  ## TRAINING DATA
  outMaxDG <- ModelEvaluate(predicted = dfTRAINDG %>%
                              select(contains('SEG'), contains('QTargetLT'), WsAEUSRT, WsANASRT, ATP),
                            dfTRAINDG$Target,
                            dfTRAINDG$MaxSCGList)  # based on segList

  outMinDG <- ModelEvaluate(predicted = dfTRAINDG %>%
                              select(contains('SEG'), contains('QTargetLT'), WsAEUSRT, WsANASRT, ATP),
                            dfTRAINDG$Target,
                            dfTRAINDG$MinSCGList)  # based on segList

  write.csv(outMaxDG$ACC, file.path(METRCDIR, paste0(REGION, '_DG_ACC_TRAIN_MAX_SCG.csv')), row.names = F)
  write.csv(outMinDG$ACC, file.path(METRCDIR, paste0(REGION, '_DG_ACC_TRAIN_MIN_SCG.csv')), row.names = F)

  # ============================================================================ #
  ## TESTING DATA
  outMaxDG <- ModelEvaluate(predicted = dfTESTDG %>%
                              select(contains('SEG'), contains('QTargetLT'), WsAEUSRT, WsANASRT, ATP),
                            dfTESTDG$Target,
                            dfTESTDG$MaxSCGList)  # based on segList

  outMinDG <- ModelEvaluate(predicted = dfTESTDG %>%
                              select(contains('SEG'), contains('QTargetLT'), WsAEUSRT, WsANASRT, ATP),
                            dfTESTDG$Target,
                            dfTESTDG$MinSCGList)  # based on segList

  write.csv(outMaxDG$ACC, file.path(METRCDIR, paste0(REGION, '_DG_ACC_TEST_MAX_SCG.csv')), row.names = F)
  write.csv(outMinDG$ACC, file.path(METRCDIR, paste0(REGION, '_DG_ACC_TEST_MIN_SCG.csv')), row.names = F)

  # ============================================================================ #
  ## TESTING DATA WITH SEGMENTED BY ProdTrainCount
  outTrainCountMaxDG <- ModelEvaluate(predicted = dfTESTDG %>%
                                        select(contains('SEG'), contains('QTargetLT'), WsAEUSRT, WsANASRT, ATP),
                                      dfTESTDG$Target,
                                      paste0(dfTESTDG$MaxSCGList,
                                             ifelse(dfTESTDG$ProdTrainCount > 10, '_COUNT>10', '_COUNT<=10')),
                                      QTarget)
  outTrainCountMinDG <- ModelEvaluate(predicted = dfTESTDG %>%
                                        select(contains('SEG'), contains('QTargetLT'), WsAEUSRT, WsANASRT, ATP),
                                      dfTESTDG$Target,
                                      paste0(dfTESTDG$MinSCGList,
                                             ifelse(dfTESTDG$ProdTrainCount > 10, '_COUNT>10', '_COUNT<=10')),
                                      QTarget)

  write.csv(outTrainCountMaxDG$ACC, file.path(METRCDIR, paste0(REGION, '_DG_ACC_TEST_MAX_SCG_TCOUNT.csv')), row.names = F)
  write.csv(outTrainCountMinDG$ACC, file.path(METRCDIR, paste0(REGION, '_DG_ACC_TEST_MIN_SCG_TCOUNT.csv')), row.names = F)

  # ============================================================================ #
  ## TESTING DATA WITH SEGMENTED BY Receive Date
  outSegMonMaxDG1 <- ModelEvaluate(predicted = dfTESTDG %>%
                                     select(contains('SEG'), contains('QTargetLT'), WsAEUSRT, WsANASRT, ATP),
                                   dfTESTDG$Target,
                                   substr(dfTESTDG$HPREDate, 1, 7),
                                   QTarget)
  outSegMonMaxDG2 <- ModelEvaluate(predicted = dfTESTDG %>%
                                     select(contains('SEG'), contains('QTargetLT'), WsAEUSRT, WsANASRT, ATP),
                                   dfTESTDG$Target,
                                   paste0(dfTESTDG$MaxSCGList, substr(dfTESTDG$HPREDate, 1, 7)),
                                   QTarget)

  outSegMonMinDG1 <- ModelEvaluate(predicted = dfTESTDG %>%
                                     select(contains('SEG'), contains('QTargetLT'), WsAEUSRT, WsANASRT, ATP),
                                   dfTESTDG$Target,
                                   substr(dfTESTDG$HPREDate, 1, 7),
                                   QTarget)

  outSegMonMinDG2 <- ModelEvaluate(predicted = dfTESTDG %>%
                                     select(contains('SEG'), contains('QTargetLT'), WsAEUSRT, WsANASRT, ATP),
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
                         select(contains('SEG'), contains('QTargetLT'), WsAEUSRT, WsANASRT, ATP),
                       dfTRAIN$Target, dfTRAIN$SCG, QTarget)

  outSP <- ModelEvaluate(predicted = dfTRAIN %>%
                           select(contains('SEG'), contains('QTargetLT'), WsAEUSRT, WsANASRT, ATP),
                         observed  = dfTRAIN$Target,
                         group     = paste(dfTRAIN$SCG, dfTRAIN$Supplier, sep = '_'), QTarget)

  RECPlot(out$REC, ncol = 3, nrow = 3, filename = file.path(METRCDIR, paste0(REGION, '_REC_TRAIN.png')))
  write.csv(rbind(out$ACC, outSP$ACC), file.path(METRCDIR, paste0(REGION, '_ACC_TRAIN.csv')), row.names = F)

  # ============================================================================ #
  ## TESTING DATA
  out <- ModelEvaluate(predicted = dfTEST %>%
                         select(contains('SEG'), contains('QTargetLT'), WsAEUSRT, WsANASRT, ATP),
                       dfTEST$Target, dfTEST$SCG, QTarget)

  outSP <- ModelEvaluate(predicted = dfTEST %>%
                           select(contains('SEG'), contains('QTargetLT'), WsAEUSRT, WsANASRT, ATP),
                         observed  = dfTEST$Target,
                         group     = paste(dfTEST$GroupList, dfTEST$Supplier, sep = '_'), QTarget)

  RECPlot(out$REC, ncol = 3, nrow = 3, filename = file.path(METRCDIR, paste0(REGION, '_REC_TEST.png')))
  write.csv(rbind(out$ACC, outSP$ACC), file.path(METRCDIR, paste0(REGION, '_ACC_TEST.csv')), row.names = F)

  # ============================================================================ #
  ## TESTING DATA WITH SEGMENTED BY ProdTrainCount
  outTrainCount <- ModelEvaluate(predicted = dfTEST %>%
                                   select(contains('SEG'), contains('QTargetLT'), WsAEUSRT, WsANASRT, ATP),
                                 dfTEST$Target, paste0(dfTEST$SCG, ifelse(dfTEST$ProdTrainCount > 10, '_COUNT>10', '_COUNT<=10')), QTarget)

  write.csv(outTrainCount$ACC, file.path(METRCDIR, paste0(REGION, '_ACC_TEST_TCOUNT.csv')), row.names = F)

  # ============================================================================ #
  ## TESTING DATA WITH SEGMENTED BY HPREDate
  ## VERIFY THE SEASONAL INFLUENCE

  outSegMon1 <- ModelEvaluate(predicted = dfTEST %>%
                                select(contains('SEG'), contains('QTargetLT'), WsAEUSRT, WsANASRT, ATP),
                              dfTEST$Target,
                              substr(dfTEST$HPREDate, 1, 7), QTarget)

  outSegMon2 <- ModelEvaluate(predicted = dfTEST %>%
                                select(contains('SEG'), contains('QTargetLT'), WsAEUSRT, WsANASRT, ATP),
                              dfTEST$Target,
                              paste0(dfTEST$SCG, substr(dfTEST$HPREDate, 1, 7)), QTarget)

  write.csv(rbind(outSegMon1$ACC, outSegMon2$ACC),
            file.path(METRCDIR, paste0(REGION, '_ACC_TEST_SEGMON.csv')), row.names = F)

  # ============================================================================ #
  ## Actual LT VS Fitted LT Plots

  modelstoplot <- c('RF_BASE_SCG_UNSEG'
                    ,'RF_ENG_ALL_UNSEG'
                    ,'RF_ENG_SCG_UNSEG')

  # WsA SRT Plots
  png(filename = file.path(METRCDIR, paste0(REGION, "_WsA_SRT_Plot_Test.png")), type = 'cairo',
      width = 2000,
      height = 2000,
      res = 250)

  plot_colorByDensity(dfTEST$WsAEUSRT,
                      dfTEST$Target,
                      xlab="WsA SRT EDT",
                      ylab="RE to FS LT",
                      main="Lead Time vs WsA SRT (Test Data)")

  dev.off()

  png(filename = file.path(METRCDIR, paste0(REGION, "_WsA_SRT_Plot_Train.png")), type = 'cairo',
      width = 2000,
      height = 2000,
      res = 250)

  plot_colorByDensity(dfTRAIN$WsAEUSRT,
                      dfTRAIN$Target,xlab="WsA SRT EDT",
                      ylab="RE to FS LT",
                      main="Lead Time vs WsA SRT (Training Data)")

  dev.off()

  # Model Plots
  for (i in 1:length(modelstoplot)) {
    png(filename = file.path(METRCDIR, paste0(REGION, '_', modelstoplot[i],"_Plot_Test.png")), type = 'cairo',
        width = 2000,
        height = 2000,
        res = 250)
    plot_colorByDensity(dfTEST[[modelstoplot[i]]],
                        dfTEST$Target,
                        xlab=paste0("Fitted Quantile RF model (Q=",Quantile*100,"th Percentile)"),
                        ylab="RE to FS LT",
                        main=paste0("Actual Lead Time vs Model ",
                                    modelstoplot[i],"\n (Test Data)"))
    dev.off()

    png(filename = file.path(METRCDIR, paste0(REGION, '_', modelstoplot[i],"_Plot_Train.png")), type = 'cairo',
        width = 2000,
        height = 2000,
        res = 250)

    plot_colorByDensity(dfTRAIN[[modelstoplot[i]]],
                        dfTRAIN$Target,
                        xlab=paste0("Fitted Quantile RF model (Q=",Quantile*100,"th Percentile)"),
                        ylab="RE to FS LT",
                        main=paste0("Actual Lead Time vs Model ",
                                    modelstoplot[i],"\n (Training Data)"))

    dev.off()
  }
}

################################  END OF SCRIPT #################################
