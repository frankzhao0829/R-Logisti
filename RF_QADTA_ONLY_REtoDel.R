## Build and Test models based on QADTA Data ONLY
## Limited to Q2 & Q3 Data in FY16
## Last Modified @ 2016-11-24 09:00:46  GMT ------------------------------

## For Microsoft R Open
#checkpoint::checkpoint('2016-10-31')

################################################################################
################################## CONSTANTS ###################################
################################################################################

#RFPKG <- 'ranger'
#RFPKG <- 'randomForest'
RFPKG <- 'quantregForest'

if (RFPKG=='quantregForest') Quantile <- 0.7 else Quantile <- NULL

#TARGETCMPLXTY <- c('CTO', 'Complex CTO', 'Configured Rack', 'Rack Solution')
REGION <- 'APJ'

PRFTCNTRMAP <- setNames(
  c('Americas', 'Asia Pacific', 'EMEA'),
  c('AMS'     , 'APJ'         , 'EMEA')
)

#FILE <- paste0('data/transformed/', REGION, '_HLI_CLEAN.rds')
FILE <- paste0('data/transformed/', 'QADTA_Raw_Data.rds')

################################################################################
############################## LOADING PACKAGES ################################
################################################################################

library(dplyr)
library(RFPKG, character.only = TRUE)
library(ggplot2)
library(doParallel)
library(foreach)
library(bizdays)
library(countrycode)

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

QADTAData <- readRDS(FILE)

QADTAData <- QADTAData %>% distinct()

## Check relationship between Sales Order ID and Sales Quote ID
QADTAData %>% group_by(SALES_ORDER_ID) %>%
  summarise(c = n_distinct(`qv SLS_QTN_ID`)) %>% filter(c > 1)

QADTAClean <- QADTAData %>%
  arrange(Profit_Center_L0_Name, DG_RE_DE, SALES_ORDER_ID, desc(`qv SLS_QTN_VRSN_SQN_NR`)) %>%
  group_by(Profit_Center_L0_Name, DG_RE_DE, SALES_ORDER_ID) %>% slice(1) %>% ungroup()

RFDataSet <- QADTAClean %>%
  filter(Profit_Center_L0_Name == PRFTCNTRMAP[REGION] & !is.na(RE_TO_DEL)) %>%
  select(DelQuarter = Delivery_Quarter,
         DelMonth = Delivery_Fiscal_Month,
         DeliveryGroupID = DG_RE_DE,
         SubRegion = Profit_Center_L2_Name,
         CustName = GA_AMID_4_NAME,
         SalesOrderID = SALES_ORDER_ID,
         calcREtoDel = RE_TO_DEL,
         SRT,
         SRT_PADDING_VALUE,
         QUOTED_EDT,
         FPDelDate = First_Planned_Delivery_Date,
         CountryCode = Country_CD)

RFDataSet <- RFDataSet %>%
  mutate(HPREDate = sapply(strsplit(DeliveryGroupID, '_'), function(x) x[2]),
         Delivery = sapply(strsplit(DeliveryGroupID, '_'), function(x) x[3])) %>%
  mutate(HPREDate = as.Date(HPREDate, format = '%m/%d/%Y'),
         Delivery = as.Date(Delivery, format = '%m/%d/%Y'))

# Select Q2 & Q3 data for FY16
#RFDataSet <- RFDataSet %>%
#  filter(DelQuarter %in% c('Q2', 'Q3') & substr(DelMonth, 4, 5) == '16')

RFDataSet <- RFDataSet %>% filter(HPREDate >= as.Date('2016-02-01') & HPREDate <= as.Date('2016-07-31'))

################################################################################
############################# TRAIN & TEST SPLIT ###############################
################################################################################

nobs <- nrow(RFDataSet)

## Method 1: 70/30 Split

set.seed(42)

# SPLIT IN HLI LEVEL
#train <- sample(nobs, 0.70*nobs)
#test <- which(! 1:nobs %in% train )

# SPLIT IN DG LEVEL
DG <- unique(RFDataSet$DeliveryGroupID)
deliverygroups <- data.frame(DeliveryGroupID = DG, randgroup = runif(length(DG)))

RFDataSet <- merge(RFDataSet, deliverygroups, by="DeliveryGroupID")
train <- which(RFDataSet$randgroup <= 0.7)
test <- which(RFDataSet$randgroup > 0.7)

## Method 2: Delivery Date Week No. Split

## This works as this week number for 2016: http://www.epochconverter.com/weeks/2016
#train <- which((as.integer(strftime(as.Date(RFDataSet$Delivery), format="%U"))) %in% 18:30)
#train <- which((as.integer(strftime(as.Date(RFDataSet$HPREDate), format="%U"))) < 24)
#test  <- which((as.integer(strftime(as.Date(RFDataSet$Delivery), format="%U"))) %in% 31:33)
#test <- which(! 1:nobs %in% train )

################################################################################
############################ ENGINEERING FEATURES ##############################
################################################################################

MeanLTSubRegion <- RFDataSet[train, ] %>%
  group_by(SubRegion) %>%
  summarise(MeanREtoDel = mean(calcREtoDel, na.rm = TRUE))

RFDataSet <- RFDataSet %>% left_join(MeanLTSubRegion)

# WEEK No. From/To Clean Date To/From Quarter End
RFDataSet <- RFDataSet %>%
  mutate(WksAftMonBeg = pmin(GetWksAftMonBeg(HPREDate), 4),
         WksBefMonEnd = pmin(GetWksBefMonEnd(HPREDate), 4),
         WksAftQtrBeg = pmin(GetWksAftQtrBeg(HPREDate), 7),
         WksBefQtrEnd = pmin(GetWksBefQtrEnd(HPREDate), 7))

if (REGION == 'EMEA') {
  top20CtryCode <- names(tail(sort(table(RFDataSet$CountryCode)), 18))

  RFDataSet <- RFDataSet %>%
    mutate(CountryCodeNew = countrycode(CountryCode, 'iso2c', 'region'))

  RFDataSet[RFDataSet$CountryCode == 'CS', 'CountryCodeNew'] <- 'Southern Europe'
  RFDataSet[RFDataSet$CountryCode == 'TW', 'CountryCodeNew'] <- 'Eastern Asia'

  RFDataSet <- RFDataSet %>%
    mutate(CountryCodeNew = ifelse(CountryCode %in% top20CtryCode, CountryCode, CountryCodeNew))
}


################################################################################
############################## IMPUTE NA VALUES ################################
################################################################################

RFDataSet %>% contain(NA) %>% length()

# --TODOTODO--: Imputation can be improved
# Impute NA with a fix value '?'
RFDataSet[is.na(RFDataSet)] <- '?'

length(unique(RFDataSet$SalesOrderID))

################################################################################
######################## TARGET LIST AND GROUP SETTING #########################
################################################################################

# FOR ORDER: RE TO DEL
targetList <- c('calcREtoDel')

# Unseg List - no data segmentation
unsegList <- rep('ALL', nrow(RFDataSet))

# Build models by SupplierComplexityGroup
segList <- unsegList

# ADD grouping to dataframe for convenience (MeanLT calculation for example)
RFDataSet$segList <- segList

################################################################################
####################### MODEL BUILD PARAMETERS SETTING #########################
################################################################################

## SET INPUT PARAMETERS HERE

baseCatInput <- c(
  'SubRegion'
  , ifelse(REGION == 'EMEA', 'CountryCodeNew', 'CountryCode')
)

engCatInput <- NULL

baseNumInput <- c(
  'SRT'
  , 'SRT_PADDING_VALUE'
  , 'QUOTED_EDT'
)

engNumInput <- c(
  'MeanREtoDel'
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

engInput  <- c(baseInput, engCatInput, engNumInput)

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

#rfBASESCG <- ModelBuild(RFPKG = RFPKG,
#                        data = RFDataSet[train, ],
#                        input = baseInput,
#                        targetList = targetList,
#                        group = segList[train])
#
### RF VARIABLE IMPORTANCE
#
#lapply(names(rfBASESCG), function(nm, myList) {
#  VarImp(RFPKG = RFPKG, rfList = myList[[nm]], filename = paste0('VarImp_BASESCG_', nm, '.png'))
#}, myList = rfBASESCG)
#
### S2 - Predictions
#
#prBASESCGTrain <- ModelPredict(RFPKG = RFPKG,
#                               modelList = rfBASESCG,
#                               data = RFDataSet[train, ],
#                               targetList = targetList,
#                               group = segList[train], Quantile)
#
#prBASESCGTest <- ModelPredict(RFPKG = RFPKG,
#                              modelList = rfBASESCG,
#                              data = RFDataSet[test, ],
#                              targetList = targetList,
#                              group = segList[test], Quantile)
#
##saveRDS(rfBASESCG, 'rfBASESCG.rds')
##rm(rfBASESCG)
##rfBASESCG <- readRDS('rfBASESCG.rds')

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

#rfENGSCG <- ModelBuild(RFPKG = RFPKG,
#                       data = RFDataSet[train, ],
#                       input = engInput,
#                       targetList = targetList,
#                       group = segList[train])
#
#lapply(names(rfENGSCG), function(nm, myList) {
#  VarImp(RFPKG = RFPKG, rfList = myList[[nm]], filename = paste0('VarImp_ENGSCG_', nm, '.png'))
#}, myList = rfENGSCG)
#
### S4 - Predictions
#
#prENGSCGTrain <- ModelPredict(RFPKG = RFPKG,
#                              modelList = rfENGSCG,
#                              data = RFDataSet[train, ],
#                              targetList = targetList,
#                              group = segList[train], Quantile)
#
#prENGSCGTest <- ModelPredict(RFPKG = RFPKG,
#                             modelList = rfENGSCG,
#                             data = RFDataSet[test, ],
#                             targetList = targetList,
#                             group = segList[test], Quantile)

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

PaddingFactor <- 0.0

dfTRAIN <- data.frame(
  SalesOrderID      = RFDataSet[train, ]$SalesOrderID,
  DeliveryGroupID   = RFDataSet[train, ]$DeliveryGroupID,
  RF_BASE_ALL_UNSEG = ceiling(prBASEALLTrain$calcREtoDel) + PaddingFactor,
  RF_ENG_ALL_UNSEG  = ceiling(prENGALLTrain$calcREtoDel) + PaddingFactor,
  QUOTED_EDT        = RFDataSet[train, ]$QUOTED_EDT,
  #ATP               = RFDataSet[train, ]$ATPREtoDel,
  ALLMeanLT         = ceiling(mean(RFDataSet[train, ]$calcREtoDel)),
  Target            = RFDataSet[train, ]$calcREtoDel
)

dfTEST <- data.frame(
  SalesOrderID      = RFDataSet[test, ]$SalesOrderID,
  DeliveryGroupID   = RFDataSet[test, ]$DeliveryGroupID,
  RF_BASE_ALL_UNSEG = ceiling(prBASEALLTest$calcREtoDel) + PaddingFactor,
  RF_ENG_ALL_UNSEG  = ceiling(prENGALLTest$calcREtoDel) + PaddingFactor,
  QUOTED_EDT        = RFDataSet[test, ]$QUOTED_EDT,
  #ATP               = RFDataSet[test, ]$ATPREtoDel,
  ALLMeanLT         = ceiling(mean(RFDataSet[train, ]$calcREtoDel)), # IMPORTANT
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
                              QUOTED_EDT,
                              ALLMeanLT
                       ),
                     dfTRAIN$Target)

RECPlot(out$REC, ncol = 1, nrow = 1, filename = paste0(REGION, '_REC_TRAIN.png'))
write.csv(out$ACC, paste0(REGION, '_ACC_TRAIN.csv'), row.names = F)
# ============================================================================ #
## TESTING DATA
# ONLY LEAD TIME SEGMENTED MODELS ARE SELECTED
out <- ModelEvaluate(predicted = dfTEST %>%
                       select(contains('SEG'),
                              QUOTED_EDT,
                              ALLMeanLT
                       ),
                     dfTEST$Target)

out$ACC

RECPlot(out$REC, ncol = 1, nrow = 1, filename = paste0(REGION, '_REC_TEST.png'))
write.csv(out$ACC, paste0(REGION, '_ACC_TEST.csv'), row.names = F)

# ============================================================================ #
## Actual LT VS Fitted LT Plots

modelstoplot <- c(
  'RF_BASE_ALL_UNSEG'
  ,'RF_ENG_ALL_UNSEG'
)

# QUOTED_EDT Plots
png(filename = "QUOTED_EDT_Plot.png", type = 'cairo',
    width = 2000,
    height = 2000,
    res = 250)

plot_colorByDensity(dfTEST$QUOTED_EDT,
                    dfTEST$Target,
                    xlab="Quoted EDT",
                    ylab="RE to Del LT",
                    main="Lead Time vs Quoted EDT (Test Data)")

dev.off()

png(filename = "QUOTED_EDT_Plot_Train.png", type = 'cairo',
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
  png(filename = paste0(modelstoplot[i],"_Plot.png"), type = 'cairo',
      width = 2000,
      height = 2000,
      res = 250)
  plot_colorByDensity(dfTEST[,modelstoplot[i]],
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

  plot_colorByDensity(dfTRAIN[,modelstoplot[i]],
                      dfTRAIN$Target,
                      xlab=paste0("Fitted Quantile RF model (Q=",Quantile*100,"th Percentile)"),
                      ylab="RE to Del LT",
                      main=paste0("Actual Lead Time vs Model ",
                                  modelstoplot[i],"\n (Training Data)"))

  dev.off()
}
