library(dplyr)

source('R/FcstComp.R')
source('R/RFModel.R')

REGION <- 'EMEA'
Quantile <- 0.8

################################################################################
########################## LOAD DATA & ROLLUP TO DG ############################
################################################################################

# ============================================================================ #
## Load Data from csv files

## ------ RE TO DEL ------
dfTRAIN <- read.csv(paste0(REGION, '_TRAIN_Predictions.csv'), stringsAsFactors = FALSE)
dfTEST <- read.csv(paste0(REGION, '_TEST_Predictions.csv'), stringsAsFactors = FALSE)

## ------ RE TO FS ------
dfTRAINREtoFS <- read.csv(paste0(REGION, '_REtoFS_TRAIN_Predictions.csv'), stringsAsFactors = FALSE)
dfTESTREtoFS <- read.csv(paste0(REGION, '_REtoFS_TEST_Predictions.csv'), stringsAsFactors = FALSE)

## ------ FS TO DEL ------
dfTRAINFStoDel <- read.csv(paste0(REGION, '_FStoDel_TRAIN_Predictions.csv'), stringsAsFactors = FALSE)
dfTESTFStoDel <- read.csv(paste0(REGION, '_FStoDel_TEST_Predictions.csv'), stringsAsFactors = FALSE)

################################################################################
############################## FILTERING ON Q3 #################################
################################################################################

dfTRAIN <- dfTRAIN %>%
  filter(as.Date(sapply(strsplit(DeliveryGroupID, '_'), "[[", 3), '%m/%d/%Y') >= '2016-05-01' &
           as.Date(sapply(strsplit(DeliveryGroupID, '_'), "[[", 3), '%m/%d/%Y') <= '2016-07-31')

dfTEST <- dfTEST %>%
  filter(as.Date(sapply(strsplit(DeliveryGroupID, '_'), "[[", 3), '%m/%d/%Y') >= '2016-05-01' &
           as.Date(sapply(strsplit(DeliveryGroupID, '_'), "[[", 3), '%m/%d/%Y') <= '2016-07-31')

dfTRAINREtoFS <- dfTRAINREtoFS %>%
  filter(as.Date(sapply(strsplit(DeliveryGroupID, '_'), "[[", 3), '%m/%d/%Y') >= '2016-05-01' &
           as.Date(sapply(strsplit(DeliveryGroupID, '_'), "[[", 3), '%m/%d/%Y') <= '2016-07-31')

dfTESTREtoFS <- dfTESTREtoFS %>%
  filter(as.Date(sapply(strsplit(DeliveryGroupID, '_'), "[[", 3), '%m/%d/%Y') >= '2016-05-01' &
           as.Date(sapply(strsplit(DeliveryGroupID, '_'), "[[", 3), '%m/%d/%Y') <= '2016-07-31')

dfTRAINFStoDel <- dfTRAINFStoDel %>%
  filter(as.Date(sapply(strsplit(DeliveryGroupID, '_'), "[[", 3), '%m/%d/%Y') >= '2016-05-01' &
           as.Date(sapply(strsplit(DeliveryGroupID, '_'), "[[", 3), '%m/%d/%Y') <= '2016-07-31')

dfTESTFStoDel <- dfTESTFStoDel %>%
  filter(as.Date(sapply(strsplit(DeliveryGroupID, '_'), "[[", 3), '%m/%d/%Y') >= '2016-05-01' &
           as.Date(sapply(strsplit(DeliveryGroupID, '_'), "[[", 3), '%m/%d/%Y') <= '2016-07-31')

################################################################################
################################ ROLLUP TO DG ##################################
################################################################################

# ============================================================================ #
## TRAINING DATA

dfTRAIN <- dfTRAIN %>% group_by(DeliveryGroupID) %>%
  summarise(MaxGroupList      = max(GroupList),
            MinGroupList      = min(GroupList),
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
            ALLMeanLT         = max(ALLMeanLT),
            SCGMeanLT         = max(SCGMeanLT),
            Target            = max(Target)
  ) %>% ungroup()

# ============================================================================ #
## TESTING DATA

dfTEST <- dfTEST %>% group_by(DeliveryGroupID) %>%
  summarise(MaxGroupList      = max(GroupList),
            MinGroupList      = min(GroupList),
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
            ALLMeanLT         = max(ALLMeanLT),
            SCGMeanLT         = max(SCGMeanLT),
            Target            = max(Target)
  ) %>% ungroup()

################################################################################
############################# EVALUATE THE MODEL ###############################
################################################################################

# ============================================================================ #
## TRAINING DATA
# ONLY LEAD TIME SEGMENTED MODELS ARE SELECTED
outMaxDG <- ModelEvaluate(predicted = dfTRAIN %>%
                       select(contains('SEG'),
                              QUOTED_EDT,
                              ATP,
                              ALLMeanLT,
                              SCGMeanLT
                       ),
                     dfTRAIN$Target,
                     dfTRAIN$MaxGroupList)

outMinDG <- ModelEvaluate(predicted = dfTRAIN %>%
                       select(contains('SEG'),
                              QUOTED_EDT,
                              ATP,
                              ALLMeanLT,
                              SCGMeanLT
                       ),
                     dfTRAIN$Target,
                     dfTRAIN$MinGroupList)

write.csv(outMaxDG$ACC, paste0(REGION, '_DG_ACC_TRAIN_MAX_SCG.csv'), row.names = F)
write.csv(outMinDG$ACC, paste0(REGION, '_DG_ACC_TRAIN_MIN_SCG.csv'), row.names = F)

# ============================================================================ #
## TESTING DATA
# ONLY LEAD TIME SEGMENTED MODELS ARE SELECTED
outMaxDG <- ModelEvaluate(predicted = dfTEST %>%
                       select(contains('SEG'),
                              QUOTED_EDT,
                              ATP,
                              ALLMeanLT,
                              SCGMeanLT
                       ),
                     dfTEST$Target,
                     dfTEST$MaxGroupList)

outMinDG <- ModelEvaluate(predicted = dfTEST %>%
                       select(contains('SEG'),
                              QUOTED_EDT,
                              ATP,
                              ALLMeanLT,
                              SCGMeanLT
                       ),
                     dfTEST$Target,
                     dfTEST$MinGroupList)

write.csv(outMaxDG$ACC, paste0(REGION, '_DG_ACC_TEST_MAX_SCG.csv'), row.names = F)
write.csv(outMinDG$ACC, paste0(REGION, '_DG_ACC_TEST_MIN_SCG.csv'), row.names = F)

outMaxDG$ACC
outMinDG$ACC

# ============================================================================ #
## Actual LT VS Fitted LT Plots

modelstoplot <- c('RF_BASE_SCG_UNSEG'
                  ,'RF_BASE_SCG_SEG'
                  ,'RF_ENG_ALL_UNSEG'
                  ,'RF_ENG_ALL_SEG'
                  ,'RF_ENG_SCG_UNSEG'
                  ,'RF_ENG_SCG_SEG')

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

################################################################################
####################### EVALUATE THE MODEL - RE TO FS ##########################
################################################################################

dfTRAINREtoFS <- dfTRAINREtoFS %>% group_by(DeliveryGroupID) %>%
  summarise(MaxGroupList      = max(GroupList),
            MinGroupList      = min(GroupList),
            RF_BASE_ALL_SEG   = max(RF_BASE_ALL_SEG),
            RF_BASE_SCG_SEG   = max(RF_BASE_SCG_SEG),
            RF_ENG_ALL_SEG    = max(RF_ENG_ALL_SEG),
            RF_ENG_SCG_SEG    = max(RF_ENG_SCG_SEG),
            SRT               = max(SRT),
            Target            = max(Target)
  ) %>% ungroup()

dfTESTREtoFS <- dfTESTREtoFS %>% group_by(DeliveryGroupID) %>%
  summarise(MaxGroupList      = max(GroupList),
            MinGroupList      = min(GroupList),
            RF_BASE_ALL_SEG   = max(RF_BASE_ALL_SEG),
            RF_BASE_SCG_SEG   = max(RF_BASE_SCG_SEG),
            RF_ENG_ALL_SEG    = max(RF_ENG_ALL_SEG),
            RF_ENG_SCG_SEG    = max(RF_ENG_SCG_SEG),
            SRT               = max(SRT),
            Target            = max(Target)
  ) %>% ungroup()

# ============================================================================ #
## TRAINING DATA
# ONLY LEAD TIME SEGMENTED MODELS ARE SELECTED
outMaxDG <- ModelEvaluate(predicted = dfTRAINREtoFS %>% select(contains('SEG'), SRT),
                          observed  = dfTRAINREtoFS$Target,
                          group     = dfTRAINREtoFS$MaxGroupList)

outMinDG <- ModelEvaluate(predicted = dfTRAINREtoFS %>% select(contains('SEG'), SRT),
                          observed  = dfTRAINREtoFS$Target,
                          group     = dfTRAINREtoFS$MinGroupList)

write.csv(outMaxDG$ACC, paste0(REGION, '_REtoFS_DG_ACC_TRAIN_MAX_SCG.csv'), row.names = F)
write.csv(outMinDG$ACC, paste0(REGION, '_REtoFS_DG_ACC_TRAIN_MIN_SCG.csv'), row.names = F)

# ============================================================================ #
## TESTING DATA
# ONLY LEAD TIME SEGMENTED MODELS ARE SELECTED
outMaxDG <- ModelEvaluate(predicted = dfTESTREtoFS %>% select(contains('SEG'), SRT),
                          observed  = dfTESTREtoFS$Target,
                          group     = dfTESTREtoFS$MaxGroupList)

outMinDG <- ModelEvaluate(predicted = dfTESTREtoFS %>% select(contains('SEG'), SRT),
                          observed  = dfTESTREtoFS$Target,
                          group     = dfTESTREtoFS$MinGroupList)

write.csv(outMaxDG$ACC, paste0(REGION, '_REtoFS_DG_ACC_TEST_MAX_SCG.csv'), row.names = F)
write.csv(outMinDG$ACC, paste0(REGION, '_REtoFS_DG_ACC_TEST_MIN_SCG.csv'), row.names = F)

################################################################################
###################### EVALUATE THE MODEL - FS TO DEL ##########################
################################################################################

dfTRAINFStoDel <- dfTRAINFStoDel %>% group_by(DeliveryGroupID) %>%
  summarise(MaxGroupList      = max(GroupList),
            MinGroupList      = min(GroupList),
            RF_BASE_ALL_SEG   = max(RF_BASE_ALL_SEG),
            RF_BASE_SCG_SEG   = max(RF_BASE_SCG_SEG),
            RF_ENG_ALL_SEG    = max(RF_ENG_ALL_SEG),
            RF_ENG_SCG_SEG    = max(RF_ENG_SCG_SEG),
            SRT_PADDING_VALUE = max(SRT_PADDING_VALUE),
            Target            = max(Target)
  ) %>% ungroup()

dfTESTFStoDel <- dfTESTFStoDel %>% group_by(DeliveryGroupID) %>%
  summarise(MaxGroupList      = max(GroupList),
            MinGroupList      = min(GroupList),
            RF_BASE_ALL_SEG   = max(RF_BASE_ALL_SEG),
            RF_BASE_SCG_SEG   = max(RF_BASE_SCG_SEG),
            RF_ENG_ALL_SEG    = max(RF_ENG_ALL_SEG),
            RF_ENG_SCG_SEG    = max(RF_ENG_SCG_SEG),
            SRT_PADDING_VALUE = max(SRT_PADDING_VALUE),
            Target            = max(Target)
  ) %>% ungroup()

# ============================================================================ #
## TRAINING DATA
# ONLY LEAD TIME SEGMENTED MODELS ARE SELECTED
outMaxDG <- ModelEvaluate(predicted = dfTRAINFStoDel %>% select(contains('SEG'), SRT_PADDING_VALUE),
                          observed  = dfTRAINFStoDel$Target,
                          group     = dfTRAINFStoDel$MaxGroupList)

outMinDG <- ModelEvaluate(predicted = dfTRAINFStoDel %>% select(contains('SEG'), SRT_PADDING_VALUE),
                          observed  = dfTRAINFStoDel$Target,
                          group     = dfTRAINFStoDel$MinGroupList)

write.csv(outMaxDG$ACC, paste0(REGION, '_FStoDel_DG_ACC_TRAIN_MAX_SCG.csv'), row.names = F)
write.csv(outMinDG$ACC, paste0(REGION, '_FStoDel_DG_ACC_TRAIN_MIN_SCG.csv'), row.names = F)

# ============================================================================ #
## TESTING DATA
# ONLY LEAD TIME SEGMENTED MODELS ARE SELECTED
outMaxDG <- ModelEvaluate(predicted = dfTESTFStoDel %>% select(contains('SEG'), SRT_PADDING_VALUE),
                          observed  = dfTESTFStoDel$Target,
                          group     = dfTESTFStoDel$MaxGroupList)

outMinDG <- ModelEvaluate(predicted = dfTESTFStoDel %>% select(contains('SEG'), SRT_PADDING_VALUE),
                          observed  = dfTESTFStoDel$Target,
                          group     = dfTESTFStoDel$MinGroupList)

write.csv(outMaxDG$ACC, paste0(REGION, '_FStoDel_DG_ACC_TEST_MAX_SCG.csv'), row.names = F)
write.csv(outMinDG$ACC, paste0(REGION, '_FStoDel_DG_ACC_TEST_MIN_SCG.csv'), row.names = F)
