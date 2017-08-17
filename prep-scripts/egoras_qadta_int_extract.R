## EXTRACT INTERSECTION OF EGORAS AND QADTA_WEEK38 DATASET IN DB
## Last Modified @ 2016-11-18 03:28:42  GMT ------------------------------

options( java.parameters = "-Xmx12g")
TRANS.DATA.DIR <- 'data/transformed/'

library(RJDBC)
library(dplyr)

# Connect to DB
drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver", "~/jdbc-driver/sqljdbc4.jar")
dbhandle <- dbConnect(drv, "jdbc:sqlserver://C9W27725.itcs.hpecorp.net;databaseName=cpx", 'sa', 'Sasa@1234')

################################################################################
################################# DATA IMPORT ##################################
################################################################################

## ALL QADTA DATA
sqlQADTA <- 'SELECT * FROM [dbo].[QADTA_Sep_Week_38_2016]'
QADTAData <- dbGetQuery(dbhandle, sqlQADTA)

## EGORAS RECORDS WITH DG EXISTING IN THE QADTA DATASET
sqlIntTemp <- "SELECT * FROM [dbo].[Region_HLI_CLEAN] aa WHERE EXISTS
(
  SELECT * FROM [cpx].[dbo].[QADTA_Sep_Week_38_2016] a
  INNER JOIN
  [dbo].[Region_HLI_CLEAN] b ON
  a.[Profit_Center_L0_Name] = b.[Profit Center L0 Name] AND
  a.[DG_RE_DE] = b.[DG_RE-DE]
  WHERE b.[ASAP Flag] = 'Y'
  AND b.[Profit Center L0 Name] = '%s'
  AND aa.[Sales Order ID] = b.[Sales Order ID]
  AND aa.[Sales Order Line Item ID] = b.[Sales Order Line Item ID]
)"

resAPJ <- dbGetQuery(dbhandle, sprintf(sqlIntTemp, 'Asia Pacific'))
resAMS <- dbGetQuery(dbhandle, sprintf(sqlIntTemp, 'Americas'))
resEMEA <- dbGetQuery(dbhandle, sprintf(sqlIntTemp, 'EMEA'))

# Disconnect from DB
dbDisconnect(dbhandle)

################################################################################
############################### DATA PROCESSING ################################
################################################################################

QADTAData <- QADTAData %>% distinct()

## Check relationship between Sales Order ID and Sales Quote ID
QADTAData %>% group_by(SALES_ORDER_ID) %>%
  summarise(c = n_distinct(`qv SLS_QTN_ID`)) %>% filter(c > 1)

SRTData <- QADTAData %>%
  arrange(Profit_Center_L0_Name, DG_RE_DE, SALES_ORDER_ID, desc(`qv SLS_QTN_VRSN_SQN_NR`)) %>%
  group_by(Profit_Center_L0_Name, DG_RE_DE, SALES_ORDER_ID) %>% slice(1) %>% ungroup() %>%
  select(`Profit Center L0 Name` = `Profit_Center_L0_Name`,
         `DG_RE-DE` = DG_RE_DE,
         `Sales Order ID` = SALES_ORDER_ID,
         SRT, SRT_PADDING_VALUE, QUOTED_EDT)

## Padding 0s for Sales Order ID in order to join with Egoras Data
## TODO: Currently assuming the length is 10 (However, AMS Egoras do have ID with length 12)
SRTData <- SRTData %>%
  mutate(`Sales Order ID` = sprintf('%010.0f', as.numeric(`Sales Order ID`)))

SRTData <- SRTData[complete.cases(SRTData), ]
#saveRDS(SRTData, 'QADTA_SRT.rds')

resAPJ %>% semi_join(SRTData) %>% nrow()
resAPJ <- resAPJ %>% inner_join(SRTData)
saveRDS(resAPJ, paste0(TRANS.DATA.DIR, 'APJ_QADTA_HLI.rds'))

resAMS %>% semi_join(SRTData) %>% nrow()
resAMS <- resAMS %>% inner_join(SRTData)
saveRDS(resAMS, paste0(TRANS.DATA.DIR, 'AMS_QADTA_HLI.rds'))

resEMEA %>% semi_join(SRTData) %>% nrow()
resEMEA <- resEMEA %>% inner_join(SRTData)
saveRDS(resEMEA, paste0(TRANS.DATA.DIR, 'EMEA_QADTA_HLI.rds'))
