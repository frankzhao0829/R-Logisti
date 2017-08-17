library(RODBC)
library(dplyr)

dbhandle <- odbcDriverConnect('driver={SQL Server};server=ec4t00951.itcs.entsvcs.net;database=cpx;uid=SA;pwd=Sasa@1234')

PL_GBU_MAP <- read.csv('PL_BU_CHANGE_FY17.csv', stringsAsFactors = FALSE)

sqlSave(dbhandle, PL_GBU_MAP, tablename = 'PL_GBU_CHANGE_FY17', rownames = FALSE)

TEMP <- PL_GBU_MAP %>% filter(REGION == 'EMEA') %>% select(PL, GBU = NEWBU)
saveRDS(TEMP, 'data/transformed/EMEA_PL_GBU_MAP.rds')

TEMP <- PL_GBU_MAP %>% filter(REGION == 'Americas') %>% select(PL, GBU = NEWBU)
saveRDS(TEMP, 'data/transformed/AMS_PL_GBU_MAP.rds')

TEMP <- PL_GBU_MAP %>% filter(REGION == 'Asia Pacific') %>% select(PL, GBU = NEWBU)
saveRDS(TEMP, 'data/transformed/APJ_PL_GBU_MAP.rds')
