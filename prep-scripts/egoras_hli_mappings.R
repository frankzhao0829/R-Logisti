library(dplyr)
APJMAP <- readRDS('data/transformed/APJ_SO_LN_ITM_HLI_MAP.rds')
APJMAP <- APJMAP %>% mutate(REGION = 'Asia Pacific') %>% select(REGION, SO_ID, SO_LN_ITM_ID, HLI_ID)
AMSMAP <- readRDS('data/transformed/AMS_SO_LN_ITM_HLI_MAP.rds')
AMSMAP <- AMSMAP %>% mutate(REGION = 'Americas') %>% select(REGION, SO_ID, SO_LN_ITM_ID, HLI_ID)
EMEAMAP <- readRDS('data/transformed/EMEA_SO_LN_ITM_HLI_MAP.rds')
EMEAMAP <- EMEAMAP %>% mutate(REGION = 'EMEA') %>% select(REGION, SO_ID, SO_LN_ITM_ID, HLI_ID)

MAP <- rbind(APJMAP, AMSMAP, EMEAMAP)
write.csv(MAP, 'Egoras_HLI_Mappings.csv', row.names = F, quote = FALSE)

# bcp cpx.dbo.Egoras_HLI_Mappings in "Egoras_HLI_Mappings.csv" -S C9W27725.itcs.hpecorp.net -t, -T -c -F2
