library(data.table)
library(RODBC)
library(dplyr)

dbhandle <- odbcDriverConnect('driver={ODBC Driver 13 for SQL Server};server=ec4t00951;database=cpx;uid=SA;pwd=Sasa@1234')

sku_lead_time <- data.table::fread('sku_lead_time_20170320.csv', na.strings = c(''))

sku_lead_time <- sku_lead_time %>%
  mutate(Region = case_when(.$Region == 'EU' ~ 'EUROPE',
                            .$Region == 'NA' ~ 'NORTHAMERICA',
                            TRUE ~ NA_character_))

varTypes <- c(UpdatedDT = "datetime")

sqlSave(dbhandle, sku_lead_time, tablename = 'SKU_Lead_Time_20170320', append = TRUE, rownames = FALSE, fast = FALSE, varTypes = varTypes)
close(dbhandle)
