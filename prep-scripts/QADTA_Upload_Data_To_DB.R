library(data.table)
library(RODBC)
library(dplyr)

dbhandle <- odbcDriverConnect('driver={ODBC Driver 13 for SQL Server};server=ec4t00951;database=cpx;uid=SA;pwd=Sasa@1234')

QADTA_Jan <- data.table::fread('QADTA_Jan_Week_15_2017.txt', sep = '|', quote = "\"", na.strings = c('', 'NA', 'NULL', '#VALUE!'))

a <- QADTA_Jan %>% mutate(`Difference (Z-S)` = as.integer(`Difference (Z-S)`),
                          Diff_RE_TO_DEL_QUOTED_EDT = as.integer(Diff_RE_TO_DEL_QUOTED_EDT),
                          Diff_First_Planned_Del_DT_HP_Receive_DT = as.integer(Diff_First_Planned_Del_DT_HP_Receive_DT))

a[, sapply(a, is.numeric)] <- sapply(a[, sapply(a, is.numeric)], as.integer)

a <- a %>%
  mutate(DELIVERY = as.POSIXct(strptime(DELIVERY, '%m/%d/%Y')),
         First_Planned_Delivery_Date = as.POSIXct(strptime(First_Planned_Delivery_Date, '%m/%d/%Y')),
         Customer_Requested_Delivery_Date = as.POSIXct(strptime(Customer_Requested_Delivery_Date, '%m/%d/%Y')))

varTypes = c(DELIVERY = "datetime",
             First_Planned_Delivery_Date = 'datetime',
             Customer_Requested_Delivery_Date  = 'datetime')

sqlSave(dbhandle, a, tablename = 'QADTA_Jan_Week_15_2017', append = TRUE, rownames = FALSE, fast = FALSE, varTypes = varTypes)
close(dbhandle)
