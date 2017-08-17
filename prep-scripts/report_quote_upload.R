library(data.table)
library(RODBC)
library(dplyr)

dbhandle <- odbcDriverConnect('driver={ODBC Driver 13 for SQL Server};server=ec4t00951;database=cpx;uid=SA;pwd=Sasa@1234')

################################################################################
################## READ AND INSERT INTO DB - Quote Headers #####################
################################################################################

hpe_quotes <- data.table::fread('HPE_Quote_headers.csv', sep = '|', quote = "\"")
sqlSave(dbhandle, hpe_quotes, tablename = 'HPE_Quote_Headers', append = TRUE, rownames = FALSE)

# Check SRT_PADDING_VALUE & ORIG_ASSET Relations
hpe_quotes  %>% group_by(ORIG_ASSET) %>% summarise(HAS_SRT_PV = all(SRT_PADDING_VALUE != ''))

################################################################################
################ READ AND INSERT INTO DB - Quote Item Details ##################
################################################################################

hpe_quote_items <- data.table::fread('HPE_Quote_item_detail.csv', sep = '|', quote = "\"", check.names = TRUE)

# There are two columns named qi.PA_EXPIRY_DT
# Check whether they are the same
sum(hpe_quote_items$qi.PA_EXPIRY_DT != hpe_quote_items$qi.PA_EXPIRY_DT.1)

hpe_quote_items <- hpe_quote_items %>% select(-qi.PA_EXPIRY_DT.1)

sqlSave(dbhandle, hpe_quote_items, tablename = 'HPE_Quote_Item_Detail', rownames = FALSE)

close(dbhandle)
