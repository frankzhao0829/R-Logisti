################################################################################
########### Generate mappings from Plant Code/SRPC to ShippingPoint ############
################################################################################

# Process script to generate the shipping point mapping based on Jozo's csv file
# See table EMEAShippingPointMappings in cpx DB
# Add missing combinations of Plant Code + Shipping Receiving Point Code

EMEASPMappings <- read.csv('data/transformed/EMEAShippingPointMappings.csv', stringsAsFactors = F)

EMEASPMappings <- EMEASPMappings %>% select(-Plant.Receiving.Point)

names(EMEASPMappings) <- c('PlantCode', 'ShipRePointCode', 'ShippingPoint')

AddtionalMappings <- data.frame(
  PlantCode       = c('0125', '0176', '0126', '0143', '0131', '0131', '0145', '0155', '0155', '0146', '0198', '0153', '0157', '0154', 'G100'),
  ShipRePointCode = c('5200', '5200', '5200', '32FN', '5200', '32FN', '5200', '32FN', '5200', '32FN', '5200', '5200', '0154', '32FN', 'G142')
)

AddtionalMappings <- AddtionalMappings %>% mutate(ShippingPoint = ifelse(substr(PlantCode, 1, 1) == '0', 'NonEMEASupplier', 'OEM-CZ'))

EMEASPMappings <- rbind(EMEASPMappings, AddtionalMappings)

saveRDS(EMEASPMappings, 'data/transformed/EMEASPMappings.rds')

################################################################################
############################# SAVE MAPPING TO DB ###############################
################################################################################

library(RJDBC)
library(rjson)

# DB Credential is saved in a json file for safety reason
# Load the credential info
cred <- fromJSON(file = 'db_credential.json')

# Connect to DB
# Linux Platform
#drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver", "~/jdbc-driver/sqljdbc4.jar")

# Windows Platform
drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver", "C:\\Program Files\\Microsoft JDBC Driver 6.0 for SQL Server\\sqljdbc_6.0\\enu\\sqljdbc4.jar")

dbhandle <- dbConnect(drv, "jdbc:sqlserver://ec4t00951.itcs.entsvcs.net;databaseName=cpx", cred$username, cred$password)
