################################################################################
################################# Data Import ##################################
################################################################################

options( java.parameters = "-Xmx10g")
TRANS.DATA.DIR <- 'data/transformed/'

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

################################################################################
############################ EMEA ALL SHP DTL DATA #############################
################################################################################

queryEMEASHPDTL <- "SELECT * FROM Egoras_SHP_DTL WHERE [Profit Center L0 Name] = 'EMEA';"

resEMEA <- dbGetQuery(dbhandle, queryEMEASHPDTL)

saveRDS(resEMEA, paste0(TRANS.DATA.DIR, 'EMEA_SHP_DTL.rds'))

################################################################################
############################# AMS ALL SHP DTL DATA #############################
################################################################################

queryAMSSHPDTL <- "SELECT * FROM Egoras_SHP_DTL WHERE [Profit Center L0 Name] = 'Americas';"

resAMS <- dbGetQuery(dbhandle, queryAMSSHPDTL)

saveRDS(resAMS, paste0(TRANS.DATA.DIR, 'AMS_SHP_DTL.rds'))

################################################################################
############################# AMS ALL SHP DTL DATA #############################
################################################################################

queryAPJSHPDTL <- "SELECT * FROM Egoras_SHP_DTL WHERE [Profit Center L0 Name] = 'Asia Pacific';"

resAPJ <- dbGetQuery(dbhandle, queryAPJSHPDTL)

saveRDS(resAPJ, paste0(TRANS.DATA.DIR, 'APJ_SHP_DTL.rds'))

# Disconnect from DB
dbDisconnect(dbhandle)
