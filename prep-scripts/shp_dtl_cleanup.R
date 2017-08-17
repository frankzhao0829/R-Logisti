## SHIPMENT DETAILS CLEANUP AND ROLLUP TO HLI LEVEL
## Author: Chunmeng Zhang (chunmeng.zhang@hpe.com)
## Requires: shp_dtl_load.R to extract the RDS raw files from DB

# Last Modified:
# 2016-10-17 07:50:56 ------------------------------

library(dplyr)
library(countrycode)
library(doParallel)

# Register the parallel backend for %dopar%
registerDoParallel(cores = 4)

################################################################################
############################ SET REGION PARAMETER ##############################
################################################################################

REGION = 'EMEA'
DATAFILE <- paste0('data/transformed/', REGION, '_SHP_DTL.rds')

################################################################################
############################## Helper Functions ################################
################################################################################

FindRoot <- function(child, parent, group) {
  ## Label each row with their root HLI
  # child  - child item id vector
  # parent - parent item id vector
  # group  - sales order id vector
  # TODO:
  # Current version requires all the vectors to be SORTED by group
  # As we use foreach %dopar% to parallelize the computation

  library(foreach)

  #registerDoParallel(cores = 4)

  root <- character(length(child))
  glist <- unique(group)

  # Initial parent id assignment based on id numbers - DEPRECATED
  #assignParentID <- function(x) { sprintf("%06d", as.numeric(x) - 10) }

  root <- foreach(i = 1:length(glist), .combine = 'c') %dopar% {

    # index in the original vector
    idx <- which(group == glist[i])

    subChild <- child[idx]
    subParent <- parent[idx]

    ll <- length(subChild)

    ROOT <- unique(subChild[subParent == '000000'])

    ROOTMAP <- setNames(ROOT, ROOT)

    for (j in 1:ll) {
      if (subParent[j] != '000000') {
        if(!subParent[j] %in% ROOT) {
          if (j == 1) { # means the parent is missing
            ROOTMAP[subChild[j]] <- subParent[j]
          } else {
            ROOTMAP[subChild[j]] <- ROOTMAP[subChild[j - 1]]
          }
        } else {
          ROOTMAP[subChild[j]] <- ROOTMAP[subParent[j]]
        }
      }
    }
    ROOTMAP[subChild]
  }

  root
  #res[order(match(group, glist))] <- root
  #res
}

dup <- function(x) duplicated(x) | duplicated(x, fromLast = T)
pull <- function(x,y) {x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]}
contain <- function(x, v) { names(x)[sapply(x, function(y) if(is.na(v)) sum(is.na(y)) > 0 else v %in% y)] }

################################################################################
########################### LOAD DATA FROM RDS FILE ############################
################################################################################

# LOAD DATA
# THE RDS file is retrived by querying the Egoras_SHP_DTL with region set to REGION using R with RJDBC
shipDetails <- readRDS(DATAFILE)
nrow(shipDetails)
## EMEA: 773263
## AMS : 590344
## APJ : 569436

# Check Higher Level Item Flag and Parent Sales Order Item ID relationships
sum(shipDetails$`Higher Level Item Flag` == 'Y' & shipDetails$`Parent Sales Order Item ID` != '000000')
## EMEA: 4139
## AMS : 52141
## APJ : 0

sum(shipDetails$`Higher Level Item Flag` != 'Y' & shipDetails$`Parent Sales Order Item ID` == '000000')
## 0

sum(shipDetails$`Parent Sales Order Item ID` == shipDetails$`Sales Order Line Item ID`)
## EMEA: 4139
## AMS : 0
## APJ : 0

# Change those wrong Parent Sales Order Item ID by using Higher Level Item Flag
if (REGION == 'EMEA') {
  shipDetails[shipDetails$`Higher Level Item Flag` == 'Y', 'Parent Sales Order Item ID'] <- '000000'
}

################################################################################
###################### SOURCE SYSTEM 2027 VISTA WORKAROUND #####################
################################################################################
## FOR AMS ONLY

if (REGION == 'AMS') {

  NAParentSOSet <- shipDetails %>%
    filter(is.na(`OSS Higher Level Item`) & `Sales Order Source System Key` == '2027') %>%
    pull(`Sales Order ID`) %>% unique()
  ## 15 sales orders

  shipDetails <- shipDetails %>%
    filter(!`Sales Order ID` %in% NAParentSOSet)

  # Set Parent Sales Order Item ID by OSS Higher Level Item for 2027
  shipDetails <- shipDetails %>%
    mutate(`Parent Sales Order Item ID` =
             ifelse(`Sales Order Source System Key` == '2027',
                    `OSS Higher Level Item`,
                    `Parent Sales Order Item ID`))

  shipDetails <- shipDetails %>%
    mutate(`Parent Sales Order Item ID` = sprintf("%06d", as.numeric(`Parent Sales Order Item ID`)))

  #sort(unique(shipDetails$`Parent Sales Order Item ID`))

  sum(shipDetails$`Parent Sales Order Item ID` == shipDetails$`Sales Order Line Item ID`)
  ## 0
}

################################################################################
####################### MULTI-SHIPMENT LINE ITEMS CHECK ########################
################################################################################

# As the Egoras_SHP_DTL table's primary key is Shipment ID  + Shipment Line Item ID
# Dup Sales Order ID + Sales Order Line Item IDs are guranteed to be shipped in multi-shipment lines

dupSalesOrderItems <- shipDetails[dup(shipDetails[, c('Sales Order ID', 'Sales Order Line Item ID')]), ]
## EMEA: 1509  obs
## AMS : 14828 obs
## APJ : 43    obs

# =========================================================================== #
# Check multiple first planned delivery date for partial shipment items

## Note that na_rm = TRUE only works for dplyr-0.4.3
## For dplyr-0.5.0 and above use na.rm = TRUE instead
inconsLineItemsSet <- dupSalesOrderItems %>% filter(`Parent Sales Order Item ID` == '000000') %>%
  group_by(`Sales Order ID`, `Sales Order Line Item ID`) %>%
  summarise(c = n_distinct(`First Planned Delivery Date`, na.rm = TRUE)) %>%
  filter(c > 1) %>% ungroup() %>%
  select(`Sales Order ID`, `Sales Order Line Item ID`)

write.csv(shipDetails %>% semi_join(inconsLineItemsSet),
          paste0(REGION, '_MultiShip_MultiFPDD.csv'), row.names = FALSE)

# =========================================================================== #

## Some rows have NO VALUE in several fields (could be due to EDW processing, dummy rows)
# Check whether the duplication is caused by NO VALUE rows
dupSalesOrderItems %>%
  group_by(`Sales Order ID`, `Sales Order Line Item ID`) %>%
  summarise(c = sum(`Product Descr` != 'NO VALUE')) %>%
  filter(c == 1) %>% nrow()
## EMEA: 642
## AMS : 0
## APJ : 0

dupSalesOrderItems %>% filter(`Product Descr` == 'NO VALUE') %>% nrow()
## EMEA: 642 - Each NO VALUE row has one and only one real data row associated with it
## AMS : 0
## APJ : 0

temp <- dupSalesOrderItems %>% filter(`Product Descr` == 'NO VALUE')

filteredSalesOrderItems <- dupSalesOrderItems %>%
  semi_join(temp, by = c('Sales Order ID', 'Sales Order Line Item ID')) %>%
  filter(`Product Descr` != 'NO VALUE')

multiShipItems <- dupSalesOrderItems %>% setdiff(rbind(temp, filteredSalesOrderItems))
## EMEA: 225
## AMS : 14828
## APJ : 43

#write.csv(multiShipItems, paste0(REGION, '_Multi_Ship_Items.csv'), row.names = F)

multiShipItems %>%
  group_by(`Sales Order ID`, `Sales Order Line Item ID`) %>%
  summarise(c = n()) %>% nrow()
## EMEA: 105  line items
## AMS : 5955 line items
## APJ : 21   line items

################################################################################

#multiShipItems %>% select(-`Shipment ID`, -`Shipment Line Item ID`) %>% distinct() %>% nrow()
#
#a <- multiShipItems %>% filter(`Sales Order Source System Key` != '2027')
#
#idx <- NULL
#itemCount <- NULL
#
#for(i in seq(ncol(a))) {
#  v <- a %>%
#    select(`Sales Order ID`, `Sales Order Line Item ID`, i) %>%
#    .[complete.cases(.), ] %>%
#    distinct() %>% group_by(`Sales Order ID`, `Sales Order Line Item ID`) %>%
#    summarise(c = n()) %>% filter(c > 1) %>% nrow()
#  if (v != 0) {
#    idx <- c(idx, i)
#    itemCount <- c(itemCount, v)
#  }
#}
#
#diffCols <- cbind(names(multiShipItems)[idx], itemCount)
#
#sqSet <- multiShipItems %>% group_by(`Sales Order ID`, `Sales Order Line Item ID`) %>%
#  summarise(c = n_distinct(`Base Quantity`), d = n_distinct(Delivery)) %>% filter(c == 1 & d == 1) %>% ungroup() %>%
#  select(`Sales Order ID`, `Sales Order Line Item ID`)
#
#b <- multiShipItems %>% semi_join(sqSet)
#
#write.csv(b, 'AMS_Multi_Ship_Same_Quantity.csv', row.names = F)

################################################################################


idx <- NULL
itemCount <- NULL

for(i in seq(ncol(multiShipItems))) {
  v <- multiShipItems %>%
    select(`Sales Order ID`, `Sales Order Line Item ID`, i) %>%
    .[complete.cases(.), ] %>%
    distinct() %>% group_by(`Sales Order ID`, `Sales Order Line Item ID`) %>%
    summarise(c = n()) %>% filter(c > 1) %>% nrow()
  if (v != 0) {
    idx <- c(idx, i)
    itemCount <- c(itemCount, v)
  }
}

diffCols <- cbind(names(multiShipItems)[idx], itemCount)
## EMEA: 84  columns have inconsistent values in multiple shipments
## AMS : 172 columns have inconsistent values in mulitple shipments
## APJ : 64  columns have inconsistent values in mulitple shipments

names(multiShipItems)[-idx]

# TODO: REFINE THE LOGIC
# Arrange the multiple ship line items to select the latest gating item
multiShipItems <- multiShipItems %>% arrange(`Sales Order ID`,
                           `Sales Order Line Item ID`,
                           is.na(Delivery),
                           desc(Delivery),
                           desc(`Customer Ship`),
                           desc(`Hub Receive`),
                           desc(`Factory Ship`),
                           desc(`Production Done`))

# Aggregate the Base Quantity and USD Amount and select gating items
aggSalesOrderItems <- multiShipItems %>%
  group_by(`Sales Order ID`, `Sales Order Line Item ID`) %>%
  mutate(`Base Quantity` = sum(`Base Quantity`),
         `Secured Position Net USD Amount` = sum(`Secured Position Net USD Amount`)) %>%
  slice(1) %>% ungroup() %>% mutate(MultiShipFlag = 'Y')

#write.csv(aggSalesOrderItems, 'Aggregated_Items.csv', row.names = F)

shipDetails <- shipDetails[!dup(shipDetails[, c('Sales Order ID', 'Sales Order Line Item ID')]), ]

shipDetails <- rbind(shipDetails, filteredSalesOrderItems) %>%
  mutate(MultiShipFlag = 'N') %>%
  rbind(aggSalesOrderItems) %>%
  arrange(`Sales Order ID`, `Sales Order Line Item ID`)

## EMEA: 772501 obs
## AMS : 581224 obs
## APJ : 569414 obs

#===============================================================================

# TODO TODO
# - Remove Redundant records (Product Descr = NO VALUE)

shipDetails %>% contain('NO VALUE')
## EMEA: [1] "Sales Product Type Descr" "Product Descr"            "Product Family Descr"
## APJ : [1] "Sales Product Type Descr" "Product Descr"            "Product Family Descr"
## AMS : 0

shipDetails %>%
  select(`Sales Product Type Descr`,
         `Product Descr`,
         `Product Family Descr`,
         `Product Line ID`,
         `Product ID`,
         `Supplier Complexity Group`) %>%
  distinct() %>%
  filter(`Sales Product Type Descr` == 'NO VALUE' |
           `Product Descr` == 'NO VALUE' |
           `Product Family Descr` == 'NO VALUE')

shipDetails %>% filter(`Product Descr` == 'NO VALUE') %>%
  filter(is.na(Delivery)) %>%
  pull(`Product ID`) %>% unique()
## EMEA: HC790A/NSN-EDI
## APJ : DRFREIGHT/HP_ESD_INST_IG_SW

################################################################################
####################### CHECK SALES ORDERS WITH NO PARENT ######################
################################################################################

shipDetails <- shipDetails %>% group_by(`Sales Order ID`) %>%
  mutate(TEMPCOUNT = sum(`Parent Sales Order Item ID` == '000000')) %>%
  filter(TEMPCOUNT != 0) %>% ungroup() %>% select(-TEMPCOUNT)
## EMEA: 772292 rows
## AMS : 568233 rows
## APJ : 569273 rows

length(unique(shipDetails$`Sales Order ID`))
## EMEA: 86136 Sales Orders
## AMS : 63086 Sales Orders
## APJ : 73215 Sales Orders

################################################################################
########## ADD HLI_ID COLUMN FOR AGGREGATION AND GENERATE CONFIG TEXT ##########
################################################################################

sub <- shipDetails %>% select(SO_ID = `Sales Order ID`,
                              SO_LN_ITM_ID = `Sales Order Line Item ID`,
                              PARNT_SO_LN_ITM_ID = `Parent Sales Order Item ID`)

sub <- sub %>% arrange(SO_ID, SO_LN_ITM_ID, PARNT_SO_LN_ITM_ID)

# Check missing parents in each sales order

missingLineItems <- sub %>%
  group_by(SO_ID) %>%
  mutate(MissingLabel = PARNT_SO_LN_ITM_ID %in%
           setdiff(PARNT_SO_LN_ITM_ID, c(SO_LN_ITM_ID, '000000'))) %>%
  filter(MissingLabel)

nrow(missingLineItems)
## EMEA: 38252 missing line items
## AMS : 32831 missing line items
## APJ : 46079 missing line items

ptm <- proc.time()
root <- FindRoot(sub$SO_LN_ITM_ID, sub$PARNT_SO_LN_ITM_ID, sub$SO_ID)
proc.time() - ptm

## EMEA: Elapsed 757 seconds
## AMS : Elapsed 418 seconds
## APJ : Elapsed 483 seconds

sum(is.na(root))
## 0

salesOrderHLIMap <- cbind(sub[, c('SO_ID', 'SO_LN_ITM_ID')], HLI_ID = root)

saveRDS(salesOrderHLIMap, paste0(REGION, '_SO_LN_ITM_HLI_MAP.rds'))
salesOrderHLIMap <- readRDS(paste0(REGION, '_SO_LN_ITM_HLI_MAP.rds'))

compFIMap <- salesOrderHLIMap %>%
  inner_join(missingLineItems, by = c('SO_ID', 'SO_LN_ITM_ID')) %>%
  select(`Sales Order ID` = SO_ID, `Sales Order Line Item ID` = PARNT_SO_LN_ITM_ID, HLI_ID) %>%
  distinct() %>% group_by(`Sales Order ID`, HLI_ID) %>%
  mutate(`Product ID` = paste0('ComponentFI_', seq_along(`Sales Order Line Item ID`)))

nrow(compFIMap)
## EMEA: 13577
## AMS : 13079
## APJ : 20521

shipDetails <- shipDetails %>%
  left_join(salesOrderHLIMap, c('Sales Order ID' = 'SO_ID', 'Sales Order Line Item ID' = 'SO_LN_ITM_ID'))

configText <- shipDetails %>% select(`Sales Order ID`, `Sales Order Line Item ID`, HLI_ID, `Product ID`) %>%
  union(compFIMap) %>% arrange(`Sales Order ID`, `Sales Order Line Item ID`) %>%
  group_by(`Sales Order ID`, HLI_ID) %>%
  summarise(ITM_COUNT = n(),
            ConfigText = paste(`Product ID`, collapse = ' ')) %>%
  ungroup()

nrow(configText)
## EMEA: 305780 obs
## AMS : 188363 obs
## APJ : 173140 obs

################################################################################
############### Sales Order Header Level Aggregation Analysis ##################
################################################################################

# Sales order header levels with multiple complexity groups
# Ignore NA Complexity Group

#multiCxGrpSet <- shipDetails %>%
#  group_by(`Sales Order ID`, HLI_ID) %>%
#  summarise(c = n_distinct(`Complexity Groups`, na.rm = TRUE)) %>%
#  filter(c > 1) %>% pull(`Sales Order ID`) %>% unique()
#
#length(multiCxGrpSet)
## 292 Sales Orders

#shipDetails <- shipDetails %>% filter(!`Sales Order ID` %in% multiCxGrpSet)

#multiASAPSOSet <- shipDetails %>% group_by(`Sales Order ID`, HLI_ID) %>%
#  summarise(c = n_distinct(`ASAP Flag`)) %>%
#  filter(c > 1) %>% pull(`Sales Order ID`) %>% unique()
#
#length(multiASAPSOSet)
## 38 Sales Orders

#cleanShipDetails <- shipDetails %>% filter(!`Sales Order ID` %in% multiASAPSOSet)

#nrow(cleanShipDetails)
## 756990

#cleanShipDetails %>% group_by(`Sales Order ID`) %>%
#  summarise(c = n_distinct(`Shipment to ISO Country Code`)) %>%
#  filter(c > 1) %>% nrow()
## 0 rows, Country Code is in Header level

#saveRDS(cleanShipDetails, paste0(REGION, '_SHP_DTL_CLEAN.rds'))

################################################################################
######################### GENERATE HIGHER LEVEL DATA ###########################
################################################################################

# Generate High Level Data for Modeling
higherLevelData <- shipDetails %>% filter(`Parent Sales Order Item ID` == '000000')

higherLevelData <- higherLevelData %>%
  filter(!(is.na(Delivery) | is.na(`First Planned Delivery Date`) | `First Planned Delivery Date` < `HP Receive Date`))
nrow(higherLevelData)
## EMEA: 264860 rows
## AMS : 155862 rows
## APJ : 133138 rows

higherLevelData <- higherLevelData %>% left_join(configText)

higherLevelData %>% filter(is.na(ConfigText)) %>% nrow()
## 0

length(unique(higherLevelData$`Sales Order ID`))
## EMEA: 79241 sales orders
## AMS : 56421 sales orders
## APJ : 59141 sales orders

higherLevelData %>% filter(`Product Descr` == 'NO VALUE') %>% nrow()
## EMEA: only 1 row, so remove it
## AMS : 0
## APJ : 0

## For EMEA ONLY
higherLevelData <- higherLevelData %>% filter(`Product Descr` != 'NO VALUE')

higherLevelData %>% group_by(`Complexity Groups`) %>% summarise(HLICount = n())

## EMEA
## Complexity Groups HLICount
##               BTO    19282
##       Complex CTO     4170
##   Configured Rack     4223
##               CTO    50409
##        HPN Option    64576
##        PPS Option   120166
##     Rack Solution     2033

## AMS
## Complexity Groups HLICount
##               BTO    13072
##       Complex CTO    11916
##   Configured Rack     4319
##               CTO    32707
##        HPN Option    23131
##        PPS Option    66765
##     Rack Solution     3952

## APJ
## Complexity Groups HLICount
##               BTO    19414
##       Complex CTO     3901
##   Configured Rack     1430
##               CTO    39103
##        HPN Option    19209
##        PPS Option    49329
##     Rack Solution      752

higherLevelData %>% filter(MultiShipFlag == 'Y') %>% nrow()
## EMEA: 27
## AMS : 3467
## APJ : 16

################################################################################
####################### ADD COUNTRY REGION INFORMATION #########################
################################################################################

if (REGION == 'EMEA') {
  top20CtryCode <- names(tail(sort(table(higherLevelData$`Shipment to ISO Country Code`)), 20))

  higherLevelData <- higherLevelData %>%
    mutate(Ctry_Rgn = countrycode(`Shipment to ISO Country Code`, 'iso2c', 'region'))

  higherLevelData[higherLevelData$`Shipment to ISO Country Code` == 'CS', 'Ctry_Rgn'] <- 'Southern Europe'
  higherLevelData[higherLevelData$`Shipment to ISO Country Code` == 'TW', 'Ctry_Rgn'] <- 'Eastern Asia'

  higherLevelData <- higherLevelData %>%
    mutate(Ctry_Rgn = ifelse(`Shipment to ISO Country Code` %in% top20CtryCode,
                             `Shipment to ISO Country Code`,
                             Ctry_Rgn))

  higherLevelData %>% select(`Shipment to ISO Country Code`, Ctry_Rgn) %>% distinct()
} else {
  higherLevelData <- higherLevelData %>% mutate(Ctry_Rgn = `Shipment to ISO Country Code`)
}

################################################################################
########################## LOAD CLEAN HLI DATA TO DB ###########################
################################################################################

saveRDS(higherLevelData, paste0(REGION, '_HLI_CLEAN.rds'))

## SAVE TO TSV SO THAT WE CAN UPLOAD TO DB THROUGH BULK INSERT
write.table(higherLevelData, paste0(REGION, '_HLI_CLEAN.txt'), quote = FALSE, sep = '\t',
            eol = "\r\n", na = "", row.names = FALSE)

################################################################################
######################## DATA EXPLORATION EXPERIMENTS ##########################
################################################################################
## FOR EMEA

probSet1 <- shipDetails %>% filter(`Sales Order ID` %in% c('7218001907', '7218291446', '7218359711'))
write.csv(probSet1, 'Prob_Set_1.csv', row.names = F)
