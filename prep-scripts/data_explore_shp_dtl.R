library(RODBC)
library(data.table)
#source('R/DeepDescribe.R')

#conn.str <- 'driver={SQL Server};server=C9W27725.itcs.hpecorp.net;database=cpx;trusted_connection=true'
#dbhandle <- odbcDriverConnect(conn.str)

#EMEA.SO.QUERY <- "SELECT * FROM [dbo].[Sales_Order_Configs_v] WHERE [Profit Center L0 Name] = 'EMEA'"
#raw.emea.data <- sqlQuery(dbhandle, EMEA.SO.QUERY)
#write.csv(raw.emea.data, 'emea_sales_order.csv', row.names = F)

################################################################################
############################## Helper Function #################################
################################################################################

DeepDescribe <- function(x, descript = as.character(sys.call())[2]) {

  # Load library
  library(Hmisc)

  x.descr <- describe(x, descript, exclude.missing = F)

  # Change frequency count from matrix to data frame
  for (i in seq_along(x.descr)) {
    if (is.matrix(x.descr[[i]]$values)) {
      x.descr[[i]]$freqCount <- TRUE
      x.descr[[i]]$values <- as.data.frame(x.descr[[i]]$values)
    } else if (is.character(x.descr[[i]]$values)) {
      # Handle incorrect encodings
      x.descr[[i]]$values <- iconv(x.descr[[i]]$values, to = 'utf-8')
    }
  }

  # Add frequancy count
  for(name in names(x)) {
    # Add Hi5 and Lo5 Freq Count
    if (length(unique(x[[name]])) > 20) {
      freq <- FreqCount(x[[name]])
      low.Freq <- head(freq, 5)
      high.Freq <- tail(freq, 5)
      res.freq <- rbind(low.Freq, high.Freq)
      x.descr[[name]]$freqCountHL <- as.data.frame(res.freq)
      # Encoding Problem handling
      x.descr[[name]]$freqCountName <- iconv(dimnames(res.freq)[[1]], to = "utf-8")
    }

    # Add pysch statistics to numeric variable
    if (is.numeric(x[[name]])) {
      res.psych.stats <- as.data.frame(psych::describe(x[[name]]))
      drops <- c('vars', 'n')  # drop vars & n, duplicated info
      res.psych.stats <- res.psych.stats[, !(names(res.psych.stats) %in% drops)]

      x.descr[[name]]$psychStats <- res.psych.stats
    }
  }

  res.list <- list(columns = x.descr,
                   descript = attr(x.descr, 'descript'),
                   dimensions = attr(x.descr, 'dimensions'))

  res.list
}

FreqCount <- function(x) {
  library(Hmisc)
  tab <- wtd.table(x, normwt=FALSE, na.rm=FALSE, type='table')

  pct <- round(100*tab/sum(tab))
  counts <- t(as.matrix(tab))
  counts <- rbind(counts, pct)
  dimnames(counts)[[1]]<- c("Frequency","pct")
  sorted.counts <- t(counts)[order(t(counts)[, 1]), ]

  sorted.counts
}

GenerateDescribeJSON <- function(x, description, out.folder = 'outputs/json/') {
 writeLines(rjson::toJSON(DeepDescribe(x, descript = description)), paste0(out.folder, description, '.json'))
}

################################################################################
######################## Generate Exploration Report ###########################
################################################################################

OUT.FOLDER <- 'outputs/json/'
f <- 'data/APJ_SO.csv'

# I use data.table 1.9.7 DEV version (installed using devtools from GitHub) to read data
# Choose your way of loading data
raw.data <- data.table::fread(f, sep = ',', quote = '', fill = T, integer64 = 'character')

GenerateDescribeJSON(raw.data, 'Shipment Details for APJ', OUT.FOLDER)

# A json file will be generated and we need to generate the HTML file using a node js tool I write
# https://github.hpe.com/DTrackerPOC/describe2html

################################################################################
######################### Lead Time Profile Explore ############################
################################################################################

RFDataSet %>% group_by(Receive = substr(HPREDate, 1, 7),
                       Delivery = substr(Delivery, 1, 7)) %>%
  summarise(Count = n(), WBefQ = mean(WksBefQtrEnd, na.rm = TRUE),
            RE_to_Del = mean(calcREtoDel, na.rm = TRUE))
