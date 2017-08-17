## Get count of week numbers based on Month/Quarter Begin/End date

GetWksBefMonEnd <- function(x) {
  # Get a date in next month
  nextMonth <- as.Date(strftime(x, format = '%Y-%m-02')) + 30
  lastDay <- as.Date(strftime(nextMonth, format="%Y-%m-01")) - 1

  diffDays <- lastDay - as.Date(x) + 1

  return(as.integer(ceiling(diffDays / 7)))
}

GetWksAftMonBeg <- function(x) {
  firstDay <- as.Date(strftime(x, format="%Y-%m-01"))

  diffDays <- as.Date(x) - firstDay + 1

  return(as.integer(ceiling(diffDays / 7)))
}

GetWksBefQtrEnd <- function(x) {
  NEXTQTRMAP <- c(2, 5, 5, 5, 8, 8, 8, 11, 11, 11, 2, 2)

  nextQtrBegMon <- sprintf('%02d', NEXTQTRMAP[as.integer(strftime(x, format="%m"))])
  yr <- as.integer(strftime(x, format="%Y"))
  qtrYr <- ifelse(strftime(x, format="%m") %in% c('11', '12'), yr + 1, yr)
  qtrEndDay <- as.Date(paste(qtrYr, nextQtrBegMon, '01', sep = '-')) - 1

  diffDays <- qtrEndDay - as.Date(x) + 1

  return(as.integer(ceiling(diffDays / 7)))
}

GetWksAftQtrBeg <- function(x) {
  QTRMAP <- c(11, 2, 2, 2, 5, 5, 5, 8, 8, 8, 11, 11)

  qtrBegMon <- sprintf('%02d', QTRMAP[as.integer(strftime(x, format="%m"))])
  yr <- as.integer(strftime(x, format="%Y"))
  qtrYr <- ifelse(strftime(x, format="%m") == '01', yr - 1, yr)
  qtrBegDay <- as.Date(paste(qtrYr, qtrBegMon, '01', sep = '-'))

  diffDays <- as.Date(x) - qtrBegDay + 1

  return(as.integer(ceiling(diffDays / 7)))
}
