###########################################################################################
## Author @ Logan (tfan@hpe.com)
## Last Modified @ 2016-11-15 03:40:50  GMT ------------------------------
# This function selects stratified sample from a dataframe.
# df is the data frame which will be sampled,
# strata is an array which contains categorical cols which specifies the stratification
# index is the key which can uniquely identify each row
# trainrate define % of trainset in total dataset
# apply the function on a dataframe will return a list -- splitresult
# splitresult$unique_row -- unique rows which can’t be stratified.
# splitresult$train -- rows should be in train set
# splitresult$test  -- rows should be in test set, splitresult$$unique_row are included
###########################################################################################

StrataSample <- function(df, strata, index = 'SO_HLI', trainrate = 0.7) {
  df$Strata <- apply(df[, strata], 1, paste, collapse = '|')
  dflist <- split(df, df$Strata)
  trainset <- list()
  splitresult <- list()
  unique_row <- c()
  dftrain <- data.frame()
  for (i in 1:length(dflist)) {
    rownum <- nrow(dflist[[i]])
    if(rownum > 1) {
      train <- sample(nrow(dflist[[i]]), trainrate*nrow(dflist[[i]]))
      trainset[[i]] <- dflist[[i]][train, index]
      dftrain <- rbind(dftrain, trainset[[i]])
    } else {
      unique_row <- c(unique_row, dflist[[i]][[index]])
    }
  }
  dftrain$Trainflag <- 1
  df <- df %>% left_join(dftrain)
  splitresult[['train']] <-c(1:nrow(df))[!is.na(df$Trainflag)]
  splitresult[['test']] <- c(1:nrow(df))[is.na(df$Trainflag)]
  splitresult[['unique_row']] <- c(1:nrow(df))[df[[index]] %in% unique_row]
  return(splitresult)
}
