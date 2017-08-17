GenHierCatGroupFeat <- function(data, group, f = rep(TRUE, nrow(data)), y, FUN, ...) {

  library(dplyr)

  FUN2 <- function(x) FUN(x, ...)

  X <- data[, c(group, y)]

  X$RESCOL <- FUN2(X[[y]])

  for (i in seq_along(group)) {

    g <- group[seq_len(i)]

    ## Require dplyr 0.5.0
    #temp <- X[f, ] %>% group_by_(.dots = g) %>% summarise_at(y, funs(TEMP = FUN2)) %>% ungroup()

    temp <- aggregate(x = X[[y]][f], by = lapply(subset(X[f, ], select = g), '['), FUN2)
    names(temp)[names(temp) == 'x'] <- 'TEMP'

    X <- X %>% left_join(temp, by = g) %>% mutate(RESCOL = ifelse(is.na(TEMP), RESCOL, TEMP)) %>% select(-TEMP)
  }

  return(X$RESCOL)
}

## TEST CASES
#tst <- data.frame(a = rep(c('1', '2'), 10), b = rep(c('1', '2', '3', '4'), 5), x = 1:20, y = c(1:15, rep(NA, 5)))
#GenHierCatGroupFeat(tst, group = c('b'), y = 'x', FUN = sum)
#GenHierCatGroupFeat(tst, group = c('a'), f = 1:10, y = 'x', FUN = sum)
#GenHierCatGroupFeat(tst, group = c('a', 'b'), y = 'x', FUN = sum)
#GenHierCatGroupFeat(tst, group = c('a', 'b'), f = 1:10, y = 'x', FUN = sum)
#GenHierCatGroupFeat(tst, group = c('a', 'b'), f = 1:18, y = 'y', FUN = sum)
#GenHierCatGroupFeat(tst, group = c('a', 'b'), f = 1:18, y = 'y', FUN = sum, na.rm = TRUE)
