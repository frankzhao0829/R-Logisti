################################################################################
## Functions related to random forest model build/predict/evaluate and miscs
## Author        : Chunmeng Zhang (chunmeng.zhang@hpe.com)
## Last Modified @ 2016-11-18 07:14:47  GMT ------------------------------
## Including     :
## - RFTrain       -> Train using randomForest package
## - RGTrain       -> Train using ranger package
## - RQTrain       -> Train using quantregForest package
## - ModelBuild    -> Build the random forest model based on option RFPKG
## - ModelPredict  -> Make predictions using models built with RFPredict
## - ModelEvaluate -> Evaluate the model using FcstComp.R
## - VarImp        -> Plot/Save the variable importance plots
## - RECPlot       -> Plot/Save the REC curves

################################################################################

VALIDPKG <- c('ranger', 'randomForest', 'quantregForest', 'partykit')

RGTrain <- function(data, target) {
  ## Train model using ranger package

  print('Train using ranger package...')
  library(ranger)
  rf <- ranger::ranger(as.formula(paste0(target, ' ~ .')),
                       num.trees = 500,
                       mtry = floor(ncol(data) / 3),
                       data = data,
                       respect.unordered.factors = TRUE,
                       importance = 'permutation',
                       write.forest = TRUE,
                       seed = 42)
  return(rf)

}

RFTrain <- function(data, target, ntree = 125, ncore = 4) {
  ## Train model using randomForest package (with parallel options)

  print('Train using randomForest with doParallel packages...')
  library(doParallel)
  library(randomForest)

  cl <- makeCluster(ncore)
  registerDoParallel(cl)
  on.exit(stopCluster(cl))

  ptm <- proc.time()

  rf <- foreach(n = rep(ntree, ncore), .combine = randomForest::combine, .multicombine=TRUE, .packages = 'randomForest') %dopar% {
    ## Did not use formula for speed reason
    randomForest(x = data[, -which(target == names(data))],
                 y = data[[target]],
                 ntree = n,
                 #mtry = 9,
                 #na.action = randomForest::na.roughfix,
                 replace = FALSE,
                 importance =TRUE)
  }

  print(proc.time() - ptm)
  return(rf)
}

RQTrain <- function(data, target) {
  ## Train model using quantregForest package

  print('Train using quantregForest package...')
  library(quantregForest)

  rf <- quantregForest(
    x = data[, -which(target == names(data))],
    y = data[[target]],
    nthreads = 4,
    nodesize = 10,
    importance = TRUE)

  return(rf)

}

PKTrain <- function(data, target) {
  print('Train using partykit & rpart package...')
  library(rpart)
  library(partykit)

  fit <- rpart(as.formula(paste0(target, ' ~ .')),
               data,
               method = 'anova')

  fit <- as.party(fit)

  return(fit)
}

## WRAPPER FUNCTION FOR BUILDING MODELS WITH/WITHOUT SEGMENTATION
ModelBuild <- function(RFPKG, data, input, targetList, group) {

  if (!RFPKG %in% VALIDPKG) {
    stop(paste0('ERROR! NO support for ', RFPKG, '!'))
  }

  if (nrow(data) != length(group)) {
    stop('group length should be same as the data rows!')
  }

  library(doParallel)
  library(foreach)

  dataList <- split(data, group)

  # Get the values of group
  print(paste0(' -- Split into ',
               length(dataList),
               ' groups: ',
               paste0(names(dataList), collapse = ' ')))

  print(paste0(' -- Target List: ',
               paste0(targetList, collapse = ' ')))

  lapply(dataList, function(x) {
    foreach(target = targetList, .final = function(x) setNames(x, targetList)) %do% {
      if (RFPKG == 'ranger') {                    # BLOCK FOR RANGER

        if (is.list(input)) {
          RGTrain(x[, c(unlist(input[target], use.names = F), target)], target)
        } else {
          RGTrain(x[, c(input, target)], target)
        }

      } else if (RFPKG == 'randomForest') {       # BLOCK FOR RANDOMFOREST

        if (is.list(input)) {
          RFTrain(x[, c(unlist(input[target], use.names = F), target)], target)
        } else {
          RFTrain(x[, c(input, target)], target)
        }

      } else if (RFPKG == 'quantregForest') {     # BLOCK FOR quantregForest

        if (is.list(input)) {
          RQTrain(x[, c(unlist(input[target], use.names = F), target)], target)
        } else {
          RQTrain(x[, c(input, target)], target)
        }
      } else if (RFPKG == 'partykit') {           # BLOCK FOR partykit
        if (is.list(input)) {
          PKTrain(x[, c(unlist(input[target], use.names = F), target)], target)
        } else {
          PKTrain(x[, c(input, target)], target)
        }
      } else if (RFPKG == 'YOUR METHOD HERE!!') { # BLOCK FOR MORE ...
        # ADD YOUR MODEL BUILD METHOD HERE!!

      }
    }
  })
}

ModelPredict <- function(RFPKG, modelList, data, targetList, group, quantile = 0.5) {

  if (!RFPKG %in% VALIDPKG) {
    stop(paste0('ERROR! NO support for ', RFPKG, '!'))
  }

  if (nrow(data) != length(group)) {
    stop('group length should be same as the data rows!')
  }

  glist <- unique(group)

  pr <- data.frame(matrix(vector(), nrow(data), length(targetList),
                          dimnames = list(c(), targetList)),
                   stringsAsFactors = F)

  for(i in seq_along(glist)) {
    idx <- which(group == glist[i])
    pr[idx, ] <- foreach(target = targetList, .combine = 'cbind') %do% {
      if (RFPKG == 'ranger') {

        res <- predict(modelList[[glist[i]]][[target]], data = data[idx, ])$predictions

      } else if (RFPKG == 'randomForest') {

        res <- predict(modelList[[glist[i]]][[target]], newdata = data[idx, ])

      } else if (RFPKG == 'quantregForest') {

        print(paste0('Q = ', quantile, ' Group = ', glist[i], ' Target = ', target))
        res <- predict(modelList[[glist[i]]][[target]], newdata = data[idx, ], quantile)

      } else if (RFPKG == 'partykit') {
        print(paste0('Q = ', quantile, ' Group = ', glist[i], ' Target = ', target))
        res <- predict(modelList[[glist[i]]][[target]], newdata = data[idx, ],
                       FUN = function(y, w = 1) quantile(y, probs = quantile))
      }
      return(res)
    }
  }

  pr
}

ModelEvaluate <- function(predicted, observed, group = NULL, QTarget = 0.8) {
  library(foreach)

  if (nrow(predicted) != length(observed)) {
    stop('predicted values should have same length as observed!')
  }

  out <- list()
  out$overall <- FcstComp(predicted, observed, QTarget)

  if(!is.null(group)) {
    if (nrow(predicted) != length(group)) {
      stop('group length should be same as predicted!')
    }
    for(g in unique(group)) {
      idx <- which(g == group)
      out[[g]] <- FcstComp(predicted[idx, ], observed[idx], QTarget)
    }
  }

  temp1 <- lapply(out, function(x) x$ACC)
  temp2 <- lapply(out, function(x) x$REC)

  acc <- foreach(nn = names(temp1), .combine = 'rbind') %do% {
    cbind(Label = nn, temp1[[nn]])
  }

  rec <- foreach(nn = names(temp1), .combine = 'rbind') %do% {
    cbind(Label = nn, temp2[[nn]])
  }

  res <- list(ACC = acc, REC = rec)
}

VarImp <- function(RFPKG, rfList, filename) {
  ## Draw Variable Importance Plot for a list of RF models
  ## Assuming all rf models have the same list of inputs
  if (!RFPKG %in% VALIDPKG) {
    stop(paste0('ERROR! NO support for ', RFPKG, '!'))
  }

  if(missing(RFPKG)) {
    stop('Error, RANDOM FOREST PKG is not specified!')
  }

  nc <- length(rfList)

  if(nc == 0) {
    stop('Error, No models in the list!')
  }

  library(ggplot2)

  if (RFPKG == 'ranger') {

    tt1 <- lapply(rfList, function(x) {
      varImp <- ranger::importance(x)

      dfImp <- as.data.frame(varImp)
      dfImp$var <- row.names(dfImp)

      dfImp
    })

    nr <- length(tt1[[1]]$var)

    first <- tt1[[1]]

    g <- ggplot(first, aes(x = reorder(var, varImp), y = varImp)) +
      geom_bar(stat='identity', position = 'dodge') + coord_flip() +
      ylab("relative importance (%)") + theme(legend.position="none",
                                              text=element_text(size=13),
                                              axis.title.y=element_blank())

    rs <- lapply(tt1[-1], function(x, p) { p %+% x }, p = g)

    rs <- c(list(g), rs)

    names(rs) <- names(tt1)

    for(nn in names(rs)) {
      rs[[nn]] <- rs[[nn]] + ggtitle(nn)
    }

    png(filename = filename, type = 'cairo',
        width = 1200 * nc,
        height = 1500 + 40 * nr,
        res = 250)

    do.call(gridExtra::grid.arrange, c(rs, list(nrow = 1, ncol = nc)))

    dev.off()

  } else if ((RFPKG == 'randomForest') | (RFPKG == 'quantregForest')) {

    nr <- nrow(randomForest::importance(rfList[[1]]))

    png(filename = filename, type = 'cairo',
        width = 1000 * nc,
        height = 1000 + 40 * nr,
        res = 250)

    par(mfrow = c(1, nc))

    lapply(names(rfList), function(x, l) {
      try(randomForest::varImpPlot(l[[x]], type = 1, main = x), silent = TRUE)
    }, l = rfList)

    dev.off()

  } else {
    stop(paste0('ERROR! NO support for ', RFPKG, '!'))
  }
}

RECPlot <- function(REC, nrow, ncol, filename = NULL) {
  RECList <- split(REC, f = REC$Label)

  first <- RECList[[1]]

  p <- ggplot(first, aes(x = tolerance, y = accuracy, group = model, colour = model)) +
    geom_line(size = 0.75) + xlim(0, 20)

  rs <- lapply(RECList[-1], function(x, p) { p %+% x }, p = p)

  rs <- c(list(p), rs)

  names(rs) <- names(RECList)

  for(nn in names(rs)) {
    rs[[nn]] <- rs[[nn]] + ggtitle(paste0(nn, ' REC'))
  }


  if (!is.null(filename)) {

    png(filename = filename, type = 'cairo', width = 6000, height = 4000, res = 300)
    do.call(gridExtra::grid.arrange, c(rs, list(nrow = nrow, ncol = ncol)))
    dev.off()

    print(paste0('REC Plot save to ', filename, ' successfully!'))

  } else {
    do.call(gridExtra::grid.arrange, c(rs, list(nrow = nrow, ncol = ncol)))
  }
}

#Scatter plot function with Density heat map
plot_colorByDensity <- function(x1, x2, xlab="", ylab="", main="") {
  xymax <- max(x1,x2)
  df <- data.frame(x1,x2)
  x <- densCols(x1, x2, colramp=colorRampPalette(c("black", "white")))
  df$dens <- col2rgb(x)[1,] + 1L
  cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
  df$col <- cols[df$dens]

  plot(x2~x1, data=df[order(df$dens),],
       ylim=c(0,xymax), xlim=c(0,xymax),
       pch=20, col=col, cex=2,
       xlab=paste0(xlab,"\n"), ylab=ylab, main=main)

  abline(lsfit(x1,x2), col="orange", lwd=2, lty=2)
  abline(0.0, 1.0, lwd=2, col="green", lty=2)

  usr <- par( "usr" )

  text(usr[2]*.85,usr[4]*.95,"1-to-1 Line",cex=1,col="green")
  text(usr[2]*.85,usr[3]+usr[4]*.05,"Least Squares Line",cex=1,col="orange")
  text(.88*xymax,.9*xymax,"ON-TIME", srt=43,col="green4")
  legend(.2*xymax,.86*xymax,legend=("above green line ==> late"),bty="n",text.col="red")
  legend("right",legend=("below green line ==> early"),bty="n",text.col="royalblue")
  legend(x = 0.135*xymax, y = -0.145*xymax, legend=(paste("[QADTA(-3,0)=",
                                                          sprintf("%.2f",fdtfc3(x1,x2)),
                                                          "%, QADTA(-x,0)=",
                                                          sprintf("%.2f",fdtfcx(x1,x2)),"%]")),
         bty="n", text.col="blue", xpd=T)
}
