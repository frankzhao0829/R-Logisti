## Evaluate and compare model performance by different metrics
## Require: rminer package

# LAST MODIFIED
# 2016-10-18 06:47:38 ------------------------------

FcstComp <- function(predicted, observed, QTarget, naive.fcst = NULL, seas.naive.fcst = NULL) {
  # predicted - predictions by different models
  # observed - actual observations
  # naive.fcst - predictions by naive forecast P[t] = P[t-1]
  # seas.naive.fcst - predictions by seasonal naive forecast P[t] = P[t-f] (f - seasonal period)

  library(forecast)

  dd1 <- NULL
  ACC <- NULL

  for (i in 1:ncol(predicted)){
    # OBTAIN SEUDO R2 - CORRELATION.
    fitcorr <- format(cor(predicted[[i]], observed)^2, digits=4)
    # GENERATE REC CURVE
    m1 <- rminer::mmetric(observed, predicted[[i]], c('REC'))

    if (length(m1$rec) > 2) {
      m2 <- rminer::mmetric(observed, predicted[[i]], c('NAREC'))
      dd1 <- rbind(dd1, data.frame(tolerance = m1$rec[, 1], accuracy = m1$rec[, 2], model = colnames(predicted)[i]))
    } else {
      m2 <- NA
      dd1 <- rbind(dd1, data.frame(tolerance = m1$rec[1], accuracy = m1$rec[2], model = colnames(predicted)[i]))
    }

    mase   <- fmase  (predicted[[i]], observed)
    smape  <- fsmape (predicted[[i]], observed)
    dtfc3  <- fdtfc3 (predicted[[i]], observed)
    dtfcx  <- fdtfcx (predicted[[i]], observed)
    dtfc0  <- fdtfc0 (predicted[[i]], observed)
    avglt  <- fAVGLT (predicted[[i]], observed)
    sdlt   <- fSDLT  (predicted[[i]], observed)
    avglto <- fAVGLTo(predicted[[i]], observed)
    sdlto  <- fSDLTo (predicted[[i]], observed)
    qlto   <- fQLTo  (predicted[[i]], observed, QTarget)
    wdtfc  <- 0.75 * dtfcx + 0.25 * dtfc3
    nrows  <- fnrows (observed)

    if (!is.null(naive.fcst)) {
      FVANaive <- round(fFVA(predicted[[i]], observed, naive.fcst), digits = 5)
    } else {
      FVANaive <- NA
    }
    if (!is.null(seas.naive.fcst)) {
      FVASeasNaive <- round(fFVA(predicted[[i]], observed, seas.naive.fcst), digits = 5)
    } else {
      FVASeasNaive <- NA
    }

    temp <- data.frame(Method = as.character(colnames(predicted)[i]),
                       accuracy(observed, predicted[[i]]),
                       MASE   = mase,   sMAPE   = smape,
                       DTFC3  = dtfc3,  DTFCX   = dtfcx,
                       WDTFC  = wdtfc,  DTFC0   = dtfc0,
                       AVGLT  = avglt,  SDLT    = sdlt,
                       AVGLTo = avglto, SDLTo   = sdlto,
                       NAREC  = m2,     R2      = fitcorr,
                       NRows  = nrows,  QTarget = qlto)
    ACC <- rbind(ACC, temp)
  }

  res <- list(REC = dd1, ACC = ACC)

  res
}

fmase <- function(predicted, observed) {
  error <- 0

  if (length(observed) != length(predicted)) {
    return(NULL)
  } else if (length(observed) == 0 | length(predicted) == 0) {
    return(NULL)
  }
  else {
    denom = (sum(abs(observed[2:length(observed)] - observed[1:(length(observed) - 1)])))/(length(observed) - 1)
    error = sum((abs(observed-predicted)) / denom)/length(observed)
  }
  return(error)
}

fsmape <- function(predicted, observed) {
  error <- 0

  if (length(observed) != length(predicted)) {
    return(NULL)
  } else if (length(observed) == 0 | length(predicted) == 0) {
    return(NULL)
  } else {
    error = sum((abs(observed-predicted)) / (observed+predicted)) / length(observed);
    # denom = (sum(abs(observed[2:length(observed)] - observed[1:(length(observed) - 1)])))/(length(observed) - 1)
    #  error = sum((abs(observed-predicted)) / denom)/length(observed);
  }
  return(100.0 * error)
}

fFVA <- function(predicted, observed, naive) {
  error <- 0

  if (length(observed) != length(predicted)) {
    return(NULL)
  } else if (length(observed) == 0 | length(predicted) == 0) {
    return(NULL)
  } else {
    denom = sum(abs(observed - naive))
    error = 1 - sum((abs(observed-predicted)) / denom)
  }

  return(error)
}

fdtfc3 <- function(predicted, observed){

  if (length(observed) != length(predicted)) {
    stop('predicted should have same length as observed!')

  } else if (length(observed) == 0) {
    stop('length should not be 0!')

  } else {
    hits <- sum((predicted >= observed) & (predicted-observed <= 3)) #dtfc(-3,0) is based on 4 apparently
    acc <- 100 * hits / length(observed)
  }
  return (acc)
}

fdtfcx <- function(predicted, observed){

  if (length(observed) != length(predicted)) {
    stop('predicted should have same length as observed!')

  } else if (length(observed) == 0) {
    stop('length should not be 0!')

  } else {
    hits <- sum((predicted >= observed))

    acc = 100 * hits / length(observed)
  }

  return (acc)
}

fdtfc0 <- function(predicted, observed){

  if (length(observed) != length(predicted)) {
    stop('predicted should have same length as observed!')

  } else if (length(observed) == 0) {
    stop('length should not be 0!')

  } else {
    hits <- sum((predicted == observed))

    acc = 100 * hits / length(observed)
  }

  return (acc)
}

fAVGLT <- function(predicted, observed){

  if (length(observed) != length(predicted)) {
    stop('predicted should have same length as observed!')

  } else if (length(observed) == 0) {
    stop('length should not be 0!')

  } else {
    avgLT <- mean(predicted)
  }

  return (avgLT)
}

fSDLT <- function(predicted, observed){

  if (length(observed) != length(predicted)) {
    stop('predicted should have same length as observed!')

  } else if (length(observed) == 0) {
    stop('length should not be 0!')

  } else {
    sdLT <- sd(predicted)
  }

  return (sdLT)
}
fAVGLTo <- function(predicted, observed){

  if (length(observed) != length(predicted)) {
    stop('predicted should have same length as observed!')

  } else if (length(observed) == 0) {
    stop('length should not be 0!')

  } else {
    avgLT <- mean(observed)
  }

  return (avgLT)
}

fSDLTo <- function(predicted, observed){

  if (length(observed) != length(predicted)) {
    stop('predicted should have same length as observed!')

  } else if (length(observed) == 0) {
    stop('length should not be 0!')

  } else {
    sdLT <- sd(observed)
  }

  return (sdLT)
}

fnrows <- function(observed){
  return (length(observed))
}

fQLTo <- function(predicted, observed, QTarget){
  if (length(observed) != length(predicted)) {
    stop('predicted should have same length as observed!')

  } else if (length(observed) == 0) {
    stop('length should not be 0!')

  } else {
    QLT <- as.numeric(quantile(observed, QTarget))
  }

  return (QLT)
}
