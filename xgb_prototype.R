library(doParallel)
library(vtreat)
library(xgboost)
library(caret)

################################################################################
############################### Helper Function ################################
################################################################################

DTFC3 <- function(preds, dtrain) {
  labels <- getinfo(dtrain, 'label')
  preds <- ceiling(preds)
  hits <- sum((preds >= labels) & (preds - labels <= 3)) #dtfc(-3,0) is based on 4 apparently
  acc <- 100 * hits / length(labels)
  return (list(metric = 'DTFC3', value = acc))
}

################################################################################
############## Redefine Necessary Data Frame Based On RFDataSet ################
################################################################################

overallTarget <- 'calcREtoDel'
df_train <- RFDataSet[train, c(engInput$calcREtoDel, overallTarget)]


# Create parallel cluster for vtreat usage
ncore <- 4
cl <- makeCluster(ncore)

## Use mkCrossFrameNExperiment over designTreatmentsN
## https://cran.r-project.org/web/packages/vtreat/vignettes/vtreatCrossFrames.html
ptm <- proc.time()
cd <- mkCrossFrameNExperiment(df_train, c(engInput$calcREtoDel), overallTarget, parallelCluster = cl)
proc.time() - ptm
# ~ 20 mins for Egoras 70% training data

df_score <- cd$treatments$scoreFrame

#new_vars <- df_score$varName[df_score$sig < 1 / nrow(df_score)]
new_vars <- df_score$varName

df_test <- prepare(cd$treatments, RFDataSet[test, c(baseInput)], pruneSig = NULL, varRestriction = new_vars)
#df_test <- prepare(cd$treatments, RFDataSet[test, c(engInput$calcREtoDel)], pruneSig = NULL)

xgb_params_1 <- list(
  objective = "reg:linear",
  max_depth = 8,
  eta = 0.1,
  #gamma = 0.1,
  colsample_bytree = 0.6,
  #eval_metric = DTFC3,
  eval_metric = 'rmse',
  min_child_weight = 1
)

xgb <- xgb.train(
  params = xgb_params_1,
  data = xgb.DMatrix(data = as.matrix(cd$crossFrame[, new_vars]),
                     label = cd$crossFrame$calcREtoDel),
  nrounds = 2000,
  watchlist = list(test = xgb.DMatrix(data = as.matrix(df_test),
                                      label = RFDataSet[test,]$calcREtoDel)),
  print.every.n = 5,
  early.stop.round = 50,
  maximize = FALSE
)

# Get and Plot the Variable Importance
imp_matrix <- xgb.importance(feature_names = new_vars, model = xgb)

png(filename = 'XGB_VarImp_Plot.png', type = 'cairo',
    width = 3000,
    height = 6000,
    res = 250)

print(xgb.plot.importance(importance_matrix = imp_matrix))

dev.off()

################################################################################
############################## Model Evaluation ################################
################################################################################

pr_xgb_test <- predict(xgb, as.matrix(df_test))
pr_xgb_train <- predict(xgb, as.matrix(cd$crossFrame[, new_vars]))

PaddingFactor <- 0.0

dfTRAIN <- data.frame(
  SalesOrderID      = RFDataSet[train, ]$SalesOrderID,
  SOLineItemID      = RFDataSet[train, ]$SOLineItemID,
  XGB_ALL_UNSEG     = ceiling(pr_xgb_train) + PaddingFactor,
  ATP               = RFDataSet[train, ]$ATPREtoDel,
  ALLMeanLT         = ceiling(mean(RFDataSet[train, ]$calcREtoDel)),
  #SCGMeanLT         = RFDataSet[train, ]$MeanLT,
  Target            = RFDataSet[train, ]$calcREtoDel
)

dfTEST <- data.frame(
  SalesOrderID      = RFDataSet[test, ]$SalesOrderID,
  SOLineItemID      = RFDataSet[test, ]$SOLineItemID,
  XGB_ALL_UNSEG     = ceiling(pr_xgb_test) + PaddingFactor,
  ATP               = RFDataSet[test, ]$ATPREtoDel,
  ALLMeanLT         = ceiling(mean(RFDataSet[train, ]$calcREtoDel)), # IMPORTANT
  #SCGMeanLT         = RFDataSet[test, ]$MeanLT,
  Target            = RFDataSet[test, ]$calcREtoDel
)

outTRAIN <- ModelEvaluate(predicted = dfTRAIN %>% select(contains('SEG'), ATP, ALLMeanLT), dfTRAIN$Target)
outTRAIN$ACC

outTEST <- ModelEvaluate(predicted = dfTEST %>% select(contains('SEG'), ATP, ALLMeanLT), dfTEST$Target)
outTEST$ACC

RECPlot(outTEST$REC, ncol = 1, nrow = 1, filename = 'REC_TEST.png')
RECPlot(outTRAIN$REC, ncol = 1, nrow = 1, filename = 'REC_TRAIN.png')

write.csv(outTEST$ACC, 'ACC_TEST.csv', row.names = F)
write.csv(outTRAIN$ACC, 'ACC_TRAIN.csv', row.names = F)

# Density Plot
plot(density(dfTEST %>% filter(Target < 100) %>% pull(Target), adjust = 10))
lines(density(dfTEST %>% filter(Target < 100) %>% pull(ATP), adjust = 10))
lines(density(dfTEST %>% filter(Target < 100) %>% pull(XGB_ALL_UNSEG), adjust = 10))
lines(density(dfTEST %>% filter(Target < 100) %>% pull(ALLMeanLT), adjust = 10))

################################################################################
########################### HYPERPARAMETER TUNING ##############################
################################################################################

# Grid Search
xgb_grid <- expand.grid(
  nrounds = seq(500, 1500, 500),
  max_depth = seq(3, 9, 2),
  eta = 0.1,
  gamma = 0,
  colsample_bytree = seq(0.2, 1, 0.2),
  min_child_weight = 1
)

# Random Search
grid_len <- 10
xgb_grid <- data.frame(
  nrounds = sample(1:2000, size = grid_len, replace = TRUE),
  max_depth = sample(1:10, size = grid_len, replace = TRUE),
  eta = runif(grid_len, min = 0.001, max = 0.6),
  gamma = runif(grid_len, min = 0, max = 10),
  colsample_bytree = runif(grid_len, min = 0.3, max = 0.7),
  min_child_weight = sample(0:20, size = grid_len, replace = TRUE)
)


# Train Control
xgb_control <- trainControl(
  method = 'cv',
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = 'all',
  allowParallel = TRUE
)

# Subset the training data for speed
#sub_train <- cd$crossFrame %>% sample_n(50000)
sub_train <- cd$crossFrame

xgb_train <- caret::train(
  x = as.matrix(sub_train[, new_vars]),
  y = sub_train$calcREtoDel,
  trControl = xgb_control,
  tuneGrid = xgb_grid,
  method = 'xgbTree'
)

################################################################################
################################ EXIT CLEANUP ##################################
################################################################################

stopCluster(cl)
