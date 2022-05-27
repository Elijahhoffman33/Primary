library(caret)
library(doParallel)

reg_ctrl <- caret::trainControl(method = "repeatedcv", number=10)
seed <- 333
metric <- "RMSE"


# Run separate models on dataset - linear and non linear

# Linear regression

cl <- makePSOCKcluster(5)
registerDoParallel(cl)


set.seed(seed)
lm_mod <- caret::train(
  TTBS_mins ~ ., 
  data = ED, 
  method = "lm", metric = metric, #preProc=c("center", "scale"), 
  trControl=reg_ctrl
)

#Regression Tree
rpart_mod <- caret::train(
  TTBS_mins ~ ., 
  data = ED, 
  method = "rpart", metric = metric, #preProc=c("center", "scale"), 
  trControl=reg_ctrl
)
#Random regression forest


rf_mod <- caret::train(
  TTBS_mins ~ ., 
  data = ED, 
  method = "rf", metric = metric,# preProc=c("center", "scale"), 
  trControl=reg_ctrl
)

stopCluster(cl)

results <- resamples(
  list(Linear = lm_mod,
       RPART = rpart_mod,
       RandomForest = rf_mod)
)


scales <- list(x=list(relation="free"), y=list(relation="free"))
dotplot(results)
bwplot(results, scales=scales)

