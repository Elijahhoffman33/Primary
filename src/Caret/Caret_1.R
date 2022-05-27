library(caret)
library(doParallel)


### preproessing 
balanced_stranded %<>%  drop_na()
scaled <- preProcess(balanced_stranded[,2:ncol(balanced_stranded)], method=c("scale")) #Center could be added here
transformed <- predict(scaled, balanced_stranded[,2:ncol(balanced_stranded)])
summary(transformed)

#### custom metric
rsq <- function(actual, predicted){
  RSS <- sum((actual-predicted)^2)
  TSS <- sum((actual-mean(actual))^2)
  R2 <- (1-(RSS/TSS))
  return(R2)
}
rsq_c =  function(data, lev = NULL, model = NULL) {
  rsq(data$obs,data$pred)
}

# f1 <- function(data, lev = NULL, model = NULL) {
#   f1_val <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = lev[1])
#   c(F1 = f1_val)
# }

reg_ctrl <- caret::trainControl(method = "repeatedcv", number=5,repeats = 10)
seed <- 333
metric <- "RMSE"


# Linear regression

cl <- makePSOCKcluster(5)
registerDoParallel(cl)

set.seed(seed)
lm_mod <- caret::train(
  BD ~ ., 
  data = data[,!'TOC'], 
  method = "lm", metric = metric, #preProc=c("center", "scale"), 
  trControl=reg_ctrl
)

lm_mod_imp <- caret::train(
  BD ~ ., 
  data = imputed_data[,!'TOC'], 
  method = "lm", metric = metric, #preProc=c("center", "scale"), 
  trControl=reg_ctrl
)
lm_mod_imp_norm <- caret::train(
  BD ~ ., 
  data = imputed_data[,!'TOC'], 
  method = "lm", metric = metric, preProc=c("center", "scale"), 
  trControl=reg_ctrl
)


cl <- makePSOCKcluster(13)
registerDoParallel(cl)

rf_mod <- caret::train(
  BD ~ ., 
  data = data[,!'TOC'], 
  method = "rf", metric = metric,# preProc=c("center", "scale"), 
  trControl=reg_ctrl
)

rf_mod_imp <- caret::train(
  BD ~ ., 
  data = imputed_data[,!'TOC'], 
  method = "rf", metric = metric,# preProc=c("center", "scale"), 
  trControl=reg_ctrl
)

rf_mod_imp_norm <- caret::train(
  BD ~ ., 
  data = imputed_data[,!'TOC'], 
  method = "rf", metric = metric, preProc=c("center", "scale"), 
  trControl=reg_ctrl
)
stopCluster(cl)

# results <- resamples(
#   list(Linear = lm_mod,
#        Linear_imp = lm_mod_imp,
#        Linear_imp_norm = lm_mod_imp_norm,
#        RandomForest = rf_mod,
#        RandomForest_imp = rf_mod_imp,
#        RandomForest_imp_norm = rf_mod_imp_norm)
# )
results <- resamples(
  list(RandomForest_imp = rf_mod_imp,
       RandomForest_imp_less = rf_mod_imp_less,
       RandomForest_imp_preconvert = rf_mod_preconvert))

scales <- list(x=list(relation="free"), y=list(relation="free"))
dotplot(results)
bwplot(results, scales=scales)

### Output ----

model = rf_mod_imp$finalModel
model$importance
saveRDS(model, file = "ARF_Simulation/models/BD_RF")



True = model$y
Pred = model$predicted
result = data.table(True = model$y,Pred = model$predicted)

binsreg(x=True,y=Pred,ci=c(3,3))
plot(Pred,True)

### Actual prediction 
cols = model$xNames
x = data[,..cols] %>% as.data.frame
data$BD = predict(model,x)

### iRF model
# irf_model = iRF(x = imputed_data[,!c('TOC','BD')],
#     y = imputed_data[,BD],n.core = 10)

# cols = colnames(imputed_data[,!c('TOC','BD')])
# irf_results = Run_Model(imputed_data,
#                       cols_x = cols,cols_y ='BD',Model='iRF',
#                     K=5,single=F,interactions = T,summary = F,ncores = 10)



