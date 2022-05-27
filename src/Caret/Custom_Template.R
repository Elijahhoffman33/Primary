#Using caret() to determine the optimum value for grnn() smooth parameter    
grnnFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  #use argument names EXACTLY as here in all functions
  library(grnn)
  dat <- data.frame(y, x)
  s <- smooth(learn(dat), sigma=param$sigma)
  return(s)
}

grnnPred <- function(modelFit, newdata, preProc=NULL, submodels=NULL) {
  library(grnn)
  library(foreach)
  xlst <- split(newdata, 1:nrow(newdata))
  pred <- foreach(i = xlst, .combine = rbind) %do% {
    #grnn() can only compute a prediction for one sample at a time
    guess(modelFit, as.matrix(i)) #provide x values as matrix
  }
}

grnnSort <- function(x) {
  x[order(x$sigma),]
  print(x)
}

grnnGrid <- function(x, y, len=NULL) {
  #only one tuning parameter sigma
  data.frame(sigma=seq(1,4,.05)) #search range
}

grnnLev <- function(x) {
  lev(x)
}

#list of params/functions
lpgrnn <- list(
  library="grnn",
  type="Regression",
  parameters=data.frame(parameter="sigma", class="numeric", label="Sigma"),
  grid=grnnGrid,
  fit=grnnFit,
  predict=grnnPred,
  prob=NULL,
  levels=grnnLev,
  sort=grnnSort)

library(caret)
set.seed(123)
x1 <- rep(1:100) + rnorm(100,0,1)
x2 <- rep(1:100) + rnorm(100,0,1)
tr <- data.frame(y=x1*x2, x1, x2)
set.seed(998)
fitControl <- trainControl(method="repeatedcv", repeats=5)
set.seed(825)
res <- train(y~., data=tr, method=lpgrnn, metric="RMSE", trControl = fitControl)
print(res)
print(res$finalModel$sigma)
plot(res)