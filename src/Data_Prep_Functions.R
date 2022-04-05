library(caret)
data.pre <- preProcess(data, method="range") 
?caret::preProcess

robust_scalar<- function(x){(x- median(x)) /(quantile(x,probs = .75)-quantile(x,probs = .25))}
Standardization = function(x){(x-mean(x))/sd(x)}
Min_max_norm = function(x){(x-min(x))/(max(x)-min(x))}
