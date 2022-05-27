### Functions ----

entropy <- function(d){
  x <- 0
  d = d + 10e-10
  for(i in d){
    x <- x + (i * log(i, base = 2))
  }
  return(-x)
}

cross.entropy <- function(p, phat){
  # x <- 0
  # for(i in 1:length(p)){
  #   print()
  #   x <- x + (p[i] * log(phat[i],base=2))
  # }
  p = p + 10e-10
  phat = phat + 10e-10
  sum((p * log(phat,base=2))) * -1
}

KL.divergence = function(p,q){
  x = 0
  p = p + 10e-10
  q = q + 10e-10
  for(i in 1:length(p)){
    x <- x + (p[i] * log2(p[i]/q[i]))
  }
  return(x)
}

### 

x = rnorm(10000)
breaks = get_breaks(x,n = 20,style = 'equal',cut=F)$brks
x = (hist(x,breaks=breaks)$counts/10000)
y = runif(10000)
breaks = get_breaks(y,n = 20,style = 'equal',cut=F)$brks
y = (hist(y,breaks=20)$counts/10000)

cross.entropy(x,y)
entropy(x)
entropy(y)
cross.entropy(x,x)

sapply(y,entropy) %>% plot(.)
sapply(x,\(z) log2(1/z)) %>% plot(.)
cross.entropy(c(1,1,1),rep(.9,3))

# KL and Cross entropty relation

cross.entropy(x,y)
entropy(x) + KL.divergence(x,y)

###

get_cont_probs = function(x,return_breaks=F){
  breaks = get_breaks(x,n = 100,style = 'equal',cut=F)$brks
  x1 = hist(x,breaks=breaks)$counts/length(x)
  if(return_breaks==F){
    return(x1)
  } else{
  return(list(x1,breaks))
}} 

norm_1 = rnorm(10000,mean = 100,sd = 10)
probs_1 = get_cont_probs(c(0,norm_1,200))

# z = lapply(seq(0,9.9,.1),\(x){
z = mclapply(seq(100,-100),mc.cores=8,\(x){
  # quick MC simulation 
  # y1 = sapply(seq(100),\(y){
  print(x)
  # x1 = get_cont_probs(rnorm(10000,mean = 100,sd = 10 - x))
  
  z = c(0,norm_1-x,200)
  Q = get_cont_probs(z,T)[[1]]
  
  cross.entropy(probs_1,Q)
  # }) #%>% mean
}) %>% unlist

plot(z)


x = c(0,norm_1,200)
x1 = get_cont_probs(x,T)
X = x1[[2]][-length(x1[2])]
Prob = x1[[1]]
plot(X,Prob)

P = x1[[1]]
x = c(0,norm_1-50,200)
Q = get_cont_probs(x,T)[[1]]

### standard error theory testing ----
norm_2 = rnorm(10000,mean = 100,sd = 10)

y1 = sapply(seq(100),\(y){
  print(y)
  sapply(seq(1000),\(z){
    sd(sample(norm_2,y))
  }) %>% mean
}) 

# take estimates of 
plot(y1/sqrt(seq(100)))



# stderror <- function(x) sd(x)/sqrt(length(x))
# sapply(y1,stderror) %>% plot

###

