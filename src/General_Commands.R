

###Clear Workspace
rm(list = ls(all.names = TRUE))
gc(full = TRUE)
.rs.restartR()

for (i in 1:10) 
  gc(reset = T)

library(NCmisc)
memory.summary(unit = "gb")

###Mapbox
set_defaults(map_service = "mapbox", map_type = "satellite",
             map_token = "pk.eyJ1Ijoic2lyaXVzMzMiLCJhIjoiY2t5NmRmdGVjMHVtajJ1cXZ1ZG96b3B4ZyJ9.Q-g4nUhhfsVhdtD2_kCUwA")


###gc data usage

smallest.sv <- function(){
  A <- matrix(rnorm(1e6), 1e3);
  mysvd <- svd(A);
  return(tail(mysvd$d, 1))
}

tt <- sum(.Internal(gc(FALSE, TRUE, TRUE))[13:14])
x <- smallest.sv()
sum(.Internal(gc(FALSE, FALSE, TRUE))[13:14]) - tt
#62 MB
rm(x)


###Creating breaks for plotting 

get_breaks = function(x,n=NA,style='fixed',breaks,cut=T){
  if(style=='fixed'){
    n = length(breaks) + 1
    x1 = classInt::classIntervals(x, style = style, fixedBreaks = breaks)
  } else{
    x1 = classInt::classIntervals(x,n=n,style=style)
  }
  if(cut==T){
    return(cut(x,x1$brks))
  } else(return(x1))
}

###Searching whole list
ls <- ls()
collect <- c()
for(i in ls(model_depo[[1]][[2]][[1]]$iRF$model$rf.list)){
  collect <- c(collect,object_size(eval(parse(text=paste('model_depo[[1]][[2]][[1]]$iRF$model$rf.list$',i,sep='')))))
}
collect 


###Create basic text files

#Ashely_Bash_Run_1


out <- c()
for(i in 1:10){
  out <- c(out,rep(i,93))
}

table <- data.frame(paste(rep(seq(1,93),10),out,sep='-'))
write.table(table,file = '/mnt/c/Users/elija/Desktop/Lab_Shit/R_Data/Export/Elijah_Run_List_5.txt',quote = FALSE,row.names = FALSE,col.names = FALSE)

#Sorting additional runs

files <- list.files(path= "/mnt/c/users/elija/Desktop/Lab_Shit/Covid/Data/Elijah_Data/Permuted_V2/",full.names = TRUE)
index <- which(str_detect(files,pattern=paste(seq(45,60),collapse="|")))

out <- c()
for(i in 1:10){
  out <- c(out,rep(i,length(index)))
}

table <- data.frame(paste(rep(index,10),out,sep='-'))
write.table(table,file = '/mnt/c/Users/elija/Desktop/Lab_Shit/R_Data/Export/Elijah_Run_List_4.txt',quote = FALSE,row.names = FALSE,col.names = FALSE)

(median*abs(actual-predicted))/median

#smORF_Variable_AA
table <- data.frame(paste(rep(seq(50,450,10),9),rep(c(rep(1,41),rep(2,41),rep(3,41)),3),c(rep(100,41*3),rep(101,41*3),rep(102,41*3)),sep='_'))
out <- c()
for(i in 201:210){
  out <- c(out,rep(i,26*2))
}

table <- data.frame(paste(rep(seq(50,450,10),30),rep(c(rep(1,26),rep(2,26),rep(3,26)),10),out,sep='_'))
library(stringr)

#combined

str <- 'a_bitch_ass_path'

out <- c()
for(i in paste('~',seq(1,9),sep='')){
  out <- c(out,rep(i,41))
}



table <- data.frame(paste(rep(seq(50,450,10),9),rep(rep(4.5,41),9),out,sep='_'))

###NERSC Covid runs
str <- '$SCRATCH/taskfarmer/Covid_iRF_2/Wrapper_test.sh'

out <- c()
for(i in paste(str,seq(1,4),sep=' ')){
  out <- c(out,paste(i,rep(seq(1,12))))
}

paste(out,seq(0,8*length(out),8))



table <- data.frame(out)


###Exclude
table <- as.data.frame(table[which(str_detect(table[,1],pattern = '_2_',negate=TRUE)),])
table <- as.data.frame(table[which(str_detect(table[,1],pattern = paste(seq(100,150,10),collapse='|'),negate=TRUE)),])
table <- as.data.frame(table[which(str_detect(table[,1],pattern = paste(paste('^',seq(50,90,10),sep=''),collapse='|'),negate=FALSE)),])
write.table(table,file = '/users/elija/Desktop/Lab_Shit/R_Data/Export/Test.txt',quote = FALSE,row.names = FALSE,col.names = FALSE)

###Exclude_Combined
table <- as.data.frame(table[which(str_detect(table[,1],pattern = paste(seq(100,150,10),collapse='|'),negate=TRUE)),])
table <- as.data.frame(table[which(str_detect(table[,1],pattern = paste(paste('^',seq(50,90,10),sep=''),collapse='|'),negate=TRUE)),])
write.table(table,file = '/users/elija/Desktop/Lab_Shit/R_Data/Export/Test.txt',quote = FALSE,row.names = FALSE,col.names = FALSE)


###Servers
SERVER1=chardonnay.lbl.gov
SERVER2=shiraz.lbl.gov
SERVER3=maple.lbl.gov
SERVER4=sauvignon.lbl.gov
SERVER5=merlot.lbl.gov



###Resample Predicton
N <- 200
rsample <- function(N,n,pop=nrow(df_non_smORFs)){
  out <- c()
  for(iiii in(1:N)){
    s1 <- sample(pop,size = n,replace=FALSE)
    s2 <- sample(s1,size=n*.8,replace=FALSE)
    out <- c(out,s2)
  }
  counts <- table(out)
  return(hist(counts))
}

rsample(345,720,14000)

object_size <- function(obj){
  return(format(object.size(obj), units = "auto"))
}

ls <- ls()
collect <- c()
for(i in ls){
  collect <- c(collect,object_size(eval(parse(text=i))))
}
collect
ls[126]
data.frame(ls,collect)

par(mfrow=c(1,1))

###Delete a list of files
sapply(files,unlink)

###ggplot, remove gridlines and jazz
+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

###Change df factors to characters
data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
###Change df characters to numeric
data <- data.frame(lapply(data, as.numeric), stringsAsFactors=FALSE)


library(microbenchmark)
i= 6000:6500
res= microbenchmark(combine(Index_l[i]), times = 1000)
autoplot(res)

###Add progress bar
pb <- txtProgressBar(min = 0, max = length(iRF_All), style = 3)
setTxtProgressBar(pb, ii)
close(pb)

###Save as png

png(filename = paste('/users/elija/Desktop/Lab_Shit/R_Data/Export/Covid_Microbiome/Surface_Plots/Classification/Hinge_2/',paste(Int[1],Int[2],sep='_'),'.png',sep=''),width=802,height = 796)
plot_surface(Int[1],Int[2],-2,'classification')
dev.off()
###Save rendered plot as is
dev.print(pdf, 'filename.pdf')

###Paper ggplot theme
+ theme(legend.text = element_text(size = 12)) + theme(legend.title = element_text(size = 14)) + theme(plot.title = element_text(size = 16,hjust = 0.5)) + theme(axis.title = element_text(size = 14)) + theme(axis.text = element_text(size = 12))

###Edit a function
trace(iRF,edit=TRUE)
untrace(iRF)

###Calculate and plot Slope

library(zoo)
library(data.table)
library(ggplot2)
rollingSlope.lm.fit <- function(vector) {
  
  a <- coef(.lm.fit(cbind(1, seq(vector)), vector))[2]
  return(a)
  
}

tmp <- rollapply(data$Mean,width=5,FUN=rollingSlope.lm.fit,fill=NA)
tmp <- tmp*5

data <- cbind(data,tmp)
colnames(data)[4] <- 'Slope'
data$Index <- seq(1,length(data$Genes))
data <- as.data.frame(melt.data.table(as.data.table(data), measure.vars=c("Mean", "Slope")),id.vars='Index')

ggplot(subset(data,subset=data$Index%in%seq(1,40)), aes(Index, value))+ geom_point(aes(color=variable))



###Functions

# improved list of objects

.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) class(x)[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    format(utils::object.size(x), units = "auto") })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Length/Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# shorthand
lsos <- function(n=10.,... ) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

lsos(n=50)

###print cols nicely
list_cols = function(x){
  data.frame(Cols=colnames(x),Index=seq_along(x))
}

###Extract S3 method
findMethod <- function(generic, ...) {
  ch <- deparse(substitute(generic))
  f <- X <- function(x, ...) UseMethod("X")
  for(m in methods(ch)) assign(sub(ch, "X", m, fixed = TRUE), "body<-"(f, value = m))
  X(...)
}

findMethod(as.dist,hc)
findMethod(as.matrix,hc)
getS3method('as.matrix','hclust')


reset_par <- function(){
  op <- structure(list(xlog = FALSE, ylog = FALSE, adj = 0.5, ann = TRUE,
                       ask = FALSE, bg = "transparent", bty = "o", cex = 1, 
                       cex.axis = 1, cex.lab = 1, cex.main = 1.2, cex.sub = 1, 
                       col = "black", col.axis = "black", col.lab = "black", 
                       col.main = "black", col.sub = "black", crt = 0, err = 0L, 
                       family = "", fg = "black", fig = c(0, 1, 0, 1), 
                       fin = c(6.99999895833333, 6.99999895833333), font = 1L, 
                       font.axis = 1L, font.lab = 1L, font.main = 2L, 
                       font.sub = 1L, lab = c(5L, 5L, 7L), las = 0L, 
                       lend = "round", lheight = 1, ljoin = "round", lmitre = 10, 
                       lty = "solid", lwd = 1, mai = c(1.02, 0.82, 0.82, 0.42), 
                       mar = c(5.1, 4.1, 4.1, 2.1), mex = 1, mfcol = c(1L, 1L), 
                       mfg = c(1L, 1L, 1L,1L), mfrow = c(1L, 1L), 
                       mgp = c(3, 1, 0), mkh = 0.001, new = FALSE, 
                       oma = c(0, 0, 0, 0), omd = c(0, 1, 0, 1), 
                       omi = c(0, 0, 0,0), pch = 1L, 
                       pin = c(5.75999895833333, 5.15999895833333),
                       plt = c(0.117142874574832, 0.939999991071427, 
                               0.145714307397962, 0.882857125425167), 
                       ps = 12L, pty = "m", smo = 1, srt = 0, tck = NA_real_, 
                       tcl = -0.5, usr = c(0.568, 1.432, 0.568, 1.432), 
                       xaxp = c(0.6, 1.4, 4), xaxs = "r", xaxt = "s", 
                       xpd = FALSE, yaxp = c(0.6, 1.4, 4), yaxs = "r", 
                       yaxt = "s", ylbias = 0.2), 
                  .Names = c("xlog", "ylog", "adj", "ann", "ask", "bg", 
                             "bty", "cex", "cex.axis", "cex.lab", "cex.main", "cex.sub", 
                             "col", "col.axis", "col.lab", "col.main", "col.sub", "crt", 
                             "err", "family", "fg", "fig", "fin", "font", "font.axis", 
                             "font.lab", "font.main", "font.sub", "lab", "las", "lend", 
                             "lheight", "ljoin", "lmitre", "lty", "lwd", "mai", "mar", 
                             "mex", "mfcol", "mfg", "mfrow", "mgp", "mkh", "new", "oma",
                             "omd", "omi", "pch", "pin", "plt", "ps", "pty", "smo", 
                             "srt", "tck", "tcl", "usr", "xaxp", "xaxs", "xaxt", "xpd", 
                             "yaxp", "yaxs", "yaxt", "ylbias"))
  par(op)
}
