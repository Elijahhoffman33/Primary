library(ggplot2)
library(data.table)
library(scales)
data <- fread(file="/mnt/c/Users/elija/Desktop/Main/Projects/Primary/src/Logging_Functions/Log.txt")
data[,V1:=NULL]
data <- melt(data=data,id.vars = 'minute')
print(data)

colnames(data) <- c('Minutes','Resource','Percentage')
breaks=20
gg <- ggplot(data=data) + geom_line(aes(x=Minutes,y=Percentage,col=Resource)) + 
  # scale_color_manual(values=c('#0072B2','#009E73')) +
  scale_x_continuous(breaks = pretty(data$Minutes, n = breaks)) +
    # scale_y_continuous(breaks = seq(0,100,10))
  coord_cartesian(ylim=c(0,100))
gg + theme_classic() 


data[Resource=='ram',max(Percentage)]
data[Resource=='ram',max(Percentage)-min(Percentage)] * 24.3888 / 100

max(subset(data,subset=data$Resource=='cpu')$Percentage)
max(subset(data,subset=data$Resource=='cpu')$Percentage) * 250 / 100
#cpu = blue ram = green
