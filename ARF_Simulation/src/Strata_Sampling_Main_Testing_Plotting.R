### Sample Function ----
x1 = Samples_dt[,.(Avg=mean(Samples),Var = var(Samples)),by=c('n','iter','Strata')] %>% 
  merge(proportion,by=c('n','Strata'))

### Sum of W_h * Y^_h
### W_h = npop / npop_total 
### Y^_h = Strata mean
Mean = x1[, .(Avg = sum(npop*Avg)/sum(npop)) ,c('n','iter')]
### W_h^2 * (S_h^2 / n_h) * (1 - f_n)
### S_h^2 = Variance of Strata
### 1 - f_n = fpc = finite populations correction
x1[,fpc :=  (1 - (Stratum_size / npop))]
# x1[,fpc := fpc > .95]

Var = x1[, .(Var = sum((npop/sum(npop))^2 * (Var/Stratum_size) * fpc)),
         c('n','iter')] 
out = cbind(Mean,Var[,'Var'])

Var[n==35,mean(sqrt(Var))]
Mean[n==35,sd(Avg)]
# out[,SE := sqrt(Var/sqrt(n)]

out[,SD := sqrt(Var)]
out[n==35,mean(SD)]
out[n==35,hist(Avg)]


### Test Plotting

og[,plot(n,Avg)]
abline(h=data_in[,mean(om)])

out[,plot(n,Avg)]
abline(h=data_in[,mean(om)])


x1[Strata==1,] %>% ggplot(aes(x=Avg,y=Var)) + geom_point(alpha=.3)

out[,] %>% ggplot(aes(x=n,y=Avg)) + geom_point(alpha=.3) +
  scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
  geom_hline(yintercept = data_in[,mean(om)],alpha=.6) + theme_classic(base_size = 22)
# ggMarginal(p, type="density") 

### Plot single densities
out[n%in%c(10,35,70)] %>% ggplot(aes(x=Avg)) +
  # geom_point(alpha=1)
  # geom_histogram(bins=200,alpha=.6,position = 'identity') +# scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
  geom_density(alpha=.8,position = 'identity',col="#7570B3",fill='#7570B3') + scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
  labs(x='%om') + theme_classic(base_size = 22) + facet_wrap('n')


### Strata level
x_s = x1[, .(Avg=mean(Avg),Var = mean(Var),SD=sqrt(mean(Var))),
         c('n','Strata')]

x_s %>% ggplot(aes(x=n,y=Avg,col=SD)) + geom_point(alpha=1)

### Just x1 
x1[n%in%c(10,35,70)] %>% ggplot(aes(x=Avg,col=factor(Strata),fill=factor(Strata))) +
  # geom_point(alpha=1)
  # geom_histogram(bins=200,alpha=.6,position = 'identity') +# scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
  geom_density(alpha=.8,position = 'identity') + scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
  labs(x='%om') + theme_classic(base_size = 22) + #coord_cartesian(ylim = c(0,10000)) +
  geom_text(data = x_s[n%in%c(10,35,70), ],
            aes(y=Inf,x=Avg, label=SD %>% round(3) ),
            vjust=3.5) + facet_wrap('n') # geom_text(aes(label = Global_Sales), vjust = -0.5)+ 

x_s[n%in%c(10,35,70), ]
data_in[,sd(om),Strata][order(Strata)]


### Metrics 
library(metrics)
x = out[,.(Var=var(Avg),Bias=bias(data_in[,mean(om)],Avg), MSE=mse(data_in[,mean(om)],Avg)),'n']
# x[,c_MSE := Var + Bias^2]
x = x %>% mutate_if(is.numeric,round,6)

x %>% ggplot(aes(x=n,y=Bias,col=Var %>% sqrt)) + geom_point(alpha=1)

### Running function ----
# Proportion testing 
Proportion = Allocate_Strata(data_in,allocation,Runs)


x2 = lapply(c('PPS','Neyman','Wright'),\(Algo){
  x = Allocate_Strata(data_in,Algo,Runs)
  x1 = x[,.(ratio=smart_round(Stratum_size / n,9)),c('n','Strata')] %>% 
    cbind(.,data.table(Method=Algo))
  x1$Strata = factor(x1$Strata)
  x1
}) %>% rbindlist
x2

ggplot(x2,aes(x=Strata,y=ratio,fill=Method)) + geom_violin(width=1,position = 'dodge') +
  theme_classic() +
  geom_boxplot(alpha=0,width=1,position = 'dodge',outlier.size=.5,outlier.alpha = 0) +
  stat_summary(fun=mean, geom="point",position = position_dodge(width = 1), shape=20,size=5) 

x2[,.(Ratio = .SD[Method=='Wright',ratio] - .SD[Method=='Neyman',ratio]),c('n','Strata')] %>% 
  ggplot(aes(x=n,y=Ratio,col=Strata)) + geom_point() + theme_classic()
