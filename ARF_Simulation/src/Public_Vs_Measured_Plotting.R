source('src/General_Setup_Edit.R')
source('../Soil_Stratification_Sampling/Main_Wrapper.R')

data = fread('ARF_Simulation/data/input/Input_Sets/Full.csv')
data = fread('ARF_Simulation/data/input/Leflore_Sets/Input_Sets/Full.csv')

Rescale_om(data)
Get_Weighted_Avg_Depths(data)

data$cl = data$cl %>% as.factor()


plot = data %>% melt(measure.vars=c('sg_soc','polaris_om','gssurgo_om_r'))

dir.create('ARF_Simulation/scratch/Public_Vs_Measured_Plots')

scale = 20
gg1 = ggplot(plot,aes(x=value,fill=variable))+
  geom_histogram(bins=150,alpha=.8,position = 'identity') + scale_color_brewer(palette = 'Dark2') + 
  scale_fill_brewer(palette = 'Dark2') +
  labs(x='%om',color='Data Source') + theme_classic(base_size = scale) 
  # coord_cartesian(ylim = c(0,1000)) 
gg1
ggsave(gg1,filename = 'ARF_Simulation/scratch/Public_Vs_Measured_Plots/Leflore_histograms.png',scale=2)#width = 665*2,height = 474*2,units = 'px')
# density version 
gg2 = ggplot(plot,aes(x=value,fill=variable))+
  geom_density(bins=150,alpha=.8,position = 'identity') + scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
  labs(x='%om',color='Data Source') + theme_classic(base_size = scale) 
  # coord_cartesian(ylim = c(0,20)) 

gg2
ggsave(gg2,filename = 'ARF_Simulation/scratch/Public_Vs_Measured_Plots/Leflore_density.png',scale=2)#,width = 665*2,height = 474*2,units = 'px')

### Actual versus predicted 


gg3 = ggplot(plot,aes(x=value,y=om,color=variable))+
  geom_point(alpha=.3,size=.5) + geom_line(stat='smooth',method="loess",se=F,span=1,aes(group=variable),
                                          col='black', linetype="dashed",alpha=.8) +
  scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
  labs(x='Public Estimated %OM',y='Sample %OM',color='Data Source') + theme_classic(base_size = scale) +
  guides(color = guide_legend(override.aes = list(size = 3,alpha=1)))
gg3 = ggMarginal(gg3,type="density",groupFill = T)
gg3
ggsave(gg3,filename = 'ARF_Simulation/scratch/Public_Vs_Measured_Plots/Leflore_Public_vs_Estimated_Scatter.png',scale=2)#,width = 665*2,height = 474*2,units = 'px')

### Residuals plot
gg4 = ggplot(plot,aes(x=value,y=om-value,color=variable))+
  geom_point(alpha=.3,size=.5) + geom_line(stat='smooth',method="loess",se=F,span=1,aes(group=variable),
                                             col='black', linetype="dashed",alpha=.8) +
  scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
  labs(x='Public Estimated %OM',y='Residuals',color='Data Source') + theme_classic(base_size = scale) +
  guides(color = guide_legend(override.aes = list(size = 3,alpha=1)))

gg4 = ggMarginal(gg4,type="density",groupFill = T)
gg4
ggsave(gg4,filename = 'ARF_Simulation/scratch/Public_Vs_Measured_Plots/Leflore_Public_vs_resuduals_scatter.png',scale=2)#,width = 665*2,height = 474*2,units = 'px')

### Residuals violin 
gg5  = ggplot(plot,aes(x=variable,y=om-value,fill=variable))+
  geom_violin(alpha=1) +
  scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
  labs(x='Public Estimated %OM',y='Residuals') + theme_classic(base_size = scale) +
  geom_boxplot(width=.1,outlier.size=.5,outlier.alpha = 0,alpha=0) + 
  stat_summary(fun=mean, geom="point", shape=20,size=5) 

gg5 
ggsave(gg5,filename = 'ARF_Simulation/scratch/Public_Vs_Measured_Plots/ARF_public_residuals_violin.png',scale=2)#,width = 665*2,height = 474*2,units = 'px')

### Summary Stats
library(Metrics)
cols = c('polaris_om','sg_soc','gssurgo_om_r','om')

data[!is.na(gssurgo_om_r),.(Variable=cols,Cor=lapply(.SD,cor,om),'R^2'=lapply(.SD,rsq,actual=om),MSE=lapply(.SD,mse,actual=om),MdAE=lapply(.SD,mdae,actual=om),
        SD=lapply(.SD,sd),Mean=lapply(.SD,mean)),
     .SDcols=cols,'cl'] 

