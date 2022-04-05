
library(microbenchmark)
# Bench_Results = lapply(1:nrow(subset),function(i){
Bench_Results = list()
for (i in 1:nrow(subset)){
  bench = microbenchmark(
    dq_inner = Sampling_Simulation('Run_1',Data[field_id==subset[i,field_id]],3,Vars,Vars_in,Iterations = 100,Iter_runs = 10,
                                   allocation = 'Neyman',sampler_type = 'not',Par = 'inner',batch_limit = 200),
    dq_outer = Sampling_Simulation('Run_1',Data[field_id==subset[i,field_id]],3,Vars,Vars_in,Iterations = 1000,Iter_runs = 1,
                                   allocation = 'Neyman',sampler_type = 'not',Par = 'outer',batch_limit = 200),
    base_inner = Sampling_Simulation('Run_1',Data[field_id==subset[i,field_id]],3,Vars,Vars_in,Iterations = 100,Iter_runs = 10,
                                     allocation = 'Neyman',sampler_type = 'base',Par = 'inner',batch_limit = 200),
    base_outer = Sampling_Simulation('Run_1',Data[field_id==subset[i,field_id]],3,Vars,Vars_in,Iterations = 1000,Iter_runs = 1,
                                     allocation = 'Neyman',sampler_type = 'base',Par = 'outer',batch_limit = 200),
    # dq_outer = Sampling_Simulation('Run_1',Data[field_id==subset[i,field_id]],3,Vars,Vars_in,Iterations = 100,Iter_runs = 10,
    #                                allocation = 'Neyman',sampler_type = 'not',Par = 'outer',batch_limit = 200),
    times=10
  )
  gc(full=T)
  b1 = summary(bench) %>% as.data.table
  b1$Acres = subset[1,Acres]
  Acres = subset[1,Acres]
  Bench_Results[[i]] = list(b1,bench,Acres)
}


autoplot(bench) + theme_classic()

plot = lapply(seq_along(Bench_Results),function(i){
  x = Bench_Results[[i]]
  x1 = '[['(x,2)
  data.table(expr=x1$expr,time=x1$time*10^-9,N=subset[i,N],Acres=subset[i,Acres])
  
}) %>% rbindlist()


plot$Acres = as.factor(round(plot$Acre))
plot$N = as.factor(plot$N)

ggplot(plot[!expr%in%c('dq_inner','base_inner')],aes(y=time,x=Acres,col=expr,fill=expr)) + geom_violin() +
  theme_classic() + coord_flip()


y = plot[expr=='base_outer',time]/plot[expr=='dq_outer',time]
y1 = data.table(Acres=plot[expr=='dq_inner',Acres],Ratio=y)
ggplot(y1,aes(x = Acres,y=Ratio)) + geom_hline(yintercept = 1) +  + theme_classic() +
  geom_boxplot()
geom_point()
