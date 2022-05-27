source('src/General_Setup_Edit.R')
library(ggExtra)
library(ggplot2)
library(tmap)

library(GGally)
library(DataExplorer)

library(binsreg)
library(plotly)

data = fread('USDA_Data/data/USDA_Data.csv')
colnames(data) = colnames(data) %>% str_replace_all(' ','_') %>% 
  str_remove_all(c(' |,|<|>|/|(|)')) %>% str_remove_all(':')
data[,sapply(.SD,\(x) sum(!is.na(x)))]

data = data %>% filter(!is.na(Bulk_Density_2mm_Fraction_Ovendry)&
                         !is.na(Estimated_Organic_Carbon_Total_C_S_prep))

setnames(data,c('Bulk_Density_2mm_Fraction_Ovendry','Estimated_Organic_Carbon_Total_C_S_prep'),
         c('BD','SOC'))

### get weighted averages
cols = colnames(data)[c(16:50)]
data = Get_Weighted_Avg_Depths_truncated(data,cols) %>% filter(!is.na(BD))

### Get TOC
om = data$SOC
bd = data$BD

# BD and SOC are in % weight
get_toc = function(om,bd,depth,scale='hectare'){
  # total carbon in metric tons = fraction of carbon (%om / 100) * BD g/cm * Volume m3
  area = switch(scale,
                'acre'=4046.86,
                'hectare'=10000)
  (om / 100) * bd * (area * depth/100) 
}

data$TOC = get_toc(data[,SOC],
                             data[,BD],
                             30)


# data$depth_range = get_breaks(data$Top_depth_cm,style = 'fixed',breaks=c(-.01,5,15,30,60,100,200),)
### Map
# x = get_SF(data %>% filter(!is.na(Long_Site)),c_cols=c('Long_Site','Lat_Site'))
# tm_basemap("Esri.WorldImagery") + tm_shape(x) + tm_dots('SOC')

### Plotting ----
data[,State_N :=.N,'State']
data[,County_N :=.N,'County']
tbl = data[data[,rev(order(State_N,County_N))],.(State,County,State_N,County_N) ] %>% unique
tbl

plot = copy(data) 
# plot = plot[State_N>150]
# plot = plot[County_N>8]
# plot[,.(.N,Cor=cor(SOC,BD)),c('State','County')]

### Data Cleaning ----
x = plot_missing(plot)
M_data = x$data
cols = M_data[order(M_data$pct_missing),
       .SD[ pct_missing > .25,feature]] %>% as.character #12
plot[,(cols):=NULL]

cols = colnames(plot)[9:27]
#### Imputation ----
library(missForest)
library(doParallel)

cl <- makePSOCKcluster(13)
registerDoParallel(cl)

imp = missForest(xmis = plot[,..cols],verbose = T,parallelize = 'forests',
                 maxiter = 10,variablewise = T)

stopCluster(cl)

Imp_Summary = data.table(Variable = colnames(imp$ximp),
           Mean = sapply(imp$ximp,mean),RMSE = sqrt(imp$OOBerror))
Imp_Summary[,Percent_Deviation := (RMSE/Mean)*100]
Imp_Summary = merge(Imp_Summary,M_data,by.x='Variable',by.y='feature') %>% 
  mutate_if(is.numeric, round,3) 
Imp_Summary

imputed_data = copy(plot)
data = plot
imputed_data[,(cols) := imp$ximp]


### Remove non modelling columns 

cols = colnames(plot)[-c(10,16,17,18,20,22,27)]
data[,(cols):=NULL]
imputed_data[,(cols):=NULL]

data = data[,.SD[apply(.SD,1,\(x) !any(is.na(x)) )]]


# plot = plot[SOC<10&BD<3]


# tmp = plot[,9:25]
# plot_prcomp(na.omit(tmp),
#             variance_cap = 0.9, nrow = 3L, ncol = 3L)#,prcomp_args = c(rank. = 109))

##### 5/6/22 ----
# hist(plot$SOC,50)
# hist(plot$BD,50)
# plot[,hist(BD*SOC)]
# 
# plot[,.(BD,SOC)] %>% cov

# ecdf_soc =ecdf(plot$SOC)
# ecdf_bd =ecdf(plot$BD)
# 
# seq = seq(min(plot$BD),max(plot$BD),.01)
# 
# get_ecdf =function(x){
#   x1 = ecdf(x)
#   sapply(seq(min(x),max(x),
#              length=1000),x1)
# }
# 
# get_ecdf(plot$BD) %>% plot
# get_ecdf(plot$SOC) %>% plot

# plot(seq,sapply(seq,ecdf_bd))
# 
# dx <- density(plot$BD)
# dx %>% plot
# 
# (sum(sapply(seq(1,1.5,.001),ecdf_bd))/length(seq(1,1.5,.001))) - 
#   sum(sapply(seq(0,1,.001),ecdf_bd))/length(seq(0,1,.001)) 

###
# Cor_plot = plot[,.(.N,Cor=cor(SOC,BD)),c('State','County')][rev(order(N)),]
# Cor_plot[,plot(N,Cor)]
# Cor_plot[rev(order(Cor)),plot(Cor)]

#### check soil texture ----
x = as.data.frame(plot[,c("Clay","Silt_Total","Sand_Total")])
x[is.na(x)] = 0
x = soiltexture::TT.normalise.sum(x %>% as.data.frame,
                                  css.names = c("Clay","Silt_Total","Sand_Total"))
if(any(is.na(rowSums(x)==0))){
  index = which(is.na(rowSums(x)==0))
  plot = plot[!index,]
  x=x[-index,]
}

x1 = soiltexture::TT.points.in.classes(x,class.sys = 'USDA.TT',
                                       css.names = c("Clay","Silt_Total","Sand_Total"))
plot$Soil_Texture  = apply(x1,1,function(x){
  which(x>0) %>% names
}) %>% unlist

plot_ly(x=plot$Silt_Total, y=plot$Sand_Total, z=plot$Clay, type="scatter3d",
        size=1,color=plot$Soil_Texture)

plot_ly(x=plot$BD, y=plot$SOC, z=plot$TOC, type="scatter3d",
        size=1,color=plot$Cor)


#### binned scatterplot ----

plot[,c('N','Cor'):=list(.N,cor(SOC,BD)),c('State','County')]
plot[,.(N=.N,Cor=Cor[[1]]),c('State','County')][rev(order(N))][N>7,plot(N,Cor)]
plot[,.(N=.N,Cor=Cor[[1]]),c('State','County')][rev(order(N))][N>7 & Cor>.45]
plot1 = plot[State=="New Mexico"&County=='Otero']

### Or 
plot1 = plot[N>7,]
plot1$cl = get_breaks(plot1$Cor,breaks=3)
plot1 = plot1[SOC<4&BD<3]
# plot1 = plot1[SOC<4&BD<3][cl=="(-0.283,0.412]"]

plot1 = plot1 %>% filter(
  Soil_Texture%in% names(which(plot1[,table(Soil_Texture)]>20))
)
plot1[,.(.N,Cor=cor(BD,SOC)),c('State','Soil_Texture')]
plot1[State=='Texas'&Soil_Texture=='SiLo',cor(BD,SOC)] #,method='spearman'

binsreg(x=plot1$BD,y=plot1$SOC,by=plot1$Soil_Texture)

binsreg(x=plot1$BD,y=plot1$SOC)
binsreg(x=plot1$BD,y=plot1$SOC,by=plot1$cl,line=c(2,2),ci=c(3,3))

binsreg(x=plot1$BD,y=plot1$SOC,by=plot1$cl,line=c(2,2))
binsreg(x=plot1$BD,y=plot1$SOC,by=plot1$Soil_Texture,line=c(2,2))

ggplot(plot1,aes(x=BD,y=SOC,col=Soil_Texture)) + geom_point() + theme_classic()


###

Cor_plot[N>7,hist(Cor)]


plot[!is.na(Cor)&County_N>7,sapply(.SD,\(x) cor(x,Cor)),.SDcols=cols]
tmp = plot[County_N>7]

source('../Soil_Stratification_Sampling/src/Functions/iRF_Wrapper.R')
cols = colnames(plot)[c(9:25)]
cols = cols[!cols%in%c('SOC','BD')]
cols = colnames(plot)[c(3,9:25,31)][c(-8,-16)]

single = Run_Model(plot,cols,cols_y ='Cor',Model='iRF',
                K=3,single=F,interactions = T,ncores = 10)

summary = Run_Model(plot,cols,cols_y ='Cor',Model='iRF',
                   K=3,single=F,interactions = T,summary = T,ncores = 10)

### Plotting ----

gg = ggplot(plot1 %>% filter(SOC<10),aes(x=SOC,y=BD,col=Soil_Texture)) + 
  # gg = ggplot(data %>% filter(SOC<5),aes(x=SOC,y=BD)) +
  geom_point(size=.7) + theme_classic() +
  geom_smooth(method = 'loess',se = F,span=.3,size=.5) 
ggMarginal(gg,type="density",groupFill=T,size = 10,bins=60)  
ggMarginal(gg,type="violin",groupFill=T,size = 10)  
ggMarginal(gg,type="box",groupFill=T,size = 10)  
ggMarginal(gg,type="density")




# violin 
ggplot(data %>% filter(SOC<5),aes(x=depth_range,y=SOC,fill=depth_range)) + theme_classic() + geom_violin()  +
  geom_boxplot(width=.03,outlier.size=.5,outlier.alpha = 0,alpha=0)


# Facet histograms
tmp = plot[!is.na(Cor)&County_N>7,]
tmp$Cor_gp = get_breaks(tmp$Cor,n = 3,style = 'quantile') 
tmp$Cor_gp = get_breaks(tmp$Cor,n = 2,breaks = c(-1,0,1),style = 'fixed') 
plot1 = melt(tmp,measure.vars = c(cols,'Cor'))

ggplot(plot1,aes(x=value,fill=Cor_gp))+
  geom_histogram(bins=50,alpha=.8,position = 'identity')+
  # geom_density(alpha=.3) +
  facet_wrap(~variable,scales = 'free') +
  theme_classic(base_size = 16)  + scale_fill_brewer(palette = 'Dark2')


### Umap ----
library(umap)
library(dbscan)

index = plot[,..cols] %>% apply(1,\(x) any(is.na(x))) 

tmp = plot[!index,]
umap <- umap(tmp[,..cols],n_components=2,n_neighbors=10)
umap <- as.data.frame(umap$layout)
colnames(umap) <- c('x','y')

ggplot(data=umap) + geom_point(aes(x=x,y=y)) + theme_classic()

cl <- hdbscan(umap,minPts = 25)
cl <- as.character(cl$cluster)

ggplot(data=umap) + geom_point(aes(x=x,y=y,col=cl)) + theme_classic()

## 3D
umap <- umap(tmp[,..cols],n_components=3,n_neighbors=20)
umap <- as.data.frame(umap$layout)
colnames(umap) <- c('x','y','z')
cl <- hdbscan(umap,minPts = 50)
cl <- as.character(cl$cluster)

plot_ly(x=umap$x, y=umap$y, z=umap$z, type="scatter3d",color=cl,size=1)

#### hclust ----
M = dist(umap)
M = dist(plot[,..cols])
hc = hclust(M,method = 'ward.D2')

hclusCut <- function(x, k, d.meth = 'ward.D2', ...){
  list(cluster = cutree(hclust(as.dist(x), method=d.meth), k=k))
  # list(cluster = cutree(hcut(as.dist(x), method=d.meth), k=k))
}
K_max = 10

gs <- cluster::clusGap(M %>% as.matrix, FUN = hclusCut, K.max = 10, B = 30)
gs$Tab = gs$Tab[!index,]
K = cluster::maxSE(f = gs$Tab[, "gap"], SE.f = gs$Tab[, "SE.sim"],
                   method = "Tibs2001SEmax", SE.factor = 1)

factoextra::fviz_nbclust(M %>% as.matrix,FUNcluster =  hclusCut,
                         k.max = K_max,method = "silhouette") + 
  labs(subtitle = "Silhouette method",)
factoextra::fviz_nbclust(M %>% as.matrix,FUNcluster =  hclusCut,
                         k.max = K_max,method = "wss") + 
  labs(subtitle = "Elbow method")

K = 5
row_dend = as.dendrogram(hclust(M,method='ward.D2'))
row_dend = dendextend::color_branches(row_dend, k = K)
plot(row_dend)
cl = hclusCut(M,K)$cluster %>% as.factor()

plot_ly(x=umap$x, y=umap$y, z=umap$z, type="scatter3d",color=cl,size=1)
tmp$cl = cl

# Soil_Texture
plot_ly(x=plot$Silt_Total, y=plot$Sand_Total, z=plot$Clay, type="scatter3d",
        size=1,color=cl)

## umap low d
umap_2d <- umap(tmp[,..cols],n_components=2,n_neighbors=10)
umap_2d <- as.data.frame(umap_2d$layout)
cl <- hdbscan(umap,minPts = 30)
cl <- as.character(cl$cluster)
x = umap %>% as.data.table
x$cl = cl
colnames(umap_2d) <- c('x','y')
# cl = cl %>% as.character

ggplot(data=umap_2d) + geom_point(aes(x=x,y=y,col=cl)) + theme_classic()

## hi d
umap <- umap(tmp[,..cols],n_components=10,n_neighbors=10)
umap <- as.data.frame(umap$layout)
cl <- hdbscan(umap,minPts = 30)
cl <- as.character(cl$cluster)
x = umap %>% as.data.table
x$cl = cl

hi_d_plot = x %>% melt(id.vars='cl')
ggplot(hi_d_plot,aes(x=value,fill=cl))+
  geom_histogram(bins=50,alpha=.8,position = 'identity')+
  # geom_density(alpha=.3) +
  facet_wrap(~variable,scales = 'free') +
  theme_classic(base_size = 16)  + scale_fill_brewer(palette = 'Dark2')

cols_tmp = c(cols,c('cl','Cor'))
hi_d_plot = tmp[,..cols_tmp] %>% melt(id.vars='cl')
ggplot(hi_d_plot,aes(x=value,fill=cl))+
  geom_histogram(bins=50,alpha=.8,position = 'identity')+
  # geom_density(alpha=.3) +
  facet_wrap(~variable,scales = 'free') +
  theme_classic(base_size = 16)  + scale_fill_brewer(palette = 'Dark2')

#### ggpairs ----
library(GGally)
x1 = ggpairs(plot[,..cols],mapping = aes(color = cl),
             upper = list(continuous = wrap('cor',size=2), combo = "box_no_facet",
                          discrete = "count"),
             lower = list(continuous = wrap("points", alpha = 0.4,size=0.075), 
                          combo = "facethist", discrete = "facetbar"),
             diag = list(continuous = wrap("densityDiag",alpha=.5), discrete = "barDiag")
             )
             
x1 + theme_classic()


#### Texture Clustering----
plot1 = copy(plot)
plot1 = plot1[SOC<5,]

# cols = colnames(plot)[c(16:28)]
cols = c("Clay","Silt_Total","Sand_Total","BD",'SOC')
cols = c("BD",'SOC')
M = dist(plot1[,..cols])

hclusCut <- function(x, k, d.meth = 'ward.D2', ...){
  list(cluster = cutree(hclust(as.dist(x), method=d.meth), k=k))
  # list(cluster = cutree(hcut(as.dist(x), method=d.meth), k=k))
}

K = 5
row_dend = as.dendrogram(hclust(M,method='ward.D2'))
row_dend = dendextend::color_branches(row_dend, k = K)
plot(row_dend)
cl = hclusCut(M,K)$cluster %>% as.factor()

# Soil_Texture
plot_ly(x=plot1$Silt_Total, y=plot1$Sand_Total, z=plot1$Clay, type="scatter3d",
        size=1,color=cl)
plot1$cl = cl
cols1 = c(cols,'cl')
cols1 = c(cols,'Soil_Texture')
cols1 = c(cols,'fiscal_year')
cols1 = c(cols,'County')
cols1 = c('SOC','BD','TOC')

# index = which( table(plot1$fiscal_year) > 100) %>% names()
# plot1$fiscal_year = as.character(plot1$fiscal_year)
# plot1 = plot1[as.character(fiscal_year)%in%index]
cols = colnames(plot1)[c(9:25,31)][c(-8,-16)]
# x1 = ggpairs(plot[,..cols1],mapping = aes(color = fiscal_year),
x1 = ggpairs(plot[,..cols1],
             upper = list(continuous = wrap('cor',size=2.5), combo = "box_no_facet",
                          discrete = "count"),
             lower = list(continuous = wrap("points", alpha = 0.4,size=0.075), #0.075 
                          combo = "facethist", discrete = "facetbar"),
             diag = list(continuous = wrap("densityDiag",alpha=.5), discrete = "barDiag")
)

x1 + theme_classic()


### Functions ----

Get_Weighted_Avg_Depths_truncated = function(data,cols){
  parallel::mclapply(unique(data$id),mc.cores = 8,\(x){
    x1 = data[id==x]
    x1 = x1[Top_depth_cm<30]
    
    # if greater than 30cm, set to 30cm
    x1[Bottom_depth_cm>30,Bottom_depth_cm := 30]
    
    weights = x1[,Bottom_depth_cm-Top_depth_cm] / 30 
    x2 = x1[,lapply(.SD,weighted.mean,weights,na.rm=T),.SDcols=cols] 
    cbind(x1[1,1:8],x2)
}) %>% rbindlist
}


### Scratch


gg = ggplot(plot[CS_N>100&Top_depth_cm<15&`Estimated Organic Carbon, Total C, S prep`<2.5&`Estimated Organic Carbon, Total C, S prep`>0],
            aes(color=County,fill=County,x=`Bulk Density, <2mm Fraction, 1/3 Bar`,y=`Estimated Organic Carbon, Total C, S prep`)) +
  geom_point(alpha=1,size=.3)  + geom_line(stat='smooth',se = T,method = 'loess',span=8) + theme_classic()
ggMarginal(gg,type="density",groupFill=T,size = 10,bins=60) 

