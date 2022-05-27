source('src/General_Setup_Edit.R')

library(SmartEDA)

data = fread('ARF_Simulation/data/input/Leflore_Sets/Input_Sets/Full.csv')
data = data %>% filter(cl==1)

tmap_options(max.raster = c(plot = 1e7, view = 1e8)) #$1e6
quick_map(data,'topo_slope',basemap = T,mode='plot',samples=F,style = 'quantile',Lhist = T,n=10,SB_Pos = 'left')

dir.create('scratch')
data[,6:42] %>% colnames
ExpReport(
  plot,
  Target="Cor",
  label=NULL,
  op_file="scratch/EDA_Report_USDA.html",
  op_dir=getwd())

library(DataExplorer)
configure_report(
  # add_plot_str = FALSE,
  # add_plot_qq = FALSE,
  # add_plot_prcomp = FALSE,
  # add_plot_boxplot = FALSE,
  # add_plot_scatterplot = FALSE,
  global_theme_config = 
    quote(theme_classic(base_size = 18))
)

cols = colnames(plot)[c(9:26)]
plot[,..cols] %>%
  create_report(
    output_file = 'EDA_Report_USDA_BD.html_Subset',
    report_title = "EDA Report - USDA Data - BD",
    y = "BD",
    config = configure_report(global_theme_config = 
                                quote(theme_classic(base_size = 16))))

  
