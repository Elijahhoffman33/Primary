source('src/General_Setup_Edit.R')



microclimates = st_read('USDA_Grant_Figures/data/microclimate_v3.gpkg')

# basemap prep
bbox_new = st_bbox(microclimates)
basemap = basemaps::basemap(bbox_new)

Sites = data.table(Name=c('Arva Intelligence Research Farm',
                          'University of Madison, Wisconsin',
                          'Conservation Technology Information Center',
                          'Prairie View A&M University (HBCU)',
                          'Prairie View A&M University (HBCU)'),
                   lng = c(-91.70,-89.52,-87.16,-95.94,-101.99),
                   lat = c(34.42,44.13,40.76,30.09,33.70)
                   ) %>% st_as_sf(coords = c('lng','lat'))

text = c('  Arva Intelligence Research Farm\n  Crops: rice, corn, soybeans, sorghum\n  Acres: 60,000\n  Towers: 4',
         
         'University of Madison, Wisconsin\nCrops: corn, potatoes\nAcres: 60,000\nTowers: 4',
         
         'Conservation Technology Information Center\nCrops: corn, soybeans, wheat\nAcres: 60,000\nTowers: 4',
         
         'Prairie View A&M University (HBCU), Site 1\nCrops: rice, corn, sorghum\nAcres: 60,000\nTowers: 4',
         
         'Prairie View A&M University\n(HBCU), Site 2\nCrops: cotton, corn, sorghum\nAcres: 60,000\nTowers: 4')
Sites$text = text
icon = tmap_icons(file='USDA_Grant_Figures/data/png-transparent-eddy-covariance-li-cor-biosciences-infrared-gas-analyzer-biotechnology-licor-biotechnology-anemometer-pyranometer-removebg-preview.png')
icon_arva = tmap_icons(file='USDA_Grant_Figures/data/Arva_Emblem_cropped.png')
tmap_mode('plot')
t = tm_shape(basemap) + tm_rgb() +
tm_shape(microclimates) + tm_polygons(col='DN',palette='inferno',legend.show = FALSE,
                                      style='cont',alpha=.8) +
  tm_shape(Sites) + tm_symbols(shape = 18,col = '#009400',size=3)
  # tm_shape(Sites) + tm_symbols(shape = icon_arva, size = 1,ymod=1,border.lwd = NA)
# tm_shape(Sites) + tm_symbols(shape = icon, size = 4, border.lwd = NA,ymod = 1.3) 

  # tm_text('text',just='left',size = .5,xmod = .6,ymod=.91) +
  # tm_style('white', frame = T, fontface = 2)
t
tmap_save(t,filename = 'USDA_Grant_Figures/figure_diamond_1.png',width = 1920,1080)
tmap_save(t,filename = 'USDA_Grant_Figures/USDA_Grant_Figure_seperate_basemap.html')

### 'View' version ----
tmap_mode('view')
tv = tm_basemap("Esri.WorldImagery") +
 tm_shape(microclimates) + tm_polygons(col='DN',palette='inferno',legend.show = FALSE,
                                      style='cont',alpha=.8) +
  tm_shape(Sites) + tm_symbols(shape = 18,col = '#009400',size=3)

tmap_save(tv,filename = 'USDA_Grant_Figures/USDA_Grant_Figure.html')


tm = tmap_grob(t)

ibrary(ggplot2)
library(tmap)
library(cowplot)
library(ggplotify)
library(ggtext)
Sites = st_set_crs(x,st_crs(microclimates))
coords = st_coordinates(Sites) %>% as.data.frame

# text box
df = data.frame(
  label = rep("Lorem ipsum dolor **sit amet,** consectetur adipiscing elit,
    sed do *eiusmod tempor incididunt* ut labore et dolore magna
    aliqua.", 2),
  x = c(-2026762.18, 3176581.03),
  y = c(14529801, 16224220),
  hjust = c(0, 0),
  vjust = c(0, 0),
  orientation = c("upright", "right-rotated"),
  color = c("black", "blue"),
  fill = c("cornsilk", "white")
)

ggplot(microclimates) + geom_sf() +
geom_textbox(data=df,aes(x=x,y=y),label= 'hek')
# annotate("label", x = min(coords$X), y = max(coords$X), label = "avg rate") 
ggplot()

tm1 <- tmap_grob(t)

plot_grid(tm1,gg)



