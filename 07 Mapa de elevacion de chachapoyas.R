
library(ggplot2)
library(raster)
library(tidyverse)
library(elevatr)
library(sf)
library(sp)
library(ggspatial)
library( maptools)
library(ggnewscale)
library(tmap)
#-----------------------------------------------------------------------------------
Peru_p  <- getData('GADM', country='Peru', level=3) %>% st_as_sf()
chachapoyas  <- subset(Peru_p, NAME_3  == "Chachapoyas")
Huancas      <- subset(Peru_p, NAME_3  == "Huancas")
chachapoyas_xy <- cbind(chachapoyas, st_coordinates(st_centroid(chachapoyas$geometry)))
Huancas_xy <- cbind(Huancas , st_coordinates(st_centroid(Huancas$geometry)))
Ecosistemas_chacha <- st_read ("SHP/Ecosistemas_chacha.shp")
chachapoyasunion   <- st_read ("SHP/chachapoyasunion.shp")
dem12 = raster("Raster/ASTGTM_S07W078_dem.tif")

Area_alt <- crop(dem12,chachapoyasunion)                           #   
Area_alt<- Area_alt<- mask(Area_alt, chachapoyasunion)
plot(Area_alt)
dem.p          <-  rasterToPoints(Area_alt)
df             <-  data.frame(dem.p)
colnames(df) = c("lon", "lat", "alt")

aps            = terrain(Area_alt , opt = "aspect", unit= "degrees")
dem.pa          <-  rasterToPoints(aps  )
df_a            <-  data.frame(dem.pa)

slope = terrain(Area_alt , opt = "slope") 
aspect = terrain(Area_alt , opt = "aspect")
hill = hillShade(slope, aspect, angle = 40, direction = 270)

hill.pa          <-  rasterToPoints(hill)
hill.pa_a            <-  data.frame(hill.pa)

cortes <- c(1000,1500,2000,2500, 3000, 3350)
cols <-c("#5F8141","#779F44","#93C649","#A9DD55","#CBD689","#ECE5B2","#E1C678","#978055","#45280E")

ggplot()+
  geom_sf(data =chachapoyas)+
  geom_sf(data =Huancas)+
  geom_raster(data = df , aes(lon,lat, fill = alt), size =0.2)+
  scale_fill_gradientn(colours = cols,
                       na.value = 'white',name="Elevacion\n(m.s.n.m)",breaks = cortes ,
                       labels = c("[menor a - 270] ","[270 - 499]", "[500 - 999]", "[1000 - 1999]", "[2000 - 2999]",
                                  "[3000 - 3999]"))+
  new_scale_fill() +
  geom_sf(data =Ecosistemas_chacha , aes(alpha=ECO_LAYER), alpha = 0.5) +
  
  geom_raster(data = df_a, aes(x=x, y=y, alpha=aspect),fill="gray20")+
  scale_alpha(guide=FALSE,range = c(0,1.00))   +
  geom_label(data =  chachapoyas_xy , aes(x= X, y=Y, label = NAME_3), size = 2.5, color="black", fontface = "bold",fontfamily = "serif")+
  geom_label(data =  Huancas_xy , aes(x= X, y=Y, label = NAME_3), size = 2.5, color="black", fontface = "bold",fontfamily = "serif") +
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme_bw()+
  labs(x="",y="",title  = "")+
  theme(panel.grid.major = element_line(color = gray(.7),linetype = "dashed", size = 0.2),
        axis.text = element_text(colour = "black", size = 8),
        axis.text.y  = element_text(angle = 90),
        plot.title = element_text(size = 21, hjust = 0.5, color = "#4e4d47", family="serif"),
        plot.subtitle = element_text(size = 11, hjust = 0.8, face = "italic", color = "#4e4d47"),
        plot.caption = element_text(size = 8, hjust = 0.95, color = "#4e4d47", family="serif"),
        plot.margin = unit(c(.5,.5,.2,.5), "cm"),
        legend.text =element_text(size=9, family="serif"),
        legend.title = element_text(size=9, family="serif"),
        legend.key.size = unit(0.3, "cm"), 
        legend.key.width = unit(0.9,"cm"),
        legend.position = c(0.92,0.17))


Grafica=ggplot()+
  geom_raster(data = hill.pa_a, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill() +
  geom_raster(data = df_a, aes(x=x, y=y, alpha=aspect),fill="gray20")+
  scale_alpha(guide=FALSE,range = c(0,1.00))   +
  new_scale_fill() +
  geom_raster(data = df , aes(lon,lat, fill = alt), size =0.2, alpha=0.6)+
  scale_fill_gradientn(colours = cols,
                       na.value = 'white',name="Elevacion\n(m.s.n.m)",breaks = cortes ,
                       labels = c("[1000 - 1499]", "[1500 - 1999]",
                                  "[2000 - 2499]","[2500 - 2999]", "[3000 - 3299]","[3300 - 3350]"))+
  geom_label(data =  chachapoyas_xy , aes(x= X, y=Y, label = NAME_3), size = 2.5, color="black", fontface = "bold",fontfamily = "serif")+
  geom_label(data =  Huancas_xy , aes(x= X, y=Y, label = NAME_3), size = 2.5, color="black", fontface = "bold",fontfamily = "serif") +
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(.7),linetype = "dashed", size = 0.2),
        axis.text = element_text(colour = "black", size = 8),
        axis.text.y  = element_text(angle = 90),
        plot.title = element_text(size = 21, hjust = 0.5, color = "#4e4d47", family="serif"),
        plot.subtitle = element_text(size = 11, hjust = 0.8, face = "italic", color = "#4e4d47"),
        plot.caption = element_text(size = 8, hjust = 0.95, color = "#4e4d47", family="serif"),
        plot.margin = unit(c(.5,.5,.2,.5), "cm"),
        legend.text =element_text(size=9, family="serif"),
        legend.title = element_text(size=9, family="serif"),
        legend.key.size = unit(0.3, "cm"), 
        legend.key.width = unit(0.6,"cm"),
        legend.position = c(0.8,0.6))+
  geom_sf(data =chachapoyas, fill=NA)+
  geom_sf(data =Huancas, fill=NA)+
  new_scale_fill() +
  geom_sf(data =Ecosistemas_chacha , aes(fill=ECO_LAYER), alpha = 0.2) +
  scale_fill_brewer(palette   = "Greens")+
  labs(x="",y="",title  = "")+
  guides(fill = guide_legend())+
  annotate(geom = "text", x = -77.82, y = -6.10, label = "MAPA de RELIEVE \ndel", family="serif", color = "black", size = 3)+
  annotate(geom = "text", x = -77.82, y = -6.11, label = "DISTRITO DE CHACHAPOYAS \nY HUANCAS", family="serif", color = "black", size = 5)+
  annotate(geom = "text", x = -77.81, y = -6.29, label = "Data: DEM SRTM \n#Aprende R desde Cero Para SIG \nGorky Florez Castillo", family="serif", color = "black", size =3)+
  labs(title = "Mapa de elevacion de y relive",
       #subtitle  = "Distritos de arequipa",
       caption = "Author: Gorky Florez (@gflorezc) Original Idea: Aprende R desde cero, Geometries: RStudio Data: ING-Peru, 2022;",
       size = "f")

ggsave(plot = Grafica  ,"Mapas/Altitud de Chachapoyas.png", units = "cm", 
       width = 21,height = 21, dpi = 900)
  
#------------------------------------------------------------------------
cols <-c("#5F8141","#779F44","#93C649","#A9DD55","#CBD689","#ECE5B2","#E1C678","#978055","#45280E")
map =tm_shape(hill) +
  tm_raster(palette = gray(0:100 / 100), style = "cont", legend.show = FALSE)+
  tm_shape(chachapoyasunion)+
  tm_borders("white",lwd=2)+
  tm_text("NAME_3",size = .9, col="black",shadow=TRUE,
          bg.color="white", bg.alpha=.45)+
  tm_shape(Area_alt) +
  tm_raster(alpha = 0.5, palette = cols,n=12,
            legend.show = T, title="Elevacion(m.s.n.m)")+
  tm_compass(type="arrow", position=c("right", "top"), text.color = "black")+
  tm_scale_bar(width = 0.25, text.size = 0.5, text.color = "black", color.dark = "lightsteelblue4", 
               position = c(.7, 0.005), lwd = 1, color.light= "white")+
  tm_shape(Ecosistemas_chacha)+
  tm_polygons("ECO_LAYER", style="quantile", palette = "Greens", n=25, legend.show = T, alpha = 0.2,
              title="Ecosistemas \nChachapoyas")+
  tm_layout(title = "MAPA de RELIEVE del \nDistrito de Chachapoyas", fontfamily = "serif",
            title.position =  c(.5, .9),
            panel.labels = c("R y RStudio con paquete Tmap"),
            legend.position = c(.75, .4))+
  tm_credits("Data: DEM SRTM \n#Aprende R desde Cero Para SIG \nGorky Florez Castillo",  bg.color="white", bg.alpha=.45,
             position = c(0.37, 0.0001), col = "black", fontface="bold", fontfamily = "serif")

  
tmap_save(map , "Mapas/Mapa de la reserva del manu.png", dpi = 1200, height = 10)
  
  


