# Cargando librerías ------------------------------------------------------
library(magrittr)
c("dplyr", "tidyr", "lubridate", "ggplot2", "lattice", "tilegramsR",
  "ggmap", "RgoogleMaps", "data.table", "OpenStreetMap", "nominatim",
  "httr", "XML", "RCurl", "rvest", "reshape2", "leaflet", "maptools",
  "sp", "rgdal") %>% 
  sapply(require, character.only=T)


####################################   
#Creado por Fernando Dorantes Nieto <(°) 
#                                     ( >)
#                                      /| 
####################################

# Funciones ---------------------------------------------------------------
"%!in%" <- function(x,y)!("%in%"(x,y))

cuantos = function(x) {
  sapply(x, function(y){
    y %>%  strsplit(" ") %>%  unlist %>%  length
  })
}

prueba = "Estaba la pájara pinta sentada bajo el verde limón"
cuantos(prueba)

# Lista de paises ---------------------------------------------------------
paises = "https://simple.wikipedia.org/wiki/List_of_countries" %>%  
  read_html() %>%   html_nodes("a") %>%  html_text() %>%  .[35:293]

paises = gsub("change|change source", NA, paises) %>% 
  na.omit() %>%  as.character()
paises = gsub("People's Republic of China", "China", paises, fixed=T)
paises = gsub("Republic of China", "Taiwan", paises )
paises = gsub(" (Aotearoa)", "", paises, fixed = T)
paises = c(paises, "Antarctica")
# paises = gsub("Democratic Republic of the Congo", "Congo", paises, fixed = T)   
# paises = gsub("Republic of the Congo", "Congo", paises, fixed = T)
# paises = gsub("Republic of the Congo", "Congo", paises, fixed = T)

# Estudio aves ------------------------------------------------------------
aves = read.csv("~/Descargas/Clements-Checklist-v2017-August-2017.csv",
                header = T)
names(aves)<- tolower(names(aves))
extintas = aves %>%  filter(!is.na(extinct)) %>%  filter(category=="species")

extintas %>%  group_by(family) %>%  tally %>% 
  ggplot(aes(x= family, y=n))+ geom_bar(stat="identity", fill="steelblue")+
  theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1))

extintas %>%  group_by(order) %>%  tally %>% 
  ggplot(aes(x= reorder(order, -n), y=n))+ geom_bar(stat="identity", fill="steelblue")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  geom_text(aes(label=n),
            position=position_dodge(width=0.9), vjust=-0.25)

aves = aves %>%  filter(range!="") %>%  
  select(category, scientific.name,range, order, family) %>% 
  .[aves$scientific.name %!in%  extintas$scientific.name,] 

ave = aves %>%  select(scientific.name, range) %>% as.matrix
x = ave[, 2]
lugar = strsplit(x, " ")
names(lugar) = ave[, 1]
lugar = melt(lugar)
names(lugar)<- c("Lugar", "Especie")
lugar = lugar %>% 
  mutate(Lugar = gsub("\\(", "", Lugar)) %>% 
  mutate(Lugar = gsub("\\)", "", Lugar)) %>% 
  mutate(Lugar = gsub("\\[", "", Lugar)) %>% 
  mutate(Lugar = gsub("\\]", "", Lugar)) %>% 
  mutate(id= 1:length(Lugar))

p = lugar %>% 
  mutate(Lugar = gsub("\\(|\\)|\\[|\\]", "", Lugar)) %>% 
  mutate(id= 1:length(Lugar))


####sitio para pruebas
indice  = sapply(paises, grep,lugar$Lugar %>%  as.character)
#indice  = sapply(paises, agrep,lugar$Lugar %>%  as.character, max.distance=0.2)
indice2 = sapply(seq_along(indice), function(i) rep(i, length(indice[[i]]))) 
lugarT = cbind(paises[unlist(indice2)], lugar[unlist(indice),, drop=F])
lugar = lugar %>%
  mutate(Lugar = gsub("America|US|States|United", "United States", Lugar)) %>% 
  mutate(Lugar = gsub("Zealand", "New Zealand", Lugar)) %>% 
  mutate(Lugar = gsub("Siberia", "Russia", Lugar)) %>% 
  mutate(Lugar = gsub("Rica", "Costa Rica", Lugar, fixed=T)) %>% 
  mutate(Lugar = gsub("Lanka", "Sri Lanka", Lugar, fixed=T)) %>% 
  mutate(Lugar = gsub("Salvador", "El Salvador", Lugar, fixed=T)) %>% 
  mutate(Lugar = gsub("Arabia", "Saudi Arabia", Lugar, fixed=T)) %>% 
  mutate(Lugar = gsub("England", "United Kingdom", Lugar, fixed=T)) %>% 
  mutate(Lugar = gsub("Scotland", "United Kingdom", Lugar, fixed=T)) %>% 
  mutate(Lugar = gsub("Barbuda", "Antigua and Barbuda", Lugar, fixed=T)) %>% 
  mutate(Lugar = gsub("Bahamas", "The Bahamas", Lugar, fixed=T)) %>%
  mutate(Lugar = gsub("Africa", "South Africa", Lugar, fixed=T)) %>% 
  mutate(Lugar = gsub("Rico", "Puerto Rico", Lugar, fixed=T)) %>% 
  mutate(Lugar = gsub("Tobago", "Trinidad and Tobago", Lugar, fixed=T)) %>% 
  mutate(Lugar = gsub("African", "Central African Republic", Lugar, fixed=T)) %>% 
  mutate(Lugar = gsub("Papua", "Papua New Guinea", Lugar, fixed=T)) %>% 
  mutate(Lugar = gsub("Dominican", "Dominican Republic", Lugar, fixed=T)) %>% 
  mutate(Lugar = gsub("Malay", "Malaysia", Lugar, fixed=T)) %>% 
  mutate(Lugar = gsub("Congo", "Democratic Republic of the Congo", Lugar)) %>% 
  mutate(Lugar = gsub("Java", "Indonesia", Lugar)) %>% 
  mutate(Lugar = gsub("Borneo", "Indonesia", Lugar)) %>% 
  mutate(Lugar = gsub("Sumatra", "Indonesia", Lugar)) %>% 
  mutate(Lugar = gsub("Celebes|Sulawesi", "Indonesia", Lugar)) %>% 
  mutate(Lugar = gsub("Antarctic|Antarctica", "Antarctica", Lugar)) %>% 
  mutate(Lugar = gsub('"', '', Lugar, fixed=T)) %>% 
  mutate(Lugar = gsub(';', '', Lugar, fixed=T)) %>% 
  mutate(Lugar = gsub('.', '', Lugar, fixed=T)) %>% 
  mutate(Lugar = gsub(',', '', Lugar, fixed=T)) 
  
  
names(lugarT)<- c("Lugar1", "Lugar", "Especie", "id")
lugarT = lugarT[grepl("^[[:upper:]]", lugarT$Lugar1),]
lugar = lugar[grepl("^[[:upper:]]", lugar$Lugar),]
lugar = lugar[lugar$Lugar %in% paises,] 
lugarT = lugarT %>% mutate(Lugar1= gsub("Niger", "Nigeria", Lugar1))%>% 
  mutate(Lugar1=gsub("Tibet", "China", Lugar1)) %>% 
  select(-Lugar)

names(lugarT)<- names(lugar)
lugar =   rbind(lugarT, lugar) %>%  distinct(id, .keep_all=T) 

subespecies = lugar %>% 
  mutate(ifelse(cuantos(Especie)==2, "Especie", "Subespecie"))

Especie = lugar %>%
  mutate(EspecieReal =ifelse(cuantos(Especie)>2, 
                         gsub("\\s*\\w*$", "",Especie), Especie)) 

aves = aves %>%  select(scientific.name, order, family)
aves = merge(aves, Especie, by.x ="scientific.name", by.y="Especie" ) 
aves = aves %>%  mutate(Family = gsub("[[:space:]].*", "",family)) 

especies = aves %>%  group_by(Lugar) %>% 
  distinct(scientific.name, .keep_all=T) %>% tally
ordenes = aves %>%  group_by(Lugar, order) %>%  tally
familias = aves %>%  group_by(Lugar, Family) %>%  tally

lugares = aves$Lugar %>%  factor %>%  levels
coordenadas = lapply(lugares, function(x){
  return(tryCatch(
    osm_search(x, key = "KMB2SievzX7u2qGzpAzx59lG8p5fCtqK") 
    %>%  mutate(Lugar = x),
    error= function(e) next
  ))  
})
coordenadas = rbindlist(coordenadas, fill=T)
coordenadas %>%  write.csv("~/Documentos/personales/coordenadas.csv",
                           row.names=F)
coordenadas = read.csv("~/Documentos/personales/coordenadas.csv", header = T)
coordenadasA = coordenadas %>%  select(lat, lon, Lugar) 
names(coordenadasA)<-c("lat", "long", "Lugar")
aves = merge(aves, coordenadasA, by="Lugar")
paises[paises %!in% aves$Lugar]

especies = aves %>%  group_by(Lugar,lat, long) %>% 
  distinct(scientific.name, .keep_all=T) %>%  tally

data("wrld_simpl")
wrld_simpl$NAME
afr  <- wrld_simpl[wrld_simpl$REGION==2,]
asia <- wrld_simpl[wrld_simpl$REGION==142,]
eur  <- wrld_simpl[wrld_simpl$REGION==150,]
oce  <- wrld_simpl[wrld_simpl$REGION==9,]  
ame  <- wrld_simpl[wrld_simpl$REGION==19,]  
ant  <- wrld_simpl[wrld_simpl$REGION==0,]  

mundo = wrld_simpl
# mundo <- raster::shapefile(
#   "~/Documentos/personales/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp")
mundo_mapa = fortify(mundo)
ame_mapa = fortify(ame)
bnd_box <- bbox(SpatialPoints(as.matrix(mundo_mapa[, c("long", "lat")])))
data1 = data.frame(id = rownames(mundo@data), estado = mundo@data$NAME) %>% 
  mutate(estadoREAL = gsub("\\s*\\([^\\)]+\\)","",estado))

perdidos = data1[data1$estadoREAL %!in% especies$Lugar,] %>%  
  select(id , estado) %>% 
  mutate(n= 0) %>%  
  mutate(estadoREAL = gsub("\\s*\\([^\\)]+\\)","",estado)) %>% 
  select(estadoREAL, id, estado, n)


data1 = merge(data1, especies, by.x = "estadoREAL", by.y = "Lugar") %>%
  select(-long,-lat)
data1 = rbind(data1, perdidos)
data1 = merge(mundo_mapa, data1, by="id")
data1 %>%  head
data1 = data1 %>%  
  mutate(categoria = ifelse(n<100, "baja_diversidad",
                            ifelse(n>800, "alta_diversidad",
                                          "mediana_diversidad"))) %>% 
           mutate(categoria= ifelse(n==0, "Sin información", categoria))

png("~/Documentos/presentacionAves/graficos/mapaAves.png",
    width = 1200, height = 600, res = 150)
ggplot(data1, aes(x = long, y=lat, group = group))+
  geom_polygon(aes(fill=categoria))+
  #scale_fill_gradient(low ="gray", high = "blue", name="Importancia")+
  labs(x="",y="")+ theme_bw()+
  theme(legend.position = "top", legend.text = element_text(size=12),
        legend.title = element_blank())+
  coord_fixed()
dev.off()
# ggplot() + geom_map(data = mexico_mapa, map = mexico_mapa,
#                     aes(x = long, y = lat, map_id = id),
#                         color ="black", fill="white", size=0.1)+
#   geom_polygon(data = subregionP, aes(x= lon, y=lat, color=México))+
#   coord_map("lagrange", xlim = c(-120,-85), ylim=c(12,35))

write.csv(data1, "~/Documentos/personales/mapa1.csv", row.names = F)

library(cartogram)
library(tmap)
library(rgdal)
library(sp)


m= mundo 
m@data %>%  head
P = data1 %>% 
  select(estado, n, categoria) %>%  distinct(estado, .keep_all=T)


names(P)<- c("NAME", "N", "Categoria")
m@data <- merge(m@data, P, by="NAME") 
m@data  = m@data[order(match(m@data$NAME, mundo@data$NAME)),]
M= m[m$REGION==19,]
M = spTransform(M, CRS("+init=epsg:28992"))
M = spTransform(M, CRS("+init=epsg:3395"))

N= m[m$REGION==19,]
test = cartogram(N, "N", itermax=1)
tm_shape(test) + tm_fill("N", style="jenks") + 
  tm_borders() + tm_layout(frame=F)

M@data[,"N"]
leaflet(m) %>%  addPolygons()

tipos = test@data$Categoria
factpal <- colorFactor(colormap::colormap(
  nshades = length(tipos)),tipos)
leaflet(test) %>%
  addPolygons(weight=2,color='#000000', group = 'Categoria',
              fillOpacity = 0.6, opacity = 1,fillColor= ~factpal(tipos)) %>% 
  
  styleMap


leaflet(M, options=leafletOptions(
  crs = leafletCRS("+init=epsg:28992"))) %>% addPolygons()


###TESTEOS
library(getc)

styleMap <- function(
  map, style = list(background='transparent')) {
  map %>%  htmlwidgets::onRender(
    JS("function(el, x, style) {
      var myMap = this;
      if($.isEmptyObject(myMap._container.style)) {
        myMap._container.style = {};
      }
      $.each(style, function(key, value) {
        myMap._container.style[key] = value;
      });
    }"),
    data = style
  )
}


tipos = m@data$Categoria
factpal <- colorFactor(colormap::colormap(
  nshades = length(tipos)),tipos)
leaflet(m,
        options=leafletOptions(
          crs = leafletCRS("L.CRS.Simple"),
          minZoom = -1.5, maxZoom = -1.5,
          dragging = FALSE, zoomControl = FALSE,
          attributionControl = FALSE)) %>%
  addPolygons(weight=2,color='#000000', group = 'states',
              fillOpacity = 0.6, opacity = 1,fillColor= ~factpal(tipos)) %>% 
  
  styleMap

NPR1to1.centers$tilegramVa
NPR1to1 %>%  plot
Pitch_US_Population_2016_v1.centers@data

Pitch_US_Population_2016_v1 %>%  plot

dataP = data1 %>%  mutate(ifelse(n==0, 1, n))
coordinates(dataP)<- ~lat+long
proj4string(dataP) <- CRS("+init=epsg:28992")
proj4string(dataP) <- CRS(as.character(NA))
data.proj <- spTransform(dataP, CRS("+init=epsg:28992"))
data.proj
prueba <- cartogram(dataP, "n", itermax=2)


buildings_list <- split(data1,data1$id)
buildings_list <- lapply(buildings_list, function(x) { x["id"] <- NULL; x })
ps <- lapply(buildings_list, Polygon)

p1 <- lapply(seq_along(ps), function(i) Polygons(list(ps[[i]]), 
                                                 ID = names(buildings_list)[i]  ))

my_spatial_polys <- SpatialPolygons(p1, proj4string = CRS("+proj=longlat +datum=WGS84"))


my_spatial_polys_df <- SpatialPolygonsDataFrame(my_spatial_polys, 
                                                data.frame(id = unique(buildings_df$id), 
                                                           row.names = unique(buildings_df$id)))

                                    

prueba = Polygon(dataP, hole=as.logical(NA))
SpatialPolygons(prueba,proj4string=CRS(as.character(NA)))



world.carto <- quick.carto(map_dat, map_dat$size, blur = 0)
#plot(world.carto) #cartogram without anything
#spplot size, color
my.palette = c("#ff0000", "#ff8000", "#ffff00", "#bfff00","#00ff00") #red, orange, yellow, light green, dark green
spplot(world.carto, 'score', col.regions = my.palette, cuts = length(my.palette)-1,main="Choropleth of score and cartogram of size")



