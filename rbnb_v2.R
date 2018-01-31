#ref : https://rcarto.github.io/caRtosm/index.html

#cleaning
rm(list = ls())

library("data.table") #to manage dataset

#spatial data
library("sf")

#nice plot of spacial data
library("cartography")

library("units")
library("osmdata")
library("tibble") #to read shp with sf

#working on density maps
library("spatstat")
library("maptools")
library("raster")


#working directory
setwd(dir="C:/Users/acer/Desktop/PROJET MEDAS - GEO")

#hotels
hotels <- sf::read_sf( "./COUCHES/les_hotels_classes_en_ile-de-france/les_hotels_classes_en_ile-de-france.shp")


# define a bounding box
q0 <- opq(bbox = c(2.2247, 48.8188, 2.4611, 48.9019)) 

# extract Paris boundaries
q1 <- add_osm_feature(opq = q0, key = 'name', value = "Paris")
res1 <- osmdata_sf(q1)
paris <- st_geometry(res1$osm_multipolygons[1,])

# extract the Seine river
q2 <- add_osm_feature(opq = q0, key = 'name', value = "La Seine")
res2 <- osmdata_sf(q2)
seine1 <- st_geometry(res2$osm_multilines)
q2b <- add_osm_feature(opq = q0, key = 'name', 
                       value = "La Seine - Bras de la Monnaie")
res2b <- osmdata_sf(q2b)
seine2 <- st_geometry(res2b$osm_lines)

# extract Parks and Cemetaries
q3 <- add_osm_feature(opq = q0, key = 'leisure', value = "park")
res3 <- osmdata_sf(q3)
parc1 <- st_geometry(res3$osm_polygons)
parc2 <- st_geometry(res3$osm_multipolygons)
q4 <- add_osm_feature(opq = q0, key = 'landuse', value = "cemetery")
res4 <- osmdata_sf(q4)
parc3 <- st_geometry(res4$osm_polygons)

# extract Quartiers
q5 <- add_osm_feature(opq = q0, key = 'admin_level', value = "10")
res5 <- osmdata_sf(q5)
quartier <- res5$osm_multipolygons


# use Lambert 93 projection (the french cartographic projection) for all layers
parc1 <- st_transform(parc1, 2154)
parc2 <- st_transform(parc2, 2154)
parc3 <- st_transform(parc3, 2154)
paris <- st_transform(paris, 2154)
seine1 <- st_transform(seine1, 2154)
seine2 <- st_transform(seine2, 2154)
quartier <- st_transform(quartier, 2154)
hotels <- st_transform(hotels, 2154)

# make layers pretty
## Parcs and cemetaries are merged into a single layer, we only keep objects 
## greater than 1 ha
parc <- do.call(c, list(parc1, parc2, parc3))
parc <- st_union(x = st_buffer(parc,0), by_feature = F)
parc <- st_cast(parc, "POLYGON")
parc <- parc[st_area(parc)>=set_units(10000, "m^2")]
parc <- st_intersection(x = parc, y = paris)

## We only keep the part of the river within Paris boundaries
seine <- st_intersection(x = seine1, y = paris)
seine <- c(st_cast(seine[1])[2:5], seine[2])
seine <-c(seine, seine2)

## We only keep the Paris quartiers
quartier <- quartier[substr(quartier$ref.INSEE,1,2)==75,]


## Keeping hotels in paris boundaries
hotels <- hotels[hotels$departement=="75" ,]


# save data to avoid over downloading
save(list= c("paris", "quartier", "seine", "parc", "hotels"), 
     file = "data", compress = "xz")

# /!\ loading treated data
load("data")


plot(paris)
plot(parc,add=TRUE,col="darkgreen")
plot(seine,add=TRUE,col="blue", lwd = 4)
plot(st_geometry(hotels),add=TRUE,col = "darkred", pch = 16, cex = 0.4)
plot(st_geometry(quartier), col = NA,lty = 2, lwd = 0.2, add=TRUE)

#récupérer les data d'un fichier sf
quartier_2 <- quartier
st_geometry(quartier_2) <- NULL
class(quartier_2)


quartier$nhotels <- lengths(st_covers(quartier, hotels$geometry))
quartier$dhotels <- quartier$nhotels / set_units(st_area(quartier), "km^2")

#how many hotel in the neighborood ?
par(mar = c(0,0,1.2,0))
plot(paris, col = "#D9D0C9", border = NA, bg = "#FBEDDA")
plot(parc, col = "#CDEBB2", border = NA, add=T)
plot(seine, col = "#AAD3DF", add=T, lwd = 4)
plot(st_geometry(quartier), col = NA,lty = 2, lwd = 0.2, add=T)
propSymbolsLayer(x = quartier, var = "nhotels", inches = 0.2, col = "red", 
                 legend.pos = "topright", 
                 legend.title.txt = "Number of hotels")
plot(paris, add=T, lwd = 0.7)
layoutLayer(title = "How Many Hotels in the Neighbourhood?", scale = 1,
            tabtitle = TRUE, frame = F) 


par(mar = c(0,0,1.2,0))
plot(paris, col = "#D9D0C9", border = NA, bg = "#FBEDDA")
choroLayer(quartier, var = "dhotels", border = NA, 
           method = "quantile", nclass = 6,
           col = carto.pal("wine.pal", 6),
           legend.pos = "topright",
           legend.title.txt = "Hotels bensity\nper km2", add = TRUE)
plot(parc, col = "#E2CCB5", border = NA, add=T)
plot(seine, col = "#AAD3DF", add=T, lwd = 4)
plot(st_geometry(quartier), col = NA,lty = 2, lwd = 0.2, add=T)
plot(paris, add=T, lwd = 0.7)
layoutLayer(title = "How Many Hotels in the Neighbourhood?", scale = 1,
            tabtitle = TRUE, frame = F) 


par(mar = c(0,0,1.2,0))
plot(paris, col = "#D9D0C9", border = NA, bg = "#FBEDDA")
plot(parc, col = "#CDEBB2", border = NA, add=T)
plot(seine, col = "#AAD3DF", add=T, lwd = 4)
plot(st_geometry(quartier), col = NA,lty = 2, lwd = 0.2, add=T)
propSymbolsChoroLayer(x = quartier, var = "nhotels", var2 = "dhotels",
                      inches = 0.2,
                      method = "quantile", nclass = 6,
                      col = carto.pal("wine.pal", 6),
                      legend.var2.pos  = "topright", 
                      legend.var2.title.txt = "Hotels bensity\nper km2",
                      legend.var.title.txt = "Number of Hotels")
plot(paris, add=T, lwd = 0.7)
layoutLayer(title = "How Many Hotels in the Neighbourhood?", scale = 1,
            tabtitle = TRUE, frame = F, 
            author = "Map data Â© OpenStreetMap contributors, under CC BY SA.", 
            sources = "cartography 2.0.2") 

#par quartier - densité et quantité



RenameVar <- function(data,vard,varq){
  
  
  
}

DensityAndQuantitybyScale <- function(ScaleMap=quartier, VarDensity , VarQuantity ){
par(mar = c(0,0,1.2,0))
plot(paris, col = "#D9D0C9", border = NA, bg = "#FBEDDA")
plot(parc, col = "#CDEBB2", border = NA, add=T)
plot(seine, col = "#AAD3DF", add=T, lwd = 4)
plot(st_geometry(ScaleMap), col = NA,lty = 2, lwd = 0.2, add=T)
propSymbolsChoroLayer(x = ScaleMap, var = "varq", var2 = "vard",
                      inches = 0.2,
                      method = "quantile", nclass = 6,
                      col = carto.pal("wine.pal", 6),
                      legend.var2.pos  = "topright", 
                      legend.var2.title.txt = "Hotels bensity\nper km2",
                      legend.var.title.txt = "Number of Hotels")
plot(paris, add=T, lwd = 0.7)
layoutLayer(title = "How Many Hotels in the Neighbourhood?", scale = 1,
            tabtitle = TRUE, frame = F, 
            author = "Map data Â© OpenStreetMap contributors, under CC BY SA.", 
            sources = "cartography 2.0.2")
}
#test de la fonction
DensityAndQuantitybyScale(ScaleMap=quartier,
                          VarDensity="nhotels",
                          VarQuantity="dhotels")

#identifier les lignes empty point
rows <- which(!is.na(st_dimension(st_sfc(hotels$geometry))))
bb <- as(st_geometry(hotels$geometry[rows]), "Spatial")


bbowin <- as.owin(as(paris, "Spatial"))
pts <- coordinates(bb)
p <- ppp(pts[,1], pts[,2], window=bbowin)

ds <- density.ppp(p, sigma = 300, eps = c(20,20))
rasdens <- raster(ds) * 1000 * 1000
rasdens <- rasdens+1
par(mar = c(0,0,1.2,0))
bks <- getBreaks(values(rasdens), nclass = 6, method = "equal")
cols <- colorRampPalette(c("black","#940000", "white"))(length(bks)-1)


plot(paris, col = NA, border = NA, main="", bg = "#FBEDDA")
plot(rasdens, breaks= bks, col=cols, add = TRUE,legend=F)
legendChoro(pos = "topright",cex = 0.7, title.cex = 0.7,
            title.txt = "Hotels density\nKDE, sigma=300m\n(hotels per km2)",
            breaks = bks-1, nodata = FALSE,values.rnd = 0,
            col = cols)
plot(seine, col = "#AAD3DF", add=TRUE, lwd = 4)
plot(parc, col = "#CDEBB235", border = NA, add=T)
plot(quartier$geometry, add=TRUE, border = "white",lty = 2, lwd = 0.05)
plot(st_geometry(hotels), add=TRUE, col = "#0000ff90", pch = 20, cex = 0.1)

plot(paris, col = NA, add=TRUE, border = "black",lwd = 1)

north(pos = c(661171.8,6858500))

layoutLayer(title = "Hotels Density in Paris", scale = 1,
            tabtitle = TRUE, frame = FALSE,
            author = "Map data Â© OpenStreetMap contributors, under CC BY SA.", 
            sources = "cartography 2.0.2") 

