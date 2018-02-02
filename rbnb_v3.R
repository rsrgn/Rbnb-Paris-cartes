
#-----cleaning-----
rm(list = ls())

#-----library----

library(sf)
library(leaflet)

#----working directory----
setwd(dir="C:/Users/acer/Desktop/PROJET MEDAS - GEO")

#----Loading data----

map <- leaflet() %>% setView(lng = 2.349014, lat = 48.864716, zoom = 11)
map %>% addProviderTiles(providers$Stamen.Toner)

#quartiers <- geojsonio::geojson_read("./COUCHES/quartier_paris.geojson", what = "sp")

# hotels
hotels <- sf::read_sf( "./COUCHES/les_hotels_classes_en_ile-de-france/les_hotels_classes_en_ile-de-france.shp")

#quartier
quartiers2 <- sf::read_sf("./COUCHES/quartier_paris.geojson")

#rbnb
rbnb_listings_sum  <- fread("./COUCHES/InsideRBNB/listings_summary.csv")

#----Transforming data----

rbnb_sf <- st_as_sf(rbnb_listings_sum, 
                    coords=c("longitude","latitude"), 
                    crs=4326)

class(quartiers2)
class(rbnb_sf)

quartiers2$nb_hotels <- lengths(st_covers(quartiers2, hotels))
quartiers2$nb_rbnb <- lengths(st_covers(quartiers2, rbnb_sf))
quartiers2$nb_rbnb

dev.off()
par(mar = c(0,0,0,0))
plot(quartiers2$geometry)
plot(head(rbnb_sf$geometry,800),add=TRUE)


#----Map----

map2 <- leaflet(quartiers2) %>% setView(lng = 2.349014, lat = 48.864716, zoom = 10) %>% addProviderTiles(providers$Stamen.Toner)
map2 %>% addPolygons(fillColor = ~pal(nb_rbnb),
                     weight = 2,
                     opacity = 1,
                     color = "white",
                     dashArray = "3",
                     fillOpacity = 0.7)





