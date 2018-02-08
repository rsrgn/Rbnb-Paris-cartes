<<<<<<< HEAD
#-----Nettoyage de la memoire-----
rm(list = ls())

#-----Chargement des librairies----

library(leaflet)
library(sf)
library(geojsonio)
library(data.table)
library(stringr)
library(htmltools)
library(htmlwidgets)

library(lwgeom)
library(units)
#----Dossier de travail----
setwd(dir="~/MEDAS/ApprochesSpatiales/Rbnb-Paris-cartes")

#----Chargement des datasets----

# Donn√©es airbnb trait√©es sur Python
df <- fread("inputs/dff.csv", sep=";")
#df <- fread("./COUCHES/dff.csv", sep=";")
df <- df[complete.cases(df), ]

#airbnb_quartiers <- geojsonio::geojson_read("inputs/prix_airbnb_quartiers.geojson", what="sp")

# https://data.iledefrance.fr/explore/dataset/les_hotels_classes_en_ile-de-france/export/?refine.departement=75
hotel_paris <- geojsonio::geojson_read("inputs/les_hotels_classes_en_ile-de-france.geojson-2.json", what="sp")
#hotel_paris <- sf::read_sf( "./COUCHES/les_hotels_classes_en_ile-de-france/les_hotels_classes_en_ile-de-france.shp")

# https://opendata.paris.fr/explore/dataset/quartier_paris/export/?location=12,48.85889,2.34692
quartiers_paris <- sf::read_sf("inputs/quartier_paris.geojson.json")
#quartiers_paris <- sf::read_sf("./COUCHES/quartier_paris.geojson") 

# https://opendata.paris.fr/explore/dataset/arrondissements/
arrondissements_paris <- geojsonio::geojson_read("inputs/arrondissements.geojson.json", what="sp")
#arrondissements_paris <- geojsonio::geojson_read(x = "./COUCHES/arrondissements.geojson", 
#                                                 what = "sp", 
#                                                 encoding="UTF-8")

# https://www.data.gouv.fr/fr/datasets/population/#resource-eea96c00-49c8-4e0f-8b24-e88c6b624fad (TRAITEMENT SUR PYTHON)
population_paris <- fread("inputs/population_paris.csv")
#population_paris <- fread("./COUCHES/population_paris.csv")

# INSEE donn? du resensement 2014
#pop2014 <- fread("./COUCHES/pop_2014_insee.csv")

# Loueurs suspects (traitement sur python)
loueurs_suspects <- fread('inputs/possibly_illegal_hosts.csv', sep=";")
loueurs_suspects_income <- fread('inputs/possibly_illegal_hosts_monthly_income.csv', sep=';')


# DataGouv + OSM (TRAITEMENT SUR PYTHON)
loyers_ref_paris <- fread("inputs/loyer_par_quartier.csv", sep=";")
#loyers_ref_paris <- fread("./COUCHES/loyer_par_quartier.csv", sep=";")
loyers_ref_paris <- loyers_ref_paris[,c('name', 'c_qu', 'ref')]




#----correctrion d'encodage----
arrondissements_paris$l_ar <- as.character(arrondissements_paris$l_ar)
Encoding(arrondissements_paris$l_ar) <- "UTF-8"
arrondissements_paris$l_ar <- as.factor(arrondissements_paris$l_ar)

population_paris$nom_commune <- as.character(population_paris$nom_commune)
Encoding(population_paris$nom_commune) <- "UTF-8"
population_paris$nom_commune <- as.factor(population_paris$nom_commune)

#----preparation des donn√©es----

# Conversion des coordonn√©es 
df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
hotel_paris_sf <- st_as_sf(hotel_paris, coords = c("geo2", "geo1"), crs = 4326, agr = "constant")
arrondissements_paris_sf <- st_as_sf(arrondissements_paris, coords = c("geom_x_y2", "geom_x_y1"), crs = 4326, agr = "constant")

# Merge arrondissement / population pour r√©cup√©rer la pop. par arrondissement dans le g√©ojson
arrondissements_paris_sf <- merge(arrondissements_paris_sf, population_paris, by.x=c("l_ar"), by.y=c("nom_commune"))

# Merge loyers_ref / quartiers de paris pour r√©cup√©rer le loyer de ref√©rence par quartiers
quartiers_paris <- merge(quartiers_paris, loyers_ref_paris, by=c('c_qu'))

# Merge loueurs_suspects / loueurs_suspects_income
loueurs_suspects <- merge(loueurs_suspects, loueurs_suspects_income, by=c('host_id'))

# Palette de couleurs
pal <- colorNumeric("YlOrRd", NULL)


#----Question 1 - repartition spaciale RBNB VS HOTELERIE---- 

##### R√©partition spatiale de l'offre AirBNB et de l'hotellerie
### Niveau: Quartiers
## AirBNB par km¬≤

quartiers_paris$airbnb_qrt <- lengths(st_covers(quartiers_paris, df_sf))
quartiers_paris$airbnb_density <- as.numeric(quartiers_paris$airbnb_qrt / set_units(st_area(quartiers_paris), "km^2"))


labsQuartiersRbnb <- sprintf(
  "<strong>%s</strong><br/>%g offres",
  quartiers_paris$l_qu, quartiers_paris$airbnb_qrt
) %>% lapply(htmltools::HTML)

m_bnb_offre_quartiers <- leaflet(quartiers_paris) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(airbnb_qrt),
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labsQuartiersRbnb,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal,
            values = ~airbnb_qrt,
            opacity = 1.0,
            title = "Offre AirBnB / Quartiers")

m_bnb_offre_quartiers


labsQuartiersRbnb <- sprintf(
  "<strong>%s</strong><br/>%g offres RBNB par km?",
  quartiers_paris$l_qu, quartiers_paris$airbnb_density
) %>% lapply(htmltools::HTML)

#----Carte densit? rbnb VS densit? chambre d'hotels----

#calcul nombre d'hotels par quartiers
quartiers_paris$hotels_qrt <- lengths(st_covers(quartiers_paris$geometry, hotel_paris_sf$geometry))

#Calcul densite d'hotel par km? par quartier
quartiers_paris$hotels_density <- as.numeric(quartiers_paris$hotels_qrt / set_units(st_area(quartiers_paris), "km^2"))

#Calcul du nombre de chambres d'hotels par quartiers
st_covers_HotelsByQuartiers<- st_covers(quartiers_paris$geometry, hotel_paris_sf$geometry)
for(i in 1:length(st_covers_HotelsByQuartiers)){
  quartiers_paris$hotels_c_qrt[i] <- sum(
    hotel_paris_sf[["nombre_de_c"]][st_covers_HotelsByQuartiers[[i]]])
}

#Calcul densite de chambres d'hotel par km? par quartier
quartiers_paris$hotels_c_density <- as.numeric(quartiers_paris$hotels_c_qrt / set_units(st_area(quartiers_paris), "km^2"))


labsQuartiersHotels <- sprintf(
  "<strong>%s</strong><br/>%g Chambres d'hotel par km?",
  quartiers_paris$l_qu, quartiers_paris$hotels_c_density
) %>% lapply(htmltools::HTML)



m_bnb_offre_quartiers <- leaflet(quartiers_paris) %>%
  #font de carte
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  #Chorolopeth RBNB densite
  addPolygons(group="rbnb",
    color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(airbnb_density),
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labsQuartiersRbnb,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  #Legende RBNB densite
  addLegend(pal = pal,
            values = ~airbnb_density,
            opacity = 1.0,
            title = "Nombre d'annonces<br/>AirBnB par km?<br/>par quartiers",
            position = "bottomleft") %>%
  #Chorolopeh Hotels densite
  addPolygons(group="hotels", 
              color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(hotels_c_density),
              label = labsQuartiersHotels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  #Chorolopeh Hotels densite
  addLegend(pal = pal,
            values = ~hotels_c_density,
            opacity = 1.0,
            title = "Nombre de chambres<br/>d'hotels par km?<br/>par quartier",
            position = "bottomright") %>%
  # Layers control
  addLayersControl(
    baseGroups = c("rbnb", "hotels"),
    options = layersControlOptions(collapsed = FALSE),
    position = "topleft"
  )

m_bnb_offre_quartiers

#enregistrement de la carte au format html
saveWidget(widget=m_bnb_offre_quartiers , file="m_bnb_offre_quartiers.html")



#----Hotellerie----
quartiers_paris$hotels_qrt <- lengths(st_covers(quartiers_paris$geometry, hotel_paris_sf$geometry))


labsQuartiersHotels <- sprintf(
  "<strong>%s</strong><br/>%g hotels",
  quartiers_paris$l_qu, quartiers_paris$hotels_qrt
) %>% lapply(htmltools::HTML)

m_hotel_offre_quartiers <- leaflet(quartiers_paris) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(hotels_qrt),
              label = labs,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal,
            values = ~hotels_qrt,
            opacity = 1.0,
            title = "nombre d'hotels / Quartiers")

m_hotel_offre_quartiers

#----Carte densite RBNB par km2 par arrondissement----
### Niveau: Arrondissements
## AirBNB

#calcul nombre RBNB par arrondissement
arrondissements_paris_sf$airbnb_nb <- lengths(st_covers(arrondissements_paris_sf, df_sf))
#calcul densite RBNB par km2 par arrondissement 
arrondissements_paris_sf$airbnb_density <- as.numeric(arrondissements_paris_sf$airbnb_nb / set_units(st_area(arrondissements_paris_sf), "km^2"))


labs <- sprintf(
  "<strong>%s</strong><br/>%g annonces RBNB",
  arrondissements_paris_sf$l_ar, arrondissements_paris_sf$airbnb_density
) %>% lapply(htmltools::HTML)

m_bnb_offre_arrd <- leaflet(arrondissements_paris_sf) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(airbnb_density),
              label = labs,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal,
            values = ~airbnb_density,
            opacity = 1.0,
            title = "Nombre d'annonces<br/>AirBnB par km?<br/>par arrondissements")
m_bnb_offre_arrd

#----carte hotels par km2 par arrondissement----

#calcul nombre d'hotels par arrondissment
arrondissements_paris_sf$hotels_nb <- lengths(st_covers(arrondissements_paris_sf$geometry, hotel_paris_sf$geometry))

#Calcul densite d'hotel par km? par arrondissement
arrondissements_paris_sf$hotels_density <- as.numeric(arrondissements_paris_sf$hotels_nb / set_units(st_area(arrondissements_paris_sf), "km^2"))

#Calcul du nombre de chambres d'hotels par arrondissement
st_covers_HotelsByArdt <- st_covers(arrondissements_paris_sf$geometry, hotel_paris_sf$geometry)

for(i in 1:length(st_covers_HotelsByArdt)){
  arrondissements_paris_sf$hotels_c_nb[i] <- sum(
    hotel_paris_sf[["nombre_de_c"]][st_covers_HotelsByArdt[[i]]])
}

#Calcul densite de chambres d'hotel par km? par quartier
arrondissements_paris_sf$hotels_c_density <- as.numeric(arrondissements_paris_sf$hotels_c_nb / set_units(st_area(arrondissements_paris_sf), "km^2"))

labs <- sprintf(
  "<strong>%s</strong><br/>%g Nombre de<br/>chambres d'hotel par km?",
  arrondissements_paris_sf$l_ar, arrondissements_paris_sf$hotels_c_density
) %>% lapply(htmltools::HTML)

m_hotel_offre_arrd <- leaflet(arrondissements_paris_sf) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(hotels_c_density),
              label = labs,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal,
            values = ~hotels_c_density,
            opacity = 1.0,
            title = "Nombre de chambres<br/>d'hotel par km?<br/>par arrondissements")

m_hotel_offre_arrd


#----carte hotels par hab par arrondissement----

#calcul nombre d'hotels par arrondissment
arrondissements_paris_sf$hotels_nb <- lengths(st_covers(arrondissements_paris_sf$geometry, hotel_paris_sf$geometry))

#Calcul densite d'hotel par hab par arrondissement
arrondissements_paris_sf$hotels_density_hab <- as.numeric(
  arrondissements_paris_sf$hotels_nb / arrondissements_paris_sf$pop_totale)

#Calcul du nombre de chambres d'hotels par arrondissement
st_covers_HotelsByArdt <- st_covers(arrondissements_paris_sf$geometry, hotel_paris_sf$geometry)

for(i in 1:length(st_covers_HotelsByArdt)){
  arrondissements_paris_sf$hotels_c_nb[i] <- sum(
    hotel_paris_sf[["nombre_de_c"]][st_covers_HotelsByArdt[[i]]])
}

#Calcul densite de chambres d'hotel par habitant par quartier
arrondissements_paris_sf$hotels_c_density_hab <- as.numeric(
  arrondissements_paris_sf$hotels_c_nb / arrondissements_paris_sf$pop_totale)

labs <- sprintf(
  "<strong>%s</strong><br/>%g Nombre de<br/>chambres d'hotel par habitant",
  arrondissements_paris_sf$l_ar, arrondissements_paris_sf$hotels_c_density_hab
) %>% lapply(htmltools::HTML)

m_hotel_offre_arrd <- leaflet(arrondissements_paris_sf) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(hotels_c_density_hab),
              label = labs,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal,
            values = ~hotels_c_density_hab,
            opacity = 1.0,
            title = "Nombre de chambres<br/>d'hotel par habitant<br/>par arrondissements")

m_hotel_offre_arrd

#----Carte densite RBNB par hab par arrondissement----
### Niveau: Arrondissements
## AirBNB

#calcul nombre RBNB par arrondissement
arrondissements_paris_sf$airbnb_nb <- lengths(st_covers(arrondissements_paris_sf, df_sf))
#calcul densite RBNB par Hab par arrondissement 
arrondissements_paris_sf$airbnb_density_hab <- as.numeric(
  arrondissements_paris_sf$airbnb_nb / arrondissements_paris_sf$pop_totale)


labs <- sprintf(
  "<strong>%s</strong><br/>%g Nombre d'annonces<br/>RBNB par habitant",
  arrondissements_paris_sf$l_ar, arrondissements_paris_sf$airbnb_density_hab
) %>% lapply(htmltools::HTML)

m_bnb_offre_arrd <- leaflet(arrondissements_paris_sf) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(airbnb_density_hab),
              label = labs,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal,
            values = ~airbnb_density_hab,
            opacity = 1.0,
            title = "Nombre d'annonces<br/>AirBnB par habitant<br/>par arrondissements")
m_bnb_offre_arrd

#----Carte densite hab par km? arrondissement----
#Ce serait pas mal de calculer les surface hors parcs et cimeti?res

#calcul densite hab par km? par arrondissement 
arrondissements_paris_sf$density_hab_km <- as.numeric(
  arrondissements_paris_sf$pop_totale / set_units(st_area(arrondissements_paris_sf), "km^2"))

labs <- sprintf(
  "<strong>%s</strong><br/>%g habitants par km?",
  arrondissements_paris_sf$l_ar, arrondissements_paris_sf$density_hab_km
) %>% lapply(htmltools::HTML)

m_bnb_offre_arrd <- leaflet(arrondissements_paris_sf) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(density_hab_km),
              label = labs,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal,
            values = ~density_hab_km,
            opacity = 1.0,
            title = "Nombre d'habitants<br/>par km?<br/>par arrondissements")
m_bnb_offre_arrd


##### Prix moyen d'une nuit/personne 
### Niveau: Quartiers
mean_price_qtr <- list()

for (qtr_idx in 1:nrow(st_intersects(quartiers_paris, df_sf))){
  mean_price_qtr[[qtr_idx]] <- mean(df_sf[st_intersects(quartiers_paris, df_sf)[[qtr_idx]],,drop=F]$price)
}

quartiers_paris$prix_moyen <- as.integer(as.numeric(mean_price_qtr))

labs <- sprintf(
  "<strong>%s</strong><br/>Prix moyen : %g ‚Ç¨",
  quartiers_paris$l_qu, quartiers_paris$prix_moyen
) %>% lapply(htmltools::HTML)

m_bnb_prix_moyen_quartiers <- leaflet(quartiers_paris) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(prix_moyen),
              label = labs,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal,
            values = ~prix_moyen,
            opacity = 1.0,
            title = "Prix moyen nuit <br/>RBNB / quartiers")

m_bnb_prix_moyen_quartiers


### Niveau: Arrondissements
mean_price_arrdt <- list()
for (arrdt_idx in 1:nrow(st_intersects(arrondissements_paris_sf, df_sf))){
  mean_price_arrdt[[arrdt_idx]] <- mean(df_sf[st_intersects(arrondissements_paris_sf, df_sf)[[arrdt_idx]],,drop=F]$price)
}
arrondissements_paris_sf$prix_moyen <- as.integer(as.numeric(mean_price_arrdt))
labs <- sprintf(
  "<strong>%s</strong><br/>Prix moyen : %g ‚Ç¨",
  arrondissements_paris_sf$l_ar, arrondissements_paris_sf$prix_moyen
) %>% lapply(htmltools::HTML)
m_bnb_prix_moyen_arrdt <- leaflet(arrondissements_paris_sf) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(prix_moyen),
              label = labs,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal,
            values = ~prix_moyen,
            opacity = 1.0,
            title = "Prix moyen / arrdt")
m_bnb_prix_moyen_arrdt

######## Loueurs suspects
### Niveau: Quartiers
labs <- sprintf(
  "<strong>%s</strong><br/>Prix moyen : %g ‚Ç¨",
  quartiers_paris$l_qu, quartiers_paris$prix_moyen
) %>% lapply(htmltools::HTML)

mlabs <- sprintf(
  "<strong>HostID : %s</strong><br/>Estim. revenu mensuel : %g ‚Ç¨",
  loueurs_suspects$host_id, loueurs_suspects$estimated_monthly_income
) %>% lapply(htmltools::HTML)

m_bnb_loueurs_suspects_qtr <- leaflet(quartiers_paris) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(prix_moyen),
              label = labs,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal,
            values = ~prix_moyen,
            opacity = 1.0,
            title = "Prix moyen nuit <br/>RBNB / quartiers") %>%
  addMarkers(lng = ~loueurs_suspects$longitude,
             lat = ~loueurs_suspects$latitude,
             clusterOptions = markerClusterOptions(),
             label = mlabs,
             labelOptions = labelOptions(
               style = list("font-weight" = "normal", padding = "3px 8px"),
               textsize = "10px",
               direction = "auto"))

m_bnb_loueurs_suspects_qtr

### TO DO 
##### R√©partition de l'offre AirBNB proportionnellement √† la population r√©sidente [QUARTIERS SEULEMENT]
# Densit√© airbnb/pop total
quartiers_paris$densitebnb <- arrondissements_paris_sf$airbnb/arrondissements_paris_sf$pop_totale

# Leaflet map : nombre d'offre par arrd
m_bnb_densite_arrd <- leaflet(arrondissements_paris_sf) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(densitebnb)) %>%
  addLegend(pal = pal,
            values = ~densitebnb,
            opacity = 1.0,
            title = "Densit√©  AirBnB / Quartiers")
m_bnb_densite_arrd

#m %>% addMarkers(df$longitude, df$latitude, clusterOptions= markerClusterOptions())

# Leaflet map : prix moyen par quartier
m_bnb_prix_quartier <- leaflet(airbnb_quartiers) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white", dashArray = "3", weight=2, smoothFactor = 0.3, fillOpacity = 0.9,fillColor = ~pal(meanprice)) %>%
  addLegend(pal = pal, values = ~meanprice, opacity = 1.0, title = "Prix moyen AirBnB / Quartiers")
m_bnb_prix_quartier

# Leaflet map : nombre d'offre par arrondissements




# Densit√© hotel/pop total
arrondissements_paris_sf$densite_hotels <- arrondissements_paris_sf$hotels/arrondissements_paris_sf$pop_totale

# Leaflet map : nombre d'offre par arrd
m_bnb_densiteh_arrd <- leaflet(arrondissements_paris_sf) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(densite_hotels)) %>%
  addLegend(pal = pal,
            values = ~densite_hotels,
            opacity = 1.0)


m_bnb_densiteh_arrd







st_covers(arrondissements_paris_sf, hotel_paris_sf)
point.in.polygon(arrondissements_paris_sf, hotel_paris_sf)

class(arrondissements_paris_sf)
SpatialPolygonsDataFrame(arrondissements_paris_sf, hotel_paris_sf)

over(arrondissements_paris_sf, hotel_)
st_covers(quartiers_paris, arrondissements_paris_sf)


=======
#-----Nettoyage de la memoire-----
rm(list = ls())

#-----Chargement des librairies----

library(leaflet)
library(sf)
library(geojsonio)
library(data.table)
library(stringr)
library(htmltools)
library(htmlwidgets)

library(lwgeom)
library(units)
#----Dossier de travail----
setwd(dir="C:/Users/acer/Desktop/PROJET MEDAS - GEO")

#----Chargement des datasets----

# Donn√©es airbnb trait√©es sur Python
#df <- fread("Downloads/dff.csv", sep=";")
df <- fread("./COUCHES/dff.csv", sep=";")
df <- df[complete.cases(df), ]

#airbnb_quartiers <- geojsonio::geojson_read("Downloads/prix_airbnb_quartiers.geojson", what="sp")

# https://data.iledefrance.fr/explore/dataset/les_hotels_classes_en_ile-de-france/export/?refine.departement=75
#hotel_paris <- geojsonio::geojson_read("Downloads/les_hotels_classes_en_ile-de-france.geojson-2.json", what="sp")
hotel_paris <- sf::read_sf( "./COUCHES/les_hotels_classes_en_ile-de-france/les_hotels_classes_en_ile-de-france.shp")

# https://opendata.paris.fr/explore/dataset/quartier_paris/export/?location=12,48.85889,2.34692
#quartiers_paris <- sf::read_sf("Downloads/quartier_paris.geojson.json")
quartiers_paris <- sf::read_sf("./COUCHES/quartier_paris.geojson") 

# https://opendata.paris.fr/explore/dataset/arrondissements/
#arrondissements_paris <- geojsonio::geojson_read("Downloads/arrondissements.geojson.json", what="sp")
arrondissements_paris <- geojsonio::geojson_read(x = "./COUCHES/arrondissements.geojson", 
                                                 what = "sp", 
                                                 encoding="UTF-8")

# https://www.data.gouv.fr/fr/datasets/population/#resource-eea96c00-49c8-4e0f-8b24-e88c6b624fad (TRAITEMENT SUR PYTHON)
#population_paris <- fread("Downloads/population_paris.csv")
population_paris <- fread("./COUCHES/population_paris.csv")

#INSEE donnÈ du resensement 2014
#https://www.insee.fr/fr/statistiques/fichier/3137409/base-ic-evol-struct-pop-2014.zip
#fichier transformÈ (supression des entÍtes et des mÈtadonnÈes) en .csv puis renomÈ en "pop_2014_insee.csv" 
pop2014 <- fread("./COUCHES/pop_2014_insee.csv")


# DataGouv + OSM (TRAITEMENT SUR PYTHON)
#loyers_ref_paris <- fread("Downloads/loyer_par_quartier.csv", sep=";")
loyers_ref_paris <- fread("./COUCHES/loyer_par_quartier.csv", sep=";")
loyers_ref_paris <- loyers_ref_paris[,c('name', 'c_qu', 'ref')]




#----correctrion d'encodage----
arrondissements_paris$l_ar <- as.character(arrondissements_paris$l_ar)
Encoding(arrondissements_paris$l_ar) <- "UTF-8"
arrondissements_paris$l_ar <- as.factor(arrondissements_paris$l_ar)

population_paris$nom_commune <- as.character(population_paris$nom_commune)
Encoding(population_paris$nom_commune) <- "UTF-8"
population_paris$nom_commune <- as.factor(population_paris$nom_commune)

#----preparation des donn√©es----

# Conversion des coordonn√©es 
df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
hotel_paris_sf <- st_as_sf(hotel_paris, coords = c("geo2", "geo1"), crs = 4326, agr = "constant")
arrondissements_paris_sf <- st_as_sf(arrondissements_paris, coords = c("geom_x_y2", "geom_x_y1"), crs = 4326, agr = "constant")

# Merge arrondissement / population pour r√©cup√©rer la pop. par arrondissement dans le g√©ojson
arrondissements_paris_sf <- merge(arrondissements_paris_sf, population_paris, by.x=c("l_ar"), by.y=c("nom_commune"))

# Merge loyers_ref / quartiers de paris pour r√©cup√©rer le loyer de ref√©rence par quartiers
quartiers_paris <- merge(quartiers_paris, loyers_ref_paris, by=c('c_qu'))

# Palette de couleurs
pal <- colorNumeric("YlOrRd", NULL)


#----Question 1 - repartition spaciale RBNB VS HOTELERIE---- 

##### R√©partition spatiale de l'offre AirBNB et de l'hotellerie
### Niveau: Quartiers
## AirBNB par km¬≤

quartiers_paris$airbnb_qrt <- lengths(st_covers(quartiers_paris, df_sf))
quartiers_paris$airbnb_density <- as.numeric(quartiers_paris$airbnb_qrt / set_units(st_area(quartiers_paris), "km^2"))


labsQuartiersRbnb <- sprintf(
  "<strong>%s</strong><br/>%g offres",
  quartiers_paris$l_qu, quartiers_paris$airbnb_qrt
) %>% lapply(htmltools::HTML)

m_bnb_offre_quartiers <- leaflet(quartiers_paris) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(airbnb_qrt),
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labsQuartiersRbnb,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal,
            values = ~airbnb_qrt,
            opacity = 1.0,
            title = "Offre AirBnB / Quartiers")

m_bnb_offre_quartiers


labsQuartiersRbnb <- sprintf(
  "<strong>%s</strong><br/>%g offres RBNB par km≤",
  quartiers_paris$l_qu, quartiers_paris$airbnb_density
) %>% lapply(htmltools::HTML)

#----Carte densitÈ rbnb VS densitÈ chambre d'hotels----

#calcul nombre d'hotels par quartiers
quartiers_paris$hotels_qrt <- lengths(st_covers(quartiers_paris$geometry, hotel_paris_sf$geometry))

#Calcul densite d'hotel par km≤ par quartier
quartiers_paris$hotels_density <- as.numeric(quartiers_paris$hotels_qrt / set_units(st_area(quartiers_paris), "km^2"))

#Calcul du nombre de chambres d'hotels par quartiers
st_covers_HotelsByQuartiers<- st_covers(quartiers_paris$geometry, hotel_paris_sf$geometry)
for(i in 1:length(st_covers_HotelsByQuartiers)){
  quartiers_paris$hotels_c_qrt[i] <- sum(
    hotel_paris_sf[["nombre_de_c"]][st_covers_HotelsByQuartiers[[i]]])
}

#Calcul densite de chambres d'hotel par km≤ par quartier
quartiers_paris$hotels_c_density <- as.numeric(quartiers_paris$hotels_c_qrt / set_units(st_area(quartiers_paris), "km^2"))


labsQuartiersHotels <- sprintf(
  "<strong>%s</strong><br/>%g Chambres d'hotel par km≤",
  quartiers_paris$l_qu, quartiers_paris$hotels_c_density
) %>% lapply(htmltools::HTML)



m_bnb_offre_quartiers <- leaflet(quartiers_paris) %>%
  #font de carte
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  #Chorolopeth RBNB densite
  addPolygons(group="rbnb",
    color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(airbnb_density),
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labsQuartiersRbnb,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  #Legende RBNB densite
  addLegend(pal = pal,
            values = ~airbnb_density,
            opacity = 1.0,
            title = "Nombre d'annonces<br/>AirBnB par km≤<br/>par quartiers",
            position = "bottomleft") %>%
  #Chorolopeh Hotels densite
  addPolygons(group="hotels", 
              color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(hotels_c_density),
              label = labsQuartiersHotels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  #Chorolopeh Hotels densite
  addLegend(pal = pal,
            values = ~hotels_c_density,
            opacity = 1.0,
            title = "Nombre de chambres<br/>d'hotels par km≤<br/>par quartier",
            position = "bottomright") %>%
  # Layers control
  addLayersControl(
    baseGroups = c("rbnb", "hotels"),
    options = layersControlOptions(collapsed = FALSE),
    position = "topleft"
  )

m_bnb_offre_quartiers

#enregistrement de la carte au format html
saveWidget(widget=m_bnb_offre_quartiers , file="m_bnb_offre_quartiers.html")



#----Hotellerie----
quartiers_paris$hotels_qrt <- lengths(st_covers(quartiers_paris$geometry, hotel_paris_sf$geometry))


labsQuartiersHotels <- sprintf(
  "<strong>%s</strong><br/>%g hotels",
  quartiers_paris$l_qu, quartiers_paris$hotels_qrt
) %>% lapply(htmltools::HTML)

m_hotel_offre_quartiers <- leaflet(quartiers_paris) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(hotels_qrt),
              label = labs,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal,
            values = ~hotels_qrt,
            opacity = 1.0,
            title = "nombre d'hotels / Quartiers")

m_hotel_offre_quartiers

#----Carte densite RBNB par km2 par arrondissement----
### Niveau: Arrondissements
## AirBNB

#calcul nombre RBNB par arrondissement
arrondissements_paris_sf$airbnb_nb <- lengths(st_covers(arrondissements_paris_sf, df_sf))
#calcul densite RBNB par km2 par arrondissement 
arrondissements_paris_sf$airbnb_density <- as.numeric(arrondissements_paris_sf$airbnb_nb / set_units(st_area(arrondissements_paris_sf), "km^2"))


labs <- sprintf(
  "<strong>%s</strong><br/>%g annonces RBNB",
  arrondissements_paris_sf$l_ar, arrondissements_paris_sf$airbnb_density
) %>% lapply(htmltools::HTML)

m_bnb_offre_arrd <- leaflet(arrondissements_paris_sf) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(airbnb_density),
              label = labs,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal,
            values = ~airbnb_density,
            opacity = 1.0,
            title = "Nombre d'annonces<br/>AirBnB par km≤<br/>par arrondissements")
m_bnb_offre_arrd

#----carte hotels par km2 par arrondissement----

#calcul nombre d'hotels par arrondissment
arrondissements_paris_sf$hotels_nb <- lengths(st_covers(arrondissements_paris_sf$geometry, hotel_paris_sf$geometry))

#Calcul densite d'hotel par km≤ par arrondissement
arrondissements_paris_sf$hotels_density <- as.numeric(arrondissements_paris_sf$hotels_nb / set_units(st_area(arrondissements_paris_sf), "km^2"))

#Calcul du nombre de chambres d'hotels par arrondissement
st_covers_HotelsByArdt <- st_covers(arrondissements_paris_sf$geometry, hotel_paris_sf$geometry)

for(i in 1:length(st_covers_HotelsByArdt)){
  arrondissements_paris_sf$hotels_c_nb[i] <- sum(
    hotel_paris_sf[["nombre_de_c"]][st_covers_HotelsByArdt[[i]]])
}

#Calcul densite de chambres d'hotel par km≤ par quartier
arrondissements_paris_sf$hotels_c_density <- as.numeric(arrondissements_paris_sf$hotels_c_nb / set_units(st_area(arrondissements_paris_sf), "km^2"))

labs <- sprintf(
  "<strong>%s</strong><br/>%g Nombre de<br/>chambres d'hotel par km≤",
  arrondissements_paris_sf$l_ar, arrondissements_paris_sf$hotels_c_density
) %>% lapply(htmltools::HTML)

m_hotel_offre_arrd <- leaflet(arrondissements_paris_sf) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(hotels_c_density),
              label = labs,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal,
            values = ~hotels_c_density,
            opacity = 1.0,
            title = "Nombre de chambres<br/>d'hotel par km≤<br/>par arrondissements")

m_hotel_offre_arrd


#----carte hotels par hab par arrondissement----

#calcul nombre d'hotels par arrondissment
arrondissements_paris_sf$hotels_nb <- lengths(st_covers(arrondissements_paris_sf$geometry, hotel_paris_sf$geometry))

#Calcul densite d'hotel par hab par arrondissement
arrondissements_paris_sf$hotels_density_hab <- as.numeric(
  arrondissements_paris_sf$hotels_nb / arrondissements_paris_sf$pop_totale)

#Calcul du nombre de chambres d'hotels par arrondissement
st_covers_HotelsByArdt <- st_covers(arrondissements_paris_sf$geometry, hotel_paris_sf$geometry)

for(i in 1:length(st_covers_HotelsByArdt)){
  arrondissements_paris_sf$hotels_c_nb[i] <- sum(
    hotel_paris_sf[["nombre_de_c"]][st_covers_HotelsByArdt[[i]]])
}

#Calcul densite de chambres d'hotel par habitant par quartier
arrondissements_paris_sf$hotels_c_density_hab <- as.numeric(
  arrondissements_paris_sf$hotels_c_nb / arrondissements_paris_sf$pop_totale)

labs <- sprintf(
  "<strong>%s</strong><br/>%g Nombre de<br/>chambres d'hotel par habitant",
  arrondissements_paris_sf$l_ar, arrondissements_paris_sf$hotels_c_density_hab
) %>% lapply(htmltools::HTML)

m_hotel_offre_arrd <- leaflet(arrondissements_paris_sf) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(hotels_c_density_hab),
              label = labs,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal,
            values = ~hotels_c_density_hab,
            opacity = 1.0,
            title = "Nombre de chambres<br/>d'hotel par habitant<br/>par arrondissements")

m_hotel_offre_arrd

#----Carte densite RBNB par hab par arrondissement----
### Niveau: Arrondissements
## AirBNB

#calcul nombre RBNB par arrondissement
arrondissements_paris_sf$airbnb_nb <- lengths(st_covers(arrondissements_paris_sf, df_sf))
#calcul densite RBNB par Hab par arrondissement 
arrondissements_paris_sf$airbnb_density_hab <- as.numeric(
  arrondissements_paris_sf$airbnb_nb / arrondissements_paris_sf$pop_totale)


labs <- sprintf(
  "<strong>%s</strong><br/>%g Nombre d'annonces<br/>RBNB par habitant",
  arrondissements_paris_sf$l_ar, arrondissements_paris_sf$airbnb_density_hab
) %>% lapply(htmltools::HTML)

m_bnb_offre_arrd <- leaflet(arrondissements_paris_sf) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(airbnb_density_hab),
              label = labs,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal,
            values = ~airbnb_density_hab,
            opacity = 1.0,
            title = "Nombre d'annonces<br/>AirBnB par habitant<br/>par arrondissements")
m_bnb_offre_arrd

#----Carte densite hab par km≤ arrondissement----
#Ce serait pas mal de calculer les surface hors parcs et cimetiËres

#calcul densite hab par km≤ par arrondissement 
arrondissements_paris_sf$density_hab_km <- as.numeric(
  arrondissements_paris_sf$pop_totale / set_units(st_area(arrondissements_paris_sf), "km^2"))

labs <- sprintf(
  "<strong>%s</strong><br/>%g habitants par km≤",
  arrondissements_paris_sf$l_ar, arrondissements_paris_sf$density_hab_km
) %>% lapply(htmltools::HTML)

m_bnb_offre_arrd <- leaflet(arrondissements_paris_sf) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(density_hab_km),
              label = labs,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal,
            values = ~density_hab_km,
            opacity = 1.0,
            title = "Nombre d'habitants<br/>par km≤<br/>par arrondissements")
m_bnb_offre_arrd


##### Prix moyen d'une nuit/personne 
### Niveau: Quartiers
mean_price_qtr <- list()

for (qtr_idx in 1:nrow(st_intersects(quartiers_paris, df_sf))){
  mean_price_qtr[[qtr_idx]] <- mean(df_sf[st_intersects(quartiers_paris, df_sf)[[qtr_idx]],,drop=F]$price)
}

quartiers_paris$prix_moyen <- as.integer(as.numeric(mean_price_qtr))

labs <- sprintf(
  "<strong>%s</strong><br/>Prix moyen : %g ‚Ç¨",
  quartiers_paris$l_qu, quartiers_paris$prix_moyen
) %>% lapply(htmltools::HTML)

m_bnb_prix_moyen_quartiers <- leaflet(quartiers_paris) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(prix_moyen),
              label = labs,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal,
            values = ~prix_moyen,
            opacity = 1.0,
            title = "Prix moyen nuit <br/>RBNB / quartiers")

m_bnb_prix_moyen_quartiers


### Niveau: Arrondissements
mean_price_arrdt <- list()
for (arrdt_idx in 1:nrow(st_intersects(arrondissements_paris_sf, df_sf))){
  mean_price_arrdt[[arrdt_idx]] <- mean(df_sf[st_intersects(arrondissements_paris_sf, df_sf)[[arrdt_idx]],,drop=F]$price)
}
arrondissements_paris_sf$prix_moyen <- as.integer(as.numeric(mean_price_arrdt))
labs <- sprintf(
  "<strong>%s</strong><br/>Prix moyen : %g ‚Ç¨",
  arrondissements_paris_sf$l_ar, arrondissements_paris_sf$prix_moyen
) %>% lapply(htmltools::HTML)
m_bnb_prix_moyen_arrdt <- leaflet(arrondissements_paris_sf) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(prix_moyen),
              label = labs,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal,
            values = ~prix_moyen,
            opacity = 1.0,
            title = "Prix moyen / quartiers")
m_bnb_prix_moyen_arrdt



### TO DO 
##### R√©partition de l'offre AirBNB proportionnellement √† la population r√©sidente [QUARTIERS SEULEMENT]
# Densit√© airbnb/pop total
quartiers_paris$densitebnb <- arrondissements_paris_sf$airbnb/arrondissements_paris_sf$pop_totale

# Leaflet map : nombre d'offre par arrd
m_bnb_densite_arrd <- leaflet(arrondissements_paris_sf) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(densitebnb)) %>%
  addLegend(pal = pal,
            values = ~densitebnb,
            opacity = 1.0,
            title = "Densit√©  AirBnB / Quartiers")
m_bnb_densite_arrd

#m %>% addMarkers(df$longitude, df$latitude, clusterOptions= markerClusterOptions())

# Leaflet map : prix moyen par quartier
m_bnb_prix_quartier <- leaflet(airbnb_quartiers) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white", dashArray = "3", weight=2, smoothFactor = 0.3, fillOpacity = 0.9,fillColor = ~pal(meanprice)) %>%
  addLegend(pal = pal, values = ~meanprice, opacity = 1.0, title = "Prix moyen AirBnB / Quartiers")
m_bnb_prix_quartier

# Leaflet map : nombre d'offre par arrondissements




# Densit√© hotel/pop total
arrondissements_paris_sf$densite_hotels <- arrondissements_paris_sf$hotels/arrondissements_paris_sf$pop_totale

# Leaflet map : nombre d'offre par arrd
m_bnb_densiteh_arrd <- leaflet(arrondissements_paris_sf) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(densite_hotels)) %>%
  addLegend(pal = pal,
            values = ~densite_hotels,
            opacity = 1.0)


m_bnb_densiteh_arrd







st_covers(arrondissements_paris_sf, hotel_paris_sf)
point.in.polygon(arrondissements_paris_sf, hotel_paris_sf)

class(arrondissements_paris_sf)
SpatialPolygonsDataFrame(arrondissements_paris_sf, hotel_paris_sf)

over(arrondissements_paris_sf, hotel_)
st_covers(quartiers_paris, arrondissements_paris_sf)


>>>>>>> 7e2a63ca72cadc7cb1a61448c76b45e256ecdc01
quartiers_paris$arrdt <- str_sub(quartiers_paris$c_quinsee, 0, 5)