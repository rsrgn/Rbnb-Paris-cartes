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
# setwd(dir="C:/Users/acer/Desktop/PROJET MEDAS - GEO")
setwd("~/MEDAS/ApprochesSpatiales/Rbnb-Paris-cartes")

#----Chargement des datasets----

# Données airbnb traitées sur Python
df <- fread("./COUCHES/airbnb.csv", sep=";")

# https://data.iledefrance.fr/explore/dataset/les_hotels_classes_en_ile-de-france/export/?refine.departement=75
hotel_paris <- sf::read_sf( "./COUCHES/les_hotels_classes_en_ile-de-france/les_hotels_classes_en_ile-de-france.shp")
# hotel_paris <-geojsonio::geojson_read( "./COUCHES/les_hotels_classes_en_ile-de-france.geojson.json", what = "sp")

# https://opendata.paris.fr/explore/dataset/quartier_paris/export/?location=12,48.85889,2.34692
quartiers_paris <- sf::read_sf("./COUCHES/quartier_paris.geojson.json")

# https://opendata.paris.fr/explore/dataset/arrondissements/
arrondissements_paris <- geojsonio::geojson_read("./COUCHES/arrondissements.geojson.json",
                                                 what="sp",
                                                 encoding="UTF-8")

# https://www.data.gouv.fr/fr/datasets/population/#resource-eea96c00-49c8-4e0f-8b24-e88c6b624fad (TRAITEMENT SUR PYTHON)
population_paris <- fread("./COUCHES/population_paris.csv")

#INSEE donn? du recensement 2014
#https://www.insee.fr/fr/statistiques/fichier/3137409/base-ic-evol-struct-pop-2014.zip
#fichier transform? (supression des ent?tes et des m?tadonn?es) en .csv puis renom? en "pop_2014_insee.csv" 
pop2014 <- fread("./COUCHES/pop_2014_insee.csv")

# DataGouv + OSM (TRAITEMENT SUR PYTHON)
loyers_ref_paris <- fread("./COUCHES/loyer_par_quartier.csv", sep=";")
loyers_ref_paris <- loyers_ref_paris[,c('name', 'c_qu', 'ref')]

# https://jdn.carto.com/datasets
marge_airbnb <- geojsonio::geojson_read("./COUCHES/paris_les_quartiers_o_airbnb_rapporte_le_plus_compar_la_locatio.geojson.json", what="sp")

loueurs_suspects <- fread("./COUCHES/possibly_illegal_hosts.csv", sep=";")
loueurs_suspects_income <- fread("./COUCHES/possibly_illegal_hosts_monthly_income.csv", sep=";")
loueurs_suspects <- merge(loueurs_suspects, loueurs_suspects_income, by=c('host_id'))


#----correctrion d'encodage----
arrondissements_paris$l_ar <- as.character(arrondissements_paris$l_ar)
Encoding(arrondissements_paris$l_ar) <- "UTF-8"
arrondissements_paris$l_ar <- as.factor(arrondissements_paris$l_ar)

population_paris$nom_commune <- as.character(population_paris$nom_commune)
Encoding(population_paris$nom_commune) <- "UTF-8"
population_paris$nom_commune <- as.factor(population_paris$nom_commune)



#----preparation des donnees----

# Conversion des coordonnees 
df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
hotel_paris_sf <- st_as_sf(hotel_paris, coords = c("geo2", "geo1"), crs = 4326, agr = "constant")
arrondissements_paris_sf <- st_as_sf(arrondissements_paris, coords = c("geom_x_y2", "geom_x_y1"), crs = 4326, agr = "constant")

# Merge arrondissement / population pour recuperer la pop. par arrondissement dans le géojson
arrondissements_paris_sf <- merge(arrondissements_paris_sf, population_paris, by.x=c("l_ar"), by.y=c("nom_commune"))

# Merge loyers_ref / quartiers de paris pour recuperer le loyer de reference par quartiers
quartiers_paris <- merge(quartiers_paris, loyers_ref_paris, by=c('c_qu'))

#
quartiers_paris$airbnb_qrt <- lengths(st_covers(quartiers_paris, df_sf))
quartiers_paris$airbnb_density <- as.integer(as.numeric(quartiers_paris$airbnb_qrt / set_units(st_area(quartiers_paris), "km^2")))

#calcul nombre d'hotels par quartiers
quartiers_paris$hotels_qrt <- lengths(st_covers(quartiers_paris$geometry, hotel_paris_sf$geometry))

#Calcul densite d'hotel par km? par quartier
quartiers_paris$hotels_density <- as.numeric(quartiers_paris$hotels_qrt / set_units(st_area(quartiers_paris), "km^2"))

#Calcul du nombre de chambres d'hotels par quartiers
st_covers_HotelsByQuartiers <- st_covers(quartiers_paris$geometry, hotel_paris_sf$geometry)
for(i in 1:length(st_covers_HotelsByQuartiers)){
  quartiers_paris$hotels_c_qrt[i] <- sum(
    hotel_paris_sf[["nombre_de_c"]][st_covers_HotelsByQuartiers[[i]]])
}

#Calcul densite de chambres d'hotel par km? par quartier
quartiers_paris$hotels_c_density <- as.numeric(quartiers_paris$hotels_c_qrt / set_units(st_area(quartiers_paris), "km^2"))

#calcul nombre RBNB par arrondissement
arrondissements_paris_sf$airbnb_nb <- lengths(st_covers(arrondissements_paris_sf, df_sf))
#calcul densite RBNB par km2 par arrondissement 
arrondissements_paris_sf$airbnb_density <- as.integer(as.numeric(arrondissements_paris_sf$airbnb_nb / set_units(st_area(arrondissements_paris_sf), "km^2")))

#calcul nombre d'hotels par quartiers
quartiers_paris$hotels_qrt <- lengths(st_covers(quartiers_paris$geometry, hotel_paris_sf$geometry))

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

#Calcul densite d'hotel par hab par arrondissement
arrondissements_paris_sf$hotels_density_hab <- as.numeric(
  arrondissements_paris_sf$hotels_nb / arrondissements_paris_sf$pop_totale)

#Calcul densite de chambres d'hotel par habitant par quartier
arrondissements_paris_sf$hotels_c_density_hab <- as.numeric(
  arrondissements_paris_sf$hotels_c_nb / arrondissements_paris_sf$pop_totale)

#Calcul densite de chambres d'hotel par km? par quartier
arrondissements_paris_sf$hotels_c_density <- as.numeric(arrondissements_paris_sf$hotels_c_nb / set_units(st_area(arrondissements_paris_sf), "km^2"))

#calcul nombre RBNB par arrondissement
arrondissements_paris_sf$airbnb_nb <- lengths(st_covers(arrondissements_paris_sf, df_sf))

#calcul densite RBNB par Hab par arrondissement 
arrondissements_paris_sf$airbnb_density_hab <- as.numeric(
  arrondissements_paris_sf$airbnb_nb / arrondissements_paris_sf$pop_totale)

#calcul densite hab par km? par arrondissement 
arrondissements_paris_sf$density_hab_km <- as.numeric(
  arrondissements_paris_sf$pop_totale / set_units(st_area(arrondissements_paris_sf), "km^2"))

#prix moyen RBNB par quartier
mean_price_qtr <- list()

for (qtr_idx in 1:nrow(st_intersects(quartiers_paris, df_sf))){
  mean_price_qtr[[qtr_idx]] <- mean(df_sf[st_intersects(quartiers_paris, df_sf)[[qtr_idx]],,drop=F]$price)
}

quartiers_paris$prix_moyen <- as.integer(as.numeric(mean_price_qtr))

#prix moyen par arrondissement
mean_price_arrdt <- list()
for (arrdt_idx in 1:nrow(st_intersects(arrondissements_paris_sf, df_sf))){
  mean_price_arrdt[[arrdt_idx]] <- mean(df_sf[st_intersects(arrondissements_paris_sf, df_sf)[[arrdt_idx]],,drop=F]$price)
}
arrondissements_paris_sf$prix_moyen <- as.integer(as.numeric(mean_price_arrdt))

#Merge quartier / pop2014

pop2014_paris <- pop2014[pop2014$DEP=="75",]
pop2014_paris[,"P14_POP"] <- as.numeric(gsub(",", ".", as.matrix(pop2014_paris[,"P14_POP"])))
pop2014_paris$code_qrt <- as.numeric(substring(pop2014_paris$GRD_QUART, 6, 7))

for(i in 1:nrow(quartiers_paris)){
  quartiers_paris$pop2014[i] <- sum(
    pop2014_paris[pop2014_paris$code_qrt==quartiers_paris$c_qu[i],"P14_POP"]
  )
}

#calcul densite hab par km² par quartier 
quartiers_paris$density_hab_km <- as.numeric(
  quartiers_paris$pop2014 / set_units(st_area(quartiers_paris), "km^2"))
#Calcul densite de chambres d'hotel par habitant par quartier
quartiers_paris$hotels_c_density_hab <- as.numeric(
  quartiers_paris$hotels_c_qrt / quartiers_paris$pop2014)
#calcul densite RBNB par Hab par quartier 
quartiers_paris$airbnb_density_hab <- as.numeric(
  quartiers_paris$airbnb_qrt / quartiers_paris$pop2014)

hotel_paris <-geojsonio::geojson_read( "./COUCHES/les_hotels_classes_en_ile-de-france.geojson.json", what = "sp")
#----Sauvegarde des donn?es----
save(list= c("arrondissements_paris_sf", 
             "quartiers_paris",
             "loueurs_suspects",
             "marge_airbnb",
             "df",
             "hotel_paris"), 
     file = "data", compress = "xz")

#----Chargement des donn?es----
load("data")

