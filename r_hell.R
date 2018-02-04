library(leaflet)
library(sf)
library(geojsonio)
library(data.table)
library(stringr)

# Chargement des datasets
# Données airbnb traitées sur Python
df <- fread("Downloads/dff.csv", sep=";")
df <- df[complete.cases(df), ]

#airbnb_quartiers <- geojsonio::geojson_read("Downloads/prix_airbnb_quartiers.geojson", what="sp")

# https://data.iledefrance.fr/explore/dataset/les_hotels_classes_en_ile-de-france/export/?refine.departement=75
hotel_paris <- geojsonio::geojson_read("Downloads/les_hotels_classes_en_ile-de-france.geojson-2.json", what="sp")
# https://opendata.paris.fr/explore/dataset/quartier_paris/export/?location=12,48.85889,2.34692
quartiers_paris <- sf::read_sf("Downloads/quartier_paris.geojson.json")
# https://opendata.paris.fr/explore/dataset/arrondissements/
arrondissements_paris <- geojsonio::geojson_read("Downloads/arrondissements.geojson.json", what="sp")
# https://www.data.gouv.fr/fr/datasets/population/#resource-eea96c00-49c8-4e0f-8b24-e88c6b624fad (TRAITEMENT SUR PYTHON)
population_paris <- fread("Downloads/population_paris.csv")

# Conversion des coordonnées 
df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
hotel_paris_sf <- st_as_sf(hotel_paris, coords = c("geo2", "geo1"), crs = 4326, agr = "constant")
arrondissements_paris_sf <- st_as_sf(arrondissements_paris, coords = c("geom_x_y2", "geom_x_y1"), crs = 4326, agr = "constant")

# Merge arronddissement / population pour récupérer la pop. par arrondissement dans le géojson
arrondissements_paris_sf <- merge(arrondissements_paris_sf, population_paris, by.x=c("l_ar"), by.y=c("nom_commune"))

# Palette de couleurs
pal <- colorNumeric("YlOrRd", NULL)




##### Répartition spatiale de l'offre AirBNB et de l'hotellerie
### Niveau: Quartiers
## AirBNB
quartiers_paris$airbnb_qrt <- lengths(st_covers(quartiers_paris, df_sf))
m_bnb_offre_quartiers <- leaflet(quartiers_paris) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(airbnb_qrt)) %>%
  addLegend(pal = pal,
            values = ~airbnb_qrt,
            opacity = 1.0,
            title = "Offre AirBnB / Quartiers")
m_bnb_offre_quartiers

## Hotellerie
quartiers_paris$hotels_qrt <- lengths(st_covers(quartiers_paris$geometry, hotel_paris_sf$geometry))
m_hotel_offre_quartiers <- leaflet(quartiers_paris) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(hotels_qrt)) %>% 
  addLegend(pal = pal,
            values = ~hotels_qrt,
            opacity = 1.0,
            title = "Offre Hotel / Quartiers")
m_hotel_offre_quartiers


### Niveau: Arrondissements
## AirBNB
arrondissements_paris_sf$airbnb_arrd <- lengths(st_covers(arrondissements_paris_sf, df_sf))

m_bnb_offre_arrd <- leaflet(arrondissements_paris_sf) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(airbnb_arrd)) %>%
  addLegend(pal = pal,
            values = ~airbnb_arrd,
            opacity = 1.0,
            title = "Offre AirBnB / Arrondissements")
m_bnb_offre_arrd

## Hotellerie
arrondissements_paris_sf$hotels_arrd <- lengths(st_covers(arrondissements_paris_sf, hotel_paris_sf))
m_hotel_offre_arrd <- leaflet(arrondissements_paris_sf) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white",
              dashArray = "3",
              weight=2,
              smoothFactor = 0.3,
              fillOpacity = 0.9,
              fillColor = ~pal(hotels_arrd)) %>% 
  addLegend(pal = pal,
            values = ~hotels_arrd,
            opacity = 1.0,
            title = "Offre Hotels / Arrondissements")
m_hotel_offre_arrd




### TO DO 
##### Répartition de l'offre AirBNB proportionnellement à la population résidente [QUARTIERS SEULEMENT]
# Densité airbnb/pop total
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
            title = "Densité  AirBnB / Quartiers")
m_bnb_densite_arrd

#m %>% addMarkers(df$longitude, df$latitude, clusterOptions= markerClusterOptions())

# Leaflet map : prix moyen par quartier
m_bnb_prix_quartier <- leaflet(airbnb_quartiers) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color="white", dashArray = "3", weight=2, smoothFactor = 0.3, fillOpacity = 0.9,fillColor = ~pal(meanprice)) %>%
  addLegend(pal = pal, values = ~meanprice, opacity = 1.0, title = "Prix moyen AirBnB / Quartiers")
m_bnb_prix_quartier

# Leaflet map : nombre d'offre par arrondissements




# Densité hotel/pop total
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


quartiers_paris$arrdt <- str_sub(quartiers_paris$c_quinsee, 0, 5)
