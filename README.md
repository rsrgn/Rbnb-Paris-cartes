# Rbnb-Paris-cartes
Projet de cartographies de l'offre RBNB à Paris

Dans le cadre de ce projet scolaire de M1 nous avons travailler à la production de cartographie. Ce document est un résumé de notre démarche et des différents outils utilisés.

De manière générale, une fois les données sources récupérés, nouss avons travaillé à leur préparation avec Pyhton et R. Il s'agit essentiellement de nettoyage et d'agréations de données de provencances diverses. Ensuite nous avons créé des cartographies interactives en utilisant la librairie Leaflet de R. Puis nous avons utilisé Shiny pour publier ces cartes interactives sur le web avec notre rapport que nous avons créé avec R markdown. 

## Les sources de données

Notre principale source de données est le site inside Airbnb. Ce site réaliser un travail de collecte de données sur toutes les annonces airbnb publié dans certains grandes villes, dont Paris. 

http://insideairbnb.com/get-the-data.html

 D'autres sources de données nous ont permis également de préparer nos cartographies :
 
Les couches de polygones
* La carte des arrondissements : https://opendata.paris.fr/explore/dataset/arrondissements/
* La carte des quartiers administratifs de Paris : https://opendata.paris.fr/explore/dataset/quartier_paris/export/?location=12,48.85889,2.34692

Les couches de points
* Les hôtels : https://data.iledefrance.fr/explore/dataset/les_hotels_classes_en_ile-de-france/export/?refine.departement=75
* Les annonces Airbnb : http://data.insideairbnb.com/france/ile-de-france/paris/2017-04-04/data/listings.csv.gz 

Les autres sources de données :
* La population par tri iris issu du rensencement de 2014 :https://www.insee.fr/fr/statistiques/fichier/3137409/base-ic-evol-struct-pop-2014.zip
* Les loyers de références à Paris :https://www.data.gouv.fr/fr/datasets/donnees-de-lencadrement-des-loyers-a-paris-et-lille/
* Articles du JDN :http://www.journaldunet.com/economie/immobilier/1175834-location-airbnb-versus-location-classique/ 


## La préparation des données avec Python




## La préparation des données avec R 

Avec R le chargement des données cartographiques peut être assurés par différentes fonctions, nous avons utilisé la fonction geojson_read() de la librairie **geojsonio** pour réaliser les imports des fichiers au formats geojson. https://cran.r-project.org/web/packages/geojsonio/geojsonio.pdf

Dans d'autres cas nous avons également utilisé la fonction read_sf() de la librairie **sf**.

Pour les autres formats de fichiers (.csv) nous avons utilisé la fonction fread() du package **data.table** qui est réputé pour sa vitesse. https://cran.r-project.org/web/packages/data.table/data.table.pdf


## La cartographie interactive avec R

Pour les traitements géographiques nous avons utilisé principalement deux librairies :

* La librairie avec toutes les fonctionnalités de base pour les traitement géographique est **sf** --> https://cran.r-project.org/web/packages/sf/sf.pdf

Sf est la librairie qui contient toutes les fonction qui permettente de réaliser des intersections spaciales. C'est un outils de base qui permet de réaliser les principales opérations sur les données de type géographique. 

L'une des opérations les plus compliquée que nous avons eu à réaliser consistait à compter le nombre de chambre d'hotels par quartier. Le nombre de chambre d'hotel étant stocké dans un fichier géolocalisée par coordonnées gps et les découpages des quartiers étant stocké dans une autre fihiers. L'opération consistait donc à découpe le fichier des hotels par quartier en croisant les données des deux fichiers. Pour ce faire nous avont utilisé la fonction st_cover() du package sf : 

    #Calcul du nombre de chambres d'hotels par quartiers
    st_covers_HotelsByQuartiers <- st_covers(quartiers_paris$geometry, hotel_paris_sf$geometry)
    for(i in 1:length(st_covers_HotelsByQuartiers)){
      quartiers_paris$hotels_c_qrt[i] <- sum(
        hotel_paris_sf[["nombre_de_c"]][st_covers_HotelsByQuartiers[[i]]])
    }


* Pour réaliser les cartes interactives nous avons utilisé la librairie **leaflet** -->  https://cran.r-project.org/web/packages/leaflet/leaflet.pdf


## Le rapport ave R Markdown

## Le travail colaboratif avec GitHub

## La publication des cartes avec Shiny




